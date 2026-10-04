use tree_sitter::Node;

use crate::{
    position::{ShaderPosition, ShaderRange},
    shader_error::ShaderError,
    symbols::{
        shader_module::ShaderModule,
        symbol_parser::{get_name, ShaderWordRange, SymbolWordProvider},
    },
};

pub struct WgslSymbolWordProvider {}

impl WgslSymbolWordProvider {
    fn new_word(
        shader_module: &ShaderModule,
        node: Node,
        parent: Option<ShaderWordRange>,
    ) -> ShaderWordRange {
        ShaderWordRange::new(
            get_name(&shader_module.content, node).into(),
            ShaderRange::from(node.range()),
            parent,
        )
    }
    /// Get the word chain of an expression, such as `a.b.c` where c has b as parent, which has a as parent.
    fn get_expression_word(shader_module: &ShaderModule, node: Node) -> Option<ShaderWordRange> {
        match node.kind() {
            "identifier" => Some(Self::new_word(shader_module, node, None)),
            "named_component_expression" => {
                let value = node.child_by_field_name("value")?;
                let component = node.child_by_field_name("component")?;
                let parent = Self::get_expression_word(shader_module, value)?;
                Some(Self::new_word(shader_module, component, Some(parent)))
            }
            // Chain from the function, symbol lookup will use its return type.
            "call_expression" => {
                let mut cursor = node.walk();
                let callee = node
                    .named_children(&mut cursor)
                    .find(|child| child.kind() == "identifier")?;
                Some(Self::new_word(shader_module, callee, None))
            }
            // Element type is not tracked, use the array itself.
            "indexing_expression" => {
                Self::get_expression_word(shader_module, node.child_by_field_name("value")?)
            }
            "paren_expression" => Self::get_expression_word(shader_module, node.named_child(0)?),
            _ => None,
        }
    }
}

impl SymbolWordProvider for WgslSymbolWordProvider {
    fn find_word_at_position_in_node(
        &self,
        shader_module: &ShaderModule,
        node: Node,
        position: &ShaderPosition,
    ) -> Result<ShaderWordRange, ShaderError> {
        let point = tree_sitter::Point::new(position.line as usize, position.pos as usize);
        let is_word = |node: &Node| matches!(node.kind(), "identifier" | "swizzle_name");
        let word_node = match node.named_descendant_for_point_range(point, point) {
            Some(word_node) if is_word(&word_node) => word_node,
            // Cursor might be right after the word.
            _ if point.column > 0 => {
                let point = tree_sitter::Point::new(point.row, point.column - 1);
                match node.named_descendant_for_point_range(point, point) {
                    Some(word_node) if is_word(&word_node) => word_node,
                    _ => return Err(ShaderError::NoSymbol),
                }
            }
            _ => return Err(ShaderError::NoSymbol),
        };
        // If its a member of an expression, chain it with its parents.
        if let Some(parent) = word_node.parent() {
            if parent.kind() == "named_component_expression"
                && parent
                    .child_by_field_name("component")
                    .map(|component| component.id() == word_node.id())
                    .unwrap_or(false)
            {
                return Self::get_expression_word(shader_module, parent)
                    .ok_or(ShaderError::NoSymbol);
            }
        }
        Ok(Self::new_word(shader_module, word_node, None))
    }
}
