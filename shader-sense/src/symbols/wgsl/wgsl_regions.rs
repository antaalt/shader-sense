use std::collections::HashMap;

use tree_sitter::Node;

use crate::{
    position::{ShaderPosition, ShaderRange},
    shader::ShaderCompilationParams,
    shader_error::ShaderError,
    symbols::{
        prepocessor::{ShaderPreprocessor, ShaderPreprocessorContext, ShaderRegion},
        shader_module::{ShaderModule, ShaderSymbols},
        symbol_parser::{get_name, SymbolRegionFinder},
        symbol_provider::{SymbolIncludeCallback, SymbolProvider},
    },
};

/// Find regions from WESL conditional compilation attributes (@if, @elif & @else).
/// Feature flags are read from defines, and are disabled if not defined.
pub struct WgslRegionFinder {}

impl WgslRegionFinder {
    fn is_feature_enabled(context: &ShaderPreprocessorContext, feature: &str) -> bool {
        match context.get_define_value(feature) {
            Some(value) => !matches!(value.trim().to_lowercase().as_str(), "0" | "false"),
            None => false,
        }
    }
    /// Evaluate a feature expression such as `debug && !(web || legacy)`.
    fn evaluate(content: &str, node: Node, context: &ShaderPreprocessorContext) -> bool {
        let evaluate_field = |field: &str| {
            node.child_by_field_name(field)
                .map(|child| Self::evaluate(content, child, context))
                .unwrap_or(false)
        };
        match node.kind() {
            "identifier" => Self::is_feature_enabled(context, get_name(content, node)),
            "bool_literal" => get_name(content, node) == "true",
            "paren_expression" => node
                .named_child(0)
                .map(|child| Self::evaluate(content, child, context))
                .unwrap_or(false),
            "unary_expression" => !evaluate_field("operand"),
            "binary_expression" => {
                let operator = node
                    .child_by_field_name("operator")
                    .map(|operator| get_name(content, operator))
                    .unwrap_or("");
                match operator {
                    "&&" => evaluate_field("left") && evaluate_field("right"),
                    "||" => evaluate_field("left") || evaluate_field("right"),
                    _ => false,
                }
            }
            _ => false,
        }
    }
    fn is_skipped_sibling(node: &Node) -> bool {
        node.kind() == "attribute" || node.is_extra()
    }
    /// Get the node an attribute applies to.
    fn get_decorated_node(attribute: Node) -> Option<Node> {
        let parent = attribute.parent()?;
        // Statement attributes are siblings of their statement in the compound statement, after its opening bracket.
        // Attributes before the bracket apply to the compound statement itself.
        let is_statement_attribute = parent.kind() == "compound_statement" && {
            let mut cursor = parent.walk();
            let bracket = parent
                .children(&mut cursor)
                .find(|child| child.kind() == "{");
            bracket
                .map(|bracket| attribute.start_byte() > bracket.start_byte())
                .unwrap_or(false)
        };
        if is_statement_attribute {
            let mut sibling = attribute.next_named_sibling();
            while let Some(node) = sibling {
                if !Self::is_skipped_sibling(&node) {
                    return Some(node);
                }
                sibling = node.next_named_sibling();
            }
            None
        } else {
            Some(parent)
        }
    }
    /// Get the node preceding a decorated node, that might hold the @if of an @elif or @else.
    fn get_previous_node(node: Node) -> Option<Node> {
        let mut sibling = node.prev_named_sibling();
        while let Some(previous) = sibling {
            if !Self::is_skipped_sibling(&previous) {
                return Some(previous);
            }
            sibling = previous.prev_named_sibling();
        }
        None
    }
    fn collect_conditional_attributes<'a>(
        content: &str,
        node: Node<'a>,
        attributes: &mut Vec<(Node<'a>, String)>,
    ) {
        if node.kind() == "attribute" {
            if let Some(name) = node.child_by_field_name("name") {
                let name = get_name(content, name);
                if matches!(name, "if" | "elif" | "else") {
                    attributes.push((node, name.into()));
                }
            }
            return;
        }
        let mut cursor = node.walk();
        for child in node.named_children(&mut cursor) {
            Self::collect_conditional_attributes(content, child, attributes);
        }
    }
}

impl SymbolRegionFinder for WgslRegionFinder {
    fn query_regions_in_node<'a>(
        &self,
        shader_module: &ShaderModule,
        _symbol_provider: &SymbolProvider,
        _shader_params: &ShaderCompilationParams,
        node: tree_sitter::Node,
        _preprocessor: &mut ShaderPreprocessor,
        context: &'a mut ShaderPreprocessorContext,
        _include_callback: &'a mut SymbolIncludeCallback<'a>,
        _old_symbols: Option<ShaderSymbols>,
    ) -> Result<Vec<ShaderRegion>, ShaderError> {
        let content = &shader_module.content;
        // Attributes are collected in document order, so an @if is always processed before its @elif & @else.
        let mut attributes = Vec::new();
        Self::collect_conditional_attributes(content, node, &mut attributes);
        // For each decorated node, whether a branch of its @if / @elif / @else chain was taken.
        let mut chains: HashMap<usize, bool> = HashMap::new();
        let mut regions: Vec<ShaderRegion> = Vec::new();
        for (attribute, name) in attributes {
            let Some(decorated_node) = Self::get_decorated_node(attribute) else {
                continue;
            };
            let condition = || {
                let mut cursor = attribute.walk();
                let arguments = attribute
                    .named_children(&mut cursor)
                    .find(|child| child.kind() == "argument_list");
                arguments
                    .and_then(|arguments| arguments.named_child(0))
                    .map(|expression| Self::evaluate(content, expression, context))
                    .unwrap_or(false)
            };
            // An @elif or @else without preceding @if is invalid, consider it as not taken.
            let is_previous_taken = || {
                Self::get_previous_node(decorated_node)
                    .and_then(|previous| chains.get(&previous.id()).copied())
                    .unwrap_or(false)
            };
            let (is_active, is_taken) = match name.as_str() {
                "if" => {
                    let is_active = condition();
                    (is_active, is_active)
                }
                "elif" => {
                    let is_previous_taken = is_previous_taken();
                    let is_active = !is_previous_taken && condition();
                    (is_active, is_previous_taken || is_active)
                }
                _ => (!is_previous_taken(), true),
            };
            chains.insert(decorated_node.id(), is_taken);
            // Region start after the conditional attribute, as #if regions start after the directive.
            regions.push(ShaderRegion::new(
                ShaderRange::new(
                    ShaderPosition::from(attribute.end_position()),
                    ShaderPosition::from(decorated_node.end_position()),
                ),
                is_active,
            ));
        }
        // Regions nested in an inactive region are inactive as well.
        let inactive_ranges: Vec<ShaderRange> = regions
            .iter()
            .filter(|region| !region.is_active)
            .map(|region| region.range.clone())
            .collect();
        for region in &mut regions {
            if region.is_active
                && inactive_ranges
                    .iter()
                    .any(|range| range.contain_bounds(&region.range))
            {
                region.is_active = false;
            }
        }
        Ok(regions)
    }
}
