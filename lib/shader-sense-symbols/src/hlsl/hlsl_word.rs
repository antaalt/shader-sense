use tree_sitter::Node;

use crate::{
    shader_module::ShaderModule,
    symbol_parser::{get_name, into_shader_range, ShaderWordRange, SymbolWordProvider},
};
use shader_sense_core::{position::ShaderPosition, shader_error::ShaderError};

pub struct HlslSymbolWordProvider {}

impl SymbolWordProvider for HlslSymbolWordProvider {
    fn find_word_at_position_in_node(
        &self,
        shader_module: &ShaderModule,
        node: Node,
        position: &ShaderPosition,
    ) -> Result<ShaderWordRange, ShaderError> {
        fn range_contain(including_range: tree_sitter::Range, position: &ShaderPosition) -> bool {
            let including_range = into_shader_range(including_range);
            including_range.contain(position)
        }
        if range_contain(node.range(), &position) {
            match node.kind() {
                // identifier = function name, variable...
                // type_identifier = struct name, class name...
                // primitive_type = float, uint...
                // string_content = include, should check preproc_include as parent.
                "identifier" | "type_identifier" | "primitive_type" => {
                    return Ok(ShaderWordRange::new(
                        get_name(&shader_module.content, node).into(),
                        into_shader_range(node.range()),
                        None,
                    ));
                }
                // Incomplete qualifier (`A::B::` while typing) is parsed as an error with flat namespace identifiers.
                "namespace_identifier" => {
                    let mut word = ShaderWordRange::new(
                        get_name(&shader_module.content, node).into(),
                        into_shader_range(node.range()),
                        None,
                    );
                    let mut current_node = node;
                    while let Some(scope_node) = current_node
                        .prev_sibling()
                        .filter(|separator| separator.kind() == "::")
                        .and_then(|separator| separator.prev_sibling())
                        .filter(|scope| scope.kind() == "namespace_identifier")
                    {
                        word.set_root_parent(ShaderWordRange::new(
                            get_name(&shader_module.content, scope_node).into(),
                            into_shader_range(scope_node.range()),
                            None,
                        ));
                        current_node = scope_node;
                    }
                    return Ok(word);
                }
                // Handle namespaces, enum and its values.
                // Nested qualifiers (A::B::c) are nested qualified_identifier, each scope is the parent of the next.
                "qualified_identifier" => {
                    let mut qualified_node = node;
                    let mut scope: Option<ShaderWordRange> = None;
                    'qualified: loop {
                        let mut iterator = qualified_node.walk();
                        for children in qualified_node.children(&mut iterator) {
                            match children.kind() {
                                "namespace_identifier" => {
                                    // Either a namespace or an enum type.
                                    let word = ShaderWordRange::new(
                                        get_name(&shader_module.content, children).into(),
                                        into_shader_range(children.range()),
                                        scope.take(),
                                    );
                                    if range_contain(children.range(), &position) {
                                        return Ok(word);
                                    }
                                    scope = Some(word);
                                }
                                "::" => continue,
                                "qualified_identifier" => {
                                    qualified_node = children;
                                    continue 'qualified;
                                }
                                "identifier" | "type_identifier" => {
                                    if range_contain(children.range(), &position) {
                                        return Ok(ShaderWordRange::new(
                                            get_name(&shader_module.content, children).into(),
                                            into_shader_range(children.range()),
                                            scope,
                                        ));
                                    }
                                }
                                _ => {
                                    return Err(ShaderError::NoSymbol); // Weird...
                                }
                            }
                        }
                        return Err(ShaderError::NoSymbol);
                    }
                }
                // TODO: should use string_content instead
                "string_literal" => {
                    let path = get_name(&shader_module.content, node);
                    return Ok(ShaderWordRange::new(
                        path[1..path.len() - 1].into(),
                        into_shader_range(node.range()),
                        None,
                    ));
                }
                "field_identifier" => {
                    fn set_parent(
                        root: &mut Option<ShaderWordRange>,
                        root_parent: ShaderWordRange,
                    ) {
                        match root {
                            Some(root) => root.set_root_parent(root_parent),
                            None => *root = Some(root_parent),
                        }
                    }
                    let mut word: Option<ShaderWordRange> = None;
                    let mut current_node = match node.prev_named_sibling() {
                        Some(prev_sibling) => prev_sibling,
                        None => return Err(ShaderError::NoSymbol), // Invalid case.
                    };
                    loop {
                        let field = current_node.next_named_sibling().unwrap();
                        match field.kind() {
                            "field_identifier" => set_parent(
                                &mut word,
                                ShaderWordRange::new(
                                    get_name(&shader_module.content, field).into(),
                                    into_shader_range(field.range()),
                                    None,
                                ),
                            ),
                            _ => {
                                return Err(ShaderError::InternalErr(format!(
                                    "Unknown word field {}",
                                    field.kind()
                                )))
                            }
                        }
                        let mut cursor = current_node.walk();
                        match cursor.node().kind() {
                            "field_expression" => {
                                cursor.goto_first_child();
                                current_node = cursor.node();
                            }
                            "call_expression" => {
                                cursor.goto_first_child();
                                match cursor.node().kind() {
                                    "field_expression" => {
                                        cursor.goto_first_child();
                                        current_node = cursor.node();
                                    }
                                    "identifier" => {
                                        let identifier = cursor.node();
                                        set_parent(
                                            &mut word,
                                            ShaderWordRange::new(
                                                get_name(&shader_module.content, identifier).into(),
                                                into_shader_range(identifier.range()),
                                                None,
                                            ),
                                        );
                                        break;
                                    }
                                    _ => return Err(ShaderError::NoSymbol),
                                }
                            }
                            "subscript_expression" => {
                                cursor.goto_first_child();
                                match cursor.node().kind() {
                                    "field_expression" => {
                                        cursor.goto_first_child();
                                        current_node = cursor.node();
                                    }
                                    "identifier" => {
                                        let identifier = cursor.node();
                                        set_parent(
                                            &mut word,
                                            ShaderWordRange::new(
                                                get_name(&shader_module.content, identifier).into(),
                                                into_shader_range(identifier.range()),
                                                None,
                                            ),
                                        );
                                        break;
                                    }
                                    _ => return Err(ShaderError::NoSymbol),
                                }
                            }
                            "identifier" => {
                                let identifier = current_node;
                                set_parent(
                                    &mut word,
                                    ShaderWordRange::new(
                                        get_name(&shader_module.content, identifier).into(),
                                        into_shader_range(identifier.range()),
                                        None,
                                    ),
                                );
                                break;
                            }
                            _ => return Err(ShaderError::NoSymbol),
                        }
                    }
                    return word.ok_or(ShaderError::NoSymbol);
                }
                _ => {
                    for child in node.children(&mut node.walk()) {
                        match self.find_word_at_position_in_node(shader_module, child, position) {
                            Ok(chain_list) => return Ok(chain_list),
                            Err(err) => {
                                if let ShaderError::NoSymbol = err {
                                    // Skip.
                                } else {
                                    return Err(err);
                                }
                            }
                        }
                    }
                }
            }
            Err(ShaderError::NoSymbol)
        } else {
            Err(ShaderError::NoSymbol)
        }
    }
}
