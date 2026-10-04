use std::path::Path;

use tree_sitter::Node;

use crate::{
    position::ShaderRange,
    symbols::{
        symbol_parser::{get_name, ShaderSymbolListBuilder, SymbolTreeParser},
        symbols::{
            ShaderMember, ShaderParameter, ShaderScope, ShaderSignature, ShaderSymbol,
            ShaderSymbolData, ShaderSymbolMode, ShaderSymbolRuntime,
        },
    },
};

pub fn get_wgsl_parsers() -> Vec<Box<dyn SymbolTreeParser>> {
    vec![
        Box::new(WgslFunctionTreeParser {}),
        Box::new(WgslStructTreeParser {}),
        Box::new(WgslTypeAliasTreeParser {}),
        Box::new(WgslVariableTreeParser {}),
        Box::new(WgslCallExpressionTreeParser {}),
        // Imports are handled by the region finder as includes, as they depend on compilation params.
    ]
}

// Queries only capture the declaration node, its content is then read through field names.
fn get_field<'a>(node: Node<'a>, field: &str) -> Option<Node<'a>> {
    node.child_by_field_name(field)
}

fn find_child<'a>(node: Node<'a>, kind: &str) -> Option<Node<'a>> {
    let mut cursor = node.walk();
    let child = node
        .named_children(&mut cursor)
        .find(|child| child.kind() == kind);
    child
}

/// Infer the type of an untyped declaration from its literal initializer.
/// Abstract types are concretized as they would be for a let or var.
fn infer_literal_type(shader_content: &str, initializer: Node) -> String {
    let literal = get_name(shader_content, initializer);
    match initializer.kind() {
        "bool_literal" => "bool".into(),
        "int_literal" if literal.ends_with('u') => "u32".into(),
        "int_literal" => "i32".into(),
        "float_literal" if literal.ends_with('h') => "f16".into(),
        "float_literal" => "f32".into(),
        _ => "".into(),
    }
}

struct WgslFunctionTreeParser {}

impl SymbolTreeParser for WgslFunctionTreeParser {
    fn get_query(&self) -> String {
        r#"(function_decl) @function"#.into()
    }
    fn process_match(
        &self,
        symbol_match: &tree_sitter::QueryMatch,
        file_path: &Path,
        shader_content: &str,
        scopes: &Vec<ShaderScope>,
        symbols: &mut ShaderSymbolListBuilder,
    ) {
        let function_node = symbol_match.captures[0].node;
        let (Some(header_node), Some(body_node)) = (
            find_child(function_node, "function_header"),
            get_field(function_node, "body"),
        ) else {
            return;
        };
        let Some(label_node) = get_field(header_node, "name") else {
            return;
        };
        let range = ShaderRange::from(label_node.range());
        let scope_stack = self.compute_scope_stack(scopes, &range);
        let scope_range = ShaderRange::from(body_node.range());
        let parameter_scope_stack = {
            let mut s = scope_stack.clone();
            s.push(ShaderScope::new(scope_range.clone()));
            s
        };
        // Get parameters & add them as function scope variable.
        let mut parameters = Vec::new();
        if let Some(parameters_node) = get_field(header_node, "parameters") {
            let mut cursor = parameters_node.walk();
            for parameter_node in parameters_node.named_children(&mut cursor) {
                let (Some(name_node), Some(type_node)) = (
                    get_field(parameter_node, "name"),
                    get_field(parameter_node, "type"),
                ) else {
                    continue;
                };
                let label: String = get_name(shader_content, name_node).into();
                let ty: String = get_name(shader_content, type_node).into();
                symbols.add_variable(ShaderSymbol {
                    label: label.clone(),
                    requirement: None,
                    data: ShaderSymbolData::Variables {
                        ty: ty.clone(),
                        count: None,
                    },
                    mode: ShaderSymbolMode::Runtime(ShaderSymbolRuntime::new(
                        file_path.into(),
                        ShaderRange::from(name_node.range()),
                        None,
                        parameter_scope_stack.clone(),
                    )),
                });
                parameters.push(ShaderParameter {
                    ty,
                    label,
                    count: None,
                    description: "".into(),
                    range: Some(ShaderRange::from(name_node.range())),
                    modifier: None,
                });
            }
        }
        symbols.add_function(ShaderSymbol {
            label: get_name(shader_content, label_node).into(),
            requirement: None,
            data: ShaderSymbolData::Functions {
                signatures: vec![ShaderSignature {
                    returnType: get_field(header_node, "return_type")
                        .map(|return_type| get_name(shader_content, return_type).into())
                        .unwrap_or("void".into()),
                    description: "".into(),
                    parameters,
                }],
            },
            mode: ShaderSymbolMode::Runtime(ShaderSymbolRuntime::new(
                file_path.into(),
                range,
                Some(ShaderScope::new(scope_range)),
                scope_stack,
            )),
        });
    }
}

struct WgslStructTreeParser {}

impl SymbolTreeParser for WgslStructTreeParser {
    fn get_query(&self) -> String {
        r#"(struct_decl) @struct"#.into()
    }
    fn process_match(
        &self,
        symbol_match: &tree_sitter::QueryMatch,
        file_path: &Path,
        shader_content: &str,
        scopes: &Vec<ShaderScope>,
        symbols: &mut ShaderSymbolListBuilder,
    ) {
        let struct_node = symbol_match.captures[0].node;
        let (Some(label_node), Some(body_node)) = (
            get_field(struct_node, "name"),
            get_field(struct_node, "body"),
        ) else {
            return;
        };
        let label: String = get_name(shader_content, label_node).into();
        let range = ShaderRange::from(label_node.range());
        let scope_stack = self.compute_scope_stack(scopes, &range);
        let mut cursor = body_node.walk();
        let members = body_node
            .named_children(&mut cursor)
            .filter_map(|member_node| {
                let name_node = get_field(member_node, "name")?;
                let type_node = get_field(member_node, "type")?;
                Some(ShaderParameter {
                    ty: get_name(shader_content, type_node).into(),
                    label: get_name(shader_content, name_node).into(),
                    count: None,
                    description: "".into(),
                    range: Some(ShaderRange::from(name_node.range())),
                    modifier: None,
                })
            })
            .collect::<Vec<ShaderParameter>>();
        symbols.add_type(ShaderSymbol {
            label: label.clone(),
            requirement: None,
            data: ShaderSymbolData::Struct {
                // In Wgsl, constructor are built from all their members.
                constructors: vec![ShaderSignature {
                    returnType: label.clone(),
                    description: format!("{} constructor", label),
                    parameters: members.clone(),
                }],
                members: members
                    .into_iter()
                    .map(|member| ShaderMember {
                        context: label.clone(),
                        parameters: member,
                    })
                    .collect(),
                methods: vec![],
            },
            mode: ShaderSymbolMode::Runtime(ShaderSymbolRuntime::new(
                file_path.into(),
                range,
                None,
                scope_stack,
            )),
        });
    }
}

struct WgslTypeAliasTreeParser {}

impl SymbolTreeParser for WgslTypeAliasTreeParser {
    fn get_query(&self) -> String {
        r#"(type_alias_decl) @alias"#.into()
    }
    fn process_match(
        &self,
        symbol_match: &tree_sitter::QueryMatch,
        file_path: &Path,
        shader_content: &str,
        scopes: &Vec<ShaderScope>,
        symbols: &mut ShaderSymbolListBuilder,
    ) {
        let alias_node = symbol_match.captures[0].node;
        let Some(label_node) = get_field(alias_node, "name") else {
            return;
        };
        let range = ShaderRange::from(label_node.range());
        let scope_stack = self.compute_scope_stack(scopes, &range);
        symbols.add_type(ShaderSymbol {
            label: get_name(shader_content, label_node).into(),
            requirement: None,
            data: ShaderSymbolData::Types {
                constructors: vec![],
            },
            mode: ShaderSymbolMode::Runtime(ShaderSymbolRuntime::new(
                file_path.into(),
                range,
                None,
                scope_stack,
            )),
        });
    }
}

struct WgslVariableTreeParser {}

impl SymbolTreeParser for WgslVariableTreeParser {
    fn get_query(&self) -> String {
        // var are always in variable_decl, while let, const & override are directly in their statement.
        r#"[
            (variable_decl)
            (global_value_decl)
            (variable_or_value_statement)
        ] @variable"#
            .into()
    }
    fn process_match(
        &self,
        symbol_match: &tree_sitter::QueryMatch,
        file_path: &Path,
        shader_content: &str,
        scopes: &Vec<ShaderScope>,
        symbols: &mut ShaderSymbolListBuilder,
    ) {
        let variable_node = symbol_match.captures[0].node;
        // variable_or_value_statement holding a variable_decl will be handled by the variable_decl match.
        let Some(label_node) = get_field(variable_node, "name") else {
            return;
        };
        let ty = match get_field(variable_node, "type") {
            Some(type_node) => get_name(shader_content, type_node).into(),
            None => {
                // Initializer is only a named field for global declarations.
                let mut cursor = variable_node.walk();
                let initializer = get_field(variable_node, "initializer").or_else(|| {
                    variable_node
                        .named_children(&mut cursor)
                        .filter(|child| child.id() != label_node.id())
                        .last()
                });
                initializer
                    .map(|initializer| infer_literal_type(shader_content, initializer))
                    .unwrap_or_default()
            }
        };
        let range = ShaderRange::from(label_node.range());
        let scope_stack = self.compute_scope_stack(scopes, &range);
        symbols.add_variable(ShaderSymbol {
            label: get_name(shader_content, label_node).into(),
            requirement: None,
            data: ShaderSymbolData::Variables { ty, count: None },
            mode: ShaderSymbolMode::Runtime(ShaderSymbolRuntime::new(
                file_path.into(),
                range,
                None,
                scope_stack,
            )),
        });
    }
}

struct WgslCallExpressionTreeParser {}

impl SymbolTreeParser for WgslCallExpressionTreeParser {
    fn get_query(&self) -> String {
        r#"[
            (call_expression)
            (func_call_statement)
        ] @call"#
            .into()
    }
    fn process_match(
        &self,
        symbol_match: &tree_sitter::QueryMatch,
        file_path: &Path,
        shader_content: &str,
        scopes: &Vec<ShaderScope>,
        symbol_builder: &mut ShaderSymbolListBuilder,
    ) {
        let call_node = symbol_match.captures[0].node;
        // Callee might be prefixed with a module path (WESL) which is a different node.
        let Some(label_node) = find_child(call_node, "identifier") else {
            return;
        };
        let range = ShaderRange::from(label_node.range());
        let scope_stack = self.compute_scope_stack(scopes, &range);
        let label: String = get_name(shader_content, label_node).into();
        let parameters = match find_child(call_node, "argument_list") {
            Some(arguments_node) => {
                let mut cursor = arguments_node.walk();
                let parameters = arguments_node
                    .named_children(&mut cursor)
                    .enumerate()
                    .map(|(i, argument)| {
                        // These name are not variable. Should find definition in symbols.
                        (format!("param{}:", i), ShaderRange::from(argument.range()))
                    })
                    .collect();
                parameters
            }
            None => vec![],
        };
        symbol_builder.add_call_expression(ShaderSymbol {
            label: label.clone(),
            requirement: None,
            data: ShaderSymbolData::CallExpression {
                label,
                range: range.clone(),
                parameters,
            },
            mode: ShaderSymbolMode::Runtime(ShaderSymbolRuntime::new(
                file_path.into(),
                range,
                None,
                scope_stack,
            )),
        });
    }
}
