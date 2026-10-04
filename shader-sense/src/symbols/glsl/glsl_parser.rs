use std::path::Path;

use crate::position::ShaderRange;
use crate::symbols::symbol_parser::ShaderSymbolListBuilder;

use crate::symbols::symbols::{ShaderMember, ShaderSymbolMode, ShaderSymbolRuntime};
use crate::symbols::{
    symbol_parser::{get_name, SymbolTreeParser},
    symbols::{ShaderParameter, ShaderScope, ShaderSignature, ShaderSymbol, ShaderSymbolData},
};

pub fn get_glsl_parsers() -> Vec<Box<dyn SymbolTreeParser>> {
    vec![
        Box::new(GlslFunctionTreeParser {}),
        Box::new(GlslStructTreeParser {}),
        Box::new(GlslVariableTreeParser {}),
        Box::new(GlslUniformBlock {}),
        Box::new(GlslCallExpressionTreeParser {}),
    ]
}

struct GlslFunctionTreeParser {}

impl SymbolTreeParser for GlslFunctionTreeParser {
    fn get_query(&self) -> String {
        // could use include_str! for scm file.
        r#"(function_definition
            type: (_) @function.return
            declarator: (function_declarator
                declarator: (identifier) @function.label
                parameters: (parameter_list 
                    ((parameter_declaration
                        type: (_) @function.param.type
                        declarator: (_) @function.param.decl
                    )(",")?)*
                )
            )
            body: (compound_statement) @function.scope
            )"#
        .into() // compound_statement is function scope.
    }
    fn process_match(
        &self,
        symbol_match: &tree_sitter::QueryMatch,
        file_path: &Path,
        shader_content: &str,
        scopes: &Vec<ShaderScope>,
        symbols: &mut ShaderSymbolListBuilder,
    ) {
        let label_node = symbol_match.captures[1].node;
        let range = ShaderRange::from(label_node.range());
        let scope_stack = self.compute_scope_stack(scopes, &range);
        // Query internal scopes variables
        let scope_node = symbol_match.captures[symbol_match.captures.len() - 1].node;
        /*let content_scope_stack = {
            let mut s = scope_stack.clone();
            s.push(range.clone());
            s
        };
        query_variables(file_path, &shader_content[scope_node.range().start_byte.. scope_node.range().end_byte], scope_node, {
            let mut s = scope_stack.clone();
            s.push(range.clone());
            s
        });*/
        debug_assert!(
            scope_stack.is_empty(),
            "In GLSL all function are global scope"
        );
        symbols.add_function(ShaderSymbol {
            label: get_name(shader_content, symbol_match.captures[1].node).into(),
            requirement: None,
            data: ShaderSymbolData::Functions {
                signatures: vec![ShaderSignature {
                    returnType: get_name(shader_content, symbol_match.captures[0].node).into(),
                    description: "".into(),
                    parameters: symbol_match.captures[2..symbol_match.captures.len() - 1]
                        .chunks(2)
                        .map(|w| ShaderParameter {
                            ty: get_name(shader_content, w[0].node).into(),
                            label: get_name(shader_content, w[1].node).into(),
                            count: None,
                            description: "".into(),
                            range: Some(ShaderRange::from(w[1].node.range())),
                            modifier: None,
                        })
                        .collect::<Vec<ShaderParameter>>(),
                }],
            },
            mode: ShaderSymbolMode::Runtime(ShaderSymbolRuntime::new(
                file_path.into(),
                range,
                Some(ShaderScope::new(ShaderRange::from(scope_node.range()))),
                scope_stack,
            )),
        });
    }
}

struct GlslUniformBlock {}

impl SymbolTreeParser for GlslUniformBlock {
    fn get_query(&self) -> String {
        r#"(declaration
            (identifier) @uniform.identifier
            (field_declaration_list
                (field_declaration 
                    type: (_) @uniform.param.type
                    declarator: (_) @uniform.param.decl
                )+
            )
            (identifier)? @uniform.name
        )"#
        .into()
    }
    fn process_match(
        &self,
        symbol_match: &tree_sitter::QueryMatch,
        file_path: &Path,
        shader_content: &str,
        _scopes: &Vec<ShaderScope>,
        symbols: &mut ShaderSymbolListBuilder,
    ) {
        let capture_count = symbol_match.captures.len();
        if capture_count % 2 == 0 {
            // name
            let identifier_node = symbol_match.captures[0].node;
            let identifier_range = ShaderRange::from(identifier_node.range());
            let uniform_block_name: String = get_name(shader_content, identifier_node).into();
            symbols.add_type(ShaderSymbol {
                label: uniform_block_name.clone(),
                requirement: None,
                data: ShaderSymbolData::Struct {
                    constructors: vec![], // No constructor for uniform.
                    members: symbol_match.captures[1..capture_count - 1]
                        .chunks(2)
                        .map(|w| ShaderMember {
                            context: uniform_block_name.clone(),
                            parameters: ShaderParameter {
                                ty: get_name(shader_content, w[0].node).into(),
                                label: get_name(shader_content, w[1].node).into(),
                                count: None,
                                description: "".into(),
                                range: Some(ShaderRange::from(w[1].node.range())),
                                modifier: None,
                            },
                        })
                        .collect(),
                    methods: vec![],
                },
                // Uniform are global stack in GLSL.
                mode: ShaderSymbolMode::Runtime(ShaderSymbolRuntime::new(
                    file_path.into(),
                    identifier_range,
                    None,
                    Vec::new(),
                )),
            });
            // Add variable of type
            let variable_node = symbol_match.captures.last().unwrap().node;
            let variable_range = ShaderRange::from(variable_node.range());
            symbols.add_variable(ShaderSymbol {
                label: get_name(shader_content, variable_node).into(),
                requirement: None,
                data: ShaderSymbolData::Variables {
                    ty: get_name(shader_content, identifier_node).into(),
                    count: None,
                },
                // Uniform are global stack in GLSL.
                mode: ShaderSymbolMode::Runtime(ShaderSymbolRuntime::new(
                    file_path.into(),
                    variable_range,
                    None,
                    Vec::new(),
                )),
            });
        } else {
            // no name, content global
            let _identifier_node = symbol_match.captures[0].node;
            for uniform_value in symbol_match.captures[1..].chunks(2) {
                let label_node = uniform_value[1].node;
                let range = ShaderRange::from(label_node.range());
                symbols.add_variable(ShaderSymbol {
                    label: get_name(shader_content, uniform_value[1].node).into(),
                    requirement: None,
                    data: ShaderSymbolData::Variables {
                        ty: get_name(shader_content, uniform_value[0].node).into(),
                        count: None,
                    },
                    // Uniform are global stack in GLSL.
                    mode: ShaderSymbolMode::Runtime(ShaderSymbolRuntime::new(
                        file_path.into(),
                        range,
                        None,
                        Vec::new(),
                    )),
                });
            }
        }
    }
}

struct GlslStructTreeParser {}

impl SymbolTreeParser for GlslStructTreeParser {
    fn get_query(&self) -> String {
        r#"(struct_specifier
            name: (type_identifier) @struct.type
            body: (field_declaration_list
                (field_declaration 
                    type: (_) @struct.param.type
                    declarator: (_) @struct.param.decl
                )+
            )
        )"#
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
        let label_node = symbol_match.captures[0].node;
        let label = get_name(shader_content, label_node).to_string();
        let range = ShaderRange::from(label_node.range());
        let scope_stack = self.compute_scope_stack(&scopes, &range);
        let members = symbol_match.captures[1..]
            .chunks(2)
            .map(|w| ShaderParameter {
                ty: get_name(shader_content, w[0].node).into(),
                label: get_name(shader_content, w[1].node).into(),
                count: None,
                description: "".into(),
                range: Some(ShaderRange::from(w[1].node.range())),
                modifier: None,
            })
            .collect::<Vec<ShaderParameter>>();
        symbols.add_type(ShaderSymbol {
            label: label.clone(),
            requirement: None,
            data: ShaderSymbolData::Struct {
                // In Glsl, constructor are auto built from all their members.
                constructors: vec![ShaderSignature {
                    returnType: "void".into(),
                    description: format!("{} constructor", label),
                    parameters: members.clone(),
                }],
                members: members
                    .into_iter()
                    .map(|m| ShaderMember {
                        context: label.clone(),
                        parameters: m,
                    })
                    .collect(),
                methods: vec![],
            },
            // TODO: compute scope
            mode: ShaderSymbolMode::Runtime(ShaderSymbolRuntime::new(
                file_path.into(),
                range,
                None,
                scope_stack,
            )),
        });
    }
}
struct GlslVariableTreeParser {}

impl SymbolTreeParser for GlslVariableTreeParser {
    fn get_query(&self) -> String {
        r#"(declaration
            type: [
                (type_identifier) @variable.type
                (primitive_type) @variable.type
            ]
            declarator: [(init_declarator
                declarator: (identifier) @variable.label
                value: (_) @variable.value
            ) 
            (identifier) @variable.label
            ]
        )"#
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
        let label_node = symbol_match.captures[1].node;
        let range = ShaderRange::from(label_node.range());
        let scope_stack = self.compute_scope_stack(&scopes, &range);
        // Check if its parameter or struct element.
        let _type_qualifier = get_name(shader_content, symbol_match.captures[0].node);
        // TODO: handle values & qualifiers..
        //let _value = get_name(shader_content, matche.captures[2].node);
        symbols.add_variable(ShaderSymbol {
            label: get_name(shader_content, symbol_match.captures[1].node).into(),
            requirement: None,
            data: ShaderSymbolData::Variables {
                ty: get_name(shader_content, symbol_match.captures[0].node).into(),
                count: None,
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

struct GlslCallExpressionTreeParser {}

impl SymbolTreeParser for GlslCallExpressionTreeParser {
    fn get_query(&self) -> String {
        r#"(call_expression
            function: (identifier) @call.identifier
            arguments: (argument_list
                "("
                    (
                        [
                            (identifier)
                            (number_literal)
                            (call_expression)
                            (unary_expression)
                            (binary_expression)
                        ] @call.parameter
                    (",")?)*
                ")"
            )
        )"#
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
        let label_node = symbol_match.captures[0].node;
        let range = ShaderRange::from(label_node.range());
        let scope_stack = self.compute_scope_stack(&scopes, &range);
        let label = get_name(shader_content, label_node).into();
        symbol_builder.add_call_expression(ShaderSymbol {
            label: label,
            requirement: None,
            data: ShaderSymbolData::CallExpression {
                label: get_name(shader_content, label_node).into(),
                range: range.clone(),
                parameters: symbol_match.captures[1..]
                    .iter()
                    .enumerate()
                    .map(|(i, e)| {
                        // These name are not variable. Should find definition in symbols.
                        (format!("param{}:", i), ShaderRange::from(e.node.range()))
                    })
                    .collect(),
            },
            // TODO: range should be range of whole expression.
            mode: ShaderSymbolMode::Runtime(ShaderSymbolRuntime::new(
                file_path.into(),
                range,
                None,
                scope_stack,
            )),
        });
    }
}
