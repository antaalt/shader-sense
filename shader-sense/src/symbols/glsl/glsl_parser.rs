use std::path::Path;

use crate::symbols::symbol_parser::ShaderSymbolListBuilder;

use crate::symbols::{
    symbol_parser::{get_name, SymbolTreeParser},
    symbols::{
        ShaderParameter, ShaderRange, ShaderScope, ShaderSignature, ShaderSymbol, ShaderSymbolData,
    },
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
        matches: tree_sitter::QueryMatch,
        file_path: &Path,
        shader_content: &str,
        scopes: &Vec<ShaderScope>,
        symbols: &mut ShaderSymbolListBuilder,
    ) {
        let label_node = matches.captures[1].node;
        let range = ShaderRange::from_range(label_node.range(), file_path.into());
        let scope_stack = self.compute_scope_stack(scopes, &range);
        // Query internal scopes variables
        let scope_node = matches.captures[matches.captures.len() - 1].node;
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
        symbols.add_function(ShaderSymbol {
            label: get_name(shader_content, matches.captures[1].node).into(),
            description: "".into(),
            version: "".into(),
            stages: vec![],
            link: None,
            data: ShaderSymbolData::Functions {
                signatures: vec![ShaderSignature {
                    returnType: get_name(shader_content, matches.captures[0].node).into(),
                    description: "".into(),
                    parameters: matches.captures[2..matches.captures.len() - 1]
                        .chunks(2)
                        .map(|w| ShaderParameter {
                            ty: get_name(shader_content, w[0].node).into(),
                            label: get_name(shader_content, w[1].node).into(),
                            count: None,
                            description: "".into(),
                        })
                        .collect::<Vec<ShaderParameter>>(),
                }],
            },
            scope: Some(ShaderRange::from_range(scope_node.range(), file_path)),
            range: Some(range),
            scope_stack: Some(scope_stack), // In GLSL, all function are global scope.
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
        matches: tree_sitter::QueryMatch,
        file_path: &Path,
        shader_content: &str,
        _scopes: &Vec<ShaderScope>,
        symbols: &mut ShaderSymbolListBuilder,
    ) {
        let capture_count = matches.captures.len();
        if capture_count % 2 == 0 {
            // name
            let identifier_node = matches.captures[0].node;
            let identifier_range =
                ShaderRange::from_range(identifier_node.range(), file_path.into());
            symbols.add_type(ShaderSymbol {
                label: get_name(shader_content, identifier_node).into(),
                description: "".into(),
                version: "".into(),
                stages: vec![],
                link: None,
                data: ShaderSymbolData::Struct {
                    constructors: vec![], // No constructor for uniform.
                    members: matches.captures[1..capture_count - 1]
                        .chunks(2)
                        .map(|w| ShaderParameter {
                            ty: get_name(shader_content, w[0].node).into(),
                            label: get_name(shader_content, w[1].node).into(),
                            count: None,
                            description: "".into(),
                        })
                        .collect::<Vec<ShaderParameter>>(),
                    methods: vec![],
                },
                scope: None,
                range: Some(identifier_range),
                scope_stack: None, // Uniform are global stack in GLSL.
            });
            // Add variable of type
            let variable_node = matches.captures.last().unwrap().node;
            let variable_range = ShaderRange::from_range(variable_node.range(), file_path.into());
            symbols.add_variable(ShaderSymbol {
                label: get_name(shader_content, variable_node).into(),
                description: "".into(),
                version: "".into(),
                stages: vec![],
                link: None,
                data: ShaderSymbolData::Variables {
                    ty: get_name(shader_content, identifier_node).into(),
                    count: None,
                },
                scope: None,
                range: Some(variable_range),
                scope_stack: None, // Uniform are global stack in GLSL.
            });
        } else {
            // no name, content global
            let _identifier_node = matches.captures[0].node;
            for uniform_value in matches.captures[1..].chunks(2) {
                let label_node = uniform_value[1].node;
                let range = ShaderRange::from_range(label_node.range(), file_path.into());
                symbols.add_variable(ShaderSymbol {
                    label: get_name(shader_content, uniform_value[1].node).into(),
                    description: "".into(),
                    version: "".into(),
                    stages: vec![],
                    link: None,
                    data: ShaderSymbolData::Variables {
                        ty: get_name(shader_content, uniform_value[0].node).into(),
                        count: None,
                    },
                    scope: None,
                    range: Some(range),
                    scope_stack: None, // Uniform are global stack in GLSL.
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
        matches: tree_sitter::QueryMatch,
        file_path: &Path,
        shader_content: &str,
        scopes: &Vec<ShaderScope>,
        symbols: &mut ShaderSymbolListBuilder,
    ) {
        let label_node = matches.captures[0].node;
        let range = ShaderRange::from_range(label_node.range(), file_path.into());
        let scope_stack = self.compute_scope_stack(&scopes, &range);
        let members = matches.captures[1..]
            .chunks(2)
            .map(|w| ShaderParameter {
                ty: get_name(shader_content, w[0].node).into(),
                label: get_name(shader_content, w[1].node).into(),
                count: None,
                description: "".into(),
            })
            .collect::<Vec<ShaderParameter>>();
        let label = get_name(shader_content, label_node).to_string();
        symbols.add_type(ShaderSymbol {
            label: label.clone(),
            description: "".into(),
            version: "".into(),
            stages: vec![],
            link: None,
            data: ShaderSymbolData::Struct {
                // In Glsl, constructor are auto built from all there members.
                constructors: vec![ShaderSignature {
                    returnType: "void".into(),
                    description: format!("{} constructor", label),
                    parameters: members.clone(),
                }],
                members: members,
                methods: vec![],
            },
            scope: None, // TODO: compute
            range: Some(range),
            scope_stack: Some(scope_stack),
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
        matches: tree_sitter::QueryMatch,
        file_path: &Path,
        shader_content: &str,
        scopes: &Vec<ShaderScope>,
        symbols: &mut ShaderSymbolListBuilder,
    ) {
        let label_node = matches.captures[1].node;
        let range = ShaderRange::from_range(label_node.range(), file_path.into());
        let scope_stack = self.compute_scope_stack(&scopes, &range);
        // Check if its parameter or struct element.
        let _type_qualifier = get_name(shader_content, matches.captures[0].node);
        // TODO: handle values & qualifiers..
        //let _value = get_name(shader_content, matche.captures[2].node);
        symbols.add_variable(ShaderSymbol {
            label: get_name(shader_content, matches.captures[1].node).into(),
            description: "".into(),
            version: "".into(),
            stages: vec![],
            link: None,
            data: ShaderSymbolData::Variables {
                ty: get_name(shader_content, matches.captures[0].node).into(),
                count: None,
            },
            scope: None,
            range: Some(range),
            scope_stack: Some(scope_stack),
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
        matches: tree_sitter::QueryMatch,
        file_path: &Path,
        shader_content: &str,
        scopes: &Vec<ShaderScope>,
        symbol_builder: &mut ShaderSymbolListBuilder,
    ) {
        let label_node = matches.captures[0].node;
        let range = ShaderRange::from_range(label_node.range(), file_path.into());
        let scope_stack = self.compute_scope_stack(&scopes, &range);
        let label = get_name(shader_content, label_node).into();
        symbol_builder.add_call_expression(ShaderSymbol {
            label: label,
            description: "".into(),
            version: "".into(),
            stages: vec![],
            link: None,
            data: ShaderSymbolData::CallExpression {
                label: get_name(shader_content, label_node).into(),
                range: range.clone(),
                parameters: matches.captures[1..]
                    .iter()
                    .enumerate()
                    .map(|(i, e)| {
                        // These name are not variable. Should find definition in symbols.
                        (
                            format!("param{}:", i),
                            ShaderRange::from_range(e.node.range(), file_path.into()),
                        )
                    })
                    .collect(),
            },
            scope: None,
            range: Some(range), // TODO: this should be range of whole expression.
            scope_stack: Some(scope_stack),
        });
    }
}
