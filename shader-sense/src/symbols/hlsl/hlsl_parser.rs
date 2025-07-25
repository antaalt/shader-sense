use std::path::Path;

use crate::symbols::symbol_parser::ShaderSymbolListBuilder;
use crate::symbols::symbols::ShaderMember;

use crate::symbols::{
    symbol_parser::{get_name, SymbolTreeParser},
    symbols::{
        ShaderMethod, ShaderParameter, ShaderRange, ShaderScope, ShaderSignature, ShaderSymbol,
        ShaderSymbolData,
    },
};

pub fn get_hlsl_parsers() -> Vec<Box<dyn SymbolTreeParser>> {
    vec![
        Box::new(HlslFunctionTreeParser { is_field: false }),
        Box::new(HlslStructTreeParser::new()),
        Box::new(HlslVariableTreeParser { is_field: false }),
        Box::new(HlslCallExpressionTreeParser {}),
    ]
}

struct HlslFunctionTreeParser {
    is_field: bool,
}

impl SymbolTreeParser for HlslFunctionTreeParser {
    fn get_query(&self) -> String {
        let field_prestring = if self.is_field { "field_" } else { "" };
        format!(
            r#"(function_definition
            type: (_) @function.return
            declarator: (function_declarator
                declarator: ({}identifier) @function.label
                parameters: (parameter_list 
                    ([
                        ((parameter_declaration
                            type: (_) @function.param.type
                            declarator: (_) @function.param.decl
                        )(",")?)
                        ((optional_parameter_declaration
                            type: (_) @function.param.type
                            declarator: (_) @function.param.decl
                        )(",")?)
                    ])*
                )
            )
            body: (compound_statement) @function.scope
        )"#,
            field_prestring
        ) // compound_statement is function scope.
          /*(semantics
              (identifier) @function.param.semantic
          )?*/
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
        // Query internal scope
        let scope_node = matches.captures[matches.captures.len() - 1].node;
        let scope_range = ShaderRange::from_range(scope_node.range(), file_path);
        let parameter_scope_stack = {
            let mut s = scope_stack.clone();
            s.push(scope_range.clone());
            s
        };
        // Get parameters & add them as function scope variable.
        let parameters = matches.captures[2..matches.captures.len() - 1]
            .chunks(2)
            .map(|w| {
                let ty: String = get_name(shader_content, w[0].node).into();
                let label: String = get_name(shader_content, w[1].node).into();
                symbols.add_variable(ShaderSymbol {
                    label: label.clone(),
                    description: "".into(),
                    version: "".into(),
                    stages: vec![],
                    link: None,
                    data: ShaderSymbolData::Variables {
                        ty: ty.clone(),
                        count: None,
                    },
                    scope: None,
                    range: Some(ShaderRange::from_range(w[1].node.range(), file_path)),
                    scope_stack: Some(parameter_scope_stack.clone()),
                });
                ShaderParameter {
                    ty: ty,
                    label: label,
                    count: None,
                    description: "".into(),
                }
            })
            .collect::<Vec<ShaderParameter>>();
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
                    parameters: parameters,
                }],
            },
            scope: Some(scope_range),
            range: Some(range),
            scope_stack: Some(scope_stack), // In GLSL, all function are global scope.
        });
    }
}

struct HlslStructTreeParser {
    var_parser: HlslVariableTreeParser,
    var_query: tree_sitter::Query,
    func_parser: HlslFunctionTreeParser,
    func_query: tree_sitter::Query,
}
impl HlslStructTreeParser {
    pub fn new() -> Self {
        // Cache for perf.
        let lang = tree_sitter_hlsl::language();
        let func_parser = HlslFunctionTreeParser { is_field: true };
        let var_parser = HlslVariableTreeParser { is_field: true };
        let var_query = var_parser.get_query();
        let func_query = func_parser.get_query();
        Self {
            var_parser,
            var_query: tree_sitter::Query::new(lang.clone(), var_query.as_str()).unwrap(),
            func_parser,
            func_query: tree_sitter::Query::new(lang.clone(), func_query.as_str()).unwrap(),
        }
    }
}
impl SymbolTreeParser for HlslStructTreeParser {
    fn get_query(&self) -> String {
        r#"(struct_specifier
            name: (type_identifier) @struct.type
            body: (field_declaration_list) @struct.content
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

        // QUERY INNER METHODS
        let mut query_cursor = tree_sitter::QueryCursor::new();
        let methods = query_cursor
            .matches(
                &self.func_query,
                matches.captures[1].node,
                shader_content.as_bytes(),
            )
            .map(|matches| {
                let mut symbols = ShaderSymbolListBuilder::new(&|_| true);
                self.func_parser.process_match(
                    matches,
                    file_path,
                    shader_content,
                    scopes,
                    &mut symbols,
                );
                symbols
                    .get_shader_symbol_list()
                    .functions
                    .into_iter()
                    .map(|f| ShaderMethod {
                        label: f.label.clone(),
                        signature: if let ShaderSymbolData::Functions { signatures } = &f.data {
                            signatures[0].clone()
                        } else {
                            panic!("Invalid function type");
                        },
                    })
                    .collect::<Vec<ShaderMethod>>()
            })
            .collect::<Vec<Vec<ShaderMethod>>>()
            .concat();

        // QUERY INNER MEMBERS
        let mut query_cursor = tree_sitter::QueryCursor::new();
        let members = query_cursor
            .matches(
                &self.var_query,
                matches.captures[1].node,
                shader_content.as_bytes(),
            )
            .map(|matches| {
                let mut symbols = ShaderSymbolListBuilder::new(&|_| true);
                self.var_parser.process_match(
                    matches,
                    file_path,
                    shader_content,
                    scopes,
                    &mut symbols,
                );
                symbols
                    .get_shader_symbol_list()
                    .variables
                    .into_iter()
                    .map(|f| ShaderMember {
                        label: f.label.clone(),
                        ty: if let ShaderSymbolData::Variables { ty, count: _ } = &f.data {
                            ty.clone()
                        } else {
                            panic!("Invalid variable type");
                        },
                        count: if let ShaderSymbolData::Variables { ty: _, count } = &f.data {
                            count.clone()
                        } else {
                            panic!("Invalid variable type");
                        },
                        description: "".into(),
                    })
                    .collect::<Vec<ShaderMember>>()
            })
            .collect::<Vec<Vec<ShaderMember>>>()
            .concat();
        symbols.add_type(ShaderSymbol {
            label: get_name(shader_content, matches.captures[0].node).into(),
            description: "".into(),
            version: "".into(),
            stages: vec![],
            link: None,
            data: ShaderSymbolData::Struct {
                constructors: vec![], // No constructor in HLSL
                members: members,
                methods: methods,
            },
            scope: None, // TODO: compute
            range: Some(range),
            scope_stack: Some(scope_stack),
        });
    }
}

struct HlslVariableTreeParser {
    is_field: bool,
}

impl SymbolTreeParser for HlslVariableTreeParser {
    fn get_query(&self) -> String {
        let field_prestring = if self.is_field { "field_" } else { "" };
        format!(
            r#"({}declaration
            (qualifiers)?
            type: (_) @variable.type
            declarator: [
                (init_declarator
                    declarator: [
                        (identifier) @variable.label
                        (array_declarator
                            declarator: (identifier) @variable.label
                            size: (_) @variable.size
                        )
                    ]
                    value: (_)
                ) 
                (array_declarator
                    declarator: (identifier) @variable.label
                    size: (_) @variable.size
                )
                ({}identifier) @variable.label
            ]
        )"#,
            field_prestring, field_prestring
        )
    }
    fn process_match(
        &self,
        matches: tree_sitter::QueryMatch,
        file_path: &Path,
        shader_content: &str,
        scopes: &Vec<ShaderScope>,
        symbol_builder: &mut ShaderSymbolListBuilder,
    ) {
        let type_node = matches.captures[0].node;
        let label_node = matches.captures[1].node;
        let size_node = if matches.captures.len() == 3 {
            Some(matches.captures[2].node)
        } else {
            None
        };
        let range = ShaderRange::from_range(label_node.range(), file_path.into());
        let scope_stack = self.compute_scope_stack(&scopes, &range);
        symbol_builder.add_variable(ShaderSymbol {
            label: get_name(shader_content, label_node).into(),
            description: "".into(),
            version: "".into(),
            stages: vec![],
            link: None,
            data: ShaderSymbolData::Variables {
                ty: get_name(shader_content, type_node).into(),
                count: size_node.map(|s| match get_name(shader_content, s).parse::<u32>() {
                    Ok(value) => value,
                    Err(_) => 0, // TODO: Need to resolve the parameter. Could use proxy tree same as for region conditions. For now, simply return zero.
                }),
            },
            scope: None,
            range: Some(range),
            scope_stack: Some(scope_stack),
        });
    }
}

struct HlslCallExpressionTreeParser {}

impl SymbolTreeParser for HlslCallExpressionTreeParser {
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
                            (field_expression)
                            (subscript_expression)
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

#[cfg(test)]
mod hlsl_parser_tests {
    use std::{path::Path, vec};

    use tree_sitter::{Query, QueryCursor};

    use crate::{
        shader::ShadingLanguage,
        symbols::{
            hlsl::hlsl_parser::{HlslFunctionTreeParser, HlslStructTreeParser},
            shader_language::ShaderLanguage,
            symbol_parser::{ShaderSymbolListBuilder, SymbolTreeParser},
            symbols::{
                ShaderMember, ShaderMethod, ShaderParameter, ShaderSignature, ShaderSymbol,
                ShaderSymbolData, ShaderSymbolList,
            },
        },
    };

    fn parse<Parser: SymbolTreeParser>(
        parser: &Parser,
        file_path: &Path,
        shader_content: &str,
    ) -> ShaderSymbolList {
        let mut symbol_list_builder = ShaderSymbolListBuilder::new(&|_| true);
        let mut query_cursor = QueryCursor::new();
        let query = Query::new(tree_sitter_hlsl::language(), parser.get_query().as_str()).unwrap();

        let mut language = ShaderLanguage::new(ShadingLanguage::Hlsl);
        let module = language.create_module(file_path, shader_content).unwrap();
        for matches in
            query_cursor.matches(&query, module.tree.root_node(), module.content.as_bytes())
        {
            parser.process_match(
                matches,
                &module.file_path,
                &module.content,
                &vec![],
                &mut symbol_list_builder,
            );
        }
        symbol_list_builder.get_shader_symbol_list()
    }
    fn compare(symbol_expected: &ShaderSymbol, symbol: &ShaderSymbol) {
        assert!(symbol_expected.label == symbol.label, "Invalid label");
        assert!(
            symbol_expected.description == symbol.description,
            "Invalid description"
        );
        assert!(symbol_expected.link == symbol.link, "Invalid link");
        match (&symbol.data, &symbol_expected.data) {
            (
                ShaderSymbolData::Types { constructors: c1 },
                ShaderSymbolData::Types { constructors: c2 },
            ) => {
                assert!(c1.len() == c2.len(), "Invalid constructors");
            }
            (
                ShaderSymbolData::Struct {
                    constructors: c1,
                    members: m1,
                    methods: me1,
                },
                ShaderSymbolData::Struct {
                    constructors: c2,
                    members: m2,
                    methods: me2,
                },
            ) => {
                assert!(c1.len() == c2.len(), "Invalid constructors");
                assert!(m1.len() == m2.len(), "Invalid members");
                assert!(me1.len() == me2.len(), "Invalid methods");
            }
            (
                ShaderSymbolData::Constants {
                    ty: _t1,
                    qualifier: _q1,
                    value: _v1,
                },
                ShaderSymbolData::Constants {
                    ty: _t2,
                    qualifier: _q2,
                    value: _v2,
                },
            ) => todo!(),
            (
                ShaderSymbolData::Functions { signatures: s1 },
                ShaderSymbolData::Functions { signatures: s2 },
            ) => {
                assert!(s1.len() == s2.len(), "Invalid functions");
            }
            (ShaderSymbolData::Keyword {}, ShaderSymbolData::Keyword {}) => {}
            (
                ShaderSymbolData::Variables {
                    ty: _t1,
                    count: _c1,
                },
                ShaderSymbolData::Variables {
                    ty: _t2,
                    count: _c2,
                },
            ) => todo!(),
            (
                ShaderSymbolData::CallExpression {
                    label: _l1,
                    range: _r1,
                    parameters: _p1,
                },
                ShaderSymbolData::CallExpression {
                    label: _l2,
                    range: _r2,
                    parameters: _p2,
                },
            ) => todo!(),
            (ShaderSymbolData::Link { target: t1 }, ShaderSymbolData::Link { target: t2 }) => {
                assert!(t1 == t2, "Mismatching link")
            }
            (ShaderSymbolData::Macro { value: v1 }, ShaderSymbolData::Macro { value: v2 }) => {
                assert!(v1 == v2, "Mismatching macro")
            }
            _ => panic!("data mismatch"),
        }
    }

    #[test]
    fn struct_parser() {
        let path = Path::new("dontcare");
        let content = r"
            struct TestStruct {
                float member0;
                float member1 = 5;
                float method() {
                }
            };
        ";
        let result = parse(&HlslStructTreeParser::new(), path, content);
        compare(
            &ShaderSymbol {
                label: "TestStruct".into(),
                description: "".into(),
                version: "".into(),
                stages: vec![],
                link: None,
                data: ShaderSymbolData::Struct {
                    constructors: vec![],
                    members: vec![
                        ShaderMember {
                            ty: "float".into(),
                            label: "member0".into(),
                            description: "".into(),
                            count: None,
                        },
                        ShaderMember {
                            ty: "float".into(),
                            label: "member1".into(),
                            description: "".into(),
                            count: None,
                        },
                    ],
                    methods: vec![ShaderMethod {
                        label: "method".into(),
                        signature: ShaderSignature {
                            returnType: "float".into(),
                            description: "".into(),
                            parameters: vec![],
                        },
                    }],
                },
                range: None,
                scope: None,
                scope_stack: None,
            },
            &result.types[0],
        );
    }

    #[test]
    fn function_parser() {
        let path = Path::new("dontcare");
        let content = r"
            void function(float p0, uint p1 = 0) {
            }
        ";
        let result = parse(&HlslFunctionTreeParser { is_field: false }, path, content);
        compare(
            &ShaderSymbol {
                label: "function".into(),
                description: "".into(),
                version: "".into(),
                stages: vec![],
                link: None,
                data: ShaderSymbolData::Functions {
                    signatures: vec![ShaderSignature {
                        returnType: "void".into(),
                        description: "".into(),
                        parameters: vec![
                            ShaderParameter {
                                ty: "float".into(),
                                label: "p0".into(),
                                description: "".into(),
                                count: None,
                            },
                            ShaderParameter {
                                ty: "uint".into(),
                                label: "p1".into(),
                                description: "".into(),
                                count: None,
                            },
                        ],
                    }],
                },
                range: None,
                scope: None,
                scope_stack: None,
            },
            &result.functions[0],
        );
    }
}
