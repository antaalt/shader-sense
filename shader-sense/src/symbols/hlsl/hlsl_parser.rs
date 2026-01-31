use std::path::Path;

use tree_sitter::StreamingIterator;

use crate::position::ShaderRange;
use crate::symbols::symbol_parser::ShaderSymbolListBuilder;
use crate::symbols::symbols::{
    ShaderEnumValue, ShaderMember, ShaderSymbolArray, ShaderSymbolMode, ShaderSymbolRuntime,
};

use crate::symbols::{
    symbol_parser::{get_name, SymbolTreeParser},
    symbols::{
        ShaderMethod, ShaderParameter, ShaderScope, ShaderSignature, ShaderSymbol, ShaderSymbolData,
    },
};

pub fn get_hlsl_parsers() -> Vec<Box<dyn SymbolTreeParser>> {
    vec![
        Box::new(HlslFunctionTreeParser { is_field: false }),
        Box::new(HlslStructTreeParser::new()),
        Box::new(HlslVariableTreeParser { is_field: false }),
        Box::new(HlslCallExpressionTreeParser {}),
        Box::new(HlslEnumTreeParser {}),
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
        symbol_match: &tree_sitter::QueryMatch,
        file_path: &Path,
        shader_content: &str,
        scopes: &Vec<ShaderScope>,
        symbols: &mut ShaderSymbolListBuilder,
    ) {
        let label_node = symbol_match.captures[1].node;
        let range = ShaderRange::from(label_node.range());
        let scope_stack = self.compute_scope_stack(scopes, &range);
        // Query internal scope
        let scope_node = symbol_match.captures[symbol_match.captures.len() - 1].node;
        let scope_range = ShaderRange::from(scope_node.range());
        let parameter_scope_stack = {
            let mut s = scope_stack.clone();
            s.push(scope_range.clone());
            s
        };
        // Get parameters & add them as function scope variable.
        let parameters = symbol_match.captures[2..symbol_match.captures.len() - 1]
            .chunks(2)
            .map(|w| {
                let ty: String = get_name(shader_content, w[0].node).into();
                let label: String = get_name(shader_content, w[1].node).into();
                symbols.add_variable(ShaderSymbol {
                    label: label.clone(),
                    requirement: None,
                    data: ShaderSymbolData::Variables {
                        ty: ty.clone(),
                        count: None,
                    },
                    mode: ShaderSymbolMode::Runtime(ShaderSymbolRuntime::new(
                        file_path.into(),
                        ShaderRange::from(w[1].node.range()),
                        None,
                        parameter_scope_stack.clone(),
                    )),
                });
                ShaderParameter {
                    ty: ty,
                    label: label,
                    count: None,
                    description: "".into(),
                    range: Some(ShaderRange::from(w[1].node.range())),
                }
            })
            .collect::<Vec<ShaderParameter>>();
        symbols.add_function(ShaderSymbol {
            label: get_name(shader_content, symbol_match.captures[1].node).into(),
            requirement: None,
            data: ShaderSymbolData::Functions {
                signatures: vec![ShaderSignature {
                    returnType: get_name(shader_content, symbol_match.captures[0].node).into(),
                    description: "".into(),
                    parameters: parameters,
                }],
            },
            mode: ShaderSymbolMode::Runtime(ShaderSymbolRuntime::new(
                file_path.into(),
                range,
                Some(scope_range),
                scope_stack,
            )),
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
        let lang = tree_sitter_hlsl::LANGUAGE_HLSL;
        let func_parser = HlslFunctionTreeParser { is_field: true };
        let var_parser = HlslVariableTreeParser { is_field: true };
        let var_query = var_parser.get_query();
        let func_query = func_parser.get_query();
        Self {
            var_parser,
            var_query: tree_sitter::Query::new(&lang.into(), var_query.as_str()).unwrap(),
            func_parser,
            func_query: tree_sitter::Query::new(&lang.into(), func_query.as_str()).unwrap(),
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
        symbol_match: &tree_sitter::QueryMatch,
        file_path: &Path,
        shader_content: &str,
        scopes: &Vec<ShaderScope>,
        symbols: &mut ShaderSymbolListBuilder,
    ) {
        let label_node = symbol_match.captures[0].node;
        let struct_name: String = get_name(shader_content, symbol_match.captures[0].node).into();
        let range = ShaderRange::from(label_node.range());
        let scope_stack = self.compute_scope_stack(&scopes, &range);

        // QUERY INNER METHODS
        let mut query_cursor = tree_sitter::QueryCursor::new();
        let mut all_matches = query_cursor.matches(
            &self.func_query,
            symbol_match.captures[1].node,
            shader_content.as_bytes(),
        );
        let mut methods = Vec::new();
        while let Some(method_match) = all_matches.next() {
            let mut symbols = ShaderSymbolListBuilder::new(&|_| true);
            self.func_parser.process_match(
                method_match,
                file_path,
                shader_content,
                scopes,
                &mut symbols,
            );
            methods.push(
                symbols
                    .get_shader_symbol_list()
                    .functions
                    .into_iter()
                    .map(|f| ShaderMethod {
                        context: struct_name.clone(),
                        label: f.label.clone(),
                        signature: if let ShaderSymbolData::Functions { signatures } = &f.data {
                            signatures[0].clone()
                        } else {
                            panic!("Invalid function type");
                        },
                        range: f.mode.map_runtime().map(|r| r.range.clone()),
                    })
                    .collect::<Vec<ShaderMethod>>(),
            );
        }

        // QUERY INNER MEMBERS
        let mut query_cursor = tree_sitter::QueryCursor::new();
        let mut all_matches = query_cursor.matches(
            &self.var_query,
            symbol_match.captures[1].node,
            shader_content.as_bytes(),
        );

        let mut members = Vec::new();
        while let Some(method_match) = all_matches.next() {
            let mut symbols = ShaderSymbolListBuilder::new(&|_| true);
            self.var_parser.process_match(
                method_match,
                file_path,
                shader_content,
                scopes,
                &mut symbols,
            );
            members.push(
                symbols
                    .get_shader_symbol_list()
                    .variables
                    .into_iter()
                    .map(|f| ShaderMember {
                        context: struct_name.clone(),
                        parameters: ShaderParameter {
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
                            range: f.mode.map_runtime().map(|r| r.range.clone()),
                        },
                    })
                    .collect::<Vec<ShaderMember>>(),
            )
        }
        symbols.add_type(ShaderSymbol {
            label: struct_name,
            requirement: None,
            data: ShaderSymbolData::Struct {
                constructors: vec![], // No constructor in HLSL
                members: members.concat(),
                methods: methods.concat(),
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

struct HlslVariableTreeParser {
    is_field: bool,
}

impl SymbolTreeParser for HlslVariableTreeParser {
    fn get_query(&self) -> String {
        let field_prestring = if self.is_field { "field_" } else { "" };
        // TODO: handle basic templating.
        format!(
            r#"({}declaration
            (qualifiers)?
            type: [
                (template_type
                    name: (type_identifier) @variable.type
                )
                (identifier) @variable.type
                (type_identifier) @variable.type
                (primitive_type) @variable.type
            ]
            declarator: [
                (init_declarator
                    declarator: [
                        (identifier) @variable.label
                        (array_declarator
                            declarator: ({}identifier) @variable.label
                            size: (_) @variable.size
                        )
                    ]
                    value: (_)
                ) 
                (array_declarator
                    declarator: ({}identifier) @variable.label
                    size: (_) @variable.size
                )
                ({}identifier) @variable.label
            ]
        )"#,
            field_prestring, field_prestring, field_prestring, field_prestring
        )
    }
    fn process_match(
        &self,
        symbol_match: &tree_sitter::QueryMatch,
        file_path: &Path,
        shader_content: &str,
        scopes: &Vec<ShaderScope>,
        symbol_builder: &mut ShaderSymbolListBuilder,
    ) {
        let type_node = symbol_match.captures[0].node;
        let label_node = symbol_match.captures[1].node;
        let size_node = if symbol_match.captures.len() == 3 {
            Some(symbol_match.captures[2].node)
        } else {
            None
        };
        let range = ShaderRange::from(label_node.range());
        let scope_stack = self.compute_scope_stack(&scopes, &range);
        symbol_builder.add_variable(ShaderSymbol {
            label: get_name(shader_content, label_node).into(),
            requirement: None,
            data: ShaderSymbolData::Variables {
                ty: get_name(shader_content, type_node).into(),
                count: size_node.map(|s| match get_name(shader_content, s).parse::<u32>() {
                    Ok(value) => ShaderSymbolArray::Fixed(value),
                    Err(_) => ShaderSymbolArray::Unsized, // TODO: Need to resolve the parameter. Could use proxy tree same as for region conditions. For now, simply unsized array.
                }),
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

struct HlslEnumTreeParser {}

impl SymbolTreeParser for HlslEnumTreeParser {
    fn get_query(&self) -> String {
        r#"(enum_specifier
            name: (type_identifier) @enum.label
            body: (enumerator_list
                ((enumerator
                    name:(identifier) @enum.value.label
                    value: (_)  @enum.value.value
                )(",")?)*
            ) @enum.scope
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
        // TODO: differentiate enum class
        // TODO: Handle no values aswell (will be tricky with count...)
        let label_node = symbol_match.captures[0].node;
        let range = ShaderRange::from(label_node.range());
        let scope_stack = self.compute_scope_stack(&scopes, &range);
        let scope_node = symbol_match.captures[1].node;
        let scope_range = ShaderRange::from(scope_node.range());
        symbol_builder.add_type(ShaderSymbol {
            label: get_name(shader_content, label_node).into(),
            requirement: None,
            data: ShaderSymbolData::Enum {
                values: symbol_match.captures[2..]
                    .chunks(2)
                    .into_iter()
                    .map(|c| ShaderEnumValue {
                        label: get_name(shader_content, c[0].node).into(),
                        description: "".into(),
                        value: Some(get_name(shader_content, c[1].node).into()),
                        range: Some(ShaderRange::from(c[0].node.range())),
                    })
                    .collect(),
            },
            mode: ShaderSymbolMode::Runtime(ShaderSymbolRuntime::new(
                file_path.into(),
                range,
                Some(scope_range),
                scope_stack,
            )),
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
                        (
                            get_name(shader_content, e.node).into(),
                            ShaderRange::from(e.node.range()),
                        )
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

#[cfg(test)]
mod hlsl_parser_tests {
    use std::{path::Path, vec};

    use tree_sitter::{Query, QueryCursor, StreamingIterator};

    use crate::{
        position::{ShaderPosition, ShaderRange},
        shader::ShadingLanguage,
        symbols::{
            hlsl::hlsl_parser::{HlslEnumTreeParser, HlslFunctionTreeParser, HlslStructTreeParser},
            shader_module_parser::ShaderModuleParser,
            symbol_list::ShaderSymbolList,
            symbol_parser::{ShaderSymbolListBuilder, SymbolTreeParser},
            symbols::{
                ShaderEnumValue, ShaderMember, ShaderMethod, ShaderParameter, ShaderScope,
                ShaderSignature, ShaderSymbol, ShaderSymbolData, ShaderSymbolMode,
                ShaderSymbolRuntime,
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
        let query = Query::new(
            &tree_sitter_hlsl::LANGUAGE_HLSL.into(),
            parser.get_query().as_str(),
        )
        .unwrap();

        let mut shader_module_parser =
            ShaderModuleParser::from_shading_language(ShadingLanguage::Hlsl);
        let module = shader_module_parser
            .create_module(file_path, shader_content)
            .unwrap();
        let mut all_matches =
            query_cursor.matches(&query, module.tree.root_node(), module.content.as_bytes());
        while let Some(symbol_match) = all_matches.next() {
            parser.process_match(
                symbol_match,
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
            (
                ShaderSymbolData::Include { target: t1 },
                ShaderSymbolData::Include { target: t2 },
            ) => {
                assert!(t1 == t2, "Mismatching link")
            }
            (
                ShaderSymbolData::Macro {
                    value: v1,
                    parameters: p1,
                },
                ShaderSymbolData::Macro {
                    value: v2,
                    parameters: p2,
                },
            ) => {
                assert!(v1 == v2, "Mismatching macro");
                assert!(p1 == p2, "Mismatching macro parameters");
            }
            (ShaderSymbolData::Enum { values: v1 }, ShaderSymbolData::Enum { values: v2 }) => {
                assert!(v1.len() == v2.len(), "Invalid enum");
            }
            _ => panic!("data mismatch"),
        }
        match (&symbol.mode, &symbol_expected.mode) {
            (ShaderSymbolMode::Intrinsic(intrinsic0), ShaderSymbolMode::Intrinsic(intrinsic1)) => {
                assert!(
                    intrinsic0.description == intrinsic1.description,
                    "Mismatching description"
                );
                assert!(intrinsic0.link == intrinsic1.link, "Mismatching link");
            }
            (ShaderSymbolMode::Runtime(runtime0), ShaderSymbolMode::Runtime(runtime1)) => {
                assert!(
                    runtime0.file_path == runtime1.file_path,
                    "Mismatching file_path"
                );
                assert!(
                    runtime0.range == runtime1.range,
                    "Mismatching range ({:?} vs {:?})",
                    runtime0.range,
                    runtime1.range
                );
                assert!(
                    runtime0.scope == runtime1.scope,
                    "Mismatching scope ({:?} vs {:?})",
                    runtime0.scope,
                    runtime1.scope
                );
                assert!(
                    runtime0.scope_stack == runtime1.scope_stack,
                    "Mismatching scope_stack"
                );
            }
            (
                ShaderSymbolMode::RuntimeContext(_context0),
                ShaderSymbolMode::RuntimeContext(_context1),
            ) => {}
            _ => panic!("mode mismatch"),
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
                requirement: None,
                data: ShaderSymbolData::Struct {
                    constructors: vec![],
                    members: vec![
                        ShaderMember {
                            context: "TestStruct".into(),
                            parameters: ShaderParameter {
                                ty: "float".into(),
                                label: "member0".into(),
                                description: "".into(),
                                count: None,
                                range: Some(ShaderRange::new(
                                    ShaderPosition::new(1, 0),
                                    ShaderPosition::new(2, 0),
                                )),
                            },
                        },
                        ShaderMember {
                            context: "TestStruct".into(),
                            parameters: ShaderParameter {
                                ty: "float".into(),
                                label: "member1".into(),
                                description: "".into(),
                                count: None,
                                range: Some(ShaderRange::new(
                                    ShaderPosition::new(1, 0),
                                    ShaderPosition::new(2, 0),
                                )),
                            },
                        },
                    ],
                    methods: vec![ShaderMethod {
                        context: "TestStruct".into(),
                        label: "method".into(),
                        signature: ShaderSignature {
                            returnType: "float".into(),
                            description: "".into(),
                            parameters: vec![],
                        },
                        range: Some(ShaderRange::new(
                            ShaderPosition::new(1, 0),
                            ShaderPosition::new(2, 0),
                        )),
                    }],
                },
                mode: ShaderSymbolMode::Runtime(ShaderSymbolRuntime::new(
                    path.into(),
                    ShaderRange::new(ShaderPosition::new(1, 19), ShaderPosition::new(1, 29)),
                    None,
                    vec![],
                )),
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
                requirement: None,
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
                                range: Some(ShaderRange::new(
                                    ShaderPosition::new(1, 0),
                                    ShaderPosition::new(2, 0),
                                )),
                            },
                            ShaderParameter {
                                ty: "uint".into(),
                                label: "p1".into(),
                                description: "".into(),
                                count: None,
                                range: Some(ShaderRange::new(
                                    ShaderPosition::new(1, 0),
                                    ShaderPosition::new(2, 0),
                                )),
                            },
                        ],
                    }],
                },
                mode: ShaderSymbolMode::Runtime(ShaderSymbolRuntime::new(
                    path.into(),
                    ShaderRange::new(ShaderPosition::new(1, 17), ShaderPosition::new(1, 25)),
                    Some(ShaderScope::new(
                        ShaderPosition::new(1, 49),
                        ShaderPosition::new(2, 13),
                    )),
                    vec![],
                )),
            },
            &result.functions[0],
        );
    }
    #[test]
    fn enum_parser() {
        let path = Path::new("dontcare");
        let content = r"
            enum class EnumClassTest {
                VALUE0 = 0,
                VALUE1 = 1,
            };
        ";
        let result = parse(&HlslEnumTreeParser {}, path, content);
        compare(
            &ShaderSymbol {
                label: "EnumClassTest".into(),
                requirement: None,
                data: ShaderSymbolData::Enum {
                    values: vec![
                        ShaderEnumValue {
                            label: "VALUE0".into(),
                            description: "".into(),
                            value: Some("0".into()),
                            range: Some(ShaderRange::new(
                                ShaderPosition::new(2, 16),
                                ShaderPosition::new(2, 22),
                            )),
                        },
                        ShaderEnumValue {
                            label: "VALUE1".into(),
                            description: "".into(),
                            value: Some("1".into()),
                            range: Some(ShaderRange::new(
                                ShaderPosition::new(3, 16),
                                ShaderPosition::new(3, 22),
                            )),
                        },
                    ],
                },
                mode: ShaderSymbolMode::Runtime(ShaderSymbolRuntime::new(
                    path.into(),
                    ShaderRange::new(ShaderPosition::new(1, 23), ShaderPosition::new(1, 36)),
                    Some(ShaderScope::new(
                        ShaderPosition::new(1, 37),
                        ShaderPosition::new(4, 13),
                    )),
                    vec![],
                )),
            },
            &result.types[0],
        );
    }
}
