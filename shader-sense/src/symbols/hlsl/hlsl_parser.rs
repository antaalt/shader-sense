use std::path::Path;

use crate::symbols::symbol_parser::ShaderSymbolTreeBuilder;
use crate::symbols::symbols::{ShaderMember, ShaderSymbolType};

use crate::symbols::{
    symbol_parser::{get_name, SymbolTreeParser},
    symbols::{
        ShaderMethod, ShaderParameter, ShaderRange, ShaderSignature, ShaderSymbol, ShaderSymbolData,
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
        symbols: &mut ShaderSymbolTreeBuilder,
    ) {
        let label_node = matches.captures[1].node;
        let range = ShaderRange::from_range(label_node.range(), file_path.into());
        // Get parameters & add them as function scope variable.
        let parameters = matches.captures[2..matches.captures.len() - 1]
            .chunks(2)
            .map(|w| {
                let ty: String = get_name(shader_content, w[0].node).into();
                let label: String = get_name(shader_content, w[1].node).into();
                symbols.add_children(
                    ShaderSymbol {
                        label: label.clone(),
                        description: "".into(),
                        version: "".into(),
                        stages: vec![],
                        link: None,
                        data: ShaderSymbolData::Variables {
                            ty: ty.clone(),
                            count: None,
                        },
                        range: Some(ShaderRange::from_range(w[1].node.range(), file_path)),
                        content: None,
                    },
                    true,
                );
                ShaderParameter {
                    ty: ty,
                    label: label,
                    count: None,
                    description: "".into(),
                }
            })
            .collect::<Vec<ShaderParameter>>();
        symbols.add_children(
            ShaderSymbol {
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
                range: Some(range),
                content: None, // In GLSL, all function are global scope.
            },
            true,
        );
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
        symbols: &mut ShaderSymbolTreeBuilder,
    ) {
        let label_node = matches.captures[0].node;
        let range = ShaderRange::from_range(label_node.range(), file_path.into());

        // QUERY INNER METHODS
        let mut query_cursor = tree_sitter::QueryCursor::new();
        let methods = query_cursor
            .matches(
                &self.func_query,
                matches.captures[1].node,
                shader_content.as_bytes(),
            )
            .map(|matches| {
                let mut symbols = ShaderSymbolTreeBuilder::new(&|_| true);
                self.func_parser
                    .process_match(matches, file_path, shader_content, &mut symbols);
                symbols
                    .get_shader_symbol_tree()
                    .iter_root()
                    .filter(|s| s.is_type(ShaderSymbolType::Functions))
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
                let mut symbols = ShaderSymbolTreeBuilder::new(&|_| true);
                self.var_parser
                    .process_match(matches, file_path, shader_content, &mut symbols);
                symbols
                    .get_shader_symbol_tree()
                    .iter_root()
                    .filter(|s| s.is_type(ShaderSymbolType::Variables))
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
        symbols.add_children(
            ShaderSymbol {
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
                range: Some(range),
                content: None, // Struct are global scope
            },
            true,
        );
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
        symbol_builder: &mut ShaderSymbolTreeBuilder,
    ) {
        let type_node = matches.captures[0].node;
        let label_node = matches.captures[1].node;
        let size_node = if matches.captures.len() == 3 {
            Some(matches.captures[2].node)
        } else {
            None
        };
        let range = ShaderRange::from_range(label_node.range(), file_path.into());
        symbol_builder.add_children(
            ShaderSymbol {
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
                range: Some(range),
                content: None, // No child for variable
            },
            false,
        );
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
        symbol_builder: &mut ShaderSymbolTreeBuilder,
    ) {
        let label_node = matches.captures[0].node;
        let range = ShaderRange::from_range(label_node.range(), file_path.into());
        let label = get_name(shader_content, label_node).into();
        symbol_builder.add_children(
            ShaderSymbol {
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
                range: Some(range), // TODO:TREE: this should be range of whole expression.
                content: None,
            },
            false,
        );
    }
}
