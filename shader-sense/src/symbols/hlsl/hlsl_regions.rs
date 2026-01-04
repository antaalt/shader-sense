use std::num::ParseIntError;

use tree_sitter::{InputEdit, Point, Query, QueryCursor, StreamingIterator};

use crate::{
    position::{ShaderFilePosition, ShaderFileRange, ShaderPosition, ShaderRange},
    shader::{ShaderCompilationParams, ShadingLanguage},
    shader_error::{ShaderDiagnostic, ShaderDiagnosticSeverity, ShaderError},
    symbols::{
        prepocessor::{
            ShaderPreprocessor, ShaderPreprocessorContext, ShaderPreprocessorDefine,
            ShaderPreprocessorInclude, ShaderRegion,
        },
        shader_module::{ShaderModule, ShaderSymbols},
        shader_module_parser::get_tree_sitter_language,
        symbol_parser::{get_name, SymbolRegionFinder},
        symbol_provider::{SymbolIncludeCallback, SymbolProvider},
    },
};

// Some nice query resources
// https://davisvaughan.github.io/r-tree-sitter/reference/query-matches-and-captures.html
// https://parsiya.net/blog/knee-deep-tree-sitter-queries/
// Check https://github.com/tree-sitter/tree-sitter-cpp/blob/master/src/grammar.json & grammar.json to inspect how node are layout
// All symbols details can be found here
// https://github.com/tree-sitter/tree-sitter-cpp/blob/master/src/grammar.json
// Here the goal is mostly to compute #if regions & mark them as active or not
// This try resolving condition using macros through tree sitter.

macro_rules! assert_tree_sitter {
    ($file_path:expr, $condition:expr) => {
        if !$condition {
            return Err(ShaderError::InternalErr(format!(
                "Unexpected query failure {}:{} at : {}",
                file!(),
                line!(),
                $file_path.display(),
            )));
        }
    };
}
macro_rules! assert_field_name {
    ($file_path:expr, $cursor:expr, $value:expr) => {
        match $cursor.field_name() {
            Some(field_name) => {
                if field_name != $value {
                    return Err(ShaderError::SymbolQueryError(
                        format!(
                            "Unexpected query field name \"{}\" (at {}:{})",
                            field_name,
                            file!(),
                            line!(),
                        ),
                        ShaderFileRange::from(
                            $file_path.clone(),
                            ShaderRange::from($cursor.node().range()),
                        ),
                    ));
                }
            }
            None => {
                return Err(ShaderError::SymbolQueryError(
                    format!("Missing query field name (at {}:{})", file!(), line!(),),
                    ShaderFileRange::from(
                        $file_path.clone(),
                        ShaderRange::from($cursor.node().range()),
                    ),
                ));
            }
        }
    };
}
macro_rules! assert_node_kind {
    ($file_path:expr, $cursor:expr, $value:expr) => {
        if $cursor.node().kind() != $value {
            return Err(ShaderError::SymbolQueryError(
                format!(
                    "Unexpected query node kind \"{}\" (at {}:{})",
                    $cursor.node().kind(),
                    file!(),
                    line!(),
                ),
                ShaderFileRange::from(
                    $file_path.clone(),
                    ShaderRange::from($cursor.node().range()),
                ),
            ));
        }
    };
}
// Better API design:
/*struct SymbolTreeCursor {}
struct SymbolTreeCursorIter {
    cursor: tree_sitter::TreeCursor,
}
impl SymbolTreeCursor {
    // Iterator for siblings.
    // Each iterator
    pub fn iter() -> SymbolTreeCursorIter {}
}*/

struct ProxyTree {
    text: String,
    parser: tree_sitter::Parser,
    tree: tree_sitter::Tree,
}

/// Proxy tree to quickly parse small strings without recreating a whole tree.
impl ProxyTree {
    pub fn new(lang: &tree_sitter::Language) -> Self {
        let mut tree_sitter_parser = tree_sitter::Parser::new();
        tree_sitter_parser
            .set_language(lang)
            .expect("Error loading grammar");

        Self {
            text: "".into(),
            tree: tree_sitter_parser.parse("", None).unwrap(),
            parser: tree_sitter_parser,
        }
    }
    pub fn parse(&mut self, text: &str) -> Option<&tree_sitter::Tree> {
        let old_end_position = ShaderRange::whole(&self.text).end;
        let new_end_position = ShaderRange::whole(&text).end;
        self.tree.edit(&InputEdit {
            start_byte: 0,
            old_end_byte: self.text.len(), // Should use byte_offset instead
            new_end_byte: text.len(),
            start_position: Point::new(0, 0),
            old_end_position: Point::new(
                old_end_position.line as usize,
                old_end_position.pos as usize,
            ),
            new_end_position: Point::new(
                new_end_position.line as usize,
                new_end_position.pos as usize,
            ),
        });
        self.text = text.into();
        self.tree = self.parser.parse(&self.text, Some(&self.tree))?;
        Some(&self.tree)
    }
}

pub struct HlslSymbolRegionFinder {
    shading_language: ShadingLanguage,
    query_if: tree_sitter::Query,
}

impl HlslSymbolRegionFinder {
    pub fn new(shading_language: ShadingLanguage) -> Self {
        let lang = get_tree_sitter_language(shading_language);
        let query_if = Query::new(
            &lang,
            r#"
        [
            (preproc_if)
            (preproc_ifdef)
        ] @inactive
        "#,
        )
        .unwrap();
        Self {
            shading_language,
            query_if,
        }
    }
    fn parse_number(number: &str) -> Result<i32, ParseIntError> {
        if number.starts_with("0x") && number.len() > 2 {
            let prefixed_number = number
                .strip_prefix("0x")
                .unwrap_or(number.strip_prefix("0X").unwrap_or(number));
            i32::from_str_radix(prefixed_number, 16)
        } else if number.starts_with("0") && number.len() > 1 {
            let prefixed_number = number.strip_prefix("0").unwrap_or(number);
            i32::from_str_radix(prefixed_number, 8)
        } else {
            number.parse::<i32>()
        }
    }
    fn get_expression_as_i32(
        proxy_tree: &mut ProxyTree,
        context: &ShaderPreprocessorContext,
        value: &str,
        position: &ShaderFilePosition,
    ) -> Result<i32, ShaderError> {
        if let Some(condition_tree) = proxy_tree.parse(value) {
            let proxy_shader_module = ShaderModule {
                tree: condition_tree.clone(), // TODO: should be a shadermodule ref to avoid cloning. Could use some trait ShaderModule.
                file_path: position.file_path.clone(),
                content: value.into(),
            };
            let mut cursor = proxy_shader_module.tree.root_node().walk();
            cursor.goto_first_child(); // Skip translation_unit node.
            loop {
                match cursor.node().kind() {
                    "expression_statement" => {
                        cursor.goto_first_child();
                        // TODO: storing variables with wrong offset here. Could use a trait offset
                        return Self::resolve_condition(
                            proxy_tree,
                            cursor,
                            &proxy_shader_module,
                            context,
                        );
                    }
                    "ERROR" => {
                        // Might be a wrong child, try to go futher if its valid
                        cursor.goto_first_child();
                    }
                    _ => {
                        break;
                    }
                }
            }
            Err(ShaderError::SymbolQueryError(
                format!("Macro expression could not be solved ({}).", value),
                ShaderFileRange::from(
                    position.file_path.clone(),
                    ShaderRange::new(position.position.clone(), position.position.clone()),
                ),
            ))
        } else {
            Err(ShaderError::SymbolQueryError(
                format!("Macro expression could not be parsed ({}).", value),
                ShaderFileRange::from(
                    position.file_path.clone(),
                    ShaderRange::new(position.position.clone(), position.position.clone()),
                ),
            ))
        }
    }
    fn get_define_as_i32_depth(
        proxy_tree: &mut ProxyTree,
        context: &ShaderPreprocessorContext,
        value: &str,
        position: &ShaderFilePosition,
        depth: u32,
    ) -> Result<i32, ShaderError> {
        if depth == 0 {
            Err(ShaderError::SymbolQueryError(
                format!("Failed to parse number_literal {}", value),
                ShaderFileRange::from(
                    position.file_path.clone(),
                    ShaderRange::new(position.position.clone(), position.position.clone()),
                ),
            ))
        } else {
            // Here we recurse define value cuz a define might just be an alias for another define.
            match context.get_define_value(value) {
                Some(define_value) => {
                    // Recurse result up to 10 times for macro that define other macro.
                    Self::get_define_as_i32_depth(
                        proxy_tree,
                        &context,
                        define_value.as_str(),
                        &position,
                        depth - 1,
                    )
                }
                None => {
                    fn contain_expression_character(s: &str) -> bool {
                        // Non exhaustive list of character marking an expression
                        // TODO: make list exhaustive ?
                        let chars = ['?', '!', '+', '-', '|', '&', '^', '/', '*'];
                        s.chars().any(|c| chars.contains(&c))
                    }
                    // Not found in define list. Try to parse as number
                    match Self::parse_number(value) {
                        Ok(parsed_value) => Ok(parsed_value),
                        Err(_) => {
                            if contain_expression_character(value) {
                                // Neither in define list, nor a number, try to parse as expression
                                Self::get_expression_as_i32(proxy_tree, context, value, position)
                            } else {
                                Ok(0) // Return false instead of error
                            }
                        }
                    }
                }
            }
        }
    }
    fn get_define_as_i32(
        proxy_tree: &mut ProxyTree,
        context: &ShaderPreprocessorContext,
        value: &str,
        position: &ShaderFilePosition,
    ) -> Result<i32, ShaderError> {
        // Recurse result up to 10 times for macro that define other macro.
        Self::get_define_as_i32_depth(proxy_tree, context, value, position, 10)
    }
    fn is_define_defined(context: &ShaderPreprocessorContext, name: &str) -> i32 {
        context.get_define_value(name).is_some() as i32
    }
    fn resolve_condition(
        proxy_tree: &mut ProxyTree,
        cursor: tree_sitter::TreeCursor,
        shader_module: &ShaderModule,
        context: &ShaderPreprocessorContext,
    ) -> Result<i32, ShaderError> {
        let mut cursor = cursor;
        match cursor.node().kind() {
            "preproc_defined" => {
                // check if macro is defined.
                let _ = r#"condition: (preproc_defined
                    "defined"
                    "("?
                    (identifier) @identifier
                    ")"?
                )"#;
                assert_tree_sitter!(shader_module.file_path, cursor.goto_first_child());
                assert_node_kind!(shader_module.file_path, cursor, "defined");
                assert_tree_sitter!(shader_module.file_path, cursor.goto_next_sibling());
                // paranthesis not mandatory...
                if cursor.node().kind() == "(" {
                    assert_tree_sitter!(shader_module.file_path, cursor.goto_next_sibling());
                }
                assert_node_kind!(shader_module.file_path, cursor, "identifier");
                let condition_macro = get_name(&shader_module.content, cursor.node());
                Ok(Self::is_define_defined(context, condition_macro))
            }
            "number_literal" => {
                // As simple as it is, but need to be warry of hexa or octal values.
                let _ = r#"condition: (number_literal)"#;
                let number_str = get_name(&shader_module.content, cursor.node());
                let parsed_number = Self::parse_number(number_str);
                match parsed_number {
                    Ok(value) => Ok(value),
                    Err(_) => Err(ShaderError::SymbolQueryError(
                        format!("Failed to parse number_literal {}", number_str),
                        ShaderFileRange::from(
                            shader_module.file_path.clone(),
                            ShaderRange::from(cursor.node().range()),
                        ),
                    )),
                }
            }
            "identifier" => {
                // An identifier is simply a macro.
                let _ = r#"condition: (identifier)"#;
                let condition_macro = get_name(&shader_module.content, cursor.node());
                let value = Self::get_define_as_i32(
                    proxy_tree,
                    context,
                    condition_macro,
                    &ShaderFilePosition::from(
                        shader_module.file_path.clone(),
                        ShaderPosition::from(cursor.node().start_position()),
                    ),
                )?;
                Ok(value)
            }
            "binary_expression" => {
                // A binary expression might compare two expression or identifier. Recurse them.
                let _ = r#"condition: (binary_expression
                    left:(_) @identifier.or.expression
                    operator:(_) @binary.operator
                    right(_)  @identifier.or.expression
                )"#;
                assert_tree_sitter!(shader_module.file_path, cursor.goto_first_child());
                assert_field_name!(shader_module.file_path, cursor, "left");
                let left_condition =
                    Self::resolve_condition(proxy_tree, cursor.clone(), shader_module, context)?;
                assert_tree_sitter!(shader_module.file_path, cursor.goto_next_sibling());
                assert_field_name!(shader_module.file_path, cursor, "operator");
                let operator = cursor.node().kind();
                assert_tree_sitter!(shader_module.file_path, cursor.goto_next_sibling());
                assert_field_name!(shader_module.file_path, cursor, "right");
                let right_condition =
                    Self::resolve_condition(proxy_tree, cursor.clone(), shader_module, context)?;
                match operator {
                    "&&" => Ok(((left_condition != 0) && (right_condition != 0)) as i32),
                    "||" => Ok(((left_condition != 0) || (right_condition != 0)) as i32),
                    "|" => Ok(((left_condition != 0) | (right_condition != 0)) as i32),
                    "&" => Ok(((left_condition != 0) & (right_condition != 0)) as i32),
                    "^" => Ok(left_condition ^ right_condition),
                    "==" => Ok((left_condition == right_condition) as i32),
                    "!=" => Ok((left_condition != right_condition) as i32),
                    "<" => Ok((left_condition < right_condition) as i32),
                    ">" => Ok((left_condition > right_condition) as i32),
                    "<=" => Ok((left_condition <= right_condition) as i32),
                    ">=" => Ok((left_condition >= right_condition) as i32),
                    ">>" => Ok(left_condition >> right_condition),
                    "<<" => Ok(left_condition << right_condition),
                    "+" => Ok(left_condition + right_condition),
                    "-" => Ok(left_condition - right_condition),
                    "*" => Ok(left_condition * right_condition),
                    "/" => Ok(left_condition / right_condition),
                    "%" => Ok(left_condition % right_condition),
                    value => Err(ShaderError::SymbolQueryError(
                        format!("Binary operator unhandled for {}", value),
                        ShaderFileRange::from(
                            shader_module.file_path.clone(),
                            ShaderRange::from(cursor.node().range()),
                        ),
                    )),
                }
            }
            "unary_expression" => {
                let _ = r#"condition: (unary_expression 
                    operator: ["!" "-" "+" "~"] @operator
                    argument: (identifier) @identifier
                )"#;
                // Operator = !~-+
                assert_tree_sitter!(shader_module.file_path, cursor.goto_first_child());
                assert_field_name!(shader_module.file_path, cursor, "operator");
                let operator = cursor.node().kind();
                assert_node_kind!(shader_module.file_path, cursor, "!");
                assert_tree_sitter!(shader_module.file_path, cursor.goto_next_sibling());
                assert_field_name!(shader_module.file_path, cursor, "argument");
                let value =
                    Self::resolve_condition(proxy_tree, cursor.clone(), shader_module, context)?;
                match operator {
                    "!" => Ok(!(value != 0) as i32), // Comparing as bool
                    "+" => Ok(value),
                    "-" => Ok(-value),
                    "~" => Ok(!value),
                    value => Err(ShaderError::SymbolQueryError(
                        format!("Unary operator unhandled for {}", value),
                        ShaderFileRange::from(
                            shader_module.file_path.clone(),
                            ShaderRange::from(cursor.node().range()),
                        ),
                    )),
                }
            }
            "parenthesized_expression" => {
                // This expression children is an anonymous condition.
                let _ = r#"condition: (parenthesized_expression 
                    "("
                    (_) @anykindofexpression
                    ")"
                )"#;
                assert_tree_sitter!(shader_module.file_path, cursor.goto_first_child());
                assert_tree_sitter!(shader_module.file_path, cursor.goto_next_sibling());
                Self::resolve_condition(proxy_tree, cursor, shader_module, context)
            }
            "call_expression" => {
                // This expression is a function call
                let _ = r#"condition: (call_expression 
                    function: (identifier) @function.name
                    arguments: (argument_list
                        "("
                        (
                            (identifier) @argument
                            (",")?
                        )*
                        ")"
                    )
                )"#;
                // TODO: solve this.
                // We need to parse its arguments, and its content.
                // It will then require to be parsed when called.
                // Should simply add Option<Vec<String>> to define for parameter. If none, its an identifier, else a call expression.
                // Will need more complex setup thant HashMap<String, String> then. HashMap<String, Define> probably
                Err(ShaderError::SymbolQueryError(
                    format!(
                        "Call expression solving not implemented ({}).",
                        get_name(&shader_module.content, cursor.node())
                    ),
                    ShaderFileRange::from(
                        shader_module.file_path.clone(),
                        ShaderRange::from(cursor.node().range()),
                    ),
                ))
            }
            value => Err(ShaderError::SymbolQueryError(
                format!("Condition unhandled for {}", value),
                ShaderFileRange::from(
                    shader_module.file_path.clone(),
                    ShaderRange::from(cursor.node().range()),
                ),
            )),
        }
    }
}
impl SymbolRegionFinder for HlslSymbolRegionFinder {
    fn query_regions_in_node<'a>(
        &self,
        shader_module: &ShaderModule,
        symbol_provider: &SymbolProvider,
        shader_params: &ShaderCompilationParams,
        node: tree_sitter::Node,
        preprocessor: &mut ShaderPreprocessor,
        context: &'a mut ShaderPreprocessorContext,
        include_callback: &'a mut SymbolIncludeCallback<'a>,
        mut old_symbols: Option<ShaderSymbols>,
    ) -> Result<Vec<ShaderRegion>, ShaderError> {
        fn get_defined_macros_for_position(
            last_position: &ShaderPosition,
            position: &ShaderPosition,
            defines: &Vec<ShaderPreprocessorDefine>,
        ) -> Vec<ShaderPreprocessorDefine> {
            defines
                .iter()
                .filter(|define| {
                    let range = define.get_range();
                    range.start >= *last_position && range.end <= *position
                })
                .cloned()
                .collect::<Vec<ShaderPreprocessorDefine>>()
        }
        // Query regions
        let mut query_cursor = QueryCursor::new();
        let mut regions = Vec::new();
        let mut last_processed_position = ShaderPosition::zero();
        let mut all_match =
            query_cursor.matches(&self.query_if, node, shader_module.content.as_bytes());
        while let Some(region_match) = all_match.next() {
            fn parse_region<'a>(
                proxy_tree: &mut ProxyTree,
                shader_module: &ShaderModule,
                symbol_provider: &SymbolProvider,
                shader_params: &ShaderCompilationParams,
                preprocessor: &mut ShaderPreprocessor,
                cursor: &mut tree_sitter::TreeCursor,
                found_active_region: bool,
                context: &mut ShaderPreprocessorContext,
                include_callback: &mut SymbolIncludeCallback<'a>,
                mut old_symbols: &mut Option<ShaderSymbols>,
                last_processed_position: &mut ShaderPosition,
            ) -> Result<Vec<ShaderRegion>, ShaderError> {
                assert_tree_sitter!(shader_module.file_path, cursor.goto_first_child());
                // Process includes here as they will impact defines which impact regions.
                let region_start = ShaderPosition::from(cursor.node().range().start_point);
                // Find include before this region that defines its context.
                // Need to filter already processed includes.
                let includes_before = preprocessor
                    .includes
                    .iter_mut()
                    .filter(|include| {
                        include.get_range().end < region_start
                            && include.get_range().start > *last_processed_position
                    })
                    .collect::<Vec<&mut ShaderPreprocessorInclude>>();
                for include_before in includes_before {
                    // Mark as processed before computing its child.
                    context.push_directory_stack(&include_before.get_absolute_path());
                    context.append_defines(get_defined_macros_for_position(
                        last_processed_position,
                        &include_before.get_range().start,
                        &preprocessor.defines,
                    ));
                    // Find include and take its data.
                    let include_old_symbol = match &mut old_symbols {
                        Some(old_symbols) => {
                            match old_symbols.preprocessor.includes.iter_mut().find(|i| {
                                i.get_absolute_path() == include_before.get_absolute_path()
                            }) {
                                Some(include) => include.cache.take(),
                                None => None,
                            }
                        }
                        None => None,
                    };
                    match symbol_provider.process_include(
                        context,
                        include_before,
                        shader_params,
                        include_callback,
                        include_old_symbol,
                    ) {
                        Ok(_) => {}
                        Err(err) => match err {
                            ShaderError::SymbolQueryError(message, shader_range) => {
                                // Include not found or limit reached.
                                preprocessor.diagnostics.push(ShaderDiagnostic {
                                    severity: ShaderDiagnosticSeverity::Warning,
                                    error: message,
                                    range: shader_range,
                                });
                            }
                            err => Err(err)?, // Propagate the error.
                        },
                    }
                    *last_processed_position = include_before.get_range().end.clone();
                }
                // Include should handle adding defines, but if no include or defines after last include, need to add remaining ones.
                context.append_defines(get_defined_macros_for_position(
                    last_processed_position,
                    &region_start,
                    &preprocessor.defines,
                ));
                *last_processed_position = region_start;
                // Process regions
                let (is_active_region, region_start) = match cursor.node().kind() {
                    "#ifdef" => {
                        assert_tree_sitter!(shader_module.file_path, cursor.goto_next_sibling());
                        assert_field_name!(shader_module.file_path, cursor, "name");
                        let condition_macro = get_name(&shader_module.content, cursor.node());
                        let position = ShaderPosition::from(cursor.node().range().end_point);
                        (
                            HlslSymbolRegionFinder::is_define_defined(context, condition_macro),
                            position,
                        )
                    }
                    "#ifndef" => {
                        assert_tree_sitter!(shader_module.file_path, cursor.goto_next_sibling());
                        assert_field_name!(shader_module.file_path, cursor, "name");
                        let condition_macro = get_name(&shader_module.content, cursor.node());
                        let position = ShaderPosition::from(cursor.node().range().end_point);
                        (
                            1 - HlslSymbolRegionFinder::is_define_defined(context, condition_macro),
                            position,
                        )
                    }
                    "#if" => {
                        assert_tree_sitter!(shader_module.file_path, cursor.goto_next_sibling());
                        assert_field_name!(shader_module.file_path, cursor, "condition");
                        let position = ShaderPosition::from(cursor.node().range().end_point);
                        (
                            match HlslSymbolRegionFinder::resolve_condition(
                                proxy_tree,
                                cursor.clone(),
                                shader_module,
                                context,
                            ) {
                                Ok(value) => value,
                                Err(err) => {
                                    match err.into_diagnostic(ShaderDiagnosticSeverity::Warning) {
                                        Some(diagnostic) => {
                                            preprocessor.diagnostics.push(diagnostic);
                                            0 // Return 0 as default
                                        }
                                        None => return Err(err),
                                    }
                                }
                            },
                            position,
                        )
                    }
                    "#else" => (
                        if found_active_region { 0 } else { 1 },
                        ShaderPosition::from(cursor.node().range().end_point),
                    ),
                    "#elif" => {
                        assert_tree_sitter!(shader_module.file_path, cursor.goto_next_sibling());
                        assert_field_name!(shader_module.file_path, cursor, "condition");
                        let position = ShaderPosition::from(cursor.node().range().end_point);
                        (
                            if found_active_region {
                                0
                            } else {
                                match HlslSymbolRegionFinder::resolve_condition(
                                    proxy_tree,
                                    cursor.clone(),
                                    shader_module,
                                    context,
                                ) {
                                    Ok(value) => value,
                                    Err(err) => match err
                                        .into_diagnostic(ShaderDiagnosticSeverity::Warning)
                                    {
                                        Some(diagnostic) => {
                                            preprocessor.diagnostics.push(diagnostic);
                                            0 // Return 0 as default
                                        }
                                        None => return Err(err),
                                    },
                                }
                            },
                            position,
                        )
                    }
                    //"#elifdef" => {}
                    child => {
                        return Err(ShaderError::SymbolQueryError(
                            format!("preproc operator not implemented: {}", child),
                            ShaderFileRange::from(
                                shader_module.file_path.clone(),
                                ShaderRange::from(cursor.node().range()),
                            ),
                        ));
                    }
                };
                // Now find alternative blocks & loop on it
                let mut regions = Vec::new();
                let mut previous_node = cursor.node();
                while cursor.goto_next_sibling() {
                    match cursor.field_name() {
                        Some(field_name) => {
                            if field_name == "alternative" {
                                let region_end =
                                    ShaderPosition::from(previous_node.range().end_point);
                                let region_range = ShaderRange::new(region_start, region_end);
                                regions.push(ShaderRegion::new(
                                    region_range.clone(),
                                    is_active_region != 0,
                                ));
                                // Filter out local define & include in region as it may impact next region in file.
                                preprocessor.defines.retain(|define| {
                                    let range = define.get_range();
                                    is_active_region != 0
                                        || (is_active_region == 0
                                            && !region_range.contain_bounds(&range))
                                });
                                preprocessor.includes.retain(|include| {
                                    is_active_region != 0
                                        || (is_active_region == 0
                                            && !region_range.contain_bounds(&include.get_range()))
                                });

                                let mut next_region = parse_region(
                                    proxy_tree,
                                    shader_module,
                                    symbol_provider,
                                    shader_params,
                                    preprocessor,
                                    cursor,
                                    (is_active_region != 0) || found_active_region,
                                    context,
                                    include_callback,
                                    old_symbols,
                                    last_processed_position,
                                )?;
                                regions.append(&mut next_region);
                                return Ok(regions);
                            } else {
                                previous_node = cursor.node();
                            }
                        }
                        None => {
                            match cursor.node().kind() {
                                // Don't store endif because we already reached the end.
                                "#endif" => {}
                                // Some node end their line too late vs their content bcs of new line node.
                                // Read their content last node instead and skip new line.
                                "preproc_call" | "preproc_def" => {
                                    let mut internal_cursor = cursor.clone();
                                    internal_cursor.goto_first_child();
                                    previous_node = internal_cursor.node();
                                    while internal_cursor.goto_next_sibling() {
                                        if internal_cursor.node().kind() != "\n" {
                                            previous_node = internal_cursor.node();
                                        }
                                    }
                                }
                                _ => previous_node = cursor.node(),
                            }
                        }
                    }
                }
                let end_position = ShaderPosition::from(previous_node.range().end_point);
                let region_range = ShaderRange::new(region_start, end_position);
                regions.push(ShaderRegion::new(
                    region_range.clone(),
                    is_active_region != 0,
                ));
                // Filter out local define & include in region as it may impact next region in file.
                preprocessor.defines.retain(|define| {
                    let range = define.get_range();
                    is_active_region != 0
                        || (is_active_region == 0 && !region_range.contain_bounds(&range))
                });
                preprocessor.includes.retain(|include| {
                    is_active_region != 0
                        || (is_active_region == 0
                            && !region_range.contain_bounds(&include.get_range()))
                });
                Ok(regions)
            }
            let node = region_match.captures[0].node;
            let mut cursor = node.walk();
            let mut proxy_tree = ProxyTree::new(&get_tree_sitter_language(self.shading_language));
            regions.append(&mut parse_region(
                &mut proxy_tree,
                shader_module,
                symbol_provider,
                shader_params,
                preprocessor,
                &mut cursor,
                false,
                context,
                include_callback,
                &mut old_symbols,
                &mut last_processed_position,
            )?)
        }
        // Handle includes that were not dealt by regions.
        let include_lefts = preprocessor
            .includes
            .iter_mut()
            .filter(|include| include.get_range().start > last_processed_position)
            .collect::<Vec<&mut ShaderPreprocessorInclude>>();
        for include_left in include_lefts {
            context.push_directory_stack(&include_left.get_absolute_path());
            context.append_defines(get_defined_macros_for_position(
                &last_processed_position,
                &include_left.get_range().start,
                &preprocessor.defines,
            ));
            // Find include and take its data.
            let include_old_symbol = match &mut old_symbols {
                Some(old_symbols) => {
                    match old_symbols
                        .preprocessor
                        .includes
                        .iter_mut()
                        .find(|i| i.get_absolute_path() == include_left.get_absolute_path())
                    {
                        Some(include) => include.cache.take(),
                        None => None,
                    }
                }
                None => None,
            };
            match symbol_provider.process_include(
                context,
                include_left,
                shader_params,
                include_callback,
                include_old_symbol,
            ) {
                Ok(_) => {}
                Err(err) => match err {
                    ShaderError::SymbolQueryError(message, shader_range) => {
                        // Include not found or limit reached.
                        preprocessor.diagnostics.push(ShaderDiagnostic {
                            severity: ShaderDiagnosticSeverity::Warning,
                            error: message,
                            range: shader_range,
                        });
                    }
                    err => Err(err)?, // Propagate the error.
                },
            }
            last_processed_position = include_left.get_range().end.clone();
        }
        let define_after_last_include = preprocessor
            .defines
            .iter()
            .filter(|define| {
                let range = define.get_range();
                range.start > last_processed_position
            })
            .cloned()
            .collect::<Vec<ShaderPreprocessorDefine>>();
        context.append_defines(define_after_last_include);

        Ok(regions)
    }
}
