use tree_sitter::{Query, QueryCursor};

use crate::{
    include::{canonicalize, IncludeHandler},
    shader::ShadingLanguageTag,
    shader_error::{ShaderDiagnostic, ShaderDiagnosticSeverity, ShaderError},
};

use super::{
    shader_language::ShaderLanguage,
    symbol_parser::{
        ShaderSymbolListBuilder, SymbolLabelChainProvider, SymbolLabelProvider, SymbolRegionFinder,
        SymbolTreeFilter, SymbolTreeParser, SymbolTreePreprocessorParser,
    },
    symbol_tree::SymbolTree,
    symbols::{
        ShaderPosition, ShaderPreprocessor, ShaderPreprocessorContext, ShaderPreprocessorInclude,
        ShaderRange, ShaderScope, ShaderSymbol, ShaderSymbolList,
    },
};

pub struct SymbolProvider {
    symbol_parsers: Vec<(Box<dyn SymbolTreeParser>, tree_sitter::Query)>,
    symbol_filters: Vec<Box<dyn SymbolTreeFilter>>,
    scope_query: Query,
    error_query: Query,

    preprocessor_parsers: Vec<(Box<dyn SymbolTreePreprocessorParser>, tree_sitter::Query)>,
    region_finder: Box<dyn SymbolRegionFinder>,
    word_chain_provider: Box<dyn SymbolLabelChainProvider>,
    word_provider: Box<dyn SymbolLabelProvider>,
}

pub type SymbolIncludeCallback<'a> = dyn FnMut(
        ShaderPreprocessorInclude,
        &mut ShaderPreprocessorContext,
        &mut IncludeHandler,
    ) -> Result<(), ShaderError>
    + 'a;

pub fn default_include_callback<T: ShadingLanguageTag>(
    include: ShaderPreprocessorInclude,
    context: &mut ShaderPreprocessorContext,
    include_handler: &mut IncludeHandler,
) -> Result<(), ShaderError> {
    if !context.has_visited(&include.absolute_path) {
        let mut language = ShaderLanguage::new(T::get_language());
        let symbol_provider = language.create_symbol_provider();
        let include_module = language.create_module(
            &include.absolute_path,
            std::fs::read_to_string(&include.absolute_path)
                .unwrap()
                .as_str(),
        )?;
        let _include_preprocessor = symbol_provider.query_preprocessor(
            &include_module,
            context,
            include_handler,
            &mut default_include_callback::<T>,
        )?;
        Ok(())
    } else {
        // Avoid stack overflow by not recomputing same file infinitely.
        Ok(())
    }
}

impl SymbolProvider {
    pub fn new(
        language: tree_sitter::Language,
        parsers: Vec<Box<dyn SymbolTreeParser>>,
        filters: Vec<Box<dyn SymbolTreeFilter>>,
        preprocessor_parsers: Vec<Box<dyn SymbolTreePreprocessorParser>>,
        region_finder: Box<dyn SymbolRegionFinder>,
        word_chain_provider: Box<dyn SymbolLabelChainProvider>,
        word_provider: Box<dyn SymbolLabelProvider>,
    ) -> Self {
        let scope_query = r#"(compound_statement
            "{"? @scope.start
            "}"? @scope.end
        ) @scope"#;
        let error_query = r#"(ERROR) @error"#;
        Self {
            symbol_parsers: parsers
                .into_iter()
                .map(|e| {
                    // Cache query
                    let query = Query::new(language, e.get_query().as_str()).unwrap();
                    (e, query)
                })
                .collect(),
            symbol_filters: filters,
            scope_query: tree_sitter::Query::new(language.clone(), scope_query).unwrap(),
            error_query: tree_sitter::Query::new(language.clone(), error_query).unwrap(),
            preprocessor_parsers: preprocessor_parsers
                .into_iter()
                .map(|e| {
                    // Cache query
                    let query = Query::new(language, e.get_query().as_str()).unwrap();
                    (e, query)
                })
                .collect(),
            region_finder: region_finder,
            word_chain_provider,
            word_provider,
        }
    }
    pub fn query_file_scopes(&self, symbol_tree: &SymbolTree) -> Vec<ShaderScope> {
        // TODO: look for namespace aswell.
        // Should be per lang instead.
        fn join_scope(mut lhs: ShaderRange, rhs: ShaderRange) -> ShaderScope {
            lhs.start = std::cmp::min(lhs.start, rhs.start);
            lhs.end = std::cmp::min(lhs.end, rhs.end);
            lhs
        }
        let mut query_cursor = QueryCursor::new();
        let mut scopes = Vec::new();
        for matche in query_cursor.matches(
            &self.scope_query,
            symbol_tree.tree.root_node(),
            symbol_tree.content.as_bytes(),
        ) {
            scopes.push(match matche.captures.len() {
                // one body
                1 => {
                    ShaderScope::from_range(matche.captures[0].node.range(), &symbol_tree.file_path)
                }
                // a bit weird, a body and single curly brace ? mergin them to be safe.
                2 => join_scope(
                    ShaderScope::from_range(
                        matche.captures[0].node.range(),
                        &symbol_tree.file_path,
                    ),
                    ShaderScope::from_range(
                        matche.captures[1].node.range(),
                        &symbol_tree.file_path,
                    ),
                ),
                // Remove curly braces from scope.
                3 => {
                    let curly_start = matche.captures[1].node.range();
                    let curly_end = matche.captures[2].node.range();
                    ShaderScope::from_range(
                        tree_sitter::Range {
                            start_byte: curly_start.end_byte,
                            end_byte: curly_end.start_byte,
                            start_point: curly_start.end_point,
                            end_point: curly_end.start_point,
                        },
                        &symbol_tree.file_path,
                    )
                }
                _ => unreachable!("Query should not return more than 3 match."),
            });
        }
        scopes
    }
    pub fn query_preprocessor<'a>(
        &self,
        symbol_tree: &SymbolTree,
        context: &'a mut ShaderPreprocessorContext,
        include_handler: &mut IncludeHandler,
        include_callback: &'a mut SymbolIncludeCallback<'a>,
    ) -> Result<ShaderPreprocessor, ShaderError> {
        // Update context.
        context
            .visited_dependencies
            .insert(symbol_tree.file_path.clone());
        if let Some(parent) = symbol_tree.file_path.parent() {
            context.directory_stack.push(canonicalize(parent).unwrap());
        }
        let mut preprocessor = ShaderPreprocessor::new(context.clone());
        for parser in &self.preprocessor_parsers {
            let mut query_cursor = QueryCursor::new();
            for matches in query_cursor.matches(
                &parser.1,
                symbol_tree.tree.root_node(),
                symbol_tree.content.as_bytes(),
            ) {
                parser.0.process_match(
                    matches,
                    &symbol_tree.file_path,
                    &symbol_tree.content,
                    &mut preprocessor,
                    include_handler,
                );
            }
        }
        // Query regions.
        // Will filter includes & defines in inactive regions
        preprocessor.regions = self.region_finder.query_regions_in_node(
            symbol_tree,
            symbol_tree.tree.root_node(),
            &mut preprocessor,
            context,
            include_handler,
            include_callback,
        )?;
        // Mark this shader as once if pragma once is set.
        if let Some(once_byte_offset) = symbol_tree.content.find("#pragma once") {
            preprocessor.once = match ShaderPosition::from_byte_offset(
                &symbol_tree.content,
                once_byte_offset,
                &symbol_tree.file_path,
            ) {
                Ok(position) => preprocessor
                    .regions
                    .iter()
                    .find(|region| !region.is_active && region.range.contain(&position))
                    .is_none(),
                Err(_err) => false,
            };
        }
        // Add errors
        let mut query_error_cursor = QueryCursor::new();
        for matches in query_error_cursor.matches(
            &self.error_query,
            symbol_tree.tree.root_node(),
            symbol_tree.content.as_bytes(),
        ) {
            preprocessor.diagnostics.push(ShaderDiagnostic {
                severity: ShaderDiagnosticSeverity::Warning,
                error: "Failed to parse this code. Some symbols might be missing from providers."
                    .into(),
                range: ShaderRange::from_range(
                    matches.captures[0].node.range(),
                    &symbol_tree.file_path,
                ),
            });
        }
        Ok(preprocessor)
    }
    pub fn query_file_symbols(
        &self,
        symbol_tree: &SymbolTree,
    ) -> Result<ShaderSymbolList, ShaderError> {
        // TODO: Should use something else than name...
        // Required for shader stage filtering...
        let file_name = symbol_tree
            .file_path
            .file_name()
            .unwrap()
            .to_string_lossy()
            .to_string();
        let filter_symbol = |symbol: &ShaderSymbol| -> bool {
            // Dont filter inactive regions here on parsing, to avoid recomputing all symbols on regions update.
            let mut is_retained = true;
            for filter in &self.symbol_filters {
                is_retained = is_retained & filter.filter_symbol(symbol, &file_name);
            }
            is_retained
        };
        let mut symbol_list_builder = ShaderSymbolListBuilder::new(&filter_symbol);
        let scopes = self.query_file_scopes(symbol_tree);
        for parser in &self.symbol_parsers {
            let mut query_cursor = QueryCursor::new();
            for matches in query_cursor.matches(
                &parser.1,
                symbol_tree.tree.root_node(),
                symbol_tree.content.as_bytes(),
            ) {
                parser.0.process_match(
                    matches,
                    &symbol_tree.file_path,
                    &symbol_tree.content,
                    &scopes,
                    &mut symbol_list_builder,
                );
            }
        }
        let symbols = symbol_list_builder.get_shader_symbol_list();
        Ok(symbols)
    }
    pub fn get_word_chain_range_at_position(
        &self,
        symbol_tree: &SymbolTree,
        position: &ShaderPosition,
    ) -> Result<Vec<(String, ShaderRange)>, ShaderError> {
        self.word_chain_provider
            .find_label_chain_at_position_in_node(
                symbol_tree,
                symbol_tree.tree.root_node(),
                position,
            )
    }
    pub fn get_word_range_at_position(
        &self,
        symbol_tree: &SymbolTree,
        position: &ShaderPosition,
    ) -> Result<(String, ShaderRange), ShaderError> {
        self.word_provider.find_label_at_position_in_node(
            symbol_tree,
            symbol_tree.tree.root_node(),
            position,
        )
    }
}
