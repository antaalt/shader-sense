use std::{cell::RefCell, collections::HashMap, path::PathBuf, rc::Rc};

use tree_sitter::{Query, QueryCursor};

use crate::{
    shader::ShadingLanguageTag,
    shader_error::{ShaderDiagnostic, ShaderDiagnosticSeverity, ShaderError},
};

use super::{
    shader_language::ShaderLanguage,
    symbol_parser::{
        ShaderSymbolTreeBuilder, SymbolLabelChainProvider, SymbolLabelProvider, SymbolRegionFinder,
        SymbolTreeFilter, SymbolTreeParser, SymbolTreePreprocessorParser,
    },
    symbol_tree::{ShaderModule, ShaderModuleHandle, ShaderSymbols, SymbolTree},
    symbols::{
        ShaderPosition, ShaderPreprocessor, ShaderPreprocessorContext, ShaderPreprocessorInclude,
        ShaderPreprocessorMode, ShaderRange, ShaderScope, ShaderSymbol, ShaderSymbolTree,
    },
};

#[derive(Default, Debug, Clone)]
pub struct ShaderSymbolParams {
    pub defines: HashMap<String, String>,
    pub includes: Vec<String>,
    pub path_remapping: HashMap<PathBuf, PathBuf>,
}

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

pub type SymbolIncludeCallback<'a> =
    dyn FnMut(&ShaderPreprocessorInclude) -> Result<Option<ShaderModuleHandle>, ShaderError> + 'a;

pub fn default_include_callback<T: ShadingLanguageTag>(
    include: &ShaderPreprocessorInclude,
) -> Result<Option<ShaderModuleHandle>, ShaderError> {
    let mut language = ShaderLanguage::new(T::get_language());
    let include_module = language.create_module(
        &include.absolute_path,
        std::fs::read_to_string(&include.absolute_path)
            .unwrap()
            .as_str(),
    )?;
    Ok(Some(Rc::new(RefCell::new(include_module))))
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
        // TODO:TREE: look for namespace aswell.
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
    pub fn query_symbols_with_context<'a>(
        &self,
        shader_module: &ShaderModule,
        context: &mut ShaderPreprocessorContext,
        include_callback: &'a mut SymbolIncludeCallback<'a>,
        old_symbols: Option<ShaderSymbols>,
    ) -> Result<ShaderSymbols, ShaderError> {
        // Either we create it from context, or we store it in context (no need to store 2 ref to it).
        let preprocessor =
            self.query_preprocessor(shader_module, context, include_callback, old_symbols)?;
        let symbol_tree = if let ShaderPreprocessorMode::OnceVisited = preprocessor.mode {
            ShaderSymbolTree::default() // if once, no symbols.
        } else {
            self.query_file_symbols(shader_module)?
        };
        Ok(ShaderSymbols {
            preprocessor,
            symbol_tree,
        })
    }
    pub fn query_symbols<'a>(
        &self,
        shader_module: &ShaderModule,
        symbol_params: ShaderSymbolParams,
        include_callback: &'a mut SymbolIncludeCallback<'a>,
        old_symbols: Option<ShaderSymbols>,
    ) -> Result<ShaderSymbols, ShaderError> {
        let mut context = ShaderPreprocessorContext::main(&shader_module.file_path, symbol_params);
        let preprocessor =
            self.query_preprocessor(shader_module, &mut context, include_callback, old_symbols)?;
        let symbol_tree = if let ShaderPreprocessorMode::OnceVisited = preprocessor.mode {
            ShaderSymbolTree::default() // if once, no symbols.
        } else {
            self.query_file_symbols(shader_module)?
        };
        Ok(ShaderSymbols {
            preprocessor,
            symbol_tree,
        })
    }
    fn query_preprocessor<'a>(
        &self,
        symbol_tree: &SymbolTree,
        context: &'a mut ShaderPreprocessorContext,
        include_callback: &'a mut SymbolIncludeCallback<'a>,
        old_symbols: Option<ShaderSymbols>,
    ) -> Result<ShaderPreprocessor, ShaderError> {
        let mut preprocessor = ShaderPreprocessor::new(context.clone());

        // Check pragma once macro.
        if let Some(_) = symbol_tree.content.find("#pragma once") {
            // Assume regions not affecting it neither does include order.
            preprocessor.mode = if context.is_visited(&symbol_tree.file_path) {
                ShaderPreprocessorMode::OnceVisited
            } else {
                ShaderPreprocessorMode::Once
            };
            return Ok(preprocessor);
        } else {
            preprocessor.mode = ShaderPreprocessorMode::Default;
        }

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
                    context,
                );
            }
        }
        // Query regions.
        // Will filter includes & defines in inactive regions
        preprocessor.regions = self.region_finder.query_regions_in_node(
            symbol_tree,
            self,
            symbol_tree.tree.root_node(),
            &mut preprocessor,
            context,
            include_callback,
            old_symbols,
        )?;
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
    fn query_file_symbols(
        &self,
        symbol_tree: &SymbolTree,
    ) -> Result<ShaderSymbolTree, ShaderError> {
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
        let mut symbol_list_builder = ShaderSymbolTreeBuilder::new(&filter_symbol);
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
                    &mut symbol_list_builder,
                );
            }
        }
        let symbols = symbol_list_builder.get_shader_symbol_tree();
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
