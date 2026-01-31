//! Main entry point to inspect symbols from a file
use std::{cell::RefCell, path::Path, rc::Rc};

use tree_sitter::{Query, QueryCursor, StreamingIterator};

use crate::{
    position::{ShaderFileRange, ShaderPosition, ShaderRange},
    shader::{ShaderCompilationParams, ShaderParams, ShadingLanguage, ShadingLanguageTag},
    shader_error::{ShaderDiagnostic, ShaderDiagnosticSeverity, ShaderError},
    symbols::{
        glsl::create_glsl_symbol_provider, hlsl::create_hlsl_symbol_provider,
        shader_module_parser::get_tree_sitter_language, symbol_parser::ShaderWordRange,
        symbols::ShaderSymbolData, wgsl::create_wgsl_symbol_provider,
    },
};

use super::{
    prepocessor::{
        ShaderPreprocessor, ShaderPreprocessorContext, ShaderPreprocessorDefine,
        ShaderPreprocessorInclude, ShaderPreprocessorMode,
    },
    shader_module::{ShaderModule, ShaderModuleHandle, ShaderSymbols},
    shader_module_parser::ShaderModuleParser,
    symbol_list::ShaderSymbolList,
    symbol_parser::{
        ShaderSymbolListBuilder, SymbolRegionFinder, SymbolTreeParser,
        SymbolTreePreprocessorParser, SymbolWordProvider,
    },
    symbols::{ShaderScope, ShaderSymbol},
};

/// A symbol provider is responsible of querying a file using tree-sitter AST in order to find all [`ShaderSymbol`] and return them to user as a [`ShaderSymbolList`]
/// It performs on a [`ShaderModule`] which need to be created by a [`ShaderModuleParser`]
pub struct SymbolProvider {
    symbol_parsers: Vec<(Box<dyn SymbolTreeParser>, tree_sitter::Query)>,
    scope_query: Query,
    error_query: Query,

    preprocessor_parsers: Vec<(Box<dyn SymbolTreePreprocessorParser>, tree_sitter::Query)>,
    region_finder: Box<dyn SymbolRegionFinder>,
    word_provider: Box<dyn SymbolWordProvider>,
}

pub type SymbolIncludeCallback<'a> =
    dyn FnMut(&ShaderPreprocessorInclude) -> Result<Option<ShaderModuleHandle>, ShaderError> + 'a;

pub fn default_include_callback<T: ShadingLanguageTag>(
    include: &ShaderPreprocessorInclude,
) -> Result<Option<ShaderModuleHandle>, ShaderError> {
    let mut shader_module_parser = ShaderModuleParser::from_shading_language(T::get_language());
    let include_module = shader_module_parser.create_module(
        &include.get_absolute_path(),
        std::fs::read_to_string(&include.get_absolute_path())
            .unwrap()
            .as_str(),
    )?;
    Ok(Some(Rc::new(RefCell::new(include_module))))
}

pub struct ProxyTree {
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
        self.tree.edit(&tree_sitter::InputEdit {
            start_byte: 0,
            old_end_byte: self.text.len(), // Should use byte_offset instead
            new_end_byte: text.len(),
            start_position: tree_sitter::Point::new(0, 0),
            old_end_position: tree_sitter::Point::new(
                old_end_position.line as usize,
                old_end_position.pos as usize,
            ),
            new_end_position: tree_sitter::Point::new(
                new_end_position.line as usize,
                new_end_position.pos as usize,
            ),
        });
        self.text = text.into();
        self.tree = self.parser.parse(&self.text, Some(&self.tree))?;
        Some(&self.tree)
    }
}

impl SymbolProvider {
    pub fn glsl() -> Self {
        create_glsl_symbol_provider(&get_tree_sitter_language(ShadingLanguage::Glsl))
    }
    pub fn hlsl() -> Self {
        create_hlsl_symbol_provider(&get_tree_sitter_language(ShadingLanguage::Hlsl))
    }
    pub fn wgsl() -> Self {
        create_wgsl_symbol_provider(&get_tree_sitter_language(ShadingLanguage::Wgsl))
    }
    pub fn from_shading_language(shading_language: ShadingLanguage) -> Self {
        match shading_language {
            ShadingLanguage::Wgsl => Self::wgsl(),
            ShadingLanguage::Hlsl => Self::hlsl(),
            ShadingLanguage::Glsl => Self::glsl(),
        }
    }
    pub(crate) fn new(
        language: &tree_sitter::Language,
        parsers: Vec<Box<dyn SymbolTreeParser>>,
        preprocessor_parsers: Vec<Box<dyn SymbolTreePreprocessorParser>>,
        region_finder: Box<dyn SymbolRegionFinder>,
        word_provider: Box<dyn SymbolWordProvider>,
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
            scope_query: tree_sitter::Query::new(language, scope_query).unwrap(),
            error_query: tree_sitter::Query::new(language, error_query).unwrap(),
            preprocessor_parsers: preprocessor_parsers
                .into_iter()
                .map(|e| {
                    // Cache query
                    let query = Query::new(language, e.get_query().as_str()).unwrap();
                    (e, query)
                })
                .collect(),
            region_finder: region_finder,
            word_provider,
        }
    }
    pub fn query_file_scopes(&self, shader_module: &ShaderModule) -> Vec<ShaderScope> {
        // TODO: look for namespace aswell.
        // Should be per lang instead.
        let mut query_cursor = QueryCursor::new();
        let mut scopes = Vec::new();
        let mut all_matches = query_cursor.matches(
            &self.scope_query,
            shader_module.tree.root_node(),
            shader_module.content.as_bytes(),
        );
        while let Some(symbol_match) = all_matches.next() {
            scopes.push(match symbol_match.captures.len() {
                // one body
                1 => ShaderScope::from(ShaderRange::from(symbol_match.captures[0].node.range())),
                // a bit weird, a body and single curly brace ? mergin them to be safe.
                2 => ShaderScope::join(
                    ShaderScope::from(ShaderRange::from(symbol_match.captures[0].node.range())),
                    ShaderScope::from(ShaderRange::from(symbol_match.captures[1].node.range())),
                ),
                // Remove curly braces from scope.
                3 => {
                    let curly_start = symbol_match.captures[1].node.range();
                    let curly_end = symbol_match.captures[2].node.range();
                    ShaderScope::from(ShaderRange::from(tree_sitter::Range {
                        start_byte: curly_start.end_byte,
                        end_byte: curly_end.start_byte,
                        start_point: curly_start.end_point,
                        end_point: curly_end.start_point,
                    }))
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
        shader_params: &ShaderCompilationParams,
        include_callback: &'a mut SymbolIncludeCallback<'a>,
        old_symbols: Option<ShaderSymbols>,
    ) -> Result<ShaderSymbols, ShaderError> {
        // Either we create it from context, or we store it in context (no need to store 2 ref to it).
        let preprocessor = self.query_preprocessor(
            shader_module,
            context,
            &shader_params,
            include_callback,
            old_symbols,
        )?;
        let mut symbol_list = if let ShaderPreprocessorMode::OnceVisited = preprocessor.mode {
            ShaderSymbolList::default() // if once, no symbols.
        } else {
            // TODO: should not always need to recompute this.
            self.query_file_symbols(shader_module, &shader_params)?
        };
        self.postprocess_symbols(
            &shader_module.file_path,
            &mut symbol_list,
            &preprocessor,
            shader_params,
        );
        Ok(ShaderSymbols {
            preprocessor,
            symbol_list,
        })
    }
    pub fn postprocess_symbols(
        &self,
        file_path: &Path,
        symbol_list: &mut ShaderSymbolList,
        preprocessor: &ShaderPreprocessor,
        shader_compilation_params: &ShaderCompilationParams,
    ) {
        // A preprocess step that filter out and develop content.
        // TODO: correctly pick lang.
        // TODO: this code is specific to hlsl & glsl, might need to be moved in correct folder for postprocessing step.
        let mut tree = ProxyTree::new(&get_tree_sitter_language(ShadingLanguage::Hlsl));
        let mut new_symbols = ShaderSymbolList::default();
        for call_expression in &symbol_list.call_expression {
            if let ShaderSymbolData::CallExpression {
                label: _,
                range: _,
                parameters: call_parameters,
            } = &call_expression.data
            {
                let expressions = preprocessor
                    .defines
                    .iter()
                    .filter(|define| define.get_name() == &call_expression.label);
                for expression in expressions {
                    if let (Some(value), Some(macro_parameters)) =
                        (expression.get_value(), expression.get_parameters())
                    {
                        // Really basic macro parser.
                        fn parse_macro(value: &str, args: &Vec<(String, String)>) -> String {
                            // Remove \ that allow new line break in macro.
                            let mut formatted_value = value.replace("\\", "");
                            // Replace arguments
                            for arg in args {
                                // TODO: handle spaces aswell.
                                formatted_value =
                                    formatted_value.replace(&format!("##{}", arg.0), &arg.1);
                                formatted_value =
                                    formatted_value.replace(&format!("{}##", arg.0), &arg.1);
                            }
                            // replace "## value" by the value of parameter
                            // replace "# value" by the value of parameter
                            // replace __VA_ARGS__ and other specific macros.
                            // TODO: nested macros & everything else
                            formatted_value
                        }
                        if macro_parameters.len() != call_parameters.len() {
                            // Macro do not match.
                            continue;
                        }
                        let parameters = call_parameters
                            .iter()
                            .zip(macro_parameters.iter())
                            .map(|((call_parameter, _), macro_parameter)| {
                                (macro_parameter.clone(), call_parameter.clone())
                            })
                            .collect();

                        let value = parse_macro(value, &parameters);
                        if let Some(tree) = tree.parse(&value) {
                            let module = ShaderModule {
                                file_path: file_path.into(),
                                content: value.clone(),
                                tree: tree.clone(), // TODO: ref somehow
                            };
                            if let Ok(macro_symbols) = self.query_file_symbols(
                                &module,
                                &ShaderCompilationParams {
                                    entry_point: None, // Remove the entry point.
                                    shader_stage: shader_compilation_params.shader_stage,
                                    hlsl: shader_compilation_params.hlsl.clone(),
                                    glsl: shader_compilation_params.glsl.clone(),
                                    wgsl: shader_compilation_params.wgsl.clone(),
                                },
                            ) {
                                new_symbols.append(macro_symbols);
                            } else {
                            }
                        } else {
                            // failed to parse macro value. Ignore.
                        }
                    } else {
                        // no value for macro. Ignore.
                    }
                }
            } else {
                unreachable!("call expression is not a call expression")
            }
        }
        symbol_list.append(new_symbols);
    }
    pub fn query_symbols<'a>(
        &self,
        shader_module: &ShaderModule,
        shader_params: ShaderParams,
        include_callback: &'a mut SymbolIncludeCallback<'a>,
        old_symbols: Option<ShaderSymbols>,
    ) -> Result<ShaderSymbols, ShaderError> {
        let mut context =
            ShaderPreprocessorContext::main(&shader_module.file_path, shader_params.context);
        self.query_symbols_with_context(
            shader_module,
            &mut context,
            &shader_params.compilation,
            include_callback,
            old_symbols,
        )
    }
    pub(super) fn process_include<'a>(
        &self,
        context: &mut ShaderPreprocessorContext,
        include: &mut ShaderPreprocessorInclude,
        shader_params: &ShaderCompilationParams,
        include_callback: &'a mut SymbolIncludeCallback<'a>,
        old_symbols: Option<ShaderSymbols>,
    ) -> Result<(), ShaderError> {
        if context.increase_depth() {
            // Get module handle using callback.
            let result = match include_callback(&include) {
                Ok(include_module_handle) => match include_module_handle {
                    Some(include_module_handle) => {
                        // Include found, deal with it.
                        let module = RefCell::borrow(&include_module_handle);
                        match self.query_symbols_with_context(
                            &module,
                            context,
                            shader_params,
                            include_callback,
                            old_symbols,
                        ) {
                            Ok(cache) => {
                                include.cache = Some(cache);
                                Ok(())
                            }
                            Err(err) => Err(err),
                        }
                    }
                    None => {
                        // Include not found.
                        Err(ShaderError::SymbolQueryError(
                            format!("Failed to find include {}", include.get_relative_path()),
                            include
                                .get_range()
                                .clone()
                                .into_file(include.get_absolute_path().into()),
                        ))
                    }
                },
                Err(err) => Err(err),
            };
            context.decrease_depth();
            assert!(
                include.cache.is_some(),
                "Failed to compute cache for file {}",
                include.get_absolute_path().display()
            );
            result
        } else {
            // Set empty symbols to avoid crash when getting symbols.
            include.cache = Some(ShaderSymbols::default());
            // Notify
            return Err(ShaderError::SymbolQueryError(
                format!(
                    "Include {} reached maximum include depth",
                    include.get_relative_path()
                ),
                include
                    .get_range()
                    .clone()
                    .into_file(include.get_absolute_path().into()),
            ));
        }
    }
    fn query_preprocessor<'a>(
        &self,
        shader_module: &ShaderModule,
        context: &'a mut ShaderPreprocessorContext,
        shader_params: &ShaderCompilationParams,
        include_callback: &'a mut SymbolIncludeCallback<'a>,
        old_symbols: Option<ShaderSymbols>,
    ) -> Result<ShaderPreprocessor, ShaderError> {
        let mut preprocessor = ShaderPreprocessor::new(context.clone());

        // Check if context dirty and we need a recompute
        // or if we can reuse old_symbols instead.
        let is_dirty = match &old_symbols {
            Some(old_symbol) => old_symbol
                .get_preprocessor()
                .context
                .is_dirty(&shader_module.file_path, &context),
            None => true, // No old_symbol.
        };
        if is_dirty {
            // Recompute everything as its dirty.
            for parser in &self.preprocessor_parsers {
                let mut query_cursor = QueryCursor::new();
                let mut all_matches = query_cursor.matches(
                    &parser.1,
                    shader_module.tree.root_node(),
                    shader_module.content.as_bytes(),
                );
                while let Some(symbol_match) = all_matches.next() {
                    parser.0.process_match(
                        symbol_match,
                        &shader_module.file_path,
                        &shader_module.content,
                        &mut preprocessor,
                        context,
                    );
                }
            }
            // Check pragma once macro.
            if preprocessor.mode == ShaderPreprocessorMode::OnceVisited {
                // Return a clean preprocessor.
                let mut empty_preprocessor = ShaderPreprocessor::new(context.clone());
                empty_preprocessor.mode = preprocessor.mode;
                return Ok(empty_preprocessor);
            }
            // Query regions.
            // Will filter includes & defines in inactive regions
            preprocessor.regions = self.region_finder.query_regions_in_node(
                shader_module,
                self,
                shader_params,
                shader_module.tree.root_node(),
                &mut preprocessor,
                context,
                include_callback,
                old_symbols,
            )?;
            // Add errors
            let mut query_error_cursor = QueryCursor::new();
            let mut all_matches = query_error_cursor.matches(
                &self.error_query,
                shader_module.tree.root_node(),
                shader_module.content.as_bytes(),
            );
            while let Some(symbol_match) = all_matches.next() {
                preprocessor.diagnostics.push(ShaderDiagnostic {
                    severity: ShaderDiagnosticSeverity::Warning,
                    error:
                        "Failed to parse this code. Some symbols might be missing from providers."
                            .into(),
                    range: ShaderFileRange::from(
                        shader_module.file_path.clone(),
                        ShaderRange::from(symbol_match.captures[0].node.range()),
                    ),
                });
            }
            Ok(preprocessor)
        } else {
            // Retrieve old symbol, maintain context up to date
            let mut old_symbols = old_symbols.unwrap();
            let included_preprocessor = old_symbols.get_preprocessor_mut();
            let included_includes: Vec<&mut ShaderPreprocessorInclude> =
                included_preprocessor.includes.iter_mut().collect();
            let mut last_position = ShaderPosition::zero();
            for included_include in included_includes {
                // Append directory stack and defines.
                context.push_directory_stack(included_include.get_absolute_path());
                context.append_defines(
                    included_preprocessor
                        .defines
                        .iter()
                        .filter(|define| {
                            let range = define.get_range();
                            range.start >= last_position
                                && range.end <= included_include.get_range().start
                        })
                        .cloned()
                        .collect::<Vec<ShaderPreprocessorDefine>>(),
                );
                // Here we take old include cache as we want to compute it.
                let old_include_cache = included_include.cache.take();
                self.process_include(
                    context,
                    included_include,
                    shader_params,
                    include_callback,
                    old_include_cache,
                )?;
                last_position = included_include.get_range().end.clone();
            }
            // Add all defines after last include to context
            let define_left = included_preprocessor
                .defines
                .iter_mut()
                .filter(|define| {
                    let range = define.get_range();
                    range.start > last_position
                })
                .map(|d| d.clone())
                .collect::<Vec<ShaderPreprocessorDefine>>();
            context.append_defines(define_left);
            Ok(old_symbols.preprocessor)
        }
    }
    fn query_file_symbols(
        &self,
        shader_module: &ShaderModule,
        shader_compilation_params: &ShaderCompilationParams,
    ) -> Result<ShaderSymbolList, ShaderError> {
        let filter_symbol = |symbol: &ShaderSymbol| -> bool {
            // Dont filter inactive regions here on parsing, to avoid recomputing all symbols on regions update.
            match &symbol.requirement {
                Some(requirement) => requirement.is_met(shader_compilation_params),
                None => true, // Not filtered
            }
        };
        let mut symbol_list_builder = ShaderSymbolListBuilder::new(&filter_symbol);
        let scopes = self.query_file_scopes(shader_module);
        for parser in &self.symbol_parsers {
            let mut query_cursor = QueryCursor::new();
            let mut all_matches = query_cursor.matches(
                &parser.1,
                shader_module.tree.root_node(),
                shader_module.content.as_bytes(),
            );
            while let Some(symbol_match) = all_matches.next() {
                parser.0.process_match(
                    symbol_match,
                    &shader_module.file_path,
                    &shader_module.content,
                    &scopes,
                    &mut symbol_list_builder,
                );
            }
        }
        Ok(symbol_list_builder.get_shader_symbol_list())
    }
    pub fn get_word_range_at_position(
        &self,
        shader_module: &ShaderModule,
        position: &ShaderPosition,
    ) -> Result<ShaderWordRange, ShaderError> {
        self.word_provider.find_word_at_position_in_node(
            shader_module,
            shader_module.tree.root_node(),
            position,
        )
    }
}
