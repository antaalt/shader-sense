use crate::{
    shader_error::ShaderError,
    symbols::{
        symbol_parser::SymbolRegionFinder,
        symbol_provider::{SymbolIncludeCallback, SymbolProvider},
        symbol_tree::{ShaderSymbols, SymbolTree},
        symbols::{ShaderPreprocessor, ShaderPreprocessorContext, ShaderRegion},
    },
};

pub struct WgslRegionFinder {}

impl SymbolRegionFinder for WgslRegionFinder {
    fn query_regions_in_node<'a>(
        &self,
        _symbol_tree: &SymbolTree,
        _symbol_provider: &SymbolProvider,
        _node: tree_sitter::Node,
        _preprocessor: &mut ShaderPreprocessor,
        _context: &'a mut ShaderPreprocessorContext,
        _include_callback: &'a mut SymbolIncludeCallback<'a>,
        _old_symbols: Option<ShaderSymbols>,
    ) -> Result<Vec<ShaderRegion>, ShaderError> {
        Ok(vec![])
    }
}
