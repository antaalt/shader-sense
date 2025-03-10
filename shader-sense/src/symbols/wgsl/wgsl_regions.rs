use crate::{
    shader_error::ShaderError,
    symbols::{
        symbol_parser::SymbolRegionFinder,
        symbol_tree::SymbolTree,
        symbols::{ShaderPreprocessor, ShaderRegion},
    },
};

pub struct WgslRegionFinder {}

impl SymbolRegionFinder for WgslRegionFinder {
    fn query_regions_in_node(
        &self,
        _symbol_tree: &SymbolTree,
        _node: tree_sitter::Node,
        _preprocessor: &mut ShaderPreprocessor,
    ) -> Result<Vec<ShaderRegion>, ShaderError> {
        Ok(vec![])
    }
}
