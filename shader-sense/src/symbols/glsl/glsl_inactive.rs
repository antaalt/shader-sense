use crate::{
    shader_error::ShaderError,
    symbols::{symbol_tree::SymbolTree, symbols::ShaderRange},
};

use super::GlslSymbolProvider;

impl GlslSymbolProvider {
    pub fn query_inactive_regions_in_node(
        &self,
        _symbol_tree: &SymbolTree,
        _node: tree_sitter::Node,
    ) -> Result<Vec<ShaderRange>, ShaderError> {
        Ok(vec![])
    }
}
