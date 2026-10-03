//! Parser specific for WGSL
mod wgsl_parser;
mod wgsl_regions;
use wgsl_parser::get_wgsl_parsers;
use wgsl_regions::WgslRegionFinder;

use crate::{
    position::ShaderPosition, shader::ShadingLanguage, shader_error::ShaderError,
    symbols::symbol_parser::ShaderWordRange,
};

use super::{
    shader_module::ShaderModule, symbol_parser::SymbolWordProvider, symbol_provider::SymbolProvider,
};

struct WgslSymbolWordProvider {}

impl SymbolWordProvider for WgslSymbolWordProvider {
    fn find_word_at_position_in_node(
        &self,
        _shader_module: &ShaderModule,
        _node: tree_sitter::Node,
        _position: &ShaderPosition,
    ) -> Result<ShaderWordRange, ShaderError> {
        return Err(ShaderError::NoSymbol);
    }
}

pub(super) fn create_wgsl_symbol_provider() -> SymbolProvider {
    SymbolProvider::new(
        ShadingLanguage::Wgsl,
        get_wgsl_parsers(),
        vec![],
        Box::new(WgslRegionFinder {}),
        Box::new(WgslSymbolWordProvider {}),
    )
}
