//! Parser specific for Slang

use crate::{
    position::ShaderPosition,
    shader_error::ShaderError,
    symbols::{
        shader_module::ShaderModule,
        slang::{slang_parser::get_slang_parsers, slang_region::SlangRegionFinder},
        symbol_parser::{ShaderWordRange, SymbolWordProvider},
        symbol_provider::SymbolProvider,
    },
};

mod slang_parser;
mod slang_region;

struct SlangSymbolWordProvider {}

impl SymbolWordProvider for SlangSymbolWordProvider {
    fn find_word_at_position_in_node(
        &self,
        _shader_module: &ShaderModule,
        _node: tree_sitter::Node,
        _position: &ShaderPosition,
    ) -> Result<ShaderWordRange, ShaderError> {
        return Err(ShaderError::NoSymbol);
    }
}

pub(super) fn create_slang_symbol_provider(
    tree_sitter_language: &tree_sitter::Language,
) -> SymbolProvider {
    SymbolProvider::new(
        tree_sitter_language,
        get_slang_parsers(),
        vec![],
        Box::new(SlangRegionFinder::new()),
        Box::new(SlangSymbolWordProvider {}),
    )
}
