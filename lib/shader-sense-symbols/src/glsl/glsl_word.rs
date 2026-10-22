use tree_sitter::Node;

use crate::{
    hlsl::HlslSymbolWordProvider,
    shader_module::ShaderModule,
    symbol_parser::{ShaderWordRange, SymbolWordProvider},
};
use shader_sense_core::{position::ShaderPosition, shader_error::ShaderError};

pub struct GlslSymbolWordProvider {
    word_provider: HlslSymbolWordProvider,
}

impl GlslSymbolWordProvider {
    pub fn new() -> Self {
        Self {
            word_provider: HlslSymbolWordProvider {},
        }
    }
}

impl SymbolWordProvider for GlslSymbolWordProvider {
    fn find_word_at_position_in_node(
        &self,
        shader_module: &ShaderModule,
        node: Node,
        position: &ShaderPosition,
    ) -> Result<ShaderWordRange, ShaderError> {
        self.word_provider
            .find_word_at_position_in_node(shader_module, node, position)
    }
}
