use crate::{
    shader::{ShaderCompilationParams, ShadingLanguage},
    shader_error::ShaderError,
    symbols::{
        hlsl::HlslSymbolRegionFinder,
        prepocessor::{ShaderPreprocessor, ShaderPreprocessorContext, ShaderRegion},
        shader_module::{ShaderModule, ShaderSymbols},
        symbol_parser::SymbolRegionFinder,
        symbol_provider::{SymbolIncludeCallback, SymbolProvider},
    },
};

pub struct GlslRegionFinder {
    region_finder: HlslSymbolRegionFinder,
}

impl GlslRegionFinder {
    pub fn new() -> Self {
        Self {
            region_finder: HlslSymbolRegionFinder::new(ShadingLanguage::Glsl),
        }
    }
}

impl SymbolRegionFinder for GlslRegionFinder {
    fn query_regions_in_node<'a>(
        &self,
        shader_module: &ShaderModule,
        symbol_provider: &SymbolProvider,
        shader_params: &ShaderCompilationParams,
        node: tree_sitter::Node,
        preprocessor: &mut ShaderPreprocessor,
        context: &'a mut ShaderPreprocessorContext,
        include_callback: &'a mut SymbolIncludeCallback<'a>,
        old_symbols: Option<ShaderSymbols>,
    ) -> Result<Vec<ShaderRegion>, ShaderError> {
        self.region_finder.query_regions_in_node(
            shader_module,
            symbol_provider,
            shader_params,
            node,
            preprocessor,
            context,
            include_callback,
            old_symbols,
        )
    }
}
