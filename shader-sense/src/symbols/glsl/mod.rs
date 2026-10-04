//! Parser specific for GLSL
mod glsl_parser;
mod glsl_preprocessor;
mod glsl_regions;
mod glsl_word;

use glsl_parser::get_glsl_parsers;
use glsl_preprocessor::get_glsl_preprocessor_parser;
use glsl_regions::GlslRegionFinder;
use glsl_word::GlslSymbolWordProvider;

use crate::shader::ShadingLanguage;

use super::symbol_provider::SymbolProvider;

pub(super) fn create_glsl_symbol_provider() -> SymbolProvider {
    SymbolProvider::new(
        ShadingLanguage::Glsl,
        get_glsl_parsers(),
        get_glsl_preprocessor_parser(),
        Box::new(GlslRegionFinder::new()),
        Box::new(GlslSymbolWordProvider::new()),
    )
}
