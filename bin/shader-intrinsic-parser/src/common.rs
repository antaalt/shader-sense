use shader_sense::{shader::ShadingLanguage, symbols::symbol_list::ShaderSymbolList};

use crate::{glsl::GlslIntrinsicParser, hlsl::HlslIntrinsicParser, wgsl::WgslIntrinsicParser};

pub fn download_file(url: &str) -> String {
    let html = ureq::get(url)
        .call()
        .expect("Failed to get page")
        .into_string()
        .expect("Failed to convert to string");
    html
}

pub trait IntrinsicParser {
    fn cache(&self, cache_path: &str);
    fn parse(&self, cache_path: &str) -> ShaderSymbolList;
}

pub fn get_intrinsic_parser(shading_language: ShadingLanguage) -> Box<dyn IntrinsicParser> {
    match shading_language {
        ShadingLanguage::Wgsl => Box::new(WgslIntrinsicParser {}),
        ShadingLanguage::Hlsl => Box::new(HlslIntrinsicParser {}),
        ShadingLanguage::Glsl => Box::new(GlslIntrinsicParser {}),
    }
}
