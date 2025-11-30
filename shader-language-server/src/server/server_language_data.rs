use shader_sense::{
    shader::ShadingLanguage,
    symbols::{shader_module_parser::ShaderModuleParser, symbol_provider::SymbolProvider},
    validator::{glslang::Glslang, naga::Naga, slang::Slang, validator::ValidatorImpl},
};

#[cfg(not(target_os = "wasi"))]
use shader_sense::validator::dxc::Dxc;

pub struct ServerLanguageData {
    pub validator: Box<dyn ValidatorImpl>,
    pub shader_module_parser: ShaderModuleParser,
    pub symbol_provider: SymbolProvider,
}

impl ServerLanguageData {
    pub fn from_shading_language(shading_language: ShadingLanguage) -> Self {
        match shading_language {
            ShadingLanguage::Hlsl => Self::hlsl(),
            ShadingLanguage::Glsl => Self::glsl(),
            ShadingLanguage::Wgsl => Self::wgsl(),
            ShadingLanguage::Slang => Self::slang(),
        }
    }
    pub fn glsl() -> Self {
        let shader_module_parser = ShaderModuleParser::from_shading_language(ShadingLanguage::Glsl);
        let symbol_provider = SymbolProvider::from_shading_language(ShadingLanguage::Glsl);
        log::info!("Using glslang for GLSL validation.");
        Self {
            validator: Box::new(Glslang::glsl()),
            shader_module_parser,
            symbol_provider,
        }
    }
    pub fn hlsl() -> Self {
        let shader_module_parser = ShaderModuleParser::from_shading_language(ShadingLanguage::Hlsl);
        let symbol_provider = SymbolProvider::from_shading_language(ShadingLanguage::Hlsl);
        Self {
            #[cfg(target_os = "wasi")]
            validator: {
                log::info!("Using glslang for HLSL validation as DXC is unsupported on WASI.");
                Box::new(Glslang::hlsl())
            },
            #[cfg(not(target_os = "wasi"))]
            validator: match Dxc::new(if let Some(dxc_path) = Dxc::find_dxc_library() {
                log::info!(
                    "Found dxc library for HLSL validation at {}",
                    dxc_path.display()
                );
                Some(dxc_path)
            } else {
                log::info!("Did not found dxc library for HLSL validation, will try to use globally available ones.");
                None
            }) {
                Ok(dxc) => {
                    log::info!(
                        "Using Dxc for HLSL. DXIL validation {}",
                        if dxc.is_dxil_validation_available() {
                            "available"
                        } else {
                            "unavailable"
                        }
                    );
                    Box::new(dxc)
                }
                Err(err) => {
                    log::error!(
                        "Failed to instantiate DXC: {}\nFallback to glslang instead.",
                        err.to_string()
                    );
                    Box::new(Glslang::hlsl())
                }
            },
            shader_module_parser,
            symbol_provider,
        }
    }
    pub fn wgsl() -> Self {
        let shader_module_parser = ShaderModuleParser::from_shading_language(ShadingLanguage::Wgsl);
        let symbol_provider = SymbolProvider::from_shading_language(ShadingLanguage::Wgsl);
        log::info!("Using Naga for WGSL validation.");
        Self {
            validator: Box::new(Naga::new()),
            shader_module_parser,
            symbol_provider,
        }
    }
    pub fn slang() -> Self {
        let shader_module_parser =
            ShaderModuleParser::from_shading_language(ShadingLanguage::Slang);
        let symbol_provider = SymbolProvider::from_shading_language(ShadingLanguage::Slang);
        log::info!("Using Slang compiler for Slang validation.");
        Self {
            validator: Box::new(Slang::new()),
            shader_module_parser,
            symbol_provider,
        }
    }
}
