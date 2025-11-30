//! Validator trait implemented for all languages.
use std::path::Path;

#[cfg(not(target_os = "wasi"))]
use crate::validator::dxc::Dxc;
use crate::{
    shader::{ShaderParams, ShaderStage, ShadingLanguage},
    shader_error::{ShaderDiagnosticList, ShaderError},
    validator::{glslang::Glslang, naga::Naga, slang::Slang},
};

/// Default include callback for [`Validator::validate_shader`]
pub fn default_include_callback(path: &Path) -> Option<String> {
    Some(std::fs::read_to_string(path).unwrap())
}

/// Trait that all validator must implement to validate files.
pub trait ValidatorImpl {
    fn validate_shader(
        &self,
        shader_content: &str,
        file_path: &Path,
        params: &ShaderParams,
        include_callback: &mut dyn FnMut(&Path) -> Option<String>,
    ) -> Result<ShaderDiagnosticList, ShaderError>;

    fn support(&self, shader_stage: ShaderStage) -> bool;

    fn get_file_name(&self, path: &Path) -> String {
        String::from(path.file_name().unwrap().to_string_lossy())
    }
}

/// Validator main entry point. Create this struct in order to validate shader
///
/// Run
/// ```
/// use shader_sense::validator::validator::Validator;
/// use shader_sense::shader::ShaderParams;
/// use std::path::Path;
/// let shader_path = Path::new("./test/hlsl/ok.hlsl");
/// let shader_content = std::fs::read_to_string(shader_path).unwrap();
/// let validator = Validator::hlsl();
/// validator.validate_shader(
///     &shader_content,
///     shader_path,
///     &ShaderParams::default(),
///     &mut |path: &Path| {
///         Some(std::fs::read_to_string(path).unwrap())
///     }
/// ).unwrap();
/// ```
pub struct Validator {
    imp: Box<dyn ValidatorImpl>,
}
impl Validator {
    /// Create a validator for Glsl.
    /// It will use glslang directly.
    pub fn glsl() -> Self {
        Self {
            imp: Box::new(Glslang::glsl()),
        }
    }
    /// Create a validator for Hlsl.
    /// It will use DXC if it is available and fallback on glslang if its not supported.
    /// Note that glslang support for HLSL is not as advanced as dxc.
    pub fn hlsl() -> Self {
        Self {
            #[cfg(not(target_os = "wasi"))]
            imp: match Dxc::new(Dxc::find_dxc_library()) {
                Ok(dxc) => Box::new(dxc),
                Err(_) => Box::new(Glslang::hlsl()), // Failed to instantiate dxc. Fallback to glslang.
            },
            #[cfg(target_os = "wasi")]
            imp: Box::new(Glslang::hlsl()),
        }
    }
    /// Create a validator for Wgsl.
    /// It will use naga directly.
    pub fn wgsl() -> Self {
        Self {
            imp: Box::new(Naga::new()),
        }
    }
    /// Create a validator for Slang.
    /// It will use slang compiler directly.
    pub fn slang() -> Self {
        Self {
            imp: Box::new(Slang::new()),
        }
    }
    /// Create a validator from the given [`ShadingLanguage`]
    pub fn from_shading_language(shading_language: ShadingLanguage) -> Self {
        match shading_language {
            ShadingLanguage::Wgsl => Self::wgsl(),
            ShadingLanguage::Hlsl => Self::hlsl(),
            ShadingLanguage::Glsl => Self::glsl(),
            ShadingLanguage::Slang => Self::slang(),
        }
    }
    /// Validate a shader and return the diagnostic list, or an error if the process failed.
    /// If diagnostic list is empty, no error were found.
    /// You can handle how your file will be loaded.
    /// The include callback is being passed the already processed absolute canonicalized path of the include.
    pub fn validate_shader(
        &self,
        shader_content: &str,
        file_path: &Path,
        params: &ShaderParams,
        include_callback: &mut dyn FnMut(&Path) -> Option<String>, // TODO: Check if we cant pass a ref instead of a copy here.
    ) -> Result<ShaderDiagnosticList, ShaderError> {
        self.imp
            .validate_shader(shader_content, file_path, params, include_callback)
    }
}
