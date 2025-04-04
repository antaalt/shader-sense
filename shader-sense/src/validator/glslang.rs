use super::validator::{ValidationParams, Validator};
use crate::{
    include::IncludeHandler,
    shader::{GlslSpirvVersion, GlslTargetClient, ShaderStage},
    shader_error::{ShaderDiagnostic, ShaderDiagnosticList, ShaderDiagnosticSeverity, ShaderError},
    symbols::symbols::{ShaderPosition, ShaderRange},
};
use glslang::{
    error::GlslangError,
    include::{IncludeResult, IncludeType},
    Compiler, CompilerOptions, ShaderInput, ShaderSource,
};
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

impl Into<glslang::ShaderStage> for ShaderStage {
    fn into(self) -> glslang::ShaderStage {
        match self {
            ShaderStage::Vertex => glslang::ShaderStage::Vertex,
            ShaderStage::Fragment => glslang::ShaderStage::Fragment,
            ShaderStage::Compute => glslang::ShaderStage::Compute,
            ShaderStage::TesselationControl => glslang::ShaderStage::TesselationControl,
            ShaderStage::TesselationEvaluation => glslang::ShaderStage::TesselationEvaluation,
            ShaderStage::Mesh => glslang::ShaderStage::Mesh,
            ShaderStage::Task => glslang::ShaderStage::Task,
            ShaderStage::Geometry => glslang::ShaderStage::Geometry,
            ShaderStage::RayGeneration => glslang::ShaderStage::RayGeneration,
            ShaderStage::ClosestHit => glslang::ShaderStage::ClosestHit,
            ShaderStage::AnyHit => glslang::ShaderStage::AnyHit,
            ShaderStage::Callable => glslang::ShaderStage::Callable,
            ShaderStage::Miss => glslang::ShaderStage::Miss,
            ShaderStage::Intersect => glslang::ShaderStage::Intersect,
        }
    }
}

pub struct Glslang {
    hlsl: bool,
    compiler: &'static Compiler,
}

impl Glslang {
    #[allow(dead_code)] // Only used for WASI (alternative to DXC)
    pub fn hlsl() -> Self {
        let compiler = Compiler::acquire().expect("Failed to create glslang compiler");
        Self {
            hlsl: true,
            compiler,
        }
    }
    pub fn glsl() -> Self {
        let compiler = Compiler::acquire().expect("Failed to create glslang compiler");
        Self {
            hlsl: false,
            compiler,
        }
    }
}

struct GlslangIncludeHandler<'a> {
    include_handler: IncludeHandler,
    include_callback: &'a mut dyn FnMut(&Path) -> Option<String>,
}

impl<'a> GlslangIncludeHandler<'a> {
    pub fn new(
        file_path: &'a Path,
        includes: Vec<String>,
        path_remapping: HashMap<PathBuf, PathBuf>,
        include_callback: &'a mut dyn FnMut(&Path) -> Option<String>,
    ) -> Self {
        Self {
            include_handler: IncludeHandler::new(file_path, includes, path_remapping),
            include_callback: include_callback,
        }
    }
}

impl glslang::include::IncludeHandler for GlslangIncludeHandler<'_> {
    fn include(
        &mut self,
        _ty: IncludeType, // TODO: should use them ?
        header_name: &str,
        _includer_name: &str,
        _include_depth: usize,
    ) -> Option<IncludeResult> {
        match self
            .include_handler
            .search_in_includes(Path::new(header_name), self.include_callback)
        {
            Some(data) => Some(IncludeResult {
                name: String::from(header_name),
                data: data.0,
            }),
            None => None,
        }
    }
}

impl Glslang {
    fn parse_errors(
        errors: &String,
        file_path: &Path,
        includes: &Vec<String>,
        path_remapping: HashMap<PathBuf, PathBuf>,
        offset_first_line: bool,
    ) -> Result<ShaderDiagnosticList, ShaderError> {
        let mut shader_error_list = ShaderDiagnosticList::empty();

        let reg = regex::Regex::new(r"(?m)^(.*?:(?:  \d+:\d+:)?)")?;
        let mut starts = Vec::new();
        for capture in reg.captures_iter(errors.as_str()) {
            if let Some(pos) = capture.get(0) {
                starts.push(pos.start());
            }
        }
        starts.push(errors.len());
        let internal_reg = regex::Regex::new(
            r"(?s)^(.*?):(?: ((?:[a-zA-Z]:)?[\d\w\.\/\\\-]+):(\d+):(\d+):)?(.+)",
        )?;
        let mut include_handler = IncludeHandler::new(file_path, includes.clone(), path_remapping);
        for start in 0..starts.len() - 1 {
            let first = starts[start];
            let length = starts[start + 1] - starts[start];
            let block: String = errors.chars().skip(first).take(length).collect();
            if block.contains("compilation errors.  No code generated.") {
                continue; // Skip this useless string.
            }
            if let Some(capture) = internal_reg.captures(block.as_str()) {
                let level = capture.get(1).map_or("", |m| m.as_str());
                let relative_path = capture.get(2).map_or("", |m| m.as_str());
                let line = capture.get(3).map_or("", |m| m.as_str());
                let pos = capture.get(4).map_or("", |m| m.as_str());
                let msg = capture.get(5).map_or("", |m| m.as_str());
                let file_path: PathBuf = match relative_path.parse::<u32>() {
                    Ok(_) => file_path.into(), // Main file
                    Err(_) => {
                        if relative_path.is_empty() {
                            file_path.into()
                        } else {
                            match include_handler.search_path_in_includes(Path::new(relative_path))
                            {
                                Some(value) => value,
                                None => file_path.into(),
                            }
                        }
                    }
                };
                let line = if offset_first_line {
                    line.parse::<u32>().unwrap_or(2) - 2
                } else {
                    line.parse::<u32>().unwrap_or(1) - 1
                };
                let pos = pos.parse::<u32>().unwrap_or(0);
                shader_error_list.push(ShaderDiagnostic {
                    severity: match level {
                        "ERROR" => ShaderDiagnosticSeverity::Error,
                        "WARNING" => ShaderDiagnosticSeverity::Warning,
                        "NOTE" => ShaderDiagnosticSeverity::Information,
                        "HINT" => ShaderDiagnosticSeverity::Hint,
                        _ => ShaderDiagnosticSeverity::Error,
                    },
                    error: String::from(msg),
                    range: ShaderRange::new(
                        ShaderPosition::new(file_path.clone(), line, pos),
                        ShaderPosition::new(file_path.clone(), line, pos),
                    ),
                });
            } else {
                return Err(ShaderError::InternalErr(format!(
                    "Failed to parse regex: {}",
                    block
                )));
            }
        }

        if shader_error_list.is_empty() {
            return Err(ShaderError::InternalErr(format!(
                "Failed to parse errors: {}",
                errors
            )));
        }
        return Ok(shader_error_list);
    }

    fn from_glslang_error(
        &self,
        err: GlslangError,
        file_path: &Path,
        params: &ValidationParams,
        offset_first_line: bool,
    ) -> Result<ShaderDiagnosticList, ShaderError> {
        match err {
            GlslangError::PreprocessError(error) => Glslang::parse_errors(
                &error,
                file_path,
                &params.includes,
                params.path_remapping.clone(),
                offset_first_line,
            ),
            GlslangError::ParseError(error) => Glslang::parse_errors(
                &error,
                file_path,
                &params.includes,
                params.path_remapping.clone(),
                offset_first_line,
            ),
            GlslangError::LinkError(error) => Glslang::parse_errors(
                &error,
                file_path,
                &params.includes,
                params.path_remapping.clone(),
                offset_first_line,
            ),
            GlslangError::ShaderStageNotFound(stage) => Err(ShaderError::InternalErr(format!(
                "Shader stage not found: {:#?}",
                stage
            ))),
            GlslangError::InvalidProfile(target, value, profile) => {
                Err(ShaderError::InternalErr(format!(
                    "Invalid profile {} for target {:#?}: {:#?}",
                    value, target, profile
                )))
            }
            GlslangError::VersionUnsupported(value, profile) => Err(ShaderError::InternalErr(
                format!("Unsupported profile {}: {:#?}", value, profile),
            )),
            err => Err(ShaderError::InternalErr(format!(
                "Internal error: {:#?}",
                err
            ))),
        }
    }
}
impl Validator for Glslang {
    fn validate_shader(
        &mut self,
        content: &String,
        file_path: &Path,
        params: &ValidationParams,
        include_callback: &mut dyn FnMut(&Path) -> Option<String>,
    ) -> Result<ShaderDiagnosticList, ShaderError> {
        let file_name = self.get_file_name(file_path);

        let (shader_stage, shader_source, offset_first_line) =
            if let Some(shader_stage) = ShaderStage::from_file_name(&file_name) {
                (shader_stage, content.clone(), false)
            } else {
                // If we dont have a stage, might require some preprocess to avoid errors.
                // glslang **REQUIRES** to have stage for linting.
                let default_stage = ShaderStage::Fragment;
                if self.hlsl {
                    // HLSL does not require version, simply assume stage.
                    (default_stage, content.clone(), false)
                } else {
                    // glslang does not support linting header file, so to lint them,
                    // Assume Fragment & add default #version if missing
                    if content.contains("#version ") {
                        // Main file with missing stage.
                        (default_stage, content.clone(), false)
                    } else {
                        // Header file with missing stage & missing version.
                        // WARN: Assumed this string is one line offset only.
                        let version_header = String::from("#version 450\n");
                        (default_stage, version_header + content.as_str(), true)
                    }
                }
            };

        let source = ShaderSource::try_from(shader_source).expect("Failed to read from source");

        let defines_copy = params.defines.clone();
        let defines: Vec<(&str, Option<&str>)> = defines_copy
            .iter()
            .map(|v| (&v.0 as &str, Some(&v.1 as &str)))
            .collect();
        let mut include_handler = GlslangIncludeHandler::new(
            file_path,
            params.includes.clone(),
            params.path_remapping.clone(),
            include_callback,
        );

        let lang_version = match params.glsl_spirv {
            GlslSpirvVersion::SPIRV1_0 => glslang::SpirvVersion::SPIRV1_0,
            GlslSpirvVersion::SPIRV1_1 => glslang::SpirvVersion::SPIRV1_1,
            GlslSpirvVersion::SPIRV1_2 => glslang::SpirvVersion::SPIRV1_2,
            GlslSpirvVersion::SPIRV1_3 => glslang::SpirvVersion::SPIRV1_3,
            GlslSpirvVersion::SPIRV1_4 => glslang::SpirvVersion::SPIRV1_4,
            GlslSpirvVersion::SPIRV1_5 => glslang::SpirvVersion::SPIRV1_5,
            GlslSpirvVersion::SPIRV1_6 => glslang::SpirvVersion::SPIRV1_6,
        };
        let input = match ShaderInput::new(
            &source,
            shader_stage.into(),
            &CompilerOptions {
                source_language: if self.hlsl {
                    glslang::SourceLanguage::HLSL
                } else {
                    glslang::SourceLanguage::GLSL
                },
                // Should have some settings to select these.
                target: if self.hlsl {
                    glslang::Target::None(Some(lang_version))
                } else {
                    if params.glsl_client.is_opengl() {
                        glslang::Target::OpenGL {
                            version: glslang::OpenGlVersion::OpenGL4_5,
                            spirv_version: None, // TODO ?
                        }
                    } else {
                        let client_version = match params.glsl_client {
                            GlslTargetClient::Vulkan1_0 => glslang::VulkanVersion::Vulkan1_0,
                            GlslTargetClient::Vulkan1_1 => glslang::VulkanVersion::Vulkan1_1,
                            GlslTargetClient::Vulkan1_2 => glslang::VulkanVersion::Vulkan1_2,
                            GlslTargetClient::Vulkan1_3 => glslang::VulkanVersion::Vulkan1_3,
                            _ => unreachable!(),
                        };
                        glslang::Target::Vulkan {
                            version: client_version,
                            spirv_version: lang_version,
                        }
                    }
                },
                messages: glslang::ShaderMessage::CASCADING_ERRORS
                    | glslang::ShaderMessage::DEBUG_INFO
                    | glslang::ShaderMessage::DISPLAY_ERROR_COLUMN
                    | if self.hlsl && params.hlsl_enable16bit_types {
                        glslang::ShaderMessage::HLSL_ENABLE_16BIT_TYPES
                    } else {
                        glslang::ShaderMessage::DEFAULT
                    },
                ..Default::default()
            },
            Some(&defines),
            Some(&mut include_handler),
        )
        .map_err(|e| self.from_glslang_error(e, file_path, &params, offset_first_line))
        {
            Ok(value) => value,
            Err(error) => match error {
                Err(error) => return Err(error),
                Ok(diag) => return Ok(diag),
            },
        };
        let _shader = match glslang::Shader::new(&self.compiler, input)
            .map_err(|e| self.from_glslang_error(e, file_path, &params, offset_first_line))
        {
            Ok(value) => value,
            Err(error) => match error {
                Err(error) => return Err(error),
                Ok(diag) => return Ok(diag),
            },
        };
        // Linking require main entry point. Should work around this somehow.
        /*let _spirv = match shader.compile().map_err(|e| self.from_glslang_error(e)) {
            Ok(value) => value,
            Err(error) => match error {
                Err(error) => return Err(error),
                Ok(diag) => return Ok((diag, include_handler.get_dependencies().clone())),
            },
        };*/

        Ok(ShaderDiagnosticList::empty()) // No error detected.
    }
}
