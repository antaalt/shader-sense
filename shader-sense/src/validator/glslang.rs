//! Validation for glsl with [`glslang`]

use super::validator::ValidatorImpl;
use crate::{
    include::IncludeHandler,
    position::{ShaderFileRange, ShaderPosition},
    shader::{GlslSpirvVersion, GlslTargetClient, ShaderParams, ShaderStage},
    shader_error::{ShaderDiagnostic, ShaderDiagnosticList, ShaderDiagnosticSeverity, ShaderError},
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

    // Cache regex for parsing.
    diagnostic_regex: regex::Regex,
    internal_diagnostic_regex: regex::Regex,
}

impl Glslang {
    #[allow(dead_code)] // Only used for WASI (alternative to DXC)
    pub fn hlsl() -> Self {
        Self::new(true)
    }
    pub fn glsl() -> Self {
        Self::new(false)
    }
    fn new(hlsl: bool) -> Self {
        let compiler = Compiler::acquire().expect("Failed to create glslang compiler");
        Self {
            hlsl: hlsl,
            compiler,
            diagnostic_regex: regex::Regex::new(r"(?m)^(.*?:(?:  \d+:\d+:)?)").unwrap(),
            internal_diagnostic_regex: regex::Regex::new(
                r"(?s)^(.*?):(?: ((?:[a-zA-Z]:)?[\d\w\.\/\\\-]+):(\d+):(\d+):)?(.+)",
            )
            .unwrap(),
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
        includes: Vec<PathBuf>,
        path_remapping: HashMap<PathBuf, PathBuf>,
        include_callback: &'a mut dyn FnMut(&Path) -> Option<String>,
    ) -> Self {
        Self {
            include_handler: IncludeHandler::main(file_path, includes, path_remapping),
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
        include_depth: usize,
    ) -> Option<IncludeResult> {
        // Glslang does not handle stack overflow natively. So put a limit there.
        if include_depth > IncludeHandler::DEPTH_LIMIT {
            None
        } else {
            match self
                .include_handler
                .search_in_includes(Path::new(header_name), self.include_callback)
            {
                Some((content, path)) => {
                    self.include_handler.push_directory_stack(&path);
                    Some(IncludeResult {
                        name: String::from(header_name),
                        data: content,
                    })
                }
                None => None,
            }
        }
    }
}

impl Glslang {
    fn parse_errors(
        &self,
        errors: &String,
        file_path: &Path,
        params: &ShaderParams,
        preamble_line_offset: usize,
    ) -> Result<ShaderDiagnosticList, ShaderError> {
        let mut shader_error_list = ShaderDiagnosticList::empty();

        let mut starts = Vec::new();
        for capture in self.diagnostic_regex.captures_iter(errors.as_str()) {
            if let Some(pos) = capture.get(0) {
                starts.push(pos.start());
            }
        }
        starts.push(errors.len());
        let mut include_handler = IncludeHandler::main(
            file_path,
            params.context.includes.clone(),
            params.context.path_remapping.clone(),
        );
        // Cache includes as its a heavy operation.
        let mut include_cache: HashMap<String, PathBuf> = HashMap::new();
        for start in 0..starts.len() - 1 {
            let begin = starts[start];
            let end = starts[start + 1];
            let block = &errors[begin..end];
            if block.contains("compilation errors.  No code generated.") {
                continue; // Skip this useless string.
            }
            if let Some(capture) = self.internal_diagnostic_regex.captures(block) {
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
                            include_cache
                                .entry(relative_path.into())
                                .or_insert_with(|| {
                                    include_handler
                                        .search_path_in_includes(Path::new(&relative_path))
                                        .unwrap_or(file_path.into())
                                })
                                .clone()
                        }
                    }
                };
                let (line, pos) = {
                    // Line is indexed from 1 in glslang, so remove one line (and preamble lines aswell).
                    // But sometimes, it return a line of zero (probably some non initialized position, or preamble issues) so check this aswell.
                    let offset = 1 + preamble_line_offset as u32;
                    let line = line.parse::<u32>().unwrap_or(offset);
                    if line < offset {
                        (0, 0)
                    } else {
                        let pos = pos.parse::<u32>().unwrap_or(0);
                        (line - offset, pos)
                    }
                };
                shader_error_list.push(ShaderDiagnostic {
                    severity: match level {
                        "ERROR" => ShaderDiagnosticSeverity::Error,
                        "WARNING" => ShaderDiagnosticSeverity::Warning,
                        "NOTE" => ShaderDiagnosticSeverity::Information,
                        "HINT" => ShaderDiagnosticSeverity::Hint,
                        _ => ShaderDiagnosticSeverity::Error,
                    },
                    error: String::from(msg),
                    range: ShaderFileRange::new(
                        file_path.clone(),
                        ShaderPosition::new(line, pos),
                        ShaderPosition::new(line, pos),
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
        params: &ShaderParams,
        preamble_line_offset: usize,
    ) -> Result<ShaderDiagnosticList, ShaderError> {
        match err {
            GlslangError::PreprocessError(error) => {
                self.parse_errors(&error, file_path, &params, preamble_line_offset)
            }
            GlslangError::ParseError(error) => {
                self.parse_errors(&error, file_path, &params, preamble_line_offset)
            }
            GlslangError::LinkError(error) => {
                self.parse_errors(&error, file_path, &params, preamble_line_offset)
            }
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
impl ValidatorImpl for Glslang {
    fn validate_shader(
        &self,
        content: &str,
        file_path: &Path,
        params: &ShaderParams,
        include_callback: &mut dyn FnMut(&Path) -> Option<String>,
    ) -> Result<ShaderDiagnosticList, ShaderError> {
        let file_name = self.get_file_name(file_path);

        // Ensure we have a newline character at the end to avoid issues with offset.
        let preamble = if !params.compilation.glsl.preamble.ends_with('\n') {
            let mut preamble = params.compilation.glsl.preamble.clone();
            preamble.push('\n');
            preamble
        } else {
            params.compilation.glsl.preamble.clone()
        };
        let preamble_line_count = preamble.lines().count();

        let (shader_stage, shader_source, preamble_line_offset) =
            if let Some(variant_stage) = params.compilation.shader_stage {
                (variant_stage, preamble + content, preamble_line_count)
            } else if let Some(shader_stage) = ShaderStage::from_file_name(&file_name) {
                (shader_stage, preamble + content, preamble_line_count)
            } else {
                // If we dont have a stage, might require some preprocess to avoid errors.
                // glslang **REQUIRES** to have stage for linting.
                let default_stage = ShaderStage::Fragment;
                if self.hlsl {
                    // HLSL does not require version, simply assume stage.
                    (default_stage, preamble + content, preamble_line_count)
                } else {
                    // glslang does not support linting header file, so to lint them,
                    // Assume Fragment & add default #version if missing
                    if content.contains("#version ") || preamble.contains("#version") {
                        // Main file with missing stage.
                        (default_stage, preamble + content, preamble_line_count)
                    } else {
                        // Header file with missing stage & missing version.
                        // WARN: Assumed this string is one line offset only.
                        let preamble_with_header = format!("#version 450\n{}", preamble);
                        (
                            default_stage,
                            preamble_with_header + content,
                            preamble_line_count + 1, // +1 for added version line.
                        )
                    }
                }
            };

        let source = ShaderSource::try_from(shader_source).expect("Failed to read from source");

        let defines_copy = params.context.defines.clone();
        let defines: Vec<(&str, Option<&str>)> = defines_copy
            .iter()
            .map(|v| (&v.0 as &str, Some(&v.1 as &str)))
            .collect();
        let mut include_handler = GlslangIncludeHandler::new(
            file_path,
            params.context.includes.clone(),
            params.context.path_remapping.clone(),
            include_callback,
        );

        let lang_version = match params.compilation.glsl.spirv {
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
                    if params.compilation.glsl.client.is_opengl() {
                        glslang::Target::OpenGL {
                            version: glslang::OpenGlVersion::OpenGL4_5,
                            spirv_version: None, // TODO ?
                        }
                    } else {
                        let client_version = match params.compilation.glsl.client {
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
                    | if self.hlsl && params.compilation.hlsl.enable16bit_types {
                        glslang::ShaderMessage::HLSL_ENABLE_16BIT_TYPES
                    } else {
                        glslang::ShaderMessage::DEFAULT
                    },
                ..Default::default()
            },
            Some(&defines),
            Some(&mut include_handler),
        )
        .map_err(|e| self.from_glslang_error(e, file_path, &params, preamble_line_offset))
        {
            Ok(value) => value,
            Err(error) => match error {
                Err(error) => return Err(error),
                Ok(diag) => return Ok(diag),
            },
        };
        let _shader = match glslang::Shader::new(&self.compiler, input)
            .map_err(|e| self.from_glslang_error(e, file_path, &params, preamble_line_offset))
        {
            Ok(value) => value,
            Err(error) => match error {
                Err(error) => return Err(error),
                Ok(diag) => return Ok(diag),
            },
        };
        // Linking require main entry point.
        // For now, glslang is expecting main entry point, no way to change this via C api.
        /*if params.entry_point.is_some() {
            let _spirv = match shader
                .compile()
                .map_err(|e| self.from_glslang_error(e, file_path, &params, offset_first_line))
            {
                Ok(value) => value,
                Err(error) => match error {
                    Err(error) => return Err(error),
                    Ok(diag) => return Ok(diag),
                },
            };
        }*/

        Ok(ShaderDiagnosticList::empty()) // No error detected.
    }
    fn support(&self, shader_stage: ShaderStage) -> bool {
        if self.hlsl {
            match shader_stage {
                ShaderStage::Vertex
                | ShaderStage::Fragment
                | ShaderStage::Compute
                | ShaderStage::Geometry
                | ShaderStage::TesselationControl
                | ShaderStage::TesselationEvaluation => true,
                _ => false,
            }
        } else {
            true // All stages supported.
        }
    }
}
