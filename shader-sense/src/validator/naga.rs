//! Validation for wgsl with [`naga`]

use naga::{
    front::wgsl::{self, ParseError},
    valid::{Capabilities, ValidationFlags},
};
use std::path::Path;

use crate::{
    position::{ShaderFileRange, ShaderPosition},
    shader::{ShaderParams, ShaderStage},
    shader_error::{ShaderDiagnostic, ShaderDiagnosticList, ShaderDiagnosticSeverity, ShaderError},
    validator::validator::CompilationResult,
};

use super::validator::ValidatorImpl;

pub struct Naga {}

impl Naga {
    pub fn new() -> Self {
        Self {}
    }
    fn from_parse_err(err: ParseError, file_path: &Path, shader_content: &str) -> ShaderDiagnostic {
        let error = err.emit_to_string(shader_content);
        let loc = err.location(shader_content);
        if let Some(loc) = loc {
            ShaderDiagnostic {
                severity: ShaderDiagnosticSeverity::Error,
                error,
                range: ShaderFileRange::new(
                    file_path.into(),
                    ShaderPosition::new(loc.line_number - 1, loc.line_position),
                    ShaderPosition::new(loc.line_number - 1, loc.line_position),
                ),
            }
        } else {
            ShaderDiagnostic {
                severity: ShaderDiagnosticSeverity::Error,
                error,
                range: ShaderFileRange::zero(file_path.into()),
            }
        }
    }
    /// Convert a SPIR-V binary module to its WGSL representation.
    pub fn spirv_to_wgsl(spirv: &[u8]) -> Result<String, ShaderError> {
        // TODO: Option should change depending on target spirv version (adjust_coordinate_space which is > SPV1.0).
        let module = naga::front::spv::parse_u8_slice(spirv, &naga::front::spv::Options::default())
            .map_err(|err| {
                ShaderError::ValidationError(format!("Failed to parse SPIR-V module: {}", err))
            })?;
        let mut validator =
            naga::valid::Validator::new(ValidationFlags::all(), Capabilities::all());
        let module_info = validator.validate(&module).map_err(|err| {
            ShaderError::ValidationError(format!(
                "Failed to validate SPIR-V module: {}",
                err.emit_to_string("")
            ))
        })?;
        naga::back::wgsl::write_string(
            &module,
            &module_info,
            naga::back::wgsl::WriterFlags::empty(),
        )
        .map_err(|err| ShaderError::InternalErr(format!("Failed to write WGSL: {}", err)))
    }
    /// Convert a WGSL shader to its SPIR-V binary representation.
    pub fn wgsl_to_spirv(shader_content: &str) -> Result<Vec<u8>, ShaderError> {
        let module = wgsl::parse_str(shader_content).map_err(|err| {
            ShaderError::ValidationError(format!(
                "Failed to parse WGSL module: {}",
                err.emit_to_string(shader_content)
            ))
        })?;
        let mut validator =
            naga::valid::Validator::new(ValidationFlags::all(), Capabilities::all());
        let module_info = validator.validate(&module).map_err(|err| {
            ShaderError::ValidationError(format!(
                "Failed to validate WGSL module: {}",
                err.emit_to_string(shader_content)
            ))
        })?;
        let words = naga::back::spv::write_vec(
            &module,
            &module_info,
            &naga::back::spv::Options::default(),
            None, // Emit every entry point.
        )
        .map_err(|err| {
            ShaderError::InternalErr(format!("Failed to write SPIR-V module: {}", err))
        })?;
        // Little endian, to match what naga::front::spv expects.
        Ok(words.iter().flat_map(|word| word.to_le_bytes()).collect())
    }
}
impl ValidatorImpl for Naga {
    fn validate_shader(
        &self,
        shader_content: &str,
        file_path: &Path,
        _params: &ShaderParams,
        _include_callback: &mut dyn FnMut(&Path) -> Option<String>,
    ) -> Result<(CompilationResult, ShaderDiagnosticList), ShaderError> {
        let module = match wgsl::parse_str(shader_content)
            .map_err(|err| Self::from_parse_err(err, file_path, shader_content))
        {
            Ok(module) => module,
            Err(diag) => {
                return Ok((CompilationResult::None, ShaderDiagnosticList::from(diag)));
            }
        };

        let mut validator =
            naga::valid::Validator::new(ValidationFlags::all(), Capabilities::all());
        if let Err(error) = validator.validate(&module) {
            let mut list = ShaderDiagnosticList::empty();
            for (span, _) in error.spans() {
                let loc = span.location(shader_content);
                list.push(ShaderDiagnostic {
                    severity: ShaderDiagnosticSeverity::Error,
                    error: error.emit_to_string(""),
                    range: ShaderFileRange::new(
                        file_path.into(),
                        ShaderPosition::new(loc.line_number - 1, loc.line_position),
                        ShaderPosition::new(loc.line_number - 1, loc.line_position),
                    ),
                });
            }
            if list.is_empty() {
                Err(ShaderError::InternalErr(
                    error.emit_to_string(shader_content),
                ))
            } else {
                Ok((CompilationResult::None, list))
            }
        } else {
            // Wgsl compile to itself
            Ok((
                CompilationResult::Wgsl(shader_content.into()),
                ShaderDiagnosticList::empty(),
            ))
        }
    }
    fn support(&self, shader_stage: ShaderStage) -> bool {
        match shader_stage {
            ShaderStage::Vertex | ShaderStage::Fragment | ShaderStage::Compute => true,
            _ => false,
        }
    }
}
