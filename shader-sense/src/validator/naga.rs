use naga::{
    front::wgsl::{self, ParseError},
    valid::{Capabilities, ValidationFlags},
};
use std::path::Path;

use crate::{
    shader::ShaderParams,
    shader_error::{ShaderDiagnostic, ShaderDiagnosticList, ShaderDiagnosticSeverity, ShaderError},
    symbols::symbols::{ShaderPosition, ShaderRange},
};

use super::validator::Validator;

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
                range: ShaderRange::new(
                    ShaderPosition::new(file_path.into(), loc.line_number - 1, loc.line_position),
                    ShaderPosition::new(file_path.into(), loc.line_number - 1, loc.line_position),
                ),
            }
        } else {
            ShaderDiagnostic {
                severity: ShaderDiagnosticSeverity::Error,
                error,
                range: ShaderRange::new(
                    ShaderPosition::new(file_path.into(), 0, 0),
                    ShaderPosition::new(file_path.into(), 0, 0),
                ),
            }
        }
    }
}
impl Validator for Naga {
    fn validate_shader(
        &self,
        shader_content: &str,
        file_path: &Path,
        _params: &ShaderParams,
        _include_callback: &mut dyn FnMut(&Path) -> Option<String>,
    ) -> Result<ShaderDiagnosticList, ShaderError> {
        let module = match wgsl::parse_str(shader_content)
            .map_err(|err| Self::from_parse_err(err, file_path, shader_content))
        {
            Ok(module) => module,
            Err(diag) => {
                return Ok(ShaderDiagnosticList::from(diag));
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
                    range: ShaderRange::new(
                        ShaderPosition::new(
                            file_path.into(),
                            loc.line_number - 1,
                            loc.line_position,
                        ),
                        ShaderPosition::new(
                            file_path.into(),
                            loc.line_number - 1,
                            loc.line_position,
                        ),
                    ),
                });
            }
            if list.is_empty() {
                Err(ShaderError::InternalErr(
                    error.emit_to_string(shader_content),
                ))
            } else {
                Ok(list)
            }
        } else {
            Ok(ShaderDiagnosticList::empty())
        }
    }
}
