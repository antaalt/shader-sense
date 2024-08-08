use hassle_rs::*;
use std::{ffi::OsStr, path::Path};

use crate::{
    common::{ShaderTree, ValidationParams, Validator},
    include::IncludeHandler,
    shader_error::{ShaderError, ShaderErrorList, ShaderErrorSeverity},
};

pub struct Dxc {
    compiler: hassle_rs::DxcCompiler,
    library: hassle_rs::DxcLibrary,

    validator: Option<hassle_rs::DxcValidator>,
    dxil: Option<hassle_rs::wrapper::Dxil>,

    #[allow(dead_code)] // Need to keep dxc alive while dependencies created
    dxc: hassle_rs::wrapper::Dxc,
}

impl hassle_rs::wrapper::DxcIncludeHandler for IncludeHandler {
    fn load_source(&mut self, filename: String) -> Option<String> {
        let path = Path::new(filename.as_str());
        self.search_in_includes(&path)
    }
}

impl From<hassle_rs::HassleError> for ShaderErrorList {
    fn from(error: hassle_rs::HassleError) -> Self {
        match error {
            HassleError::CompileError(err) => match Dxc::parse_dxc_errors(&err) {
                Ok(error_list) => error_list,
                Err(error_list) => error_list,
            },
            HassleError::ValidationError(err) => {
                ShaderErrorList::from(ShaderError::ValidationErr {
                    message: err.to_string(),
                })
            }
            HassleError::LibLoadingError(err) => ShaderErrorList::internal(err.to_string()),
            HassleError::LoadLibraryError { filename, inner } => {
                ShaderErrorList::internal(format!(
                    "Failed to load library {}: {}",
                    filename.display(),
                    inner.to_string()
                ))
            }
            HassleError::Win32Error(err) => {
                ShaderErrorList::internal(format!("Win32 error: HRESULT={}", err))
            }
            HassleError::WindowsOnly(err) => {
                ShaderErrorList::internal(format!("Windows only error: {}", err))
            }
        }
    }
}

impl Dxc {
    pub fn new() -> Result<Self, hassle_rs::HassleError> {
        let dxc = hassle_rs::Dxc::new(None)?;
        let library = dxc.create_library()?;
        let compiler = dxc.create_compiler()?;
        let (dxil, validator) = match Dxil::new(None) {
            Ok(dxil) => {
                let validator_option = match dxil.create_validator() {
                    Ok(validator) => Some(validator),
                    Err(_) => None,
                };
                (Some(dxil), validator_option)
            }
            Err(_) => (None, None),
        };
        Ok(Self {
            dxc,
            compiler,
            library,
            dxil,
            validator,
        })
    }
    fn parse_dxc_errors(errors: &String) -> Result<ShaderErrorList, ShaderErrorList> {
        let mut shader_error_list = ShaderErrorList::empty();

        let reg = regex::Regex::new(r"(?m)^(.*?:\d+:\d+: .*:.*?)$")?;
        let mut starts = Vec::new();
        for capture in reg.captures_iter(errors.as_str()) {
            if let Some(pos) = capture.get(0) {
                starts.push(pos.start());
            }
        }
        starts.push(errors.len());
        let internal_reg = regex::Regex::new(r"(?s)^(.*?):(\d+):(\d+): (.*?):(.*)")?;
        for start in 0..starts.len() - 1 {
            let first = starts[start];
            let length = starts[start + 1] - starts[start];
            let block: String = errors.chars().skip(first).take(length).collect();
            if let Some(capture) = internal_reg.captures(block.as_str()) {
                let filename = capture.get(1).map_or("", |m| m.as_str());
                let line = capture.get(2).map_or("", |m| m.as_str());
                let pos = capture.get(3).map_or("", |m| m.as_str());
                let level = capture.get(4).map_or("", |m| m.as_str());
                let msg = capture.get(5).map_or("", |m| m.as_str());
                shader_error_list.push(ShaderError::ParserErr {
                    filename: Some(String::from(filename)),
                    severity: match level {
                        "error" => ShaderErrorSeverity::Error,
                        "warning" => ShaderErrorSeverity::Warning,
                        "note" => ShaderErrorSeverity::Information,
                        "hint" => ShaderErrorSeverity::Hint,
                        _ => ShaderErrorSeverity::Error,
                    },
                    error: String::from(msg),
                    line: line.parse::<u32>().unwrap_or(0),
                    pos: pos.parse::<u32>().unwrap_or(0),
                });
            }
        }

        if shader_error_list.errors.len() == 0 {
            shader_error_list.push(ShaderError::InternalErr(format!(
                "Failed to parse errors: {}",
                errors
            )));
        }
        return Ok(shader_error_list);
    }
}
impl Validator for Dxc {
    fn validate_shader(
        &mut self,
        shader_source: String,
        filename: String,
        cwd: &Path,
        params: ValidationParams,
    ) -> Result<(), ShaderErrorList> {

        let blob = self.library.create_blob_with_encoding_from_str(&shader_source)?;

        let defines_copy = params.defines.clone();
        let defines: Vec<(&str, Option<&str>)> = defines_copy
            .iter()
            .map(|v| (&v.0 as &str, Some(&v.1 as &str)))
            .collect();

        let result = self.compiler.compile(
            &blob,
            filename.as_str(),
            "", // TODO: Could have a command to validate specific entry point (specify stage & entry point)
            "lib_6_5",
            &[], // TODO: should control this from settings (-enable-16bit-types)
            Some(&mut IncludeHandler::new(cwd, params.includes)),
            &defines,
        );

        match result {
            Ok(dxc_result) => {
                let result_blob = dxc_result.get_result()?;
                // Skip validation if dxil.dll does not exist.
                if let (Some(_dxil), Some(validator)) = (&self.dxil, &self.validator) {
                    let data = result_blob.to_vec();
                    let blob_encoding = self.library.create_blob_with_encoding(data.as_ref())?;

                    match validator.validate(blob_encoding.into()) {
                        Ok(_) => Ok(()),
                        Err(dxc_err) => {
                            let error_blob = dxc_err.0.get_error_buffer()?;
                            let error_emitted =
                                self.library.get_blob_as_string(&error_blob.into())?;
                            Err(ShaderErrorList::internal(format!(
                                "Validation failed: {}",
                                error_emitted
                            )))
                        }
                    }
                } else {
                    Ok(())
                }
            }
            Err((dxc_result, _hresult)) => {
                let error_blob = dxc_result.get_error_buffer()?;
                Err(ShaderErrorList::from(HassleError::CompileError(
                    self.library.get_blob_as_string(&error_blob.into())?,
                )))
            }
        }
    }

    fn get_shader_tree(
        &mut self,
        path: &Path,
        cwd: &Path,
        params: ValidationParams,
    ) -> Result<ShaderTree, ShaderErrorList> {
        let types = Vec::new();
        let global_variables = Vec::new();
        let functions = Vec::new();

        let source = std::fs::read_to_string(path)?;

        let path_name = path.file_name().unwrap_or(&OsStr::new("shader.hlsl"));
        let path_name_str = path_name.to_str().unwrap_or("shader.hlsl");

        let blob = self.library.create_blob_with_encoding_from_str(&source)?;

        let result = self.compiler.compile(
            &blob,
            path_name_str,
            "",
            "lib_6_5",
            &[],
            Some(&mut IncludeHandler::new(cwd, params.includes)),
            &[],
        );

        match result {
            Ok(dxc_result) => {
                let result_blob = dxc_result.get_result()?;
                let data = result_blob.to_vec();
                let blob_encoding = self.library.create_blob_with_encoding(data.as_ref())?;
                let reflector = self.dxc.create_reflector()?;
                let reflection = reflector.reflect(blob_encoding.into())?;
                // Hassle capabilities on this seems limited for now...
                // Would need to create a PR to add interface for other API.
                reflection.thread_group_size();

                Ok(ShaderTree {
                    types,
                    global_variables,
                    functions,
                })
            }
            Err((_dxc_result, _hresult)) => Err(ShaderErrorList::internal(String::from(
                "Failed to get reflection data from shader",
            ))),
        }
    }
}
