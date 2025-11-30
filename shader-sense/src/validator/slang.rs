use std::path::{Path, PathBuf};

use crate::{
    position::{ShaderFileRange, ShaderPosition},
    shader::{ShaderParams, ShaderStage},
    shader_error::{ShaderDiagnostic, ShaderDiagnosticList, ShaderDiagnosticSeverity, ShaderError},
    validator::validator::ValidatorImpl,
};

pub struct Slang {
    // Cache regex for parsing.
    diagnostic_regex: regex::Regex,
}

impl Slang {
    pub fn new() -> Self {
        Self {
            diagnostic_regex: regex::Regex::new(
                r"(?m)^(.+?)\((\d+)\)\: (warning|error|note|hint) (\d+)\:(.*?)",
            )
            .unwrap(),
        }
    }
    fn parse_errors(
        &self,
        errors: &String,
        file_path: &Path,
        _params: &ShaderParams,
    ) -> Result<ShaderDiagnosticList, ShaderError> {
        let mut shader_error_list = ShaderDiagnosticList::empty();

        for capture in self.diagnostic_regex.captures_iter(errors.as_str()) {
            let _relative_path = capture.get(1).map_or("", |m| m.as_str());
            let line = capture.get(2).map_or("", |m| m.as_str());
            let level = capture.get(3).map_or("", |m| m.as_str());
            let _code = capture.get(4).map_or("", |m| m.as_str());
            let msg = capture.get(5).map_or("", |m| m.as_str());
            let file_path: PathBuf = file_path.into(); // TODO: handle includes.
            let line = line.parse::<u32>().unwrap_or(0);
            //let pos = pos.parse::<u32>().unwrap_or(0);
            shader_error_list.push(ShaderDiagnostic {
                severity: match level {
                    "error" => ShaderDiagnosticSeverity::Error,
                    "warning" => ShaderDiagnosticSeverity::Warning,
                    "note" => ShaderDiagnosticSeverity::Information,
                    "hint" => ShaderDiagnosticSeverity::Hint,
                    _ => ShaderDiagnosticSeverity::Error,
                },
                error: String::from(msg),
                range: ShaderFileRange::new(
                    file_path.clone(),
                    ShaderPosition::new(line, 0),
                    ShaderPosition::new(line, 0),
                ),
            });
        }

        if shader_error_list.is_empty() {
            return Err(ShaderError::InternalErr(format!(
                "Failed to parse errors: {}",
                errors
            )));
        }
        return Ok(shader_error_list);
    }
}

impl ValidatorImpl for Slang {
    fn validate_shader(
        &self,
        _shader_content: &str,
        file_path: &Path,
        params: &ShaderParams,
        _include_callback: &mut dyn FnMut(&Path) -> Option<String>, // TODO: cant be used now as slang-rs does not provide way for custom load.
    ) -> Result<ShaderDiagnosticList, ShaderError> {
        // TODO: this should not be recreated.
        let global_session = shader_slang::GlobalSession::new().unwrap();
        // TODO: how to get memory ?
        let directory_path = file_path.parent().unwrap();
        let directory_path = directory_path.as_os_str().to_str().unwrap();
        let search_path = std::ffi::CString::new(directory_path).unwrap();

        // All compiler options are available through this builder.
        // New defines means new session required

        let session_options = shader_slang::CompilerOptions::default()
            .optimization(shader_slang::OptimizationLevel::High)
            .matrix_layout_row(true)
            .macro_define("key", "value");

        let target_desc = shader_slang::TargetDesc::default()
            .format(shader_slang::CompileTarget::Dxil)
            .profile(global_session.find_profile("sm_6_5"));

        let targets = [target_desc];
        let search_paths = [search_path.as_ptr()];

        let session_desc = shader_slang::SessionDesc::default()
            .targets(&targets)
            .search_paths(&search_paths)
            .options(&session_options);
        // In order to have a custom file loader which behave the same for all lang,
        // we should implement a class extending ISlangFileSystem with a custom loadFile,
        // but this is not possible to do in pure Rust. Need to implement the class in C++
        // TODO: implement filesystem to be able to use include_callback
        //session_desc.fileSystem;

        let session = global_session.create_session(&session_desc).unwrap();
        //session.load_module(name);
        match session.load_module("shader.slang") {
            Ok(_module) => Ok(ShaderDiagnosticList::empty()),
            Err(errors) => self.parse_errors(&errors.to_string(), file_path, params),
        }
    }

    fn support(&self, _shader_stage: ShaderStage) -> bool {
        true // TODO: check support
    }
}
