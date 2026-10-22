use std::io::Write;
use std::{
    cell::RefCell,
    process::{Command, Stdio},
};

use log::info;
use lsp_types::{TextEdit, Url};
use shader_sense::position::ShaderRange;
use shader_sense::{shader::ShadingLanguage, shader_error::ShaderError};

use crate::server::common::{shader_range_to_lsp_range, ServerLanguageError};
use crate::server::ServerLanguage;

impl ServerLanguage {
    fn clang_format_path() -> &'static str {
        "clang-format"
    }
    pub fn is_clang_format_available() -> bool {
        if cfg!(target_os = "wasi") {
            false // Cannot spawn an exe in wasi.
        } else {
            match Command::new(Self::clang_format_path())
                .arg("--version")
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .spawn()
            {
                Ok(child) => match child.wait_with_output() {
                    Ok(output) => {
                        info!(
                            "Using {} for formatting",
                            String::from_utf8(output.stdout)
                                .unwrap_or("clang-format version unknown".into())
                                .trim()
                        );
                        true
                    }
                    Err(_) => {
                        info!("clang-format failed to get version.");
                        false
                    }
                },
                Err(e) => {
                    if let std::io::ErrorKind::NotFound = e.kind() {
                        info!("clang-format not found, disabled formatting.");
                        false
                    } else {
                        info!("clang-format failed, disabled formatting.");
                        false
                    }
                }
            }
        }
    }
    pub fn recolt_formatting(
        &self,
        uri: &Url,
        range: Option<ShaderRange>,
    ) -> Result<Vec<TextEdit>, ServerLanguageError> {
        let cached_file = self.get_cachable_file(&uri)?;
        match &cached_file.shading_language {
            ShadingLanguage::Wgsl => {
                // TODO: Find a formatter for wgsl.
                // naga does not provide much for this...
                // wgsl analyzer does its own parsing https://github.com/wgsl-analyzer/wgsl-analyzer/blob/main/crates/wgsl_formatter/src/lib.rs
                Ok(vec![])
            }
            // HLSL & GLSL can rely on clang-format.
            ShadingLanguage::Hlsl | ShadingLanguage::Glsl => {
                let shader_module = RefCell::borrow(&cached_file.shader_module);
                let (offset, length) = match &range {
                    Some(range) => {
                        let byte_offset_start =
                            range.start.to_byte_offset(&shader_module.content)?;
                        let byte_offset_end = range.end.to_byte_offset(&shader_module.content)?;
                        let byte_length = byte_offset_end - byte_offset_start;
                        assert!(byte_length <= shader_module.content.len());
                        (byte_offset_start, byte_length)
                    }
                    None => (0, shader_module.content.len()),
                };
                info!(
                    "Offset {} and length {} for content {}",
                    offset,
                    length,
                    shader_module.content.len()
                );
                let mut child = Command::new("clang-format")
                    // Required for finding .clang-format
                    .arg(format!(
                        "--assume-filename={}",
                        shader_module.file_path.display()
                    ))
                    .arg(format!("--offset={}", offset))
                    .arg(format!("--length={}", length))
                    //.arg("--style")
                    //.arg("file") // need a .clang-format file for style
                    //.arg("--fallback-style")
                    //.arg("")
                    .stdin(Stdio::piped())
                    .stdout(Stdio::piped())
                    .spawn()?;
                // Note we place inside a scope to ensure that stdin is closed
                {
                    let mut stdin = child.stdin.take().expect("no stdin handle");
                    write!(stdin, "{}", &shader_module.content)?;
                }
                // Wait for the output and mark it as big edit chunk.
                let output = child.wait_with_output()?;
                if output.status.success() {
                    let formatted_code = String::from_utf8(output.stdout)
                        .map_err(|e| ShaderError::InternalErr(e.utf8_error().to_string()))?;
                    Ok(vec![TextEdit {
                        range: shader_range_to_lsp_range(&ShaderRange::whole(
                            &shader_module.content,
                        )),
                        new_text: formatted_code,
                    }])
                } else {
                    Err(ServerLanguageError::InternalError(
                        "Failed to parse file with clang-format".into(),
                    ))
                }
            }
        }
    }
}
