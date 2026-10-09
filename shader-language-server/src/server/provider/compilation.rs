use base64::{engine::general_purpose::STANDARD as BASE64, Engine};
use lsp_types::{request::Request, TextDocumentIdentifier, Url};
use serde::{Deserialize, Serialize};
use shader_sense::{
    shader::ShadingLanguage,
    shader_error::ShaderError,
    validator::{
        validator::CompilationResult,
        wesl::{spirv_to_wgsl, wgsl_to_spirv},
    },
};

use crate::server::{
    common::ServerLanguageError, server_file_cache::ServerFileCache,
    server_language_data::ServerLanguageData, ServerLanguage,
};

/// Custom LSP request (client -> server), method `textDocument/compilationResult`.
///
/// This is not part of standard LSP. If you are implementing a client, send this request
/// to ask the server for compilation result of given shader.
///
/// To implement it on the client side:
/// - Use the exact method string `textDocument/compilationResult` (see `METHOD` below).
/// - Send the JSON payload described by `CompilationRequestParams`. Field names are
///   camelCase (`#[serde(rename_all = "camelCase")]`).
/// - Listen for request and handle the returned value.
///
#[derive(Debug)]
pub enum CompilationRequest {}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct CompilationRequestParams {
    #[serde(flatten)]
    pub text_document: TextDocumentIdentifier,
    pub disassemble: Option<bool>, // Disassemble compilation result
    pub compilation_type: Option<CompilationType>, // requested compilation type
}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
pub enum CompilationType {
    Spirv,
    Dxil,
    Wgsl, // Wgsl used as is
}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct CompilationRequestResult {
    pub compilation_type: CompilationType,
    pub data: String, // compilation as base64 string, or disassembled result as string
}

impl Request for CompilationRequest {
    type Params = CompilationRequestParams;
    type Result = Option<CompilationRequestResult>;
    const METHOD: &'static str = "textDocument/compilationResult";
}

impl ServerLanguage {
    pub fn recolt_compilation_result(
        &self,
        uri: &Url,
        disassemble: Option<bool>,
        compilation_type: Option<CompilationType>,
    ) -> Result<Option<CompilationRequestResult>, ServerLanguageError> {
        let cached_file = self.get_cachable_file(&uri)?;
        let language_data = self.get_language_data(&cached_file.shading_language)?;
        fn get_result(
            language_data: &ServerLanguageData,
            disassemble: Option<bool>,
            compilation_result: &CompilationResult,
        ) -> Result<String, ShaderError> {
            if disassemble.unwrap_or(false) {
                Ok(language_data.validator.disassemble(compilation_result)?)
            } else {
                match &compilation_result {
                    CompilationResult::None => Err(ShaderError::InternalErr(format!(
                        "Failed compilation of shader. Check diagnostics."
                    ))),
                    CompilationResult::Dxil(dxil) => Ok(BASE64.encode(dxil)),
                    CompilationResult::Spirv(spirv) => Ok(BASE64.encode(spirv)),
                    // Do not encode wgsl as its a string already.
                    CompilationResult::Wgsl(wgsl) => Ok(wgsl.clone()),
                }
            }
        }
        fn get_cached_result(
            language_data: &ServerLanguageData,
            disassemble: Option<bool>,
            cached_file: &ServerFileCache,
        ) -> Result<Option<CompilationRequestResult>, ShaderError> {
            if let Some(data) = &cached_file.data {
                if let CompilationResult::None = data.compilation_cache {
                    if data.diagnostic_cache.diagnostics.is_empty() {
                        Err(ShaderError::InternalErr(format!(
                            "No compilation result. Ensure you have an entry point correctly set."
                        )))
                    } else {
                        Err(ShaderError::InternalErr(format!(
                            "Compilation failed because there is error in your file ({} diagnostics found)",
                            data.diagnostic_cache.diagnostics.len()
                        )))
                    }
                } else {
                    Ok(Some(CompilationRequestResult {
                        compilation_type: match &data.compilation_cache {
                            CompilationResult::None => unreachable!(),
                            CompilationResult::Dxil(_) => CompilationType::Dxil,
                            CompilationResult::Spirv(_) => CompilationType::Spirv,
                            CompilationResult::Wgsl(_) => CompilationType::Wgsl,
                        },
                        data: get_result(language_data, disassemble, &data.compilation_cache)?,
                    }))
                }
            } else {
                Err(ShaderError::InternalErr(format!(
                    "No cached compilation result available."
                )))
            }
        }
        if let Some(compilation_type) = compilation_type {
            let shading_language = cached_file.shading_language;
            match compilation_type {
                CompilationType::Spirv => match shading_language {
                    ShadingLanguage::Glsl => {
                        if self.config.is_generating_spirv(ShadingLanguage::Glsl) {
                            Ok(get_cached_result(language_data, disassemble, cached_file)?)
                            // Glsl already compile to SPIRV
                        } else {
                            Err(ServerLanguageError::InvalidParams(format!(
                                "Cannot request compilation to SPIRV for GLSL with no SPIRV version set."
                            )))
                        }
                    }
                    ShadingLanguage::Hlsl => {
                        if self.config.is_generating_spirv(ShadingLanguage::Hlsl) {
                            Ok(get_cached_result(language_data, disassemble, cached_file)?)
                        // Hlsl generate spirv already
                        } else {
                            Err(ServerLanguageError::InvalidParams(format!(
                                "Cannot request SPIRV compilation for HLSL without enabling the spirv generation."
                            )))
                        }
                    }
                    ShadingLanguage::Wgsl => {
                        if let Some(data) = &cached_file.data {
                            if let CompilationResult::Wgsl(wgsl) = &data.compilation_cache {
                                match wgsl_to_spirv(&wgsl) {
                                    Ok(spirv) => Ok(Some(CompilationRequestResult {
                                        compilation_type: CompilationType::Spirv,
                                        data: get_result(
                                            language_data,
                                            disassemble,
                                            &CompilationResult::Spirv(spirv),
                                        )?,
                                    })),
                                    Err(err) => Err(ServerLanguageError::ShaderError(err)),
                                }
                            } else {
                                Err(ServerLanguageError::InternalError(format!(
                                    "No Wgsl generated in cache."
                                )))
                            }
                        } else {
                            Err(ServerLanguageError::InternalError(format!(
                                "No cache for file."
                            )))
                        }
                    }
                },
                CompilationType::Dxil => match shading_language {
                    ShadingLanguage::Hlsl => {
                        if self.config.is_generating_spirv(ShadingLanguage::Hlsl) {
                            Err(ServerLanguageError::InvalidParams(format!(
                                "Cannot request DXIL compilation for HLSL with spirv generation enabled."
                            )))
                        } else {
                            Ok(get_cached_result(language_data, disassemble, cached_file)?)
                            // HLSL generate DXIL already
                        }
                    }
                    ShadingLanguage::Glsl | ShadingLanguage::Wgsl => {
                        Err(ServerLanguageError::InvalidParams(format!(
                            "Cannot request compilation to Dxil for {:?}",
                            shading_language
                        )))
                    }
                },
                CompilationType::Wgsl => match shading_language {
                    ShadingLanguage::Glsl | ShadingLanguage::Hlsl => {
                        if self.config.is_generating_spirv(shading_language) {
                            if let Some(data) = &cached_file.data {
                                if let CompilationResult::Spirv(spirv) = &data.compilation_cache {
                                    match spirv_to_wgsl(&spirv) {
                                        Ok(wgsl) => Ok(Some(CompilationRequestResult {
                                            compilation_type: CompilationType::Wgsl,
                                            data: wgsl,
                                        })),
                                        Err(err) => Err(ServerLanguageError::ShaderError(err)),
                                    }
                                } else {
                                    Err(ServerLanguageError::InternalError(format!(
                                        "No SPIRV generated in cache."
                                    )))
                                }
                            } else {
                                Err(ServerLanguageError::InternalError(format!(
                                    "No cache for file."
                                )))
                            }
                        } else {
                            Err(ServerLanguageError::InvalidParams(format!(
                                "Cannot request compilation to WGSL for {:?} when not generating SPIRV.",
                                shading_language
                            )))
                        }
                    }
                    ShadingLanguage::Wgsl => {
                        Ok(get_cached_result(language_data, disassemble, cached_file)?)
                    } // No cross compilation required
                },
            }
        } else {
            Ok(get_cached_result(language_data, disassemble, cached_file)?)
        }
    }
}
