use lsp_types::{request::Request, TextDocumentIdentifier, Url};
use serde::{Deserialize, Serialize};
use shader_sense::{
    shader::ShadingLanguage,
    validator::{naga::Naga, validator::CompilationResult},
};

use crate::server::{
    common::ServerLanguageError, server_file_cache::ServerFileCache, ServerLanguage,
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
    pub compilation_type: Option<CompilationType>, // requested compilation type
}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
pub enum CompilationType {
    Spirv,
    Dxil,
    Wgsl, // Wgsl used as is
}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
pub struct CompilationRequestResult {
    pub compilation_type: CompilationType,
    #[serde(with = "base64_bytes")] // Compress the data
    pub data: Vec<u8>,
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
        compilation_type: Option<CompilationType>,
    ) -> Result<Option<CompilationRequestResult>, ServerLanguageError> {
        let cached_file = self.get_cachable_file(&uri)?;
        fn get_cached_result(cached_file: &ServerFileCache) -> Option<CompilationRequestResult> {
            if let Some(data) = &cached_file.data {
                if let CompilationResult::None = data.compilation_cache {
                    None
                } else {
                    Some(CompilationRequestResult {
                        compilation_type: match &data.compilation_cache {
                            CompilationResult::None => unreachable!(),
                            CompilationResult::Dxil(_) => CompilationType::Dxil,
                            CompilationResult::Spirv(_) => CompilationType::Spirv,
                            CompilationResult::Wgsl(_) => CompilationType::Wgsl,
                        },
                        data: match &data.compilation_cache {
                            CompilationResult::None => Vec::new(),
                            CompilationResult::Dxil(dxil) => dxil.clone(),
                            CompilationResult::Spirv(spirv) => spirv.clone(),
                            CompilationResult::Wgsl(wgsl) => wgsl.clone().into_bytes(),
                        },
                    })
                }
            } else {
                None
            }
        }
        if let Some(compilation_type) = compilation_type {
            let shading_language = cached_file.shading_language;
            match compilation_type {
                CompilationType::Spirv => match shading_language {
                    ShadingLanguage::Glsl => {
                        if self.config.is_generating_spirv(ShadingLanguage::Glsl) {
                            Err(ServerLanguageError::InvalidParams(format!("Cannot request compilation to SPIRV for GLSL with no SPIRV version set.")))
                        } else {
                            Ok(get_cached_result(cached_file)) // Glsl already compile to SPIRV
                        }
                    }
                    ShadingLanguage::Hlsl => {
                        if self.config.is_generating_spirv(ShadingLanguage::Hlsl) {
                            Ok(get_cached_result(cached_file)) // Hlsl generate spirv already
                        } else {
                            Err(ServerLanguageError::InvalidParams(format!("Cannot request SPIRV compilation for HLSL without enabling the spirv generation.")))
                        }
                    }
                    ShadingLanguage::Wgsl => {
                        if let Some(data) = &cached_file.data {
                            if let CompilationResult::Wgsl(wgsl) = &data.compilation_cache {
                                match Naga::wgsl_to_spirv(&wgsl) {
                                    Ok(spirv) => Ok(Some(CompilationRequestResult {
                                        compilation_type: CompilationType::Spirv,
                                        data: spirv,
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
                            Err(ServerLanguageError::InvalidParams(format!("Cannot request DXIL compilation for HLSL with spirv generation enabled.")))
                        } else {
                            Ok(get_cached_result(cached_file)) // HLSL generate DXIL already
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
                                    match Naga::spirv_to_wgsl(&spirv) {
                                        Ok(wgsl) => Ok(Some(CompilationRequestResult {
                                            compilation_type: CompilationType::Wgsl,
                                            data: wgsl.into_bytes(),
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
                            Err(ServerLanguageError::InvalidParams(format!("Cannot request compilation to WGSL for {:?} when not generating SPIRV.", shading_language)))
                        }
                    }
                    ShadingLanguage::Wgsl => Ok(get_cached_result(cached_file)), // No cross compilation required
                },
            }
        } else {
            Ok(get_cached_result(cached_file))
        }
    }
}
