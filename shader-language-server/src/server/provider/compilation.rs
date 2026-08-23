use lsp_types::{request::Request, TextDocumentIdentifier, Url};
use serde::{Deserialize, Serialize};
use shader_sense::validator::validator::CompilationResult;

use crate::server::{common::ServerLanguageError, ServerLanguage};

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
}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
pub enum CompilationType {
    Spirv,
    Dxil,
}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
pub struct CompilationRequestResult {
    pub ty: CompilationType,
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
    ) -> Result<Option<CompilationRequestResult>, ServerLanguageError> {
        let cached_file = self.get_cachable_file(&uri)?;

        if let Some(data) = &cached_file.data {
            if let CompilationResult::None = data.compilation_cache {
                Ok(None)
            } else {
                Ok(Some(CompilationRequestResult {
                    ty: match &data.compilation_cache {
                        CompilationResult::None => unreachable!(),
                        CompilationResult::Dxil(_) => CompilationType::Dxil,
                        CompilationResult::Spirv(_) => CompilationType::Spirv,
                    },
                    data: match &data.compilation_cache {
                        CompilationResult::None => Vec::new(),
                        CompilationResult::Dxil(dxil) => dxil.clone(),
                        CompilationResult::Spirv(spirv) => spirv.clone(),
                    },
                }))
            }
        } else {
            Ok(None)
        }
    }
}
