use std::{collections::HashMap, path::PathBuf};

use lsp_types::{notification::Notification, request::Request, TextDocumentIdentifier, Url};
use serde::{Deserialize, Serialize};
use shader_sense::shader::{ShaderStage, ShadingLanguage};

use crate::server::{clean_url, ServerLanguage};

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ShaderVariant {
    pub url: Url,
    pub shading_language: ShadingLanguage,
    pub entry_point: String,
    pub stage: Option<ShaderStage>,
    pub defines: HashMap<String, String>,
    pub includes: Vec<PathBuf>,
}

// Could split with add / delete / update
#[derive(Debug)]
#[allow(dead_code)]
pub enum DidChangeShaderVariant {}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DidChangeShaderVariantParams {
    pub shader_variant: Option<ShaderVariant>,
}

impl Notification for DidChangeShaderVariant {
    type Params = DidChangeShaderVariantParams;
    const METHOD: &'static str = "textDocument/didChangeShaderVariant";
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum ShaderVariantRequest {}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ShaderVariantParams {
    #[serde(flatten)]
    pub text_document: TextDocumentIdentifier,
}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ShaderVariantResponse {
    shader_variant: Option<ShaderVariant>,
}

impl Request for ShaderVariantRequest {
    type Params = ShaderVariantParams;
    type Result = ShaderVariantResponse;
    const METHOD: &'static str = "textDocument/shaderVariant";
}

impl ServerLanguage {
    pub fn parse_variant_params(
        &self,
        value: serde_json::Value,
    ) -> Result<Option<ShaderVariant>, serde_json::Error> {
        // Keep compatibility with old client.
        #[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
        #[serde(rename_all = "camelCase")]
        pub struct OldShaderVariant {
            pub entry_point: String,
            pub stage: Option<ShaderStage>,
            pub defines: HashMap<String, String>,
            pub includes: Vec<PathBuf>,
        }
        #[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
        #[serde(rename_all = "camelCase")]
        pub struct OldDidChangeShaderVariantParams {
            pub text_document: TextDocumentIdentifier,
            pub shader_variant: Option<OldShaderVariant>,
        }
        match serde_json::from_value::<DidChangeShaderVariantParams>(value.clone()) {
            Ok(variant) => Ok(variant.shader_variant.map(|mut v| {
                v.url = clean_url(&v.url);
                v
            })),
            Err(new_err) => {
                match serde_json::from_value::<OldDidChangeShaderVariantParams>(value) {
                    Ok(new_variant) => {
                        log::warn!("Client sending old variant version. Should use the new variant version instead. Only the latest enabled variant sent to server will be used.");
                        match &self.watched_files.variant {
                            Some(current_variant) => {
                                if current_variant.url == new_variant.text_document.uri
                                    && new_variant.shader_variant.is_none()
                                {
                                    // If same variant and we removed it, mark it gone.
                                    Ok(None)
                                } else if new_variant.shader_variant.is_some() {
                                    // If we pass a variant with new URL, update (will pick the latest enabled.)
                                    let url = clean_url(&new_variant.text_document.uri);
                                    Ok(new_variant.shader_variant.map(|v| ShaderVariant {
                                        shading_language: match self.watched_files.files.get(&url) {
                                            Some(file) => file.shading_language,
                                            None => ShadingLanguage::Hlsl, // Default to HLSL as we have no way to guess it.
                                        },
                                        url: url,
                                        entry_point: v.entry_point,
                                        stage: v.stage,
                                        defines: v.defines,
                                        includes: v.includes,
                                    }))
                                } else {
                                    Ok(None)
                                }
                            }
                            // If no variant, set new one.
                            None => {
                                let url = clean_url(&new_variant.text_document.uri);
                                Ok(new_variant.shader_variant.map(|v| ShaderVariant {
                                    shading_language: match self.watched_files.files.get(&url) {
                                        Some(file) => file.shading_language,
                                        None => ShadingLanguage::Hlsl, // Default to HLSL as we have no way to guess it.
                                    },
                                    url: url,
                                    entry_point: v.entry_point,
                                    stage: v.stage,
                                    defines: v.defines,
                                    includes: v.includes,
                                }))
                            }
                        }
                    }
                    Err(_err) => Err(new_err),
                }
            }
        }
    }
}
