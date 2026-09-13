use std::{collections::HashMap, path::PathBuf};

use lsp_types::{notification::Notification, Url};
use serde::{Deserialize, Serialize};
use shader_sense::shader::{ShaderStage, ShadingLanguage};

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

/// Custom LSP notification (client -> server), method `textDocument/didChangeShaderVariant`.
///
/// This is not part of standard LSP. If you are implementing a client, send this notification
/// to tell the server which "shader variant" a file should be validated with. A variant pins a
/// validation context: an entry point, an optional shader stage, preprocessor defines and
/// include paths (see `ShaderVariant`). The server keeps the last one it receives as the active
/// variant and uses it when validating and resolving symbols.
///
/// To implement it on the client side:
/// - Use the exact method string `textDocument/didChangeShaderVariant` (see `METHOD` below).
/// - Send the JSON payload described by `DidChangeShaderVariantParams`. Field names are
///   camelCase (`#[serde(rename_all = "camelCase")]`), so `shader_variant` -> `shaderVariant`,
///   `entry_point` -> `entryPoint`, `shading_language` -> `shadingLanguage`, etc.
/// - Send `{ "shaderVariant": null }` to clear the active variant.
/// - Re-send the notification every time the user activates, edits or clears a variant; the
///   server always replaces the active variant with the payload it receives.

#[derive(Debug)]
#[allow(dead_code)]
pub enum DidChangeShaderVariant {}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DidChangeShaderVariantParams {
    /// The variant to make active, or `null` to clear it. When cleared, the server drops the
    /// pinned context and validates the file without a specific variant.
    pub shader_variant: Option<ShaderVariant>,
}

impl Notification for DidChangeShaderVariant {
    type Params = DidChangeShaderVariantParams;
    const METHOD: &'static str = "textDocument/didChangeShaderVariant";
}
