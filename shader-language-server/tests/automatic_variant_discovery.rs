// Skip all these test on WASI.
// WASI cannot spawn a server so test on pc with WASMTIME runner instead.
#![cfg(not(target_os = "wasi"))]

use std::{collections::HashMap, path::Path};

use lsp_types::request::DocumentDiagnosticRequest;
use lsp_types::{
    notification::{DidCloseTextDocument, DidOpenTextDocument},
    DidCloseTextDocumentParams, DidOpenTextDocumentParams,
};
use serde_json::json;
use shader_language_server::server::server_config::ServerSerializedConfig;
use shader_language_server::server::shader_variant::{
    DidChangeShaderVariant, DidChangeShaderVariantParams, ShaderVariant,
};
use shader_language_server::server::Transport;
use shader_sense::shader::ShadingLanguage;
use test_server::{TestFile, TestServer};

use crate::test_server::get_error_diagnostics;

mod test_server;

#[test]
fn test_automatic_variant_discovery_use_includer_context() {
    let mut server = TestServer::new(ServerSerializedConfig::default(), Transport::Stdio).unwrap();

    let file = TestFile::new(
        Path::new("../shader-sense/test/glsl/auto-variant/auto-variant.comp.glsl"),
        ShadingLanguage::Glsl,
    );
    let deps = TestFile::new(
        Path::new("../shader-sense/test/glsl/auto-variant/workgroup-layout.glsl"),
        ShadingLanguage::Glsl,
    );

    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file.item(),
    });
    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: deps.item(),
    });
    server.send_request::<DocumentDiagnosticRequest>(
        &deps.document_diagnostic_params(),
        |report| {
            let errors = get_error_diagnostics(report.unwrap());
            assert!(
                !errors.is_empty(),
                "Dependency-context diagnostics should stay disabled by default. Got {:#?}",
                errors,
            );
        },
    );
    server.update_configuration(json!({
        "automaticVariantDiscovery": true,
    }));
    server.send_request::<DocumentDiagnosticRequest>(
        &deps.document_diagnostic_params(),
        |report| {
            let errors = get_error_diagnostics(report.unwrap());
            assert!(
                errors.is_empty(),
                "Include file should inherit diagnostics context from its main shader. Got {:#?}",
                errors,
            );
        },
    );
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: deps.identifier(),
    });
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: file.identifier(),
    });
}

#[test]
fn test_automatic_variant_discovery_keep_selected_variant_context() {
    let config: ServerSerializedConfig = serde_json::from_value(json!({
        "automaticVariantDiscovery": true
    }))
    .unwrap();
    let mut server = TestServer::new(config, Transport::Stdio).unwrap();

    let invalid_main = TestFile::new(
        Path::new("../shader-sense/test/glsl/auto-variant/a-auto-variant.frag.glsl"),
        ShadingLanguage::Glsl,
    );
    let selected_variant = TestFile::new(
        Path::new("../shader-sense/test/glsl/auto-variant/auto-variant.comp.glsl"),
        ShadingLanguage::Glsl,
    );
    let deps = TestFile::new(
        Path::new("../shader-sense/test/glsl/auto-variant/workgroup-layout.glsl"),
        ShadingLanguage::Glsl,
    );

    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: invalid_main.item(),
    });
    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: selected_variant.item(),
    });
    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: deps.item(),
    });
    server.send_notification::<DidChangeShaderVariant>(&DidChangeShaderVariantParams {
        shader_variant: Some(ShaderVariant {
            url: selected_variant.url.clone(),
            shading_language: ShadingLanguage::Glsl,
            entry_point: "".into(),
            stage: None,
            defines: HashMap::new(),
            includes: Vec::new(),
        }),
    });
    server.send_request::<DocumentDiagnosticRequest>(
        &deps.document_diagnostic_params(),
        |report| {
            let errors = get_error_diagnostics(report.unwrap());
            assert!(
                errors.is_empty(),
                "Selected variant should take precedence over auto-selected includers. Got {:#?}",
                errors,
            );
        },
    );
    server.send_notification::<DidChangeShaderVariant>(&DidChangeShaderVariantParams {
        shader_variant: None,
    });
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: deps.identifier(),
    });
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: selected_variant.identifier(),
    });
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: invalid_main.identifier(),
    });
}
