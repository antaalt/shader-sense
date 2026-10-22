// Skip all these test on WASI.
// WASI cannot spawn a server so test on pc with WASMTIME runner instead.
#![cfg(not(target_os = "wasi"))]

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use lsp_types::{
    notification::{DidCloseTextDocument, DidOpenTextDocument, PublishDiagnostics},
    request::{DocumentDiagnosticRequest, DocumentSymbolRequest},
    DidCloseTextDocumentParams, DidOpenTextDocumentParams, DocumentDiagnosticParams,
    PartialResultParams, PublishDiagnosticsParams, Url, WorkDoneProgressParams,
};
use serde_json::json;
use shader_language_server::server::{
    server_config::ServerSerializedConfig,
    shader_variant::{DidChangeShaderVariant, DidChangeShaderVariantParams, ShaderVariant},
    Transport,
};
use shader_sense::shader::{ShaderStage, ShadingLanguage};

use crate::test_server::{
    get_all_diagnostics, get_error_diagnostics, has_any_document_symbol, workspace_path, TestFile,
    TestServer,
};

mod test_server;

#[test]
fn test_glsl_relative_preamble() {
    let config: ServerSerializedConfig = serde_json::from_value(json!({
        "glsl": {
            "preamble": workspace_path("glsl/helpers/preamble.glsl")
        }
    }))
    .unwrap();
    let mut server = TestServer::new(config, Transport::Stdio).unwrap();

    let file = TestFile::new("glsl/dependent-include.frag.glsl", ShadingLanguage::Glsl);
    println!("Opening file {}", file.uri);

    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file.item(),
    });
    server.send_request::<DocumentDiagnosticRequest>(
        &DocumentDiagnosticParams {
            text_document: file.identifier(),
            identifier: None,
            previous_result_id: None,
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        },
        |report| {
            let report = get_all_diagnostics(report);
            assert!(
                report.is_empty(),
                "Should not have any error with preamble file, got {:#?}",
                report
            );
        },
    );
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: file.identifier(),
    });
}
#[test]
fn test_validate() {
    let config: ServerSerializedConfig = serde_json::from_value(json!({
        "validate": false
    }))
    .unwrap();
    let mut server = TestServer::new(config, Transport::Stdio).unwrap();

    let file = TestFile::new("glsl/error-parsing.frag.glsl", ShadingLanguage::Glsl);
    println!("Opening file {}", file.uri);

    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file.item(),
    });
    server.send_request::<DocumentDiagnosticRequest>(
        &DocumentDiagnosticParams {
            text_document: file.identifier(),
            identifier: None,
            previous_result_id: None,
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        },
        |report| {
            let report = get_all_diagnostics(report);
            assert!(
                report.is_empty(),
                "Should not have any error as validate is disabled, got {:#?}",
                report
            );
        },
    );
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: file.identifier(),
    });
}
#[test]
fn test_symbols() {
    let config: ServerSerializedConfig = serde_json::from_value(json!({
        "symbols": false
    }))
    .unwrap();
    let mut server = TestServer::new(config, Transport::Stdio).unwrap();

    let file = TestFile::new("glsl/include-level.comp.glsl", ShadingLanguage::Glsl);
    println!("Opening file {}", file.uri);

    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file.item(),
    });
    server.send_request::<DocumentSymbolRequest>(&file.document_symbol_params(), |response| {
        assert!(
            !has_any_document_symbol(response),
            "Should not have any symbols"
        );
    });
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: file.identifier(),
    });
}
#[test]
fn test_partial_config_update() {
    // Set some value to something else
    let config: ServerSerializedConfig = serde_json::from_value(json!({
        "symbols": false
    }))
    .unwrap();
    let mut server = TestServer::new(config, Transport::Stdio).unwrap();

    // Partial update that should not reset symbols
    server.update_configuration(json!({
        "validate": true,
    }));

    let file = TestFile::new("glsl/include-level.comp.glsl", ShadingLanguage::Glsl);
    println!("Opening file {}", file.uri);

    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file.item(),
    });
    server.send_request::<DocumentSymbolRequest>(&file.document_symbol_params(), |response| {
        assert!(
            !has_any_document_symbol(response),
            "Should not have any symbols"
        );
    });
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: file.identifier(),
    });
}

#[test]
fn test_stage_define() {
    // Set some value to something else
    let config: ServerSerializedConfig = serde_json::from_value(json!({
        "stageDefine": {
            "fragment": {
                "VARIANT_DEFINE": "1"
            }
        }
    }))
    .unwrap();
    let mut server = TestServer::new(config, Transport::Stdio).unwrap();

    let file = TestFile::new("hlsl/variants.hlsl", ShadingLanguage::Hlsl);
    println!("Opening file {}", file.uri);

    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file.item(),
    });
    // Enforce stage with variant
    server.send_notification::<DidChangeShaderVariant>(&DidChangeShaderVariantParams {
        shader_variant: Some(ShaderVariant {
            uri: file.uri.clone(),
            shading_language: ShadingLanguage::Hlsl,
            entry_point: "mainOk".into(),
            stage: Some(ShaderStage::Fragment),
            defines: HashMap::new(),
            includes: Vec::new(),
        }),
    });
    server.send_request::<DocumentDiagnosticRequest>(
        &DocumentDiagnosticParams {
            text_document: file.identifier(),
            identifier: None,
            previous_result_id: None,
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        },
        |report| {
            let errors = get_error_diagnostics(report);
            assert!(
                errors.is_empty(),
                "Should not have any error, got {:#?}",
                errors
            );
        },
    );
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: file.identifier(),
    });
}

#[test]
fn test_config_override() {
    // Set some value to something else
    let config: ServerSerializedConfig = serde_json::from_value(json!({
        "configOverride": workspace_path("config-override.json"),
        "stageDefine": {
            "fragment": {
                "VARIANT_DEFINE": "0" // Ensure we override this with override config
            }
        }
    }))
    .unwrap();
    let mut server = TestServer::new(config, Transport::Stdio).unwrap();

    let file = TestFile::new("hlsl/variants.hlsl", ShadingLanguage::Hlsl);
    println!("Opening file {}", file.uri);

    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file.item(),
    });
    // Enforce stage with variant
    server.send_notification::<DidChangeShaderVariant>(&DidChangeShaderVariantParams {
        shader_variant: Some(ShaderVariant {
            uri: file.uri.clone(),
            shading_language: ShadingLanguage::Hlsl,
            entry_point: "mainOk".into(),
            stage: Some(ShaderStage::Fragment),
            defines: HashMap::new(),
            includes: Vec::new(),
        }),
    });
    server.send_request::<DocumentDiagnosticRequest>(
        &DocumentDiagnosticParams {
            text_document: file.identifier(),
            identifier: None,
            previous_result_id: None,
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        },
        |report| {
            let errors = get_error_diagnostics(report);
            assert!(
                errors.is_empty(),
                "Should not have any error, got {:#?}",
                errors
            );
        },
    );
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: file.identifier(),
    });
}

#[test]
fn test_disable_validation_clear_diagnostics() {
    let mut server = TestServer::new(ServerSerializedConfig::default(), Transport::Stdio).unwrap();

    // Store last diagnostics published for each file.
    let published: Rc<RefCell<HashMap<Url, usize>>> = Rc::new(RefCell::new(HashMap::new()));
    let published_handler = Rc::clone(&published);
    server.subscribe::<PublishDiagnostics, _>(move |params| {
        let params: PublishDiagnosticsParams = serde_json::from_value(params).unwrap();
        published_handler
            .borrow_mut()
            .insert(params.uri, params.diagnostics.len());
    });

    let file = TestFile::new("glsl/error-parsing.frag.glsl", ShadingLanguage::Glsl);
    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file.item(),
    });
    server.send_request::<DocumentSymbolRequest>(&file.document_symbol_params(), |_| {});
    assert!(
        published
            .borrow()
            .get(&file.uri)
            .is_some_and(|count| *count > 0),
        "Should have published errors for file, got {:#?}",
        published.borrow()
    );
    // Disabling validation should clear previously published diagnostics.
    server.update_configuration(json!({
        "validate": false,
    }));
    server.send_request::<DocumentSymbolRequest>(&file.document_symbol_params(), |_| {});
    assert!(
        published.borrow().values().all(|count| *count == 0),
        "Should have cleared all diagnostics, got {:#?}",
        published.borrow()
    );
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: file.identifier(),
    });
}
