// Skip all these test on WASI.
// WASI cannot spawn a server so test on pc with WASMTIME runner instead.
#![cfg(not(target_os = "wasi"))]

use std::path::Path;

use lsp_types::{
    notification::{DidCloseTextDocument, DidOpenTextDocument},
    request::{DocumentDiagnosticRequest, DocumentSymbolRequest},
    DidCloseTextDocumentParams, DidOpenTextDocumentParams, DocumentDiagnosticParams,
    DocumentDiagnosticReport, DocumentDiagnosticReportResult, DocumentSymbolParams,
    DocumentSymbolResponse, PartialResultParams, RelatedFullDocumentDiagnosticReport,
    WorkDoneProgressParams,
};
use serde_json::json;
use shader_language_server::server::server_config::ServerSerializedConfig;
use shader_sense::shader::ShadingLanguage;

use crate::test_server::{TestFile, TestServer};

mod test_server;

fn get_diagnostic_report(
    result: DocumentDiagnosticReportResult,
) -> RelatedFullDocumentDiagnosticReport {
    if let DocumentDiagnosticReportResult::Report(report) = result {
        if let DocumentDiagnosticReport::Full(report) = report {
            report
        } else {
            unreachable!("Should not be reached");
        }
    } else {
        unreachable!("Should not be reached");
    }
}
fn has_any_document_symbol(response: Option<DocumentSymbolResponse>) -> bool {
    let symbols = response.unwrap();
    match symbols {
        DocumentSymbolResponse::Nested(document_symbol) => !document_symbol.is_empty(),
        DocumentSymbolResponse::Flat(document_symbol) => !document_symbol.is_empty(),
    }
}
fn get_document_symbol_params(file: &TestFile) -> DocumentSymbolParams {
    DocumentSymbolParams {
        text_document: file.identifier(),
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
    }
}

#[test]
fn test_glsl_relative_preamble() {
    let config: ServerSerializedConfig = serde_json::from_value(json!({
        "glsl": {
            "preamble": "../shader-sense/test/glsl/helpers/preamble.glsl"
        }
    }))
    .unwrap();
    let mut server = TestServer::desktop(config).unwrap();

    let file = TestFile::new(
        Path::new("../shader-sense/test/glsl/dependent-include.frag.glsl"),
        ShadingLanguage::Glsl,
    );
    println!("Opening file {}", file.url);

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
            let report = get_diagnostic_report(report.unwrap());
            assert!(
                report.full_document_diagnostic_report.items.is_empty(),
                "Should not have any error with preamble file, got {:#?}",
                report.full_document_diagnostic_report.items
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
    let mut server = TestServer::desktop(config).unwrap();

    let file = TestFile::new(
        Path::new("../shader-sense/test/glsl/error-parsing.frag.glsl"),
        ShadingLanguage::Glsl,
    );
    println!("Opening file {}", file.url);

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
            let report = get_diagnostic_report(report.unwrap());
            assert!(
                report.full_document_diagnostic_report.items.is_empty(),
                "Should not have any error as validate is diabled, got {:#?}",
                report.full_document_diagnostic_report.items
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
    let mut server = TestServer::desktop(config).unwrap();

    let file = TestFile::new(
        Path::new("../shader-sense/test/glsl/include-level.comp.glsl"),
        ShadingLanguage::Glsl,
    );
    println!("Opening file {}", file.url);

    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file.item(),
    });
    let document_symbol_params = get_document_symbol_params(&file);
    server.send_request::<DocumentSymbolRequest>(&document_symbol_params, |response| {
        assert!(
            !has_any_document_symbol(response.unwrap()),
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
    let mut server = TestServer::desktop(config).unwrap();

    // Partial update that should not reset symbols
    server.update_configuration(json!({
        "validate": true,
    }));

    let file = TestFile::new(
        Path::new("../shader-sense/test/glsl/include-level.comp.glsl"),
        ShadingLanguage::Glsl,
    );
    println!("Opening file {}", file.url);

    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file.item(),
    });
    let document_symbol_params = get_document_symbol_params(&file);
    server.send_request::<DocumentSymbolRequest>(&document_symbol_params, |response| {
        assert!(
            !has_any_document_symbol(response.unwrap()),
            "Should not have any symbols"
        );
    });
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: file.identifier(),
    });
}
