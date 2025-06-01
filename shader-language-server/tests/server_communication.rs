// Skip all these test on WASI.
// WASI cannot spawn a server so test on pc with WASMTIME runner instead.
#![cfg(not(target_os = "wasi"))]

use core::panic;
use std::collections::HashMap;
use std::path::Path;

use lsp_types::request::DocumentDiagnosticRequest;
use lsp_types::{
    notification::{DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument},
    request::DocumentSymbolRequest,
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DocumentSymbolParams, DocumentSymbolResponse, PartialResultParams, Position, Range,
    TextDocumentContentChangeEvent, VersionedTextDocumentIdentifier, WorkDoneProgressParams,
};
use lsp_types::{
    DiagnosticSeverity, DocumentDiagnosticParams, DocumentDiagnosticReport,
    DocumentDiagnosticReportResult, RelatedFullDocumentDiagnosticReport,
};
use shader_language_server::server::shader_variant::{
    DidChangeShaderVariant, DidChangeShaderVariantParams, ShaderVariant,
};
use shader_sense::shader::ShadingLanguage;
use test_server::{TestFile, TestServer};

mod test_server;

fn has_symbol(response: Option<DocumentSymbolResponse>, symbol: &str) -> bool {
    let symbols = response.unwrap();
    fn has_document_symbol(document_symbol: &Vec<lsp_types::DocumentSymbol>, symbol: &str) -> bool {
        document_symbol
            .iter()
            .find(|e| {
                e.name == symbol
                    || match &e.children {
                        Some(children) => has_document_symbol(children, symbol),
                        None => false,
                    }
            })
            .is_some()
    }
    match symbols {
        DocumentSymbolResponse::Nested(document_symbol) => {
            has_document_symbol(&document_symbol, symbol)
        }
        _ => panic!("Should not be reached."),
    }
}
fn get_document_symbol_params(file: &TestFile) -> DocumentSymbolParams {
    DocumentSymbolParams {
        text_document: file.identifier(),
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
    }
}
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

#[test]
fn test_server_wasi_runtime() {
    use test_server::TestServer;

    match TestServer::wasi() {
        Some(_) => {}
        None => {
            // Should ignore test instead to be clear.
            // https://github.com/rust-lang/rust/issues/68007
            println!("WASI executable not built. Skipping.");
        }
    };
}

#[test]
fn test_variant() {
    let mut server = TestServer::desktop().unwrap();

    // Test document
    let file = TestFile::new(
        Path::new("../shader-sense/test/hlsl/variants.hlsl"),
        ShadingLanguage::Hlsl,
    );
    println!("Opening file {}", file.url);
    let document_symbol_params = get_document_symbol_params(&file);

    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file.item(),
    });
    server.send_request::<DocumentSymbolRequest>(&document_symbol_params, |response| {
        assert!(
            has_symbol(response, "mainError"),
            "Missing symbol mainError for variant"
        );
    });
    server.send_notification::<DidChangeShaderVariant>(&DidChangeShaderVariantParams {
        text_document: file.identifier(),
        shader_variant: Some(ShaderVariant {
            entry_point: "".into(),
            stage: None,
            defines: HashMap::from([("VARIANT_DEFINE".into(), "1".into())]),
            includes: Vec::new(),
        }),
    });
    server.send_request::<DocumentSymbolRequest>(&document_symbol_params, |response| {
        assert!(
            has_symbol(response, "mainOk"),
            "Missing symbol mainOk for variant"
        );
    });
    server.send_notification::<DidChangeShaderVariant>(&DidChangeShaderVariantParams {
        text_document: file.identifier(),
        shader_variant: None, // Clear for next tests
    });
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: file.identifier(),
    });
}

#[test]
fn test_variant_dependency() {
    let mut server = TestServer::desktop().unwrap();

    // Test document
    let file_variant = TestFile::new(
        Path::new("../shader-sense/test/hlsl/variants.hlsl"),
        ShadingLanguage::Hlsl,
    );
    let file_macros = TestFile::new(
        Path::new("../shader-sense/test/hlsl/macro.hlsl"),
        ShadingLanguage::Hlsl,
    );
    println!("Opening file {}", file_variant.url);
    println!("Opening file {}", file_macros.url);

    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file_variant.item(),
    });
    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file_macros.item(),
    });
    server.send_request::<DocumentDiagnosticRequest>(
        &DocumentDiagnosticParams {
            text_document: file_macros.identifier(),
            identifier: None,
            previous_result_id: None,
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        },
        |report| {
            let report = get_diagnostic_report(report);
            let errors: Vec<&lsp_types::Diagnostic> = report
                .full_document_diagnostic_report
                .items
                .iter()
                .filter(|d| match &d.severity {
                    Some(severity) => *severity == DiagnosticSeverity::ERROR,
                    None => false,
                })
                .collect();
            assert!(
                errors.len() == 1,
                "An error should trigger without the variant context. Got {:?}",
                errors
            );
        },
    );
    server.send_notification::<DidChangeShaderVariant>(&DidChangeShaderVariantParams {
        text_document: file_variant.identifier(),
        shader_variant: Some(ShaderVariant {
            entry_point: "".into(),
            stage: None,
            defines: HashMap::new(),
            includes: Vec::new(),
        }),
    });
    server.send_request::<DocumentDiagnosticRequest>(
        &DocumentDiagnosticParams {
            text_document: file_macros.identifier(),
            identifier: None,
            previous_result_id: None,
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        },
        |report| {
            let report = get_diagnostic_report(report);
            let errors: Vec<&lsp_types::Diagnostic> = report
                .full_document_diagnostic_report
                .items
                .iter()
                .filter(|d| match &d.severity {
                    Some(severity) => *severity == DiagnosticSeverity::ERROR,
                    None => false,
                })
                .collect();
            assert!(
                errors.is_empty(),
                "Macro should be imported through variant. Got {:?}",
                errors,
            );
        },
    );
    server.send_notification::<DidChangeShaderVariant>(&DidChangeShaderVariantParams {
        text_document: file_variant.identifier(),
        shader_variant: None, // Clear for next tests
    });
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: file_macros.identifier(),
    });
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: file_variant.identifier(),
    });
}
#[test]
fn test_utf8_edit() {
    let mut server = TestServer::desktop().unwrap();

    let file = TestFile::new(
        Path::new("../shader-sense/test/hlsl/utf8.hlsl"),
        ShadingLanguage::Hlsl,
    );

    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file.item(),
    });
    let utf8_content_inserted = "こんにちは世界!";
    server.send_notification::<DidChangeTextDocument>(&DidChangeTextDocumentParams {
        text_document: VersionedTextDocumentIdentifier {
            uri: file.url.clone(),
            version: 0,
        },
        content_changes: vec![TextDocumentContentChangeEvent {
            range: Some(Range {
                start: Position {
                    line: 0,
                    character: 3,
                },
                end: Position {
                    line: 0,
                    character: 3,
                },
            }),
            range_length: Some(0),
            text: utf8_content_inserted.into(),
        }],
    });
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: file.identifier(),
    });
}

#[test]
fn test_dependencies() {
    let mut server = TestServer::desktop().unwrap();

    let file = TestFile::new(
        Path::new("../shader-sense/test/glsl/include-level.comp.glsl"),
        ShadingLanguage::Glsl,
    );
    let deps0 = TestFile::new(
        Path::new("../shader-sense/test/glsl/inc0/level0.glsl"),
        ShadingLanguage::Glsl,
    );
    let deps1 = TestFile::new(
        Path::new("../shader-sense/test/glsl/inc0/inc1/level1.glsl"),
        ShadingLanguage::Glsl,
    );

    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file.item(),
    });
    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: deps0.item(),
    });
    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: deps1.item(),
    });
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: deps1.identifier(),
    });
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: file.identifier(),
    });
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: deps0.identifier(),
    });
}

#[test]
fn test_server_stack_overflow() {
    let mut server = TestServer::desktop().unwrap();

    let file = TestFile::new(
        Path::new("../shader-sense/test/hlsl/stack-overflow.hlsl"),
        ShadingLanguage::Hlsl,
    );

    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file.item(),
    });
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: file.identifier(),
    });
}
