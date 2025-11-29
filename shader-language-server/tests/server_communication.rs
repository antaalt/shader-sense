// Skip all these test on WASI.
// WASI cannot spawn a server so test on pc with WASMTIME runner instead.
#![cfg(not(target_os = "wasi"))]

use core::panic;
use std::collections::HashMap;
use std::iter::zip;
use std::path::Path;

use lsp_types::request::{
    DocumentDiagnosticRequest, HoverRequest, SemanticTokensFullRequest, WorkspaceSymbolRequest,
};
use lsp_types::{
    notification::{DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument},
    request::DocumentSymbolRequest,
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DocumentSymbolParams, DocumentSymbolResponse, PartialResultParams, Position, Range,
    TextDocumentContentChangeEvent, VersionedTextDocumentIdentifier, WorkDoneProgressParams,
};
use lsp_types::{
    DiagnosticSeverity, DocumentDiagnosticParams, DocumentDiagnosticReport,
    DocumentDiagnosticReportResult, Hover, HoverParams, RelatedFullDocumentDiagnosticReport,
    SemanticTokensParams, SemanticTokensResult, WorkspaceSymbolParams, WorkspaceSymbolResponse,
};
use shader_language_server::server::shader_variant::{
    DidChangeShaderVariant, DidChangeShaderVariantParams, ShaderVariant,
};
use shader_sense::position::ShaderPosition;
use shader_sense::shader::{ShaderStage, ShadingLanguage};
use test_server::{TestFile, TestServer};

mod test_server;

fn has_document_symbol(response: Option<DocumentSymbolResponse>, symbol: &str) -> bool {
    let symbols = response.unwrap();
    match symbols {
        DocumentSymbolResponse::Nested(document_symbol) => {
            document_symbol.iter().find(|e| e.name == symbol).is_some()
        }
        _ => panic!("Should not be reached."),
    }
}
fn has_workspace_symbol(response: Option<WorkspaceSymbolResponse>, symbol: &str) -> bool {
    let symbols = response.unwrap();
    match symbols {
        WorkspaceSymbolResponse::Flat(workspace_symbol) => {
            workspace_symbol.iter().find(|e| e.name == symbol).is_some()
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
fn get_workspace_symbol_params() -> WorkspaceSymbolParams {
    WorkspaceSymbolParams {
        query: "".into(),
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
            has_document_symbol(response.unwrap(), "mainError"),
            "Missing symbol mainError for variant"
        );
    });
    server.send_notification::<DidChangeShaderVariant>(&DidChangeShaderVariantParams {
        shader_variant: Some(ShaderVariant {
            url: file.url.clone(),
            shading_language: ShadingLanguage::Hlsl,
            entry_point: "".into(),
            stage: None,
            defines: HashMap::from([("VARIANT_DEFINE".into(), "1".into())]),
            includes: Vec::new(),
        }),
    });
    server.send_request::<DocumentSymbolRequest>(&document_symbol_params, |response| {
        assert!(
            has_document_symbol(response.unwrap(), "mainOk"),
            "Missing symbol mainOk for variant"
        );
    });
    server.send_notification::<DidChangeShaderVariant>(&DidChangeShaderVariantParams {
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
            let report = get_diagnostic_report(report.unwrap());
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
                "An error should trigger without the variant context. Got {:#?}",
                errors
            );
        },
    );
    server.send_notification::<DidChangeShaderVariant>(&DidChangeShaderVariantParams {
        shader_variant: Some(ShaderVariant {
            url: file_variant.url.clone(),
            shading_language: ShadingLanguage::Hlsl,
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
            let report = get_diagnostic_report(report.unwrap());
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
                "Macro should be imported through variant. Got {:#?}",
                errors,
            );
        },
    );
    server.send_notification::<DidChangeShaderVariant>(&DidChangeShaderVariantParams {
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

#[test]
fn test_dependency_include_guard() {
    // Test for variant dependency to have access to symbols protected by include guard
    let mut server = TestServer::desktop().unwrap();

    let variant = TestFile::new(
        Path::new("../shader-sense/test/hlsl/include-level.hlsl"),
        ShadingLanguage::Hlsl,
    );
    let deps = TestFile::new(
        Path::new("../shader-sense/test/hlsl/inc0/level0.hlsl"),
        ShadingLanguage::Hlsl,
    );
    let workspace_symbol_params = get_workspace_symbol_params();

    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: variant.item(),
    });
    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: deps.item(),
    });
    server.send_notification::<DidChangeShaderVariant>(&DidChangeShaderVariantParams {
        shader_variant: Some(ShaderVariant {
            url: variant.url.clone(),
            shading_language: ShadingLanguage::Hlsl,
            entry_point: "".into(),
            stage: Some(ShaderStage::Compute),
            defines: HashMap::new(),
            includes: Vec::new(),
        }),
    });
    server.send_request::<WorkspaceSymbolRequest>(&workspace_symbol_params, |response| {
        assert!(
            has_workspace_symbol(response.unwrap(), "methodLevel1"),
            "Missing symbol methodLevel1 for variant deps"
        );
    });
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: variant.identifier(),
    });
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: deps.identifier(),
    });
}

#[test]
fn test_hover() {
    let mut server = TestServer::desktop().unwrap();

    static FILE_PATH: &str = "../shader-sense/test/hlsl/struct.hlsl";

    fn assert_hover_value(response: Option<Hover>, value: &str) {
        let content = std::fs::read_to_string(&FILE_PATH).unwrap();
        let item_range = response.unwrap().range.unwrap();
        let start_byte_offset =
            ShaderPosition::new(item_range.start.line, item_range.start.character)
                .to_byte_offset(&content)
                .unwrap();
        let end_byte_offset = ShaderPosition::new(item_range.end.line, item_range.end.character)
            .to_byte_offset(&content)
            .unwrap();
        let hovered_item = &content[start_byte_offset..end_byte_offset];
        println!("Hovered item is {:?} at {:?}", hovered_item, item_range);
        assert!(
            hovered_item == value,
            "Hovered item {:?} is different from {:?}",
            hovered_item,
            value
        );
    }

    let file = TestFile::new(Path::new(FILE_PATH), ShadingLanguage::Hlsl);

    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file.item(),
    });
    server.send_request::<HoverRequest>(
        &HoverParams {
            text_document_position_params: file.position_params(23, 17),
            work_done_progress_params: WorkDoneProgressParams::default(),
        },
        |response| {
            assert_hover_value(response.unwrap(), "container");
        },
    );
    server.send_request::<HoverRequest>(
        &HoverParams {
            text_document_position_params: file.position_params(23, 27),
            work_done_progress_params: WorkDoneProgressParams::default(),
        },
        |response| {
            assert_hover_value(response.unwrap(), "method");
        },
    );
    server.send_request::<HoverRequest>(
        &HoverParams {
            text_document_position_params: file.position_params(23, 44),
            work_done_progress_params: WorkDoneProgressParams::default(),
        },
        |response| {
            assert_hover_value(response.unwrap(), "test2");
        },
    );
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: file.identifier(),
    });
}

#[test]
fn test_semantic_tokens() {
    let mut server = TestServer::desktop().unwrap();

    let file = TestFile::new(
        Path::new("../shader-sense/test/hlsl/semantic-token.hlsl"),
        ShadingLanguage::Hlsl,
    );

    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file.item(),
    });
    server.send_request::<SemanticTokensFullRequest>(
        &SemanticTokensParams {
            text_document: file.identifier(),
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        },
        |response| {
            let expected = [
                (lsp_types::Position::new(0, 8), "MY_MACRO"),
                (lsp_types::Position::new(2, 5), "MyEnum"),
                (lsp_types::Position::new(7, 26), "param0"),
                (lsp_types::Position::new(7, 39), "param1"),
                (lsp_types::Position::new(8, 23), "param0"),
                (lsp_types::Position::new(9, 17), "param1"),
                (lsp_types::Position::new(9, 26), "MyEnum"),
            ];
            let semantic_tokens = response.unwrap().unwrap();
            if let SemanticTokensResult::Tokens(tokens) = semantic_tokens {
                assert!(
                    tokens.data.len() == expected.len(),
                    "Expected {} inlay hint, got {}",
                    tokens.data.len(),
                    expected.len()
                );
                let mut line = 0;
                let mut pos = 0;
                for (semantic_token, (expected_position, expected_label)) in
                    zip(tokens.data.iter(), expected.iter())
                {
                    line = line + semantic_token.delta_line;
                    pos = if semantic_token.delta_line > 0 {
                        semantic_token.delta_start
                    } else {
                        pos + semantic_token.delta_start
                    };
                    assert!(
                        line == expected_position.line,
                        "Expected line {} for {}, got line {}",
                        line,
                        expected_label,
                        expected_position.line,
                    );
                    assert!(
                        pos == expected_position.character,
                        "Expected pos {} for {}, got pos {}",
                        pos,
                        expected_label,
                        expected_position.character,
                    );
                    assert!(semantic_token.length == expected_label.len() as u32);
                }
            } else {
                assert!(false);
            }
        },
    );
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: file.identifier(),
    });
}
