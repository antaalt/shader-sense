// Skip all these test on WASI.
// WASI cannot spawn a server so test on pc with WASMTIME runner instead.
#![cfg(not(target_os = "wasi"))]

use core::panic;
use std::collections::HashMap;
use std::iter::zip;
use std::net::{SocketAddr, SocketAddrV4};
use std::str::FromStr;

use base64::engine::general_purpose;
use base64::Engine;
use lsp_server::ErrorCode;
use lsp_types::notification::Cancel;
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
    CancelParams, Hover, HoverParams, SemanticTokensParams, SemanticTokensResult,
    TextDocumentIdentifier, TextDocumentItem, Url, WorkspaceSymbolParams, WorkspaceSymbolResponse,
};
use serde_json::json;
use shader_language_server::server::provider::compilation::{
    CompilationRequest, CompilationRequestResult,
};
use shader_language_server::server::provider::compilation::{
    CompilationRequestParams, CompilationType,
};
use shader_language_server::server::provider::dependecy::{
    DependencyTreeParams, DependencyTreeRequest,
};
use shader_language_server::server::server_config::ServerSerializedConfig;
use shader_language_server::server::shader_variant::{
    DidChangeShaderVariant, DidChangeShaderVariantParams, ShaderVariant,
};
use shader_language_server::server::Transport;
use shader_sense::position::ShaderPosition;
use shader_sense::shader::{ShaderStage, ShadingLanguage};
use test_server::{TestFile, TestServer};

use crate::test_server::{
    get_all_diagnostics, get_error_diagnostics, native_path, use_wasi_server, workspace_path,
};

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
fn assert_no_empty_document_symbol_name(response: Option<DocumentSymbolResponse>) {
    let symbols = response.unwrap();
    let empty_names: Vec<String> = match symbols {
        DocumentSymbolResponse::Nested(document_symbol) => document_symbol
            .iter()
            .filter(|symbol| symbol.name.trim().is_empty())
            .map(|symbol| symbol.detail.clone().unwrap_or_default())
            .collect(),
        DocumentSymbolResponse::Flat(workspace_symbol) => workspace_symbol
            .iter()
            .filter(|symbol| symbol.name.trim().is_empty())
            .map(|symbol| symbol.name.clone())
            .collect(),
    };
    assert!(
        empty_names.is_empty(),
        "Document symbols must not contain empty names. Offending details: {:#?}",
        empty_names
    );
}

#[test]
fn test_communication_stdio() {
    let mut server = TestServer::new(ServerSerializedConfig::default(), Transport::Stdio).unwrap();

    // Test document
    let file = TestFile::new("hlsl/ok.hlsl", ShadingLanguage::Hlsl);
    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file.item(),
    });
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: file.identifier(),
    });
}

#[test]
fn test_communication_tcp_connect() {
    if use_wasi_server() {
        return; // No TCP with WASI server
    }
    let mut server = TestServer::new(
        ServerSerializedConfig::default(),
        Transport::TcpConnect(SocketAddr::V4(
            SocketAddrV4::from_str("127.0.0.1:45365").unwrap(),
        )),
    )
    .unwrap();

    // Test document
    let file = TestFile::new("hlsl/ok.hlsl", ShadingLanguage::Hlsl);
    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file.item(),
    });
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: file.identifier(),
    });
}
#[test]
fn test_communication_tcp_listen() {
    if use_wasi_server() {
        return; // No TCP with WASI server
    }
    let mut server = TestServer::new(
        ServerSerializedConfig::default(),
        Transport::TcpListen(SocketAddr::V4(
            SocketAddrV4::from_str("127.0.0.1:45366").unwrap(),
        )),
    )
    .unwrap();

    // Test document
    let file = TestFile::new("hlsl/ok.hlsl", ShadingLanguage::Hlsl);
    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file.item(),
    });
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: file.identifier(),
    });
}

#[test]
fn test_variant() {
    let mut server = TestServer::new(ServerSerializedConfig::default(), Transport::Stdio).unwrap();

    // Test document
    let file = TestFile::new("hlsl/variants.hlsl", ShadingLanguage::Hlsl);
    println!("Opening file {}", file.uri);
    let document_symbol_params = get_document_symbol_params(&file);

    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file.item(),
    });
    server.send_request::<DocumentSymbolRequest>(&document_symbol_params, |response| {
        assert!(
            has_document_symbol(response, "mainError"),
            "Missing symbol mainError for variant"
        );
    });
    server.send_notification::<DidChangeShaderVariant>(&DidChangeShaderVariantParams {
        shader_variant: Some(ShaderVariant {
            uri: file.uri.clone(),
            shading_language: ShadingLanguage::Hlsl,
            entry_point: "".into(),
            stage: None,
            defines: HashMap::from([("VARIANT_DEFINE".into(), "1".into())]),
            includes: Vec::new(),
        }),
    });
    server.send_request::<DocumentSymbolRequest>(&document_symbol_params, |response| {
        assert!(
            has_document_symbol(response, "mainOk"),
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
fn test_glsl_precision_statement_document_symbols_have_names() {
    let mut server = TestServer::new(ServerSerializedConfig::default(), Transport::Stdio).unwrap();

    let file = TestFile::new("glsl/precision-only.glsl", ShadingLanguage::Glsl);
    let document_symbol_params = get_document_symbol_params(&file);

    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file.item(),
    });
    server.send_request::<DocumentSymbolRequest>(&document_symbol_params, |response| {
        assert_no_empty_document_symbol_name(response);
    });
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: file.identifier(),
    });
}

#[test]
fn test_variant_dependency() {
    let mut server = TestServer::new(ServerSerializedConfig::default(), Transport::Stdio).unwrap();

    // Test document
    let file_variant = TestFile::new("hlsl/variants.hlsl", ShadingLanguage::Hlsl);
    let file_macros = TestFile::new("hlsl/macro.hlsl", ShadingLanguage::Hlsl);
    println!("Opening file {}", file_variant.uri);
    println!("Opening file {}", file_macros.uri);

    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file_variant.item(),
    });
    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file_macros.item(),
    });
    server.send_request::<DocumentDiagnosticRequest>(
        &file_macros.document_diagnostic_params(),
        |report| {
            let errors = get_error_diagnostics(report);
            assert!(
                errors.len() > 0,
                "An error should trigger without the variant context. Got {:#?}",
                errors
            );
        },
    );
    server.send_notification::<DidChangeShaderVariant>(&DidChangeShaderVariantParams {
        shader_variant: Some(ShaderVariant {
            uri: file_variant.uri.clone(),
            shading_language: ShadingLanguage::Hlsl,
            entry_point: "".into(),
            stage: None,
            defines: HashMap::new(),
            includes: Vec::new(),
        }),
    });
    server.send_request::<DocumentDiagnosticRequest>(
        &file_macros.document_diagnostic_params(),
        |report| {
            let errors = get_error_diagnostics(report);
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
    let mut server = TestServer::new(ServerSerializedConfig::default(), Transport::Stdio).unwrap();

    let file = TestFile::new("hlsl/utf8.hlsl", ShadingLanguage::Hlsl);

    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file.item(),
    });
    let utf8_content_inserted = "こんにちは世界!";
    server.send_notification::<DidChangeTextDocument>(&DidChangeTextDocumentParams {
        text_document: VersionedTextDocumentIdentifier {
            uri: file.uri.clone(),
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
    let mut server = TestServer::new(ServerSerializedConfig::default(), Transport::Stdio).unwrap();

    let file = TestFile::new("glsl/include-level.comp.glsl", ShadingLanguage::Glsl);
    let deps0 = TestFile::new("glsl/inc0/level0.glsl", ShadingLanguage::Glsl);
    let deps1 = TestFile::new("glsl/inc0/inc1/level1.glsl", ShadingLanguage::Glsl);

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
    let mut server = TestServer::new(ServerSerializedConfig::default(), Transport::Stdio).unwrap();

    let file = TestFile::new("hlsl/stack-overflow.hlsl", ShadingLanguage::Hlsl);

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
    let mut server = TestServer::new(ServerSerializedConfig::default(), Transport::Stdio).unwrap();

    let variant = TestFile::new("hlsl/include-level.hlsl", ShadingLanguage::Hlsl);
    let deps = TestFile::new("hlsl/inc0/level0.hlsl", ShadingLanguage::Hlsl);
    let workspace_symbol_params = get_workspace_symbol_params();

    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: variant.item(),
    });
    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: deps.item(),
    });
    server.send_notification::<DidChangeShaderVariant>(&DidChangeShaderVariantParams {
        shader_variant: Some(ShaderVariant {
            uri: variant.uri.clone(),
            shading_language: ShadingLanguage::Hlsl,
            entry_point: "".into(),
            stage: Some(ShaderStage::Compute),
            defines: HashMap::new(),
            includes: Vec::new(),
        }),
    });
    server.send_request::<WorkspaceSymbolRequest>(&workspace_symbol_params, |response| {
        assert!(
            has_workspace_symbol(response, "methodLevel1"),
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
    let mut server = TestServer::new(ServerSerializedConfig::default(), Transport::Stdio).unwrap();

    static FILE_PATH: &str = "hlsl/struct.hlsl";

    fn assert_hover_value(response: Option<Hover>, value: &str) {
        let content = std::fs::read_to_string(native_path(&FILE_PATH)).unwrap();
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

    let file = TestFile::new(FILE_PATH, ShadingLanguage::Hlsl);

    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file.item(),
    });
    server.send_request::<HoverRequest>(
        &HoverParams {
            text_document_position_params: file.position_params(23, 17),
            work_done_progress_params: WorkDoneProgressParams::default(),
        },
        |response| {
            assert_hover_value(response, "container");
        },
    );
    server.send_request::<HoverRequest>(
        &HoverParams {
            text_document_position_params: file.position_params(23, 27),
            work_done_progress_params: WorkDoneProgressParams::default(),
        },
        |response| {
            assert_hover_value(response, "method");
        },
    );
    server.send_request::<HoverRequest>(
        &HoverParams {
            text_document_position_params: file.position_params(23, 44),
            work_done_progress_params: WorkDoneProgressParams::default(),
        },
        |response| {
            assert_hover_value(response, "test2");
        },
    );
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: file.identifier(),
    });
}

#[test]
fn test_semantic_tokens() {
    let mut server = TestServer::new(ServerSerializedConfig::default(), Transport::Stdio).unwrap();

    let file = TestFile::new("hlsl/semantic-token.hlsl", ShadingLanguage::Hlsl);

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
            let semantic_tokens = response.unwrap();
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

fn is_valid_dxil(dxil_bytes: &Vec<u8>) -> bool {
    const DXIL_MAGIC_NUMBER: u32 = 0x43425844;
    const DXIL_MAGIC_LE: [u8; 4] = DXIL_MAGIC_NUMBER.to_le_bytes();
    const DXIL_MAGIC_BE: [u8; 4] = DXIL_MAGIC_NUMBER.to_be_bytes();
    let magic_bytes: [u8; 4] = dxil_bytes[0..4].try_into().unwrap();
    DXIL_MAGIC_BE == magic_bytes || DXIL_MAGIC_LE == magic_bytes
}

fn is_valid_spirv(spirv_bytes: &Vec<u8>) -> bool {
    const SPIRV_MAGIC_NUMBER: u32 = 0x07230203;
    const SPIRV_MAGIC_LE: [u8; 4] = SPIRV_MAGIC_NUMBER.to_le_bytes();
    const SPIRV_MAGIC_BE: [u8; 4] = SPIRV_MAGIC_NUMBER.to_be_bytes();
    let magic_bytes: [u8; 4] = spirv_bytes[0..4].try_into().unwrap();
    SPIRV_MAGIC_BE == magic_bytes || SPIRV_MAGIC_LE == magic_bytes
}

fn validate_compilation_result(
    result: Option<CompilationRequestResult>,
    disassemble: bool,
    compilation_type: CompilationType,
    expected_len: usize,
) {
    let compilation = result.unwrap();
    assert!(
        compilation.compilation_type == compilation_type,
        "Invalid compilation type: {:?}",
        compilation.compilation_type
    );
    // Validate SPIRV
    if disassemble {
        assert!(
            match compilation_type {
                CompilationType::Spirv => compilation.data.starts_with("; SPIR-V"),
                CompilationType::Dxil => compilation.data.starts_with(";\n; Input signature:"),
                CompilationType::Wgsl => true, // Simple wgsl returned.
            },
            "Invalid disassembly start: {:?}",
            compilation.data
        );
        assert!(
            compilation.data.len() == expected_len,
            "Invalid disassembly length: {}",
            compilation.data.len()
        );
    } else {
        let bytes = match compilation_type {
            CompilationType::Wgsl => compilation.data.into_bytes(),
            _ => general_purpose::STANDARD.decode(&compilation.data).unwrap(),
        };
        assert!(
            match compilation_type {
                CompilationType::Spirv => is_valid_spirv(&bytes),
                CompilationType::Dxil => is_valid_dxil(&bytes),
                CompilationType::Wgsl => true, // No magic byte in wgsl
            },
            "Invalid magic bytes: {:?}",
            bytes
        );
        assert!(
            bytes.len() == expected_len,
            "Invalid compilation length: {}",
            bytes.len()
        );
    }
}

#[test]
fn test_compilation_glsl_spirv() {
    let mut server = TestServer::new(ServerSerializedConfig::default(), Transport::Stdio).unwrap();

    let file = TestFile::new("glsl/ok.frag.glsl", ShadingLanguage::Glsl);
    server.send_notification::<DidChangeShaderVariant>(&DidChangeShaderVariantParams {
        shader_variant: Some(ShaderVariant {
            uri: file.uri.clone(),
            shading_language: ShadingLanguage::Glsl,
            entry_point: "main".into(),
            stage: Some(ShaderStage::Fragment),
            defines: HashMap::new(),
            includes: Vec::new(),
        }),
    });
    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file.item(),
    });
    server.send_request::<DocumentDiagnosticRequest>(
        &file.document_diagnostic_params(),
        |report| {
            let report = get_all_diagnostics(report);
            assert!(
                report.is_empty(),
                "Should not have any error with file, got {:#?}",
                report
            );
        },
    );
    server.send_request::<CompilationRequest>(
        &CompilationRequestParams {
            text_document: file.identifier(),
            disassemble: None,
            compilation_type: None,
        },
        |result| validate_compilation_result(result, false, CompilationType::Spirv, 360),
    );
    server.send_request::<CompilationRequest>(
        &CompilationRequestParams {
            text_document: file.identifier(),
            disassemble: Some(true),
            compilation_type: None,
        },
        |result| validate_compilation_result(result, true, CompilationType::Spirv, 627),
    );
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: file.identifier(),
    });
}

#[test]
fn test_compilation_hlsl() {
    if use_wasi_server() {
        return; // No DXC with WASI server
    }
    let mut server = TestServer::new(ServerSerializedConfig::default(), Transport::Stdio).unwrap();

    let file = TestFile::new("hlsl/ok.hlsl", ShadingLanguage::Hlsl);
    server.send_notification::<DidChangeShaderVariant>(&DidChangeShaderVariantParams {
        shader_variant: Some(ShaderVariant {
            uri: file.uri.clone(),
            shading_language: ShadingLanguage::Hlsl,
            entry_point: "fs_main".into(),
            stage: Some(ShaderStage::Fragment),
            defines: HashMap::new(),
            includes: Vec::new(),
        }),
    });
    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file.item(),
    });
    server.send_request::<DocumentDiagnosticRequest>(
        &file.document_diagnostic_params(),
        |report| {
            let report = get_all_diagnostics(report);
            assert!(
                report.is_empty(),
                "Should not have any error with file, got {:#?}",
                report
            );
        },
    );
    server.send_request::<CompilationRequest>(
        &CompilationRequestParams {
            text_document: file.identifier(),
            disassemble: None,
            compilation_type: None,
        },
        |result| validate_compilation_result(result, false, CompilationType::Dxil, 2672),
    );
    server.send_request::<CompilationRequest>(
        &CompilationRequestParams {
            text_document: file.identifier(),
            disassemble: Some(true),
            compilation_type: None,
        },
        |result| validate_compilation_result(result, true, CompilationType::Dxil, 2797),
    );
    server.update_configuration(json!({
        "hlsl": {
            "spirv": true
        }
    }));
    server.send_request::<CompilationRequest>(
        &CompilationRequestParams {
            text_document: file.identifier(),
            disassemble: None,
            compilation_type: None,
        },
        |result| validate_compilation_result(result, false, CompilationType::Spirv, 336),
    );
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: file.identifier(),
    });
}

#[test]
fn test_compilation_wgsl() {
    let mut server = TestServer::new(ServerSerializedConfig::default(), Transport::Stdio).unwrap();

    let file = TestFile::new("wesl/lighting.wesl", ShadingLanguage::Wgsl);
    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file.item(),
    });
    server.send_request::<DocumentDiagnosticRequest>(
        &file.document_diagnostic_params(),
        |report| {
            let report = get_all_diagnostics(report);
            assert!(
                report.is_empty(),
                "Should not have any error with file, got {:#?}",
                report
            );
        },
    );
    server.send_request::<CompilationRequest>(
        &CompilationRequestParams {
            text_document: file.identifier(),
            disassemble: None,
            compilation_type: None,
        },
        |result| validate_compilation_result(result, false, CompilationType::Wgsl, 241),
    );
    server.send_request::<CompilationRequest>(
        &CompilationRequestParams {
            text_document: file.identifier(),
            disassemble: Some(true),
            compilation_type: None,
        },
        |result| validate_compilation_result(result, true, CompilationType::Wgsl, 241),
    );
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: file.identifier(),
    });
}

#[test]
fn test_invalid_method() {
    let mut server = TestServer::new(ServerSerializedConfig::default(), Transport::Stdio).unwrap();

    let file = TestFile::new("glsl/ok.frag.glsl", ShadingLanguage::Glsl);
    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file.item(),
    });
    struct UnhandledMethod;
    impl lsp_types::request::Request for UnhandledMethod {
        type Params = ();
        type Result = ();
        const METHOD: &'static str = "unhandledMethod";
    }
    server.send_request_with_error::<UnhandledMethod>(
        &(),
        |_result| {}, // Ok
        |error| assert!(error.code == ErrorCode::MethodNotFound as i32),
    );
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: file.identifier(),
    });
}

#[test]
fn test_not_watched() {
    let mut server = TestServer::new(ServerSerializedConfig::default(), Transport::Stdio).unwrap();

    let file = TestFile::new("glsl/ok.frag.glsl", ShadingLanguage::Glsl);
    server.send_request_with_error::<DocumentDiagnosticRequest>(
        &file.document_diagnostic_params(),
        |_report| {
            assert!(false, "Should have file not watched error");
        },
        |error| assert_eq!(error.code, lsp_server::ErrorCode::InternalError as i32),
    );
    // DidClose when no DidOpen should not crash.
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: file.identifier(),
    });
}

#[test]
fn test_cancel_request() {
    let mut server = TestServer::new(ServerSerializedConfig::default(), Transport::Stdio).unwrap();

    let file = TestFile::new("glsl/ok.frag.glsl", ShadingLanguage::Glsl);
    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file.item(),
    });
    server.send_request_only::<DocumentDiagnosticRequest>(&file.document_diagnostic_params());
    // Cancel request before checking for its response.
    server.send_notification::<Cancel>(&CancelParams {
        id: lsp_types::NumberOrString::Number(1),
    });
    server.expect_response::<DocumentDiagnosticRequest>(
        |_report| {
            assert!(false, "Should be canceled");
        },
        |error| assert_eq!(error.code, lsp_server::ErrorCode::RequestCanceled as i32),
    );
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: file.identifier(),
    });
}

#[test]
fn test_disabling_variant() {
    // Updating variant is done synchronously, so check it does not break async updates
    let mut server = TestServer::new(ServerSerializedConfig::default(), Transport::Stdio).unwrap();

    let file = TestFile::new("glsl/ok.frag.glsl", ShadingLanguage::Glsl);
    server.send_notification::<DidChangeShaderVariant>(&DidChangeShaderVariantParams {
        shader_variant: Some(ShaderVariant {
            uri: file.uri.clone(),
            shading_language: ShadingLanguage::Glsl,
            entry_point: "main".into(),
            stage: Some(ShaderStage::Fragment),
            defines: HashMap::new(),
            includes: Vec::new(),
        }),
    });
    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file.item(),
    });
    server.send_notification::<DidChangeShaderVariant>(&DidChangeShaderVariantParams {
        shader_variant: None,
    });
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: file.identifier(),
    });
}

#[test]
fn test_dependency_tree() {
    // Updating variant is done synchronously, so check it does not break async updates
    let mut server = TestServer::new(ServerSerializedConfig::default(), Transport::Stdio).unwrap();

    let file = TestFile::new("glsl/include-level.comp.glsl", ShadingLanguage::Glsl);
    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file.item(),
    });
    server.send_request::<DependencyTreeRequest>(
        &DependencyTreeParams {
            text_document: file.identifier(),
        },
        |dependency_tree| {
            assert_eq!(
                dependency_tree.uri.to_file_path().unwrap(),
                workspace_path("glsl/include-level.comp.glsl")
            );
            assert!(dependency_tree.includes.len() == 1);
            let dependency_tree = &dependency_tree.includes[0];
            assert_eq!(
                dependency_tree.uri.to_file_path().unwrap(),
                workspace_path("glsl/inc0/level0.glsl")
            );
            assert!(dependency_tree.includes.len() == 1);
            let dependency_tree = &dependency_tree.includes[0];
            assert_eq!(
                dependency_tree.uri.to_file_path().unwrap(),
                workspace_path("glsl/inc0/inc1/level1.glsl")
            );
            assert!(dependency_tree.includes.len() == 0);
        },
    );
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: file.identifier(),
    });
}

#[test]
fn test_untitled_uri() {
    let mut server = TestServer::new(ServerSerializedConfig::default(), Transport::Stdio).unwrap();

    // Test non uri file scheme. They should be ignored, but not crash the server.
    let file_uri = Url::parse("untitled://Untitled").unwrap();
    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: file_uri.clone(),
            language_id: "glsl".into(),
            version: 0,
            text: "#version 450\nvoid main(){}".into(),
        },
    });
    // Server should fail opening, so close will do nothing, but should not crash
    server.send_notification::<DidCloseTextDocument>(&DidCloseTextDocumentParams {
        text_document: TextDocumentIdentifier { uri: file_uri },
    });
}
