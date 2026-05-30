// Skip all these test on WASI.
// WASI cannot spawn a server so test on pc with WASMTIME runner instead.
#![cfg(not(target_os = "wasi"))]

use std::{collections::HashMap, path::Path};

use lsp_types::request::DocumentDiagnosticRequest;
use lsp_types::{
    notification::{DidChangeConfiguration, DidCloseTextDocument, DidOpenTextDocument},
    DiagnosticSeverity, DidChangeConfigurationParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DocumentDiagnosticParams, DocumentDiagnosticReport,
    DocumentDiagnosticReportResult, PartialResultParams, RelatedFullDocumentDiagnosticReport,
    WorkDoneProgressParams,
};
use serde_json::{json, Value};
use shader_language_server::server::shader_variant::{
    DidChangeShaderVariant, DidChangeShaderVariantParams, ShaderVariant,
};
use shader_sense::shader::ShadingLanguage;
use test_server::{TestFile, TestServer};

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

fn enable_automatic_variant_discovery(server: &mut TestServer) {
    server.set_workspace_configuration_response(vec![json!({
        "automaticVariantDiscovery": true,
    })]);
    server.send_notification::<DidChangeConfiguration>(&DidChangeConfigurationParams {
        settings: Value::Null,
    });
}

#[test]
fn test_automatic_variant_discovery_use_includer_context() {
    let mut server = TestServer::desktop().unwrap();

    let file = TestFile::new(
        Path::new("../shader-sense/test/glsl/include-context.comp.glsl"),
        ShadingLanguage::Glsl,
    );
    let deps = TestFile::new(
        Path::new("../shader-sense/test/glsl/workgroup-layout.glsl"),
        ShadingLanguage::Glsl,
    );

    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: file.item(),
    });
    server.send_notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
        text_document: deps.item(),
    });
    server.send_request::<DocumentDiagnosticRequest>(
        &DocumentDiagnosticParams {
            text_document: deps.identifier(),
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
                !errors.is_empty(),
                "Dependency-context diagnostics should stay disabled by default. Got {:#?}",
                errors,
            );
        },
    );
    enable_automatic_variant_discovery(&mut server);
    server.send_request::<DocumentDiagnosticRequest>(
        &DocumentDiagnosticParams {
            text_document: deps.identifier(),
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
    let mut server = TestServer::desktop().unwrap();
    enable_automatic_variant_discovery(&mut server);

    let invalid_main = TestFile::new(
        Path::new("../shader-sense/test/glsl/a-include-context.frag.glsl"),
        ShadingLanguage::Glsl,
    );
    let selected_variant = TestFile::new(
        Path::new("../shader-sense/test/glsl/include-context.comp.glsl"),
        ShadingLanguage::Glsl,
    );
    let deps = TestFile::new(
        Path::new("../shader-sense/test/glsl/workgroup-layout.glsl"),
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
        &DocumentDiagnosticParams {
            text_document: deps.identifier(),
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
