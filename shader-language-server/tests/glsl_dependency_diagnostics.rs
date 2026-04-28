// Skip all these test on WASI.
// WASI cannot spawn a server so test on pc with WASMTIME runner instead.
#![cfg(not(target_os = "wasi"))]

use std::path::Path;

use lsp_types::request::DocumentDiagnosticRequest;
use lsp_types::{
    notification::{DidCloseTextDocument, DidOpenTextDocument},
    DiagnosticSeverity, DocumentDiagnosticParams, DocumentDiagnosticReport,
    DocumentDiagnosticReportResult, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    PartialResultParams, RelatedFullDocumentDiagnosticReport, WorkDoneProgressParams,
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

#[test]
fn test_glsl_dependency_document_diagnostics_use_includer_context() {
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