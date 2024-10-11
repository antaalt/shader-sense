use std::collections::HashMap;

use log::info;
use lsp_types::{Diagnostic, PublishDiagnosticsParams, Url};

use crate::{
    server::ServerLanguage,
    shaders::{
        shader_error::{ShaderErrorSeverity, ValidatorError},
    },
};

use super::ServerFileCache;

impl ServerLanguage {
    pub fn recolt_diagnostic(
        &mut self,
        uri: &Url,
        cached_file: &ServerFileCache,
    ) -> Result<HashMap<Url, Vec<Diagnostic>>, ValidatorError> {
        // Skip non file uri.
        match uri.scheme() {
            "file" => {}
            _ => {
                return Err(ValidatorError::InternalErr(String::from(
                    "Cannot treat files without file scheme",
                )));
            }
        }
        let file_path = self.to_file_path(&uri);
        let validation_params = self.config.into_validation_params();
        let validator = self.get_validator(cached_file.shading_language);
        match validator.validate_shader(cached_file.content.clone(), file_path.as_path(), validation_params) {
            Ok((diagnostic_list, dependencies)) => {
                let mut diagnostics: HashMap<Url, Vec<Diagnostic>> = HashMap::new();
                for diagnostic in diagnostic_list.diagnostics {
                    let uri = match diagnostic.file_path {
                        Some(diagnostic_file_path) => Url::from_file_path(&diagnostic_file_path)
                            .expect(
                                format!(
                                    "Failed to convert path {} to uri",
                                    diagnostic_file_path.display()
                                )
                                .as_str(),
                            ),
                        None => uri.clone(),
                    };
                    if diagnostic
                        .severity
                        .is_required(ShaderErrorSeverity::from(self.config.severity.clone()))
                    {
                        let diagnostic = Diagnostic {
                            range: lsp_types::Range::new(
                                lsp_types::Position::new(diagnostic.line - 1, diagnostic.pos),
                                lsp_types::Position::new(diagnostic.line - 1, diagnostic.pos),
                            ),
                            severity: Some(match diagnostic.severity {
                                ShaderErrorSeverity::Hint => lsp_types::DiagnosticSeverity::HINT,
                                ShaderErrorSeverity::Information => {
                                    lsp_types::DiagnosticSeverity::INFORMATION
                                }
                                ShaderErrorSeverity::Warning => {
                                    lsp_types::DiagnosticSeverity::WARNING
                                }
                                ShaderErrorSeverity::Error => lsp_types::DiagnosticSeverity::ERROR,
                            }),
                            message: diagnostic.error,
                            source: Some("shader-validator".to_string()),
                            ..Default::default()
                        };
                        match diagnostics.get_mut(&uri) {
                            Some(value) => value.push(diagnostic),
                            None => {
                                diagnostics.insert(uri, vec![diagnostic]);
                            }
                        };
                    }
                }
                // Clear diagnostic if no errors.
                if diagnostics.get(&uri).is_none() {
                    info!(
                        "Clearing diagnostic for main file {} (diags:{:?})",
                        uri, diagnostics
                    );
                    diagnostics.insert(uri.clone(), vec![]);
                }
                // Add empty diagnostics to dependencies without errors to clear them.
                dependencies.visit_dependencies(&mut |dep| {
                    let uri = Url::from_file_path(&dep).unwrap();
                    if diagnostics.get(&uri).is_none() {
                        info!(
                            "Clearing diagnostic for deps file {} (diags:{:?})",
                            uri, diagnostics
                        );
                        diagnostics.insert(uri, vec![]);
                    }
                });
                Ok(diagnostics)
            }
            Err(err) => Err(err),
        }
    }
    pub fn publish_diagnostic(
        &mut self,
        uri: &Url,
        cached_file: &ServerFileCache,
        version: Option<i32>,
    ) {
        match self.recolt_diagnostic(uri, cached_file) {
            Ok(diagnostics) => {
                for diagnostic in diagnostics {
                    let publish_diagnostics_params = PublishDiagnosticsParams {
                        uri: diagnostic.0.clone(),
                        diagnostics: diagnostic.1,
                        version: version,
                    };
                    self.send_notification::<lsp_types::notification::PublishDiagnostics>(
                        publish_diagnostics_params,
                    );
                }
            }
            Err(err) => self.send_notification_error(format!(
                "Failed to compute diagnostic for file {}: {}",
                uri, err
            )),
        }
    }

    pub fn clear_diagnostic(&self, uri: &Url) {
        let publish_diagnostics_params = PublishDiagnosticsParams {
            uri: uri.clone(),
            diagnostics: Vec::new(),
            version: None,
        };
        self.send_notification::<lsp_types::notification::PublishDiagnostics>(
            publish_diagnostics_params,
        );
    }
}
