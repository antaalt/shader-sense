use std::{
    cell::RefCell,
    collections::HashMap,
    path::{Path, PathBuf},
    rc::Rc,
};

use log::{debug, error, info};
use lsp_types::{Diagnostic, PublishDiagnosticsParams, Url};

use crate::{
    server::read_string_lossy,
    shaders::shader_error::{ShaderErrorSeverity, ValidatorError},
};

use super::{to_file_path, ServerConnection, ServerFileCacheHandle, ServerLanguageData};

impl ServerLanguageData {
    pub fn publish_diagnostic(
        &mut self,
        connection: &ServerConnection,
        uri: &Url,
        cached_file: &ServerFileCacheHandle,
        version: Option<i32>,
    ) {
        if self.config.validate {
            match self.recolt_diagnostic(uri, cached_file) {
                Ok(diagnostics) => {
                    info!(
                        "Publishing diagnostic for file {} ({} diags)",
                        uri.path(),
                        diagnostics.len()
                    );
                    for diagnostic in diagnostics {
                        let publish_diagnostics_params = PublishDiagnosticsParams {
                            uri: diagnostic.0,
                            diagnostics: diagnostic.1,
                            version: version,
                        };
                        connection
                            .send_notification::<lsp_types::notification::PublishDiagnostics>(
                                publish_diagnostics_params,
                            );
                    }
                }
                Err(err) => connection.send_notification_error(format!(
                    "Failed to compute diagnostic for file {}: {}",
                    uri, err
                )),
            }
        } else {
            debug!("Diagnostic disabled. {:?}", self.config);
        }
    }

    pub fn clear_diagnostic(&self, connection: &ServerConnection, uri: &Url) {
        // TODO: check it exist ?
        let publish_diagnostics_params = PublishDiagnosticsParams {
            uri: uri.clone(),
            diagnostics: Vec::new(),
            version: None,
        };
        connection.send_notification::<lsp_types::notification::PublishDiagnostics>(
            publish_diagnostics_params,
        );
    }
    
    pub fn recolt_diagnostic(
        &mut self,
        uri: &Url,
        cached_file: &ServerFileCacheHandle,
    ) -> Result<HashMap<Url, Vec<Diagnostic>>, ValidatorError> {
        // Skip non file uri.
        // TODO: move elsewhere, in open file for example
        match uri.scheme() {
            "file" => {}
            _ => {
                return Err(ValidatorError::InternalErr(String::from(
                    "Cannot treat files without file scheme",
                )));
            }
        }
        let file_path = to_file_path(&uri);
        let validation_params = self.config.into_validation_params();
        let shading_language = RefCell::borrow(&cached_file).shading_language;
        let content = RefCell::borrow(&cached_file).content.clone();
        debug!("Validating file {}", file_path.display());
        match self.validator.validate_shader(
            content,
            file_path.as_path(),
            validation_params,
            &mut |path: &Path| -> Option<String> {
                let uri = Url::from_file_path(path).unwrap();
                match self.watched_files.get_watched_file(&uri) {
                    Some(rc) => Some(RefCell::borrow(&rc).content.clone()),
                    None => {
                        let content = read_string_lossy(&file_path).unwrap();
                        match self.watched_files.watch_file(
                            &uri,
                            shading_language,
                            &content,
                            &mut self.symbol_provider,
                            &self.config,
                            false,
                        ) {
                            Ok(_) => Some(content),
                            Err(err) => {
                                error!("Failed to watch file {} : {:?}", file_path.display(), err);
                                None
                            }
                        }
                    }
                }
            },
        ) {
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
                    info!("No issue found for main file. Clearing previous diagnostic {}", uri);
                    diagnostics.insert(uri.clone(), vec![]);
                }
                // Add empty diagnostics to dependencies without errors to clear them.
                dependencies.visit_dependencies(&mut |dep| {
                    let uri = Url::from_file_path(&dep).unwrap();
                    if diagnostics.get(&uri).is_none() {
                        info!("Clearing diagnostic for deps file {}", uri);
                        diagnostics.insert(uri, vec![]);
                    }
                });
                // Store dependencies
                let (removed_deps, added_deps) = {
                    let new_dependencies = &dependencies;
                    let old_dependencies = &RefCell::borrow(&cached_file).dependencies;
                    let mut added_deps = Vec::new(); // deps in new & not in old (& not in watch aswell)
                    let mut removed_deps = Vec::new(); // deps in old & not in new
                    for deps in old_dependencies {
                        if !new_dependencies.has(&deps.0) {
                            removed_deps.push(deps.0.clone());
                        }
                    }
                    new_dependencies.visit_dependencies(&mut |dep| match old_dependencies
                        .iter()
                        .find(|e| e.0 == dep)
                    {
                        Some(_) => {}
                        None => added_deps.push(PathBuf::from(dep)),
                    });
                    (removed_deps, added_deps)
                };
                // Remove old deps
                /*debug!(
                    "Removed deps: {:?}",
                    removed_deps
                );
                for removed_dep in removed_deps {
                    let deps_url = Url::from_file_path(&removed_dep).unwrap();
                    {
                        // Remove ref in deps.
                        let mut cached_file_mut = RefCell::borrow_mut(&cached_file);
                        cached_file_mut.dependencies.remove(&removed_dep);
                    }
                    // File might have been removed already as dependent on another file...
                    let _ = match self.watched_files.get_watched_file(&deps_url) {
                        Some(_) => self.watched_files.remove_watched_file(
                            &deps_url,
                            &mut self.symbol_provider,
                            &self.config,
                            false,
                        ),
                        None => Ok(false),
                    };
                }
                // Add new deps
                debug!("Added deps: {:?}", added_deps);
                for added_dep in added_deps {
                    let mut cached_file_mut = RefCell::borrow_mut(&cached_file);
                    let url = Url::from_file_path(&added_dep).unwrap();
                    match self.watched_files.get_watched_file(&url) {
                        Some(file_rc) => {
                            // Used as main file.
                            cached_file_mut
                                .dependencies
                                .insert(added_dep.into(), file_rc.clone());
                        }
                        None => {
                            // Unused. Load it from disk.
                            let content = read_string_lossy(&added_dep).unwrap();
                            let rc = match self.watched_files.watch_file(
                                &uri,
                                cached_file_mut.shading_language,
                                &content,
                                &mut self.symbol_provider,
                                &self.config,
                                false,
                            ) {
                                Ok(rc) => rc,
                                Err(err) => {
                                    return Err(ValidatorError::InternalErr(format!("{}", err)))
                                }
                            };
                            cached_file_mut
                                .dependencies
                                .insert(added_dep.into(), Rc::clone(&rc));
                        }
                    }
                }*/
                Ok(diagnostics)
            }
            Err(err) => Err(err),
        }
    }
}
