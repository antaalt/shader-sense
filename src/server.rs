use std::collections::HashMap;
use std::path::Path;
use std::str::FromStr;

use crate::common::ShadingLanguage;
use crate::common::{ValidationParams, Validator};
#[cfg(not(target_os = "wasi"))]
use crate::dxc::Dxc;
use crate::glslang::Glslang;
use crate::naga::Naga;
use crate::shader_error::{ShaderError, ShaderErrorSeverity};
use log::{debug, error, warn};
use lsp_types::notification::{DidChangeConfiguration, DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, DidSaveTextDocument, Notification};
use lsp_types::request::{Completion, DocumentDiagnosticRequest, GotoDefinition, Request, WorkspaceConfiguration};
use lsp_types::{CompletionItem, CompletionItemKind, CompletionOptionsCompletionItem, CompletionParams, CompletionResponse, ConfigurationParams, Diagnostic, DidChangeConfigurationParams, DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentDiagnosticParams, DocumentDiagnosticReport, DocumentDiagnosticReportResult, DocumentFilter, FullDocumentDiagnosticReport, GotoDefinitionParams, GotoDefinitionResponse, OneOf, PublishDiagnosticsParams, RelatedFullDocumentDiagnosticReport, TextDocumentItem, TextDocumentSyncKind, Url, WorkDoneProgressOptions};
use lsp_types::{
    InitializeParams, ServerCapabilities,
};

use lsp_server::{Connection, ErrorCode, IoThreads, Message, RequestId, Response, ResponseError};

use serde::{Deserialize, Serialize};
use serde_json::Value;

#[allow(non_snake_case)]
#[derive(Debug, Serialize, Deserialize)]
struct ServerConfig {
    includes: Vec<String>,
    defines: HashMap<String, String>,
    validateOnType: bool,
    validateOnSave: bool,
    severity: String,
}

impl Default for ServerConfig {
    fn default() -> Self {
        Self { 
            includes: Vec::new(), 
            defines: HashMap::new(),
            validateOnType: true, 
            validateOnSave: true, 
            severity: ShaderErrorSeverity::Hint.to_string(),
        }
    }
}

struct ServerFileCache {
    shading_language: ShadingLanguage,
}

struct ServerLanguage {
    connection: Connection,
    io_threads: Option<IoThreads>,
    watched_files: HashMap<Url, ServerFileCache>,
    request_id: i32,
    request_callbacks: HashMap<RequestId, fn(&mut ServerLanguage, Value)>,
    config: ServerConfig,
    validators: HashMap<ShadingLanguage, Box<dyn Validator>>,
}

impl ServerLanguage {
    pub fn new() -> Self {
        // Create the transport. Includes the stdio (stdin and stdout) versions but this could
        // also be implemented to use sockets or HTTP.
        let (connection, io_threads) = Connection::stdio();

        // Create validators.
        let mut validators : HashMap::<ShadingLanguage, Box<dyn Validator>> = HashMap::new();
        validators.insert(ShadingLanguage::Wgsl, Box::new(Naga::new()));
        #[cfg(target_os = "wasi")]
        validators.insert(ShadingLanguage::Hlsl, Box::new(Glslang::hlsl()));
        #[cfg(not(target_os = "wasi"))]
        validators.insert(ShadingLanguage::Hlsl, Box::new(Dxc::new().expect("Failed to create DXC")));
        validators.insert(ShadingLanguage::Glsl, Box::new(Glslang::glsl()));

        // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
        Self {
            connection,
            io_threads: Some(io_threads),
            watched_files: HashMap::new(),
            request_id: 0,
            request_callbacks: HashMap::new(),
            config: ServerConfig::default(),
            validators: validators,
        }
    }
    pub fn initialize(&mut self) -> Result<(), Box<dyn std::error::Error + Sync + Send>> {
        let server_capabilities = serde_json::to_value(&ServerCapabilities {
            text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
            completion_provider: Some(lsp_types::CompletionOptions {
                resolve_provider: None, // For more detailed data
                completion_item: Some(CompletionOptionsCompletionItem {
                    label_details_support: Some(true),
                }),
                ..Default::default()
            }),
            //definition_provider: Some(OneOf::Left(true)), 
            ..Default::default()
        }).expect("Failed to serialize server capabilities.");
        let initialization_params = match self.connection.initialize(server_capabilities) {
            Ok(it) => it,
            Err(e) => {
                if e.channel_is_disconnected() {
                    self.io_threads.take().unwrap().join()?;
                }
                return Err(e.into());
            }
        };    
        let client_initialization_params: InitializeParams = serde_json::from_value(initialization_params).unwrap();
        debug!("Received client params: {:#?}", client_initialization_params);

        self.request_configuration();
        
        return Ok(());
    }
    pub fn run(&mut self) -> Result<(), Box<dyn std::error::Error + Sync + Send>>
    {
        loop {
            let msg_err = self.connection.receiver.recv();
            match msg_err {
                Ok(msg) => {
                    match msg {
                        Message::Request(req) => {
                            if self.connection.handle_shutdown(&req)? {
                                return Ok(());
                            }
                            self.on_request(req)?;
                        }
                        Message::Response(resp) => {
                            self.on_response(resp)?;
                        }
                        Message::Notification(not) => {
                            self.on_notification(not)?;
                        }
                    }
                },
                Err(_) => {
                    warn!("Client disconnected");
                    break; 
                }
            }
        }
        Ok(())
    }
    fn on_request(&mut self, req: lsp_server::Request) -> Result<(), serde_json::Error> {
        match req.method.as_str() {
            DocumentDiagnosticRequest::METHOD => {
                let params : DocumentDiagnosticParams = serde_json::from_value(req.params)?;
                debug!("Received document diagnostic request #{}: {:#?}", req.id, params);
                match self.get_watched_file_lang(&params.text_document.uri) {
                    Some(shading_language) => {
                        match self.recolt_diagnostic(&params.text_document.uri, shading_language, None) {
                            Some(diagnostics) => self.send_response::<DocumentDiagnosticRequest>(
                                req.id.clone(), 
                                DocumentDiagnosticReportResult::Report(
                                    DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport{
                                        related_documents: None, // TODO: data of other files.
                                        full_document_diagnostic_report: FullDocumentDiagnosticReport{
                                            result_id: Some(req.id.to_string()),
                                            items: diagnostics,
                                        },
                                    })
                                )
                            ),
                            // Send empty report.
                            None => self.send_response::<DocumentDiagnosticRequest>(
                                req.id, 
                                DocumentDiagnosticReportResult::Report(
                                    DocumentDiagnosticReport::Full(
                                        RelatedFullDocumentDiagnosticReport::default()
                                    )
                                )
                            )
                        };
                    }
                    None => self.send_response_error(req.id, ErrorCode::InvalidParams, "Requesting diagnostic on file that is not watched".to_string())
                }
            },
            GotoDefinition::METHOD => {
                let params : GotoDefinitionParams = serde_json::from_value(req.params)?;
                debug!("Received gotoDefinition request #{}: {:#?}", req.id, params);
                let result = GotoDefinitionResponse::Array(Vec::new());
                self.send_response::<GotoDefinition>(req.id, Some(result));
            },
            Completion::METHOD => {
                let params : CompletionParams = serde_json::from_value(req.params)?;
                debug!("Received completion request #{}: {:#?}", req.id, params);
                match self.get_watched_file_lang(&params.text_document_position.text_document.uri) {
                    Some(shading_language) => {
                        match self.recolt_completion(&params.text_document_position.text_document.uri, shading_language, None)
                        {
                            Some(value) => self.send_response::<Completion>(req.id, Some(CompletionResponse::Array(value))),
                            None => self.send_response::<Completion>(req.id, None),
                        }
                        
                    },
                    None => self.send_response_error(req.id, ErrorCode::InvalidParams, "Requesting diagnostic on file that is not watched".to_string())
                }
            }
            _ => {
                warn!("Received unhandled request: {:#?}", req);
            }
        }
        Ok(())
    }
    fn on_response(&mut self, response: lsp_server::Response) -> Result<(), serde_json::Error> {
        match self.request_callbacks.remove(&response.id) {
            Some(callback) => {
                match response.result {
                    Some(result) => callback(self, result),
                    None => callback(self, serde_json::from_str("{}").unwrap()),
                }
            },
            None => warn!("Received unhandled response: {:#?}", response)
        }
        Ok(())
    }
    fn on_notification(&mut self, notification: lsp_server::Notification) -> Result<(), serde_json::Error> {
        debug!("Received notification: {}", notification.method);
        match notification.method.as_str() {
            DidOpenTextDocument::METHOD => {
                let params : DidOpenTextDocumentParams = serde_json::from_value(notification.params)?;
                match self.watch_file(&params.text_document) {
                    Ok(lang) => {
                        self.publish_diagnostic(&params.text_document.uri, lang, Some(params.text_document.text), Some(params.text_document.version));
                        debug!("Starting watching {:#?} file at {:#?}", lang, params.text_document.uri);
                    },
                    Err(()) => {
                        warn!("Received unhandled shading language : {:#?}", params.text_document);
                    }
                };
            },
            DidSaveTextDocument::METHOD => {
                let params : DidSaveTextDocumentParams = serde_json::from_value(notification.params)?;
                debug!("got did save text document: {:#?}", params.text_document.uri);
                if self.config.validateOnSave {
                    match self.get_watched_file_lang(&params.text_document.uri)  {
                        Some(shading_language) => self.publish_diagnostic(&params.text_document.uri, shading_language, params.text, None),
                        None => error!("Trying to save watched file that is not watched : {}", params.text_document.uri)
                    }
                }
            },
            DidCloseTextDocument::METHOD => {
                let params : DidCloseTextDocumentParams = serde_json::from_value(notification.params)?;
                debug!("got did close text document: {:#?}", params.text_document.uri);
                self.clear_diagnostic(&params.text_document.uri);
                self.remove_watched_file(&params.text_document.uri);
            },
            DidChangeTextDocument::METHOD => {
                let params : DidChangeTextDocumentParams = serde_json::from_value(notification.params)?;
                debug!("got did change text document: {:#?}", params.text_document.uri);
                if self.config.validateOnType {
                    match self.get_watched_file_lang(&params.text_document.uri)  {
                        Some(shading_language) => {
                            for content in params.content_changes {
                                self.publish_diagnostic(&params.text_document.uri, shading_language, Some(content.text.clone()), Some(params.text_document.version));
                            }
                        },
                        None => error!("Trying to change watched file that is not watched : {}", params.text_document.uri)
                    }
                }
            },
            DidChangeConfiguration::METHOD => {
                let params : DidChangeConfigurationParams = serde_json::from_value(notification.params)?;
                debug!("Received did change configuration document: {:#?}", params);
                // Here config received is empty. we need to request it to user.
                //let config : ServerConfig = serde_json::from_value(params.settings)?;
                self.request_configuration();
            }
            _ => {
                warn!("Received unhandled notification: {:#?}", notification);
            }
        }
        Ok(())
    }

    fn watch_file(&mut self, text_document: &TextDocumentItem) -> Result<ShadingLanguage,()>  {
        match ShadingLanguage::from_str(text_document.language_id.as_str()) {
            Ok(lang) => {
                match self.watched_files.insert(text_document.uri.clone(), ServerFileCache {
                    shading_language: lang
                }) {
                    Some(_) => { error!("Adding a file to watch that is already watched: {}", text_document.uri)},
                    None => {}
                }
                Ok(lang)
            },
            Err(()) => {
                Err(())
            }
        }
    }
    fn get_watched_file_lang(&mut self, uri: &Url) -> Option<ShadingLanguage> {
        match self.watched_files.get(uri) {
            Some(shading_language) => Some(shading_language.shading_language),
            None => None
        }
    }
    fn remove_watched_file(&mut self, uri: &Url) {
        match self.watched_files.remove(&uri) {
            Some(_) => {},
            None => warn!("Trying to remove file that is not watched : {}", uri)
        }
    }

    fn recolt_diagnostic(&mut self, uri: &Url, shading_language : ShadingLanguage, shader_source: Option<String>) -> Option<Vec<Diagnostic>> {
        // Skip non file uri.
        match uri.scheme() {
            "file" => {}
            _ => { return None; }
        }
        let file_path = uri.to_file_path().expect(format!("Failed to convert {} to a valid path.", uri).as_str());
        let shader_source_from_file = match shader_source {
            Some(source) => source,
            None => std::fs::read_to_string(&file_path).expect(format!("Failed to read shader at {}.", file_path.display()).as_str()),
        };
        let includes = self.config.includes.clone();
        let defines = self.config.defines.clone();
        let validator = self.get_validator(shading_language);
        match validator.validate_shader(
            shader_source_from_file,
            file_path.as_path(),
            ValidationParams::new(includes, defines),
        ) {
            Ok(_) => { 
                None // no diagnostic to publish
            }
            Err(err) => {
                // TODO: should send an error instead.
                let mut diagnostics = Vec::new();
                for error in err.errors {
                    match error {
                        ShaderError::ParserErr{filename: _, severity, error, line, pos} => {
                            if severity.is_required(ShaderErrorSeverity::from(self.config.severity.clone())) {
                                diagnostics.push(Diagnostic {
                                    range: lsp_types::Range::new(lsp_types::Position::new(line - 1, pos), lsp_types::Position::new(line - 1, pos)),
                                    severity: Some(match severity {
                                        ShaderErrorSeverity::Hint => lsp_types::DiagnosticSeverity::HINT,
                                        ShaderErrorSeverity::Information => lsp_types::DiagnosticSeverity::INFORMATION,
                                        ShaderErrorSeverity::Warning => lsp_types::DiagnosticSeverity::WARNING,
                                        ShaderErrorSeverity::Error => lsp_types::DiagnosticSeverity::ERROR,
                                    }),
                                    message: error,
                                    source: Some("shader-validator".to_string()),
                                    ..Default::default()
                                });
                            }
                        },
                        ShaderError::ValidationErr{message} => {
                            diagnostics.push(Diagnostic {
                                range: lsp_types::Range::new(lsp_types::Position::new(0, 0), lsp_types::Position::new(0, 0)),
                                severity: Some(lsp_types::DiagnosticSeverity::ERROR),
                                message: message,
                                source: Some("shader-validator".to_string()),
                                ..Default::default()
                            });
                        },
                        ShaderError::InternalErr(err) => {
                            diagnostics.push(Diagnostic {
                                range: lsp_types::Range::new(lsp_types::Position::new(0, 0), lsp_types::Position::new(0, 0)),
                                severity: Some(lsp_types::DiagnosticSeverity::ERROR),
                                message: format!("InternalErr({}): {}",uri.path(), err.to_string()),
                                source: Some("shader-validator".to_string()),
                                ..Default::default()
                            });
                        },
                        ShaderError::IoErr(err) => {
                            diagnostics.push(Diagnostic {
                                range: lsp_types::Range::new(lsp_types::Position::new(0, 0), lsp_types::Position::new(0, 0)),
                                severity: Some(lsp_types::DiagnosticSeverity::ERROR),
                                message: format!("IoErr({}): {}",uri.path(), err.to_string()),
                                source: Some("shader-validator".to_string()),
                                ..Default::default()
                            });
                        }
                    }
                }
                Some(diagnostics)
            }
        }
    }
    fn recolt_completion(&mut self, uri: &Url, shading_language : ShadingLanguage, shader_source: Option<String>) -> Option<Vec<CompletionItem>> {
        let file_path = uri.to_file_path().expect(format!("Failed to convert {} to a valid path.", uri).as_str());
        let shader_source_from_file = match shader_source {
            Some(source) => source,
            None => std::fs::read_to_string(&file_path).expect(format!("Failed to read shader at {}.", file_path.display()).as_str()),
        };
        let includes = self.config.includes.clone();
        let defines = self.config.defines.clone();
        let validator = self.get_validator(shading_language);
        match validator.get_shader_tree(shader_source_from_file, &file_path, ValidationParams::new(includes, defines)) {
            Ok(value) => {
                let mut items = Vec::new();
                for function in value.functions {
                    items.push(CompletionItem {
                        label: function.clone(),
                        kind: Some(CompletionItemKind::FUNCTION),
                        detail: Some(function),
                        ..Default::default()
                    })
                }
                for global_variable in value.global_variables {
                    items.push(CompletionItem {
                        label: global_variable.clone(),
                        kind: Some(CompletionItemKind::CONSTANT),
                        detail: Some(global_variable),
                        ..Default::default()
                    })
                }
                for types in value.types {
                    items.push(CompletionItem {
                        label: types.clone(),
                        kind: Some(CompletionItemKind::TYPE_PARAMETER),
                        detail: Some(types),
                        ..Default::default()
                    })
                }
                Some(items)
            },
            Err(_) => None
        }
    }

    fn request_configuration(&mut self) {
        let config = ConfigurationParams{ 
            items: vec![lsp_types::ConfigurationItem {
                scope_uri: None,
                section: Some("shader-validator".to_owned()),
            }], 
        };
        self.send_request::<WorkspaceConfiguration>(config, |server: &mut ServerLanguage, value: Value| {
            // Sent 1 item, received 1 in an array
            let mut parsed_config : Vec<ServerConfig> = serde_json::from_value(value).expect("Failed to parse received config");
            server.config = parsed_config.remove(0);
            // TODO: Should republish diagnostics for all watched files here.
        });
    }

    fn publish_diagnostic(&mut self, uri : &Url, shading_language : ShadingLanguage, shader_source: Option<String>, version: Option<i32>) {
        let publish_diagnostics_params = PublishDiagnosticsParams {
            uri: uri.clone(),
            diagnostics: match self.recolt_diagnostic(uri, shading_language, shader_source) {
                Some(diagnostics) => diagnostics,
                None => Vec::new() // No errors, publish empty diag
            }, 
            version: version,
        };
        self.send_notification::<lsp_types::notification::PublishDiagnostics>(publish_diagnostics_params);
    } 

    fn clear_diagnostic(&self, uri : &Url) {
        let publish_diagnostics_params = PublishDiagnosticsParams {
            uri: uri.clone(),
            diagnostics: Vec::new(),
            version: None,
        };
        self.send_notification::<lsp_types::notification::PublishDiagnostics>(publish_diagnostics_params);
    } 
    fn send_response<N: lsp_types::request::Request>(
        &self,
        request_id: RequestId,
        params: N::Result,
    ) {
        let response = Response::new_ok::<N::Result>(request_id, params);
        self.send(response.into());
    }
    fn send_response_error(
        &self,
        request_id: RequestId,
        code: lsp_server::ErrorCode,
        message: String,
    ) {
        let response = Response::new_err(request_id, code as i32, message);
        self.send(response.into());
    }
    fn send_notification<N: lsp_types::notification::Notification>(
        &self,
        params: N::Params,
    ) {
        let not = lsp_server::Notification::new(N::METHOD.to_owned(), params);
        self.send(not.into());
    }
    fn send_request<R: lsp_types::request::Request>(
        &mut self,
        params: R::Params,
        callback: fn(&mut ServerLanguage, Value)
    ) {
        let request_id = RequestId::from(self.request_id);
        self.request_id = self.request_id + 1;
        self.request_callbacks.insert(request_id.clone(), callback);
        let req = lsp_server::Request::new(request_id, R::METHOD.to_owned(), params);
        self.send(req.into());
    }
    fn send(&self, message : Message) {
        self.connection.sender.send(message).expect("Failed to send a message");
    }

    pub fn join(&mut self) -> std::io::Result<()> {
        match self.io_threads.take() {
            Some(h) => h.join(),
            None => Ok(()),
        }
    }

    pub fn get_validator(&mut self, shading_language: ShadingLanguage) -> &mut Box<dyn Validator> {
        self.validators.get_mut(&shading_language).unwrap()
    }
}

pub fn run() {    
    let mut server = ServerLanguage::new();

    match server.initialize() {
        Ok(()) => {},
        Err(value) => { error!("{:#?}", value); }
    }

    match server.run() {
        Ok(()) => {},
        Err(value) => { error!("{:#?}", value); }
    }

    match server.join() {
        Ok(()) => {},
        Err(value) => { error!("{:#?}", value); }
    }
}