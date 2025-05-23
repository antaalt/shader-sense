use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Debug;
use std::path::PathBuf;
use std::str::FromStr;

mod common;
mod completion;
mod debug;
mod diagnostic;
mod document_symbol;
mod goto;
mod hover;
mod inlay_hint;
mod semantic_token;
pub mod shader_variant; // pub for test.
mod signature;
mod workspace_symbol;

mod profile;
mod server_config;
mod server_connection;
mod server_file_cache;
mod server_language_data;

use debug::{DumpAstParams, DumpAstRequest, DumpDependencyParams, DumpDependencyRequest};
use log::{debug, error, info, warn};
use lsp_types::notification::{
    DidChangeConfiguration, DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument,
    DidSaveTextDocument, Notification,
};
use lsp_types::request::{
    Completion, DocumentDiagnosticRequest, DocumentSymbolRequest, FoldingRangeRequest,
    GotoDefinition, HoverRequest, InlayHintRequest, Request, SemanticTokensFullRequest,
    SignatureHelpRequest, WorkspaceConfiguration, WorkspaceSymbolRequest,
};
use lsp_types::{
    CompletionOptionsCompletionItem, CompletionParams, CompletionResponse, ConfigurationParams,
    DidChangeConfigurationParams, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentDiagnosticParams,
    DocumentDiagnosticReport, DocumentDiagnosticReportKind, DocumentDiagnosticReportResult,
    DocumentSymbolOptions, DocumentSymbolParams, DocumentSymbolResponse, FoldingRange,
    FoldingRangeKind, FoldingRangeParams, FoldingRangeProviderCapability,
    FullDocumentDiagnosticReport, GotoDefinitionParams, HoverParams, HoverProviderCapability,
    InlayHintParams, OneOf, RelatedFullDocumentDiagnosticReport, SemanticTokenType,
    SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions, SemanticTokensParams,
    SemanticTokensServerCapabilities, ServerCapabilities, SignatureHelpOptions,
    SignatureHelpParams, TextDocumentSyncKind, Url, WorkDoneProgressOptions,
    WorkspaceSymbolOptions, WorkspaceSymbolParams, WorkspaceSymbolResponse,
};
use shader_sense::shader::ShadingLanguage;

use lsp_server::{ErrorCode, Message};

use serde_json::Value;
use server_config::ServerConfig;
use server_connection::ServerConnection;
use server_file_cache::ServerLanguageFileCache;
use server_language_data::ServerLanguageData;
use shader_variant::{DidChangeShaderVariant, DidChangeShaderVariantParams};

use crate::profile_scope;

pub struct ServerLanguage {
    connection: ServerConnection,
    config: ServerConfig,
    // Cache
    watched_files: ServerLanguageFileCache,
    language_data: HashMap<ShadingLanguage, ServerLanguageData>,
}

fn clean_url(url: &Url) -> Url {
    // Workaround issue with url encoded as &3a that break key comparison.
    // Clean it by converting back & forth.
    #[cfg(not(target_os = "wasi"))]
    {
        Url::from_file_path(
            url.to_file_path()
                .expect(format!("Failed to convert {} to a valid path.", url).as_str()),
        )
        .unwrap()
    }
    // This method of cleaning URL fail on WASI due to path format. Removing it.
    #[cfg(target_os = "wasi")]
    {
        url.clone()
    }
}

impl ServerLanguage {
    pub fn new() -> Self {
        // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
        Self {
            connection: ServerConnection::new(),
            watched_files: ServerLanguageFileCache::new(),
            config: ServerConfig::default(),
            language_data: HashMap::from([
                (ShadingLanguage::Glsl, ServerLanguageData::glsl()),
                (ShadingLanguage::Hlsl, ServerLanguageData::hlsl()),
                (ShadingLanguage::Wgsl, ServerLanguageData::wgsl()),
            ]),
        }
    }
    pub fn initialize(&mut self) -> Result<(), Box<dyn std::error::Error + Sync + Send>> {
        let server_capabilities = serde_json::to_value(&ServerCapabilities {
            text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Kind(
                TextDocumentSyncKind::INCREMENTAL,
            )),
            completion_provider: Some(lsp_types::CompletionOptions {
                resolve_provider: None, // For more detailed data
                completion_item: Some(CompletionOptionsCompletionItem {
                    label_details_support: Some(true),
                }),
                trigger_characters: Some(vec![".".into()]),
                ..Default::default()
            }),
            signature_help_provider: Some(SignatureHelpOptions {
                trigger_characters: Some(vec!["(".to_owned(), ",".to_owned()]),
                retrigger_characters: None,
                work_done_progress_options: WorkDoneProgressOptions {
                    work_done_progress: None,
                },
            }),
            hover_provider: Some(HoverProviderCapability::Simple(true)),
            definition_provider: Some(lsp_types::OneOf::Left(true)),
            type_definition_provider: Some(lsp_types::TypeDefinitionProviderCapability::Simple(
                false, // Disable as definition_provider is doing it.
            )),
            folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
            document_symbol_provider: Some(OneOf::Right(DocumentSymbolOptions {
                label: None,
                work_done_progress_options: WorkDoneProgressOptions {
                    work_done_progress: None,
                },
            })),
            workspace_symbol_provider: Some(OneOf::Right(WorkspaceSymbolOptions {
                resolve_provider: None,
                work_done_progress_options: WorkDoneProgressOptions {
                    work_done_progress: None,
                },
            })),
            inlay_hint_provider: Some(OneOf::Left(true)),
            semantic_tokens_provider: Some(
                SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
                    work_done_progress_options: WorkDoneProgressOptions {
                        work_done_progress: None,
                    },
                    legend: SemanticTokensLegend {
                        token_modifiers: vec![],
                        token_types: vec![SemanticTokenType::MACRO],
                    },
                    range: None,
                    full: Some(SemanticTokensFullOptions::Bool(true)),
                }),
            ),
            ..Default::default()
        })?;
        let _client_initialization_params =
            self.connection.initialize(server_capabilities).unwrap();
        // TODO: Check features support from client params.
        debug!("Received client params");
        // Request configuration as its not sent automatically (at least with vscode)
        self.request_configuration();

        return Ok(());
    }
    pub fn run(&mut self) -> Result<(), Box<dyn std::error::Error + Sync + Send>> {
        loop {
            let msg_err = self.connection.connection.receiver.recv();
            match msg_err {
                Ok(msg) => match msg {
                    Message::Request(req) => {
                        if self.connection.connection.handle_shutdown(&req)? {
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
                },
                Err(_) => {
                    // Recv error means disconnected.
                    return Ok(());
                }
            }
        }
    }
    fn debug(&self, dbg: &impl Debug) -> String {
        if self.config.trace.is_verbose() {
            format!("{:#?}", dbg)
        } else {
            "{?}".into()
        }
    }
    fn on_request(&mut self, req: lsp_server::Request) -> Result<(), serde_json::Error> {
        match req.method.as_str() {
            DocumentDiagnosticRequest::METHOD => {
                let params: DocumentDiagnosticParams = serde_json::from_value(req.params)?;
                profile_scope!(
                    "Received document diagnostic request #{}: {}",
                    req.id,
                    self.debug(&params)
                );
                let uri = clean_url(&params.text_document.uri);
                match self.watched_files.get_file(&uri) {
                    Some(cached_file) => {
                        assert!(
                            cached_file.is_main_file(),
                            "File {} is not a main file.",
                            uri
                        );
                        match self.recolt_diagnostic(&uri) {
                            Ok(mut diagnostics) => {
                                let main_diagnostic = match diagnostics.remove(&uri) {
                                    Some(diag) => diag,
                                    None => vec![],
                                };
                                self.connection.send_response::<DocumentDiagnosticRequest>(
                                    req.id.clone(),
                                    DocumentDiagnosticReportResult::Report(
                                        DocumentDiagnosticReport::Full(
                                            RelatedFullDocumentDiagnosticReport {
                                                related_documents: Some(
                                                    diagnostics
                                                        .into_iter()
                                                        .map(|diagnostic| {
                                                            (
                                                                diagnostic.0,
                                                                DocumentDiagnosticReportKind::Full(
                                                                    FullDocumentDiagnosticReport {
                                                                        result_id: Some(
                                                                            req.id.to_string(),
                                                                        ),
                                                                        items: diagnostic.1,
                                                                    },
                                                                ),
                                                            )
                                                        })
                                                        .collect(),
                                                ),
                                                full_document_diagnostic_report:
                                                    FullDocumentDiagnosticReport {
                                                        result_id: Some(req.id.to_string()),
                                                        items: main_diagnostic,
                                                    },
                                            },
                                        ),
                                    ),
                                )
                            }
                            // Send empty report.
                            Err(error) => self.connection.send_response_error(
                                req.id.clone(),
                                lsp_server::ErrorCode::InternalError,
                                error.to_string(),
                            ),
                        };
                    }
                    None => self.connection.send_notification_error(format!(
                        "Trying to visit file that is not watched : {}",
                        uri
                    )),
                };
            }
            GotoDefinition::METHOD => {
                let params: GotoDefinitionParams = serde_json::from_value(req.params)?;
                profile_scope!(
                    "Received gotoDefinition request #{}: {}",
                    req.id,
                    self.debug(&params)
                );
                let uri = clean_url(&params.text_document_position_params.text_document.uri);
                match self.watched_files.get_file(&uri) {
                    Some(cached_file) => {
                        assert!(
                            cached_file.is_main_file(),
                            "File {} is not a main file.",
                            uri
                        );
                        let position = params.text_document_position_params.position;
                        match self.recolt_goto(&uri, position) {
                            Ok(value) => self
                                .connection
                                .send_response::<GotoDefinition>(req.id.clone(), value),
                            Err(err) => self.connection.send_response_error(
                                req.id.clone(),
                                ErrorCode::InvalidParams,
                                format!("Failed to recolt signature : {:#?}", err),
                            ),
                        }
                    }
                    None => self.connection.send_notification_error(format!(
                        "Trying to visit file that is not watched : {}",
                        uri
                    )),
                };
            }
            Completion::METHOD => {
                let params: CompletionParams = serde_json::from_value(req.params)?;
                profile_scope!(
                    "Received completion request #{}: {}",
                    req.id,
                    self.debug(&params)
                );
                let uri = clean_url(&params.text_document_position.text_document.uri);
                match self.watched_files.get_file(&uri) {
                    Some(cached_file) => {
                        assert!(
                            cached_file.is_main_file(),
                            "File {} is not a main file.",
                            uri
                        );
                        match self.recolt_completion(
                            &uri,
                            params.text_document_position.position,
                            match &params.context {
                                Some(context) => context.trigger_character.clone(),
                                None => None,
                            },
                        ) {
                            Ok(value) => self.connection.send_response::<Completion>(
                                req.id.clone(),
                                Some(CompletionResponse::Array(value)),
                            ),
                            Err(error) => self.connection.send_response_error(
                                req.id.clone(),
                                lsp_server::ErrorCode::InternalError,
                                error.to_string(),
                            ),
                        }
                    }
                    None => self.connection.send_notification_error(format!(
                        "Trying to visit file that is not watched : {}",
                        uri
                    )),
                };
            }
            SignatureHelpRequest::METHOD => {
                let params: SignatureHelpParams = serde_json::from_value(req.params)?;
                profile_scope!(
                    "Received completion request #{}: {}",
                    req.id,
                    self.debug(&params)
                );
                let uri = clean_url(&params.text_document_position_params.text_document.uri);
                match self.watched_files.get_file(&uri) {
                    Some(cached_file) => {
                        assert!(
                            cached_file.is_main_file(),
                            "File {} is not a main file.",
                            uri
                        );
                        match self
                            .recolt_signature(&uri, params.text_document_position_params.position)
                        {
                            Ok(value) => self
                                .connection
                                .send_response::<SignatureHelpRequest>(req.id.clone(), value),
                            Err(err) => self.connection.send_response_error(
                                req.id.clone(),
                                ErrorCode::InvalidParams,
                                format!("Failed to recolt signature : {:#?}", err),
                            ),
                        }
                    }
                    None => self.connection.send_notification_error(format!(
                        "Trying to visit file that is not watched : {}",
                        uri
                    )),
                };
            }
            HoverRequest::METHOD => {
                let params: HoverParams = serde_json::from_value(req.params)?;
                profile_scope!(
                    "Received hover request #{}: {}",
                    req.id,
                    self.debug(&params)
                );
                let uri = clean_url(&params.text_document_position_params.text_document.uri);
                match self.watched_files.get_file(&uri) {
                    Some(cached_file) => {
                        assert!(
                            cached_file.is_main_file(),
                            "File {} is not a main file.",
                            uri
                        );
                        let position = params.text_document_position_params.position;
                        match self.recolt_hover(&uri, position) {
                            Ok(value) => self
                                .connection
                                .send_response::<HoverRequest>(req.id.clone(), value),
                            Err(err) => self.connection.send_response_error(
                                req.id.clone(),
                                ErrorCode::InvalidParams,
                                format!("Failed to recolt signature : {:#?}", err),
                            ),
                        }
                    }
                    None => self.connection.send_notification_error(format!(
                        "Trying to visit file that is not watched : {}",
                        uri
                    )),
                }
            }
            InlayHintRequest::METHOD => {
                let params: InlayHintParams = serde_json::from_value(req.params)?;
                profile_scope!(
                    "Received inlay hint request #{}: {}",
                    req.id,
                    self.debug(&params)
                );
                let uri = clean_url(&params.text_document.uri);
                match self.watched_files.get_file(&uri) {
                    Some(cached_file) => {
                        assert!(
                            cached_file.is_main_file(),
                            "File {} is not a main file.",
                            uri
                        );
                        match self.recolt_inlay_hint(&uri, &params.range) {
                            Ok(inlay_hints) => {
                                self.connection.send_response::<InlayHintRequest>(
                                    req.id.clone(),
                                    Some(inlay_hints),
                                );
                            }
                            Err(err) => self.connection.send_response_error(
                                req.id.clone(),
                                ErrorCode::InvalidParams,
                                format!("Failed to compute inlay hint : {:#?}", err),
                            ),
                        }
                    }
                    None => self.connection.send_notification_error(format!(
                        "Trying to visit file that is not watched : {}",
                        uri
                    )),
                }
            }
            // Provider not enabled as vscode already does this nicely with grammar files
            FoldingRangeRequest::METHOD => {
                let params: FoldingRangeParams = serde_json::from_value(req.params)?;
                profile_scope!(
                    "Received folding range request #{}: {}",
                    req.id,
                    self.debug(&params)
                );
                let uri = clean_url(&params.text_document.uri);
                match self.watched_files.get_file(&uri) {
                    Some(cached_file) => {
                        assert!(
                            cached_file.is_main_file(),
                            "File {} is not a main file.",
                            uri
                        );
                        // Adding regions
                        let mut folding_ranges: Vec<FoldingRange> = cached_file
                            .data
                            .as_ref()
                            .unwrap()
                            .symbol_cache
                            .get_preprocessor()
                            .regions
                            .iter()
                            .map(|region| FoldingRange {
                                start_line: region.range.start.line,
                                start_character: Some(region.range.start.pos),
                                end_line: region.range.end.line,
                                end_character: Some(region.range.end.pos),
                                kind: Some(FoldingRangeKind::Region),
                                collapsed_text: None,
                            })
                            .collect();
                        // Adding scopes from file
                        let symbol_provider = &self
                            .language_data
                            .get(&cached_file.shading_language)
                            .unwrap()
                            .symbol_provider;
                        let scopes = symbol_provider
                            .query_file_scopes(&RefCell::borrow(&cached_file.shader_module));
                        let mut folded_scopes: Vec<FoldingRange> = scopes
                            .iter()
                            .map(|s| FoldingRange {
                                start_line: s.start.line,
                                start_character: Some(s.start.pos),
                                end_line: s.end.line,
                                end_character: Some(s.end.pos),
                                kind: Some(FoldingRangeKind::Region),
                                collapsed_text: None,
                            })
                            .collect();
                        // Adding struct to scopes.
                        //cached_file.data.get_symbols().iter().map(|e| e.0.iter().map(|e| match &e.data {
                        //    // We dont have its range stored here...
                        //    shader_sense::symbols::symbols::ShaderSymbolData::Struct { members, methods } => todo!(),
                        //}));
                        folding_ranges.append(&mut folded_scopes);
                        self.connection.send_response::<FoldingRangeRequest>(
                            req.id.clone(),
                            Some(folding_ranges),
                        );
                    }
                    None => self.connection.send_notification_error(format!(
                        "Trying to visit file that is not watched : {}",
                        uri
                    )),
                }
            }
            WorkspaceSymbolRequest::METHOD => {
                let params: WorkspaceSymbolParams = serde_json::from_value(req.params)?;
                profile_scope!(
                    "Received workspace symbol request #{}: {}",
                    req.id,
                    self.debug(&params)
                );
                let _ = params.query; // Should we filter ?
                match self.recolt_workspace_symbol() {
                    Ok(symbols) => self.connection.send_response::<WorkspaceSymbolRequest>(
                        req.id.clone(),
                        Some(WorkspaceSymbolResponse::Flat(symbols)),
                    ),
                    Err(err) => self.connection.send_notification_error(format!(
                        "Failed to compute symbols for workspace : {}",
                        err.to_string()
                    )),
                }
            }
            DocumentSymbolRequest::METHOD => {
                let params: DocumentSymbolParams = serde_json::from_value(req.params)?;
                profile_scope!(
                    "Received document symbol request #{}: {}",
                    req.id,
                    self.debug(&params)
                );
                let uri = clean_url(&params.text_document.uri);
                match self.watched_files.get_file(&uri) {
                    Some(cached_file) => {
                        assert!(
                            cached_file.is_main_file(),
                            "File {} is not a main file.",
                            uri
                        );
                        match self.recolt_document_symbol(&uri) {
                            Ok(symbols) => self.connection.send_response::<DocumentSymbolRequest>(
                                req.id.clone(),
                                Some(DocumentSymbolResponse::Nested(symbols)),
                            ),
                            Err(err) => self.connection.send_notification_error(format!(
                                "Failed to compute symbols for file {} : {}",
                                uri,
                                err.to_string()
                            )),
                        }
                    }
                    None => self.connection.send_notification_error(format!(
                        "Trying to visit file that is not watched : {}",
                        uri
                    )),
                }
            }
            // Debug request
            DumpAstRequest::METHOD => {
                let params: DumpAstParams = serde_json::from_value(req.params)?;
                profile_scope!(
                    "Received dump ast request #{}: {}",
                    req.id,
                    self.debug(&params)
                );
                let uri = clean_url(&params.text_document.uri);
                match self.watched_files.get_file(&uri) {
                    Some(cached_file) => {
                        assert!(
                            cached_file.is_main_file(),
                            "File {} is not a main file.",
                            uri
                        );
                        let ast = RefCell::borrow(&cached_file.shader_module).dump_ast();
                        self.connection
                            .send_response::<DumpAstRequest>(req.id.clone(), Some(ast));
                    }
                    None => self.connection.send_notification_error(format!(
                        "Trying to visit file that is not watched : {}",
                        uri
                    )),
                }
            }
            DumpDependencyRequest::METHOD => {
                let params: DumpDependencyParams = serde_json::from_value(req.params)?;
                profile_scope!(
                    "Received dump dependency request #{}: {}",
                    req.id,
                    self.debug(&params)
                );
                let uri = clean_url(&params.text_document.uri);
                match self.watched_files.get_file(&uri) {
                    Some(cached_file) => {
                        assert!(
                            cached_file.is_main_file(),
                            "File {} is not a main file.",
                            uri
                        );
                        let deps_tree = cached_file
                            .data
                            .as_ref()
                            .unwrap()
                            .symbol_cache
                            .dump_dependency_tree(&uri.to_file_path().unwrap());
                        self.connection.send_response::<DumpDependencyRequest>(
                            req.id.clone(),
                            Some(deps_tree),
                        );
                    }
                    None => self.connection.send_notification_error(format!(
                        "Trying to visit file that is not watched : {}",
                        uri
                    )),
                }
            }
            SemanticTokensFullRequest::METHOD => {
                let params: SemanticTokensParams = serde_json::from_value(req.params)?;
                profile_scope!(
                    "Received semantic token request #{}: {}",
                    req.id,
                    self.debug(&params)
                );
                let uri = clean_url(&params.text_document.uri);
                match self.watched_files.get_file(&uri) {
                    Some(cached_file) => {
                        assert!(
                            cached_file.is_main_file(),
                            "File {} is not a main file.",
                            uri
                        );
                        match self.recolt_semantic_tokens(&uri) {
                            Ok(semantic_tokens) => {
                                self.connection.send_response::<SemanticTokensFullRequest>(
                                    req.id.clone(),
                                    Some(semantic_tokens),
                                )
                            }
                            Err(err) => self.connection.send_notification_error(format!(
                                "Failed to recolt semantic tokens for {}: {}",
                                uri, err
                            )),
                        }
                    }
                    None => self.connection.send_notification_error(format!(
                        "Trying to visit file that is not watched : {}",
                        uri
                    )),
                }
            }
            _ => warn!("Received unhandled request: {:#?}", req),
        }
        Ok(())
    }
    fn on_response(&mut self, response: lsp_server::Response) -> Result<(), serde_json::Error> {
        match self.connection.remove_callback(&response.id) {
            Some(callback) => match response.result {
                Some(result) => callback(self, result),
                None => callback(self, serde_json::from_str("{}").unwrap()),
            },
            None => warn!("Received unhandled response: {:#?}", response),
        }
        Ok(())
    }
    fn on_notification(
        &mut self,
        notification: lsp_server::Notification,
    ) -> Result<(), serde_json::Error> {
        match notification.method.as_str() {
            DidOpenTextDocument::METHOD => {
                let params: DidOpenTextDocumentParams =
                    serde_json::from_value(notification.params)?;
                profile_scope!(
                    "Received did open text document notification for {}:{}",
                    params.text_document.uri,
                    self.debug(&params)
                );
                let uri = clean_url(&params.text_document.uri);

                // Skip non file uri.
                if uri.scheme() != "file" {
                    self.connection.send_notification_error(format!(
                        "Trying to watch file with unsupported scheme : {}",
                        uri.scheme()
                    ));
                    return Ok(());
                }
                match ShadingLanguage::from_str(params.text_document.language_id.as_str()) {
                    Ok(shading_language) => {
                        let language_data = self.language_data.get_mut(&shading_language).unwrap();
                        match self.watched_files.watch_file(
                            &uri,
                            shading_language.clone(),
                            &params.text_document.text,
                            &mut language_data.language,
                            &language_data.symbol_provider,
                            language_data.validator.as_mut(),
                            &self.config,
                        ) {
                            Ok(_) => {
                                // Should compute following after variant received.
                                // + it seems variant are coming too early on client and too late here...
                                self.publish_diagnostic(&uri, Some(params.text_document.version));
                            }
                            Err(error) => self.connection.send_notification_error(format!(
                                "Failed to watch file {}: {}",
                                uri.to_string(),
                                error.to_string()
                            )),
                        }
                    }
                    Err(_) => self.connection.send_notification_error(format!(
                        "Failed to parse language id : {}",
                        params.text_document.language_id
                    )),
                }
            }
            DidSaveTextDocument::METHOD => {
                let params: DidSaveTextDocumentParams =
                    serde_json::from_value(notification.params)?;
                let uri = clean_url(&params.text_document.uri);
                profile_scope!(
                    "Received did save text document notification for file {}:{}",
                    params.text_document.uri,
                    self.debug(&params)
                );
                // File content is updated through DidChangeTextDocument.
                match self.watched_files.get_file(&uri) {
                    Some(cached_file) => {
                        assert!(
                            cached_file.is_main_file(),
                            "File {} is not a main file.",
                            uri
                        );
                        assert!(
                            params.text.is_none()
                                || (params.text.is_some()
                                    && RefCell::borrow(&cached_file.shader_module).content
                                        == *params.text.as_ref().unwrap())
                        );
                        // Only update cache if content changed.
                        match params.text {
                            Some(text) => {
                                if text != RefCell::borrow(&cached_file.shader_module).content {
                                    let shading_language = cached_file.shading_language;
                                    let language_data =
                                        self.language_data.get_mut(&shading_language).unwrap();
                                    match self.watched_files.update_file(
                                        &uri,
                                        &mut language_data.language,
                                        None,
                                        None,
                                    ) {
                                        // Cache once all changes have been applied.
                                        Ok(_) => match self.watched_files.cache_file_data(
                                            &uri,
                                            language_data.validator.as_mut(),
                                            &mut language_data.language,
                                            &language_data.symbol_provider,
                                            &self.config,
                                            None,
                                        ) {
                                            Ok(updated_files) => {
                                                self.publish_diagnostic(&uri, None);
                                                for updated_file in updated_files {
                                                    self.publish_diagnostic(&updated_file, None);
                                                }
                                            }
                                            Err(err) => self
                                                .connection
                                                .send_notification_error(format!("{}", err)),
                                        },
                                        Err(err) => self
                                            .connection
                                            .send_notification_error(format!("{}", err)),
                                    };
                                }
                            }
                            None => {}
                        }
                    }
                    None => self.connection.send_notification_error(format!(
                        "Trying to visit file that is not watched : {}",
                        uri
                    )),
                }
            }
            DidCloseTextDocument::METHOD => {
                let params: DidCloseTextDocumentParams =
                    serde_json::from_value(notification.params)?;
                let uri = clean_url(&params.text_document.uri);
                profile_scope!(
                    "Received did close text document notification for file {}: {}",
                    params.text_document.uri,
                    self.debug(&params)
                );
                match self.watched_files.remove_file(&uri) {
                    Ok(removed_urls) => {
                        for removed_url in removed_urls {
                            self.clear_diagnostic(&self.connection, &removed_url);
                        }
                    }
                    Err(err) => self.connection.send_notification_error(format!("{}", err)),
                }
            }
            DidChangeTextDocument::METHOD => {
                let params: DidChangeTextDocumentParams =
                    serde_json::from_value(notification.params)?;
                let uri = clean_url(&params.text_document.uri);
                profile_scope!(
                    "Received did change text document notification for file {}: {}",
                    params.text_document.uri,
                    self.debug(&params)
                );
                match self.watched_files.get_file(&uri) {
                    Some(cached_file) => {
                        assert!(cached_file.is_main_file(), "{} is not a main file.", uri);
                        let shading_language = cached_file.shading_language;
                        let language_data = self.language_data.get_mut(&shading_language).unwrap();
                        // Update all content before caching data.
                        for content in &params.content_changes {
                            match self.watched_files.update_file(
                                &uri,
                                &mut language_data.language,
                                content.range,
                                Some(&content.text),
                            ) {
                                Ok(_) => {}
                                Err(err) => {
                                    self.connection.send_notification_error(format!("{}", err))
                                }
                            };
                        }
                        // Cache once all changes have been applied.
                        match self.watched_files.cache_file_data(
                            &uri,
                            language_data.validator.as_mut(),
                            &mut language_data.language,
                            &language_data.symbol_provider,
                            &self.config,
                            None,
                        ) {
                            Ok(updated_files) => {
                                self.publish_diagnostic(&uri, Some(params.text_document.version));
                                for updated_file in updated_files {
                                    self.publish_diagnostic(&updated_file, None);
                                }
                            }
                            Err(err) => self.connection.send_notification_error(format!("{}", err)),
                        }
                    }
                    None => self.connection.send_notification_error(format!(
                        "Trying to visit file that is not watched : {}",
                        uri
                    )),
                }
            }
            DidChangeConfiguration::METHOD => {
                let params: DidChangeConfigurationParams =
                    serde_json::from_value(notification.params)?;
                profile_scope!(
                    "Received did change configuration notification: {}",
                    self.debug(&params)
                );
                // Here config received is empty. we need to request it to user.
                //let config : ServerConfig = serde_json::from_value(params.settings)?;
                self.request_configuration();
            }
            DidChangeShaderVariant::METHOD => {
                let params: DidChangeShaderVariantParams =
                    serde_json::from_value(notification.params)?;
                let uri = clean_url(&params.text_document.uri);
                profile_scope!(
                    "Received did change shader variant notification: {}",
                    self.debug(&params)
                );
                // Store it in cache
                if let Some(shader_variant) = params.shader_variant {
                    self.watched_files.set_variant(uri.clone(), shader_variant);
                } else {
                    self.watched_files.remove_variant(uri.clone());
                }
                match self.watched_files.get_file(&uri) {
                    Some(cached_file) => {
                        if cached_file.is_main_file() {
                            let shading_language = cached_file.shading_language;
                            // Check all open files that rely on this variant and require a recache.
                            let relying_on_variant_uris =
                                self.watched_files.get_relying_on_files(&uri);
                            let files_to_update: Vec<(Url, Option<PathBuf>)> = {
                                let mut base = vec![(uri.clone(), None)];
                                let mut additional: Vec<(Url, Option<PathBuf>)> =
                                    relying_on_variant_uris
                                        .into_iter()
                                        .map(|f| {
                                            info!(
                                                "Updating relying file {} for variant {}",
                                                f, uri
                                            );
                                            (f, Some(uri.to_file_path().unwrap()))
                                        })
                                        .collect();
                                base.append(&mut additional);
                                base
                            };
                            for (file_to_update, dirty_deps) in files_to_update {
                                // Cache once all changes have been applied.
                                let language_data =
                                    self.language_data.get_mut(&shading_language).unwrap();
                                match self.watched_files.cache_file_data(
                                    &file_to_update,
                                    language_data.validator.as_mut(),
                                    &mut language_data.language,
                                    &language_data.symbol_provider,
                                    &self.config,
                                    dirty_deps.as_ref().map(|e| e.as_path()),
                                ) {
                                    // TODO: symbols should be republished here aswell as they might change but there is no way to do so...
                                    Ok(updated_files) => {
                                        self.publish_diagnostic(&file_to_update, None);
                                        for updated_file in updated_files {
                                            self.publish_diagnostic(&updated_file, None);
                                        }
                                    }
                                    Err(err) => {
                                        self.connection.send_notification_error(format!("{}", err))
                                    }
                                };
                            }
                        } else {
                            // Not main file, no need to update.
                        }
                    }
                    None => {} // Not watched, no need to update.
                }
            }
            _ => info!(
                "Received unhandled notification {}: {}",
                notification.method,
                self.debug(&notification)
            ),
        }
        Ok(())
    }
    fn request_configuration(&mut self) {
        let config = ConfigurationParams {
            items: vec![lsp_types::ConfigurationItem {
                scope_uri: None,
                section: Some("shader-validator".to_owned()),
            }],
        };
        self.connection.send_request::<WorkspaceConfiguration>(
            config,
            |server: &mut ServerLanguage, value: Value| {
                // Sent 1 item, received 1 in an array
                let mut parsed_config: Vec<Option<ServerConfig>> =
                    serde_json::from_value(value).expect("Failed to parse received config");
                let config = parsed_config.remove(0).unwrap_or_default();
                profile_scope!("Updating server config: {}", server.debug(&config));
                server.config = config.clone();
                // Republish all diagnostics
                let mut file_to_republish = Vec::new();
                let watched_urls: Vec<(Url, ShadingLanguage)> = server
                    .watched_files
                    .files
                    .iter()
                    .map(|(url, file)| (url.clone(), file.shading_language))
                    .collect();
                for (url, shading_language) in watched_urls {
                    profile_scope!("Updating server config for file: {}", url);
                    // Clear diags
                    server.clear_diagnostic(&server.connection, &url);
                    let language_data = server.language_data.get_mut(&shading_language).unwrap();
                    // Update symbols & republish diags.
                    if server.watched_files.files.get(&url).unwrap().is_main_file() {
                        // Cache once for main file all changes have been applied.
                        match server.watched_files.cache_file_data(
                            &url,
                            language_data.validator.as_mut(),
                            &mut language_data.language,
                            &language_data.symbol_provider,
                            &server.config,
                            None,
                        ) {
                            Ok(updated_files) => {
                                file_to_republish.push(url.clone());
                                for updated_file in updated_files {
                                    file_to_republish.push(updated_file);
                                }
                            }
                            Err(err) => server
                                .connection
                                .send_notification_error(format!("{}", err)),
                        }
                    }
                }
                // Republish all diagnostics with new settings.
                for url in &file_to_republish {
                    server.publish_diagnostic(url, None);
                }
            },
        );
    }
}

pub fn run() {
    let mut server = ServerLanguage::new();

    match server.initialize() {
        Ok(_) => info!("Server initialization successfull"),
        Err(value) => error!("Failed initalization: {:#?}", value),
    }

    match server.run() {
        Ok(_) => {
            info!("Client disconnected");
            match server.connection.join() {
                Ok(_) => info!("Server shutting down gracefully"),
                Err(value) => error!("Server failed to join threads: {:#?}", value),
            }
        }
        Err(value) => error!("Client disconnected: {:#?}", value),
    }
}
