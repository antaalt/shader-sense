//! Server implementation
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::format;
use std::net::SocketAddr;
use std::num::NonZero;
use std::str::FromStr;
use std::time::Duration;

mod async_message;
mod common;
mod debug;
mod provider;
pub mod shader_variant; // pub for test.

mod profile;
pub mod server_config; // pub for test.
mod server_connection;
mod server_file_cache;
mod server_language_data;

use crossbeam_channel::RecvTimeoutError;
use debug::{DumpAstRequest, DumpDependencyRequest};
use log::{debug, error, info, warn};
use lru::LruCache;
use lsp_types::notification::{
    Cancel, DidChangeConfiguration, DidChangeTextDocument, DidChangeWorkspaceFolders,
    DidCloseTextDocument, DidOpenTextDocument, DidSaveTextDocument, LogTrace, Notification,
    Progress, SetTrace,
};
use lsp_types::request::{
    Completion, DocumentDiagnosticRequest, DocumentSymbolRequest, FoldingRangeRequest, Formatting,
    GotoDefinition, HoverRequest, InlayHintRequest, RangeFormatting, Request,
    SemanticTokensFullRequest, SignatureHelpRequest, WorkDoneProgressCreate,
    WorkspaceSymbolRequest,
};
use lsp_types::{
    CancelParams, CompletionOptionsCompletionItem, CompletionResponse,
    DidChangeConfigurationParams, DidChangeTextDocumentParams, DidChangeWorkspaceFoldersParams,
    DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams,
    DocumentSymbolOptions, DocumentSymbolResponse, FoldingRangeProviderCapability,
    HoverProviderCapability, OneOf, ProgressParams, SemanticTokenType, SemanticTokensFullOptions,
    SemanticTokensLegend, SemanticTokensOptions, SemanticTokensServerCapabilities,
    ServerCapabilities, SetTraceParams, SignatureHelpOptions, TextDocumentSyncKind, Url,
    WorkDoneProgress, WorkDoneProgressBegin, WorkDoneProgressCreateParams, WorkDoneProgressEnd,
    WorkDoneProgressOptions, WorkDoneProgressReport, WorkspaceFoldersServerCapabilities,
    WorkspaceServerCapabilities, WorkspaceSymbolOptions, WorkspaceSymbolResponse,
};
use shader_sense::shader::ShadingLanguage;

use lsp_server::{ErrorCode, Message};

use server_config::ServerConfig;
use server_connection::ServerConnection;
use server_file_cache::ServerLanguageFileCache;
use server_language_data::ServerLanguageData;
use shader_variant::DidChangeShaderVariant;

use crate::profile_scope;
use crate::server::async_message::{AsyncCacheRequest, AsyncMessage, AsyncRequest};
use crate::server::common::{lsp_range_to_shader_range, ServerLanguageError};
use crate::server::server_config::{ServerTrace, ServerTraceLevel};
use crate::server::server_file_cache::ServerFileCache;

/// Server implementation
pub struct ServerLanguage {
    connection: ServerConnection,
    config: ServerConfig,
    // Cache
    watched_files: ServerLanguageFileCache,
    language_data: HashMap<ShadingLanguage, ServerLanguageData>,
    regex_cache: LruCache<String, regex::Regex>, // For semantic token provider who create regex on the fly
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
fn shader_error_to_lsp_error(error: &ServerLanguageError) -> ErrorCode {
    match error {
        ServerLanguageError::ShaderError(_) => ErrorCode::InternalError,
        ServerLanguageError::FileNotWatched(_) => ErrorCode::InternalError,
        ServerLanguageError::SerializationError(_) => ErrorCode::InternalError,
        ServerLanguageError::InvalidParams(_) => ErrorCode::InvalidParams,
        ServerLanguageError::MethodNotFound(_) => ErrorCode::MethodNotFound,
        ServerLanguageError::LastRequestCanceled => ErrorCode::RequestCanceled,
        ServerLanguageError::InternalError(_) => ErrorCode::InternalError,
        ServerLanguageError::IoErr(_) => ErrorCode::InternalError,
    }
}

/// Server transport method
#[derive(Debug, Default, PartialEq, Eq)]
pub enum Transport {
    #[default]
    Stdio,
    TcpListen(SocketAddr),
    TcpConnect(SocketAddr),
}

impl ServerLanguage {
    pub fn new(
        config: ServerConfig,
        transport: Transport,
        supported_shading_languages: HashSet<ShadingLanguage>,
    ) -> std::io::Result<Self> {
        // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
        info!(
            "Creating server with transport {:?} and config {:#?}",
            transport, config
        );
        Ok(Self {
            connection: match transport {
                Transport::Stdio => ServerConnection::stdio(),
                Transport::TcpListen(socket_addr) => ServerConnection::listen(socket_addr)?,
                Transport::TcpConnect(socket_addr) => ServerConnection::connect(socket_addr)?,
            },
            watched_files: ServerLanguageFileCache::new(),
            config: config,
            language_data: supported_shading_languages
                .into_iter()
                .map(|shading_language| {
                    info!("Enabling support for {:?}", shading_language);
                    (
                        shading_language,
                        ServerLanguageData::from_shading_language(shading_language),
                    )
                })
                .collect(),
            // Idea is to be able all macros for a single file in cache.
            // Else when recomputing a file, we will recreate all regex and reinsert
            // them instead of reading from cache. So we update it size at runtime.
            regex_cache: LruCache::new(NonZero::new(50).unwrap()),
        })
    }
    pub fn initialize(&mut self) -> Result<(), Box<dyn std::error::Error + Sync + Send>> {
        let is_clang_format_available = Self::is_clang_format_available();
        let server_capabilities = serde_json::to_value(&ServerCapabilities {
            text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Kind(
                TextDocumentSyncKind::INCREMENTAL,
            )),
            completion_provider: Some(lsp_types::CompletionOptions {
                resolve_provider: None, // For more detailed data
                completion_item: Some(CompletionOptionsCompletionItem {
                    label_details_support: Some(true),
                }),
                trigger_characters: Some(vec![".".into(), ":".into()]),
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
                label: Some("shader-validator".into()),
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
                        token_types: vec![
                            SemanticTokenType::MACRO,
                            SemanticTokenType::PARAMETER,
                            SemanticTokenType::ENUM_MEMBER,
                            SemanticTokenType::ENUM,
                        ],
                    },
                    range: None,
                    full: Some(SemanticTokensFullOptions::Bool(true)),
                }),
            ),
            document_formatting_provider: Some(OneOf::Left(is_clang_format_available)),
            document_range_formatting_provider: Some(OneOf::Left(is_clang_format_available)),
            workspace: Some(WorkspaceServerCapabilities {
                workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                    supported: Some(true),
                    change_notifications: Some(OneOf::Left(true)),
                }),
                ..Default::default()
            }),
            ..Default::default()
        })?;
        let client_initialization_params = self.connection.initialize(server_capabilities)?;
        // Store workspace folder
        if let Some(workspace_folders) = client_initialization_params.workspace_folders {
            self.watched_files.workspace_folder = workspace_folders
                .into_iter()
                .map(|w| {
                    info!("Watching workspace folder {} at {}", w.name, w.uri);
                    w.uri
                })
                .collect()
        }
        // TODO: Check features support from client params.
        debug!("Received client params");
        return Ok(());
    }
    fn resolve_async_request(&mut self, request: AsyncMessage) -> Result<(), ServerLanguageError> {
        // TODO: Discard all messages which version is too old.
        // Check file of request was not already removed.
        match request.get_uri() {
            Some(uri) => {
                if let Some(cached_file) = self.watched_files.files.get(uri) {
                    if !cached_file.is_cachable_file() {
                        return Err(ServerLanguageError::FileNotWatched(
                            uri.to_file_path().unwrap(),
                        ));
                    }
                } else {
                    return Err(ServerLanguageError::FileNotWatched(
                        uri.to_file_path().unwrap(),
                    ));
                }
            }
            None => {} // workspace request or such.
        }

        // For variant, we handle requesting items again on client side, so it does not change version.
        match request {
            AsyncMessage::None | AsyncMessage::UpdateCache(_) => {
                unreachable!()
            }
            AsyncMessage::DocumentSymbolRequest(async_request) => {
                profile_scope!(
                    "Received document symbol request for file {}: {}",
                    async_request.params.text_document.uri,
                    self.debug(&async_request.params)
                );
                let symbols =
                    self.recolt_document_symbol(&async_request.params.text_document.uri)?;
                self.connection.send_response::<DocumentSymbolRequest>(
                    async_request.req_id.clone(),
                    Some(DocumentSymbolResponse::Nested(symbols)),
                );
            }
            AsyncMessage::WorkspaceSymbolRequest(async_request) => {
                profile_scope!(
                    "Received workspace symbol request: {}",
                    self.debug(&async_request.params)
                );
                let _ = async_request.params.query; // TODO: Should we filter ?
                let symbols = self.recolt_workspace_symbol()?;
                self.connection.send_response::<WorkspaceSymbolRequest>(
                    async_request.req_id.clone(),
                    Some(WorkspaceSymbolResponse::Flat(symbols)),
                )
            }
            AsyncMessage::RangeFormatting(async_request) => {
                profile_scope!(
                    "Received formatting range request for file {}: {}",
                    async_request.params.text_document.uri,
                    self.debug(&async_request.params)
                );
                let formatting = self.recolt_formatting(
                    &async_request.params.text_document.uri,
                    Some(lsp_range_to_shader_range(&async_request.params.range)),
                )?;
                self.connection
                    .send_response::<Formatting>(async_request.req_id, Some(formatting));
            }
            AsyncMessage::FoldingRangeRequest(async_request) => {
                profile_scope!(
                    "Received folding range request for file {}: {}",
                    async_request.params.text_document.uri,
                    self.debug(&async_request.params)
                );
                let folding_ranges =
                    self.recolt_folding_range(&async_request.params.text_document.uri)?;
                self.connection.send_response::<FoldingRangeRequest>(
                    async_request.req_id.clone(),
                    Some(folding_ranges),
                );
            }
            AsyncMessage::Formatting(async_request) => {
                profile_scope!(
                    "Received formatting request for file {}: {}",
                    async_request.params.text_document.uri,
                    self.debug(&async_request.params)
                );
                let formatting =
                    self.recolt_formatting(&async_request.params.text_document.uri, None)?;
                self.connection
                    .send_response::<Formatting>(async_request.req_id.clone(), Some(formatting));
            }
            AsyncMessage::InlayHintRequest(async_request) => {
                profile_scope!(
                    "Received inlay hint request for file {}: {}",
                    async_request.params.text_document.uri,
                    self.debug(&async_request.params)
                );
                let inlay_hints = self.recolt_inlay_hint(
                    &async_request.params.text_document.uri,
                    &async_request.params.range,
                )?;
                self.connection.send_response::<InlayHintRequest>(
                    async_request.req_id.clone(),
                    Some(inlay_hints),
                );
            }
            AsyncMessage::HoverRequest(async_request) => {
                profile_scope!(
                    "Received hover request for file {}: {}",
                    async_request
                        .params
                        .text_document_position_params
                        .text_document
                        .uri,
                    self.debug(&async_request.params)
                );
                let position = async_request.params.text_document_position_params.position;
                let value = self.recolt_hover(
                    &async_request
                        .params
                        .text_document_position_params
                        .text_document
                        .uri,
                    position,
                )?;
                self.connection
                    .send_response::<HoverRequest>(async_request.req_id.clone(), value);
            }
            AsyncMessage::SignatureHelpRequest(async_request) => {
                profile_scope!(
                    "Received completion request for file {}: {}",
                    async_request
                        .params
                        .text_document_position_params
                        .text_document
                        .uri,
                    self.debug(&async_request.params)
                );
                let value = self.recolt_signature(
                    &async_request
                        .params
                        .text_document_position_params
                        .text_document
                        .uri,
                    async_request.params.text_document_position_params.position,
                )?;
                self.connection
                    .send_response::<SignatureHelpRequest>(async_request.req_id.clone(), value);
            }
            AsyncMessage::Completion(async_request) => {
                profile_scope!(
                    "Received completion request for file {}: {}",
                    async_request
                        .params
                        .text_document_position
                        .text_document
                        .uri,
                    self.debug(&async_request.params)
                );
                let value = self.recolt_completion(
                    &async_request
                        .params
                        .text_document_position
                        .text_document
                        .uri,
                    async_request.params.text_document_position.position,
                    match &async_request.params.context {
                        Some(context) => context.trigger_character.clone(),
                        None => None,
                    },
                )?;
                self.connection.send_response::<Completion>(
                    async_request.req_id.clone(),
                    Some(CompletionResponse::Array(value)),
                );
            }
            AsyncMessage::GotoDefinition(async_request) => {
                profile_scope!(
                    "Received gotoDefinition request for file {}: {}",
                    async_request
                        .params
                        .text_document_position_params
                        .text_document
                        .uri,
                    self.debug(&async_request.params)
                );
                let position = async_request.params.text_document_position_params.position;
                let value = self.recolt_goto(
                    &async_request
                        .params
                        .text_document_position_params
                        .text_document
                        .uri,
                    position,
                )?;
                self.connection
                    .send_response::<GotoDefinition>(async_request.req_id.clone(), value);
            }
            AsyncMessage::DocumentDiagnosticRequest(async_request) => {
                profile_scope!(
                    "Received document diagnostic request for file {}: {}",
                    async_request.params.text_document.uri,
                    self.debug(&async_request.params)
                );
                let document_diagnostic =
                    self.recolt_document_diagnostic(&async_request.params.text_document.uri)?;
                self.connection.send_response::<DocumentDiagnosticRequest>(
                    async_request.req_id.clone(),
                    document_diagnostic,
                );
            }
            AsyncMessage::SemanticTokensFullRequest(async_request) => {
                profile_scope!(
                    "Received semantic token request for file {}: {}",
                    async_request.params.text_document.uri,
                    self.debug(&async_request.params)
                );
                let semantic_tokens =
                    self.recolt_semantic_tokens(&async_request.params.text_document.uri)?;
                self.connection.send_response::<SemanticTokensFullRequest>(
                    async_request.req_id.clone(),
                    Some(semantic_tokens),
                );
            }
            AsyncMessage::DumpDependencyRequest(async_request) => {
                profile_scope!(
                    "Received dump dependency request for file {}: {}",
                    async_request.params.text_document.uri,
                    self.debug(&async_request.params)
                );
                let cached_file =
                    self.get_cachable_file(&async_request.params.text_document.uri)?;
                let deps_tree = cached_file
                    .data
                    .as_ref()
                    .unwrap()
                    .symbol_cache
                    .dump_dependency_tree(
                        &async_request
                            .params
                            .text_document
                            .uri
                            .to_file_path()
                            .unwrap(),
                    );
                self.connection.send_response::<DumpDependencyRequest>(
                    async_request.req_id.clone(),
                    Some(deps_tree),
                );
            }
            AsyncMessage::DumpAstRequest(async_request) => {
                profile_scope!(
                    "Received dump ast request for file {}: {}",
                    async_request.params.text_document.uri,
                    self.debug(&async_request.params)
                );
                let cached_file =
                    self.get_cachable_file(&async_request.params.text_document.uri)?;
                let ast = RefCell::borrow(&cached_file.shader_module).dump_ast();
                self.connection
                    .send_response::<DumpAstRequest>(async_request.req_id.clone(), Some(ast));
            }
        }
        Ok(())
    }
    pub fn run(&mut self) -> Result<(), Box<dyn std::error::Error + Sync + Send>> {
        let mut async_messages_queue = Vec::new();
        loop {
            // Use try_recv to get all messages and process update in batch
            // while discarding request for previous document versions.
            let msg_err = self
                .connection
                .connection
                .receiver
                .recv_timeout(Duration::from_millis(200));
            match msg_err {
                Ok(msg) => match msg {
                    Message::Request(req) => {
                        if self.connection.connection.handle_shutdown(&req)? {
                            return Ok(());
                        }
                        let id = req.id.clone();
                        match self.on_request(req) {
                            Ok(async_message) => async_messages_queue.push(async_message),
                            Err(err) => self.connection.send_response_error(
                                id,
                                shader_error_to_lsp_error(&err),
                                err.to_string(),
                            ),
                        }
                    }
                    Message::Response(resp) => match self.on_response(resp) {
                        Ok(async_message) => async_messages_queue.push(async_message),
                        Err(err) => self.connection.send_notification_error(err.to_string()),
                    },
                    Message::Notification(not) => match self.on_notification(not) {
                        Ok(async_message) => async_messages_queue.push(async_message),
                        Err(err) => match err {
                            ServerLanguageError::LastRequestCanceled => {
                                // Find last request and cancel it.
                                match async_messages_queue
                                    .iter()
                                    .rev()
                                    .position(|m| m.is_request())
                                {
                                    Some(last_request_index_from_end) => {
                                        let last_request_index = async_messages_queue.len()
                                            - last_request_index_from_end
                                            - 1;
                                        let canceled_request =
                                            async_messages_queue.remove(last_request_index);
                                        info!(
                                            "Request #{} has been cancelled.",
                                            canceled_request.get_request_id()
                                        );
                                        self.connection.send_response_error(
                                            canceled_request.get_request_id().clone(),
                                            ErrorCode::RequestCanceled,
                                            format!(
                                                "Request {} for file {:?} has been canceled",
                                                canceled_request.get_request_method(),
                                                canceled_request.get_uri()
                                            ),
                                        );
                                    }
                                    // Couldn't cancel last request as its probably been flushed from queue. Ignore it.
                                    None => {}
                                }
                            }
                            _ => self.connection.send_notification_error(err.to_string()),
                        },
                    },
                },
                Err(err) => match err {
                    RecvTimeoutError::Timeout => {
                        async_messages_queue.retain(|m| !matches!(m, AsyncMessage::None));
                        if async_messages_queue.len() > 0 {
                            profile_scope!(
                                "Processing queue with {} message(s)",
                                async_messages_queue.len()
                            );
                            // Now that we do not have messages in queue, batch them.
                            let mut async_queue: Vec<AsyncMessage> =
                                async_messages_queue.drain(..).collect();
                            let async_update_queue: Vec<AsyncMessage> =
                                async_queue.extract_if(.., |m| m.is_update()).collect();
                            let async_request_queue = async_queue;
                            // First we list all unique file to update.
                            let mut files_to_update = Vec::new();
                            for async_update in async_update_queue {
                                match async_update {
                                    AsyncMessage::UpdateCache(async_cache_request) => {
                                        // Simply store update for batching.
                                        files_to_update.extend(async_cache_request);
                                    }
                                    _ => unreachable!(),
                                }
                            }
                            // Update files for all requested updates.
                            if files_to_update.len() > 0 {
                                profile_scope!(
                                    "Updating {} batched file(s).",
                                    files_to_update.len()
                                );
                                let token = lsp_types::NumberOrString::Number(0);
                                self.connection.send_request::<WorkDoneProgressCreate>(
                                    WorkDoneProgressCreateParams {
                                        token: token.clone(),
                                    },
                                    |_, _| Ok(AsyncMessage::None),
                                );
                                self.connection
                                    .send_notification::<Progress>(ProgressParams {
                                        token: token.clone(),
                                        value: lsp_types::ProgressParamsValue::WorkDone(
                                            WorkDoneProgress::Begin(WorkDoneProgressBegin {
                                                title: "Analyzing shader files".into(),
                                                cancellable: Some(false),
                                                message: Some(format!(
                                                    "Analyzing {} shader file(s) for validation and symbols.",
                                                    files_to_update.len()
                                                )),
                                                percentage: Some(0),
                                            }),
                                        ),
                                    });
                                match self.watched_files.cache_batched_file_data(
                                    files_to_update,
                                    &mut self.language_data,
                                    &self.config,
                                    |file_updating, progress, total| {
                                        self.connection.send_notification::<Progress>(
                                            ProgressParams {
                                                token: token.clone(),
                                                value: lsp_types::ProgressParamsValue::WorkDone(
                                                    WorkDoneProgress::Report(
                                                        WorkDoneProgressReport {
                                                            cancellable: Some(false),
                                                            message: Some(format!(
                                                                "{}/{} {}",
                                                                progress, total, file_updating
                                                            )),
                                                            percentage: Some(
                                                                (((progress as f32)
                                                                    / (total as f32))
                                                                    * 100.0)
                                                                    as u32,
                                                            ),
                                                        },
                                                    ),
                                                ),
                                            },
                                        );
                                    },
                                ) {
                                    Ok((files_to_clear, files_to_publish)) => {
                                        for file_to_clear in files_to_clear {
                                            self.clear_diagnostic(&file_to_clear);
                                        }
                                        for file_to_publish in files_to_publish {
                                            self.publish_diagnostic(&file_to_publish, None);
                                        }
                                    }
                                    Err(err) => self.connection.send_notification_error(format!(
                                        "Failed to update cache: {}",
                                        err
                                    )),
                                }
                                self.connection
                                    .send_notification::<Progress>(ProgressParams {
                                        token: token.clone(),
                                        value: lsp_types::ProgressParamsValue::WorkDone(
                                            WorkDoneProgress::End(WorkDoneProgressEnd {
                                                message: Some("Finished analyzing files".into()),
                                            }),
                                        ),
                                    });
                            }
                            // Solve all pending request.
                            if async_request_queue.len() > 0 {
                                profile_scope!("Solving {} request", async_request_queue.len());
                                for request in async_request_queue {
                                    fn request_id_to_i32(
                                        request_id: &lsp_server::RequestId,
                                    ) -> Option<i32> {
                                        // RequestId does not implement anything to get this other than display which use fmt for string...
                                        // So remove string delimiter from display result.
                                        let req_id_as_string = request_id.to_string();
                                        let (offset_start, offset_end) = if req_id_as_string
                                            .starts_with("\"")
                                            && req_id_as_string.ends_with("\"")
                                        {
                                            (1, req_id_as_string.len() - 1)
                                        } else if req_id_as_string.starts_with("\"") {
                                            (1, req_id_as_string.len()) // Weird...
                                        } else if req_id_as_string.ends_with("\"") {
                                            (0, req_id_as_string.len() - 1) // Weird...
                                        } else {
                                            (0, req_id_as_string.len())
                                        };
                                        req_id_as_string[offset_start..offset_end]
                                            .parse::<i32>()
                                            .ok()
                                    }
                                    let req_id = request.get_request_id().clone();
                                    match self.resolve_async_request(request) {
                                        Ok(_) => {}
                                        Err(err) => {
                                            match err {
                                                ServerLanguageError::FileNotWatched(_) => {
                                                    if let Some(req_id_i32) =
                                                        request_id_to_i32(&req_id)
                                                    {
                                                        info!("Cancelling request {}", req_id);
                                                        self.connection.send_notification::<Cancel>(CancelParams {
                                                            id: lsp_types::NumberOrString::Number(req_id_i32),
                                                        })
                                                    } else {
                                                        self.connection.send_response_error(
                                                            req_id,
                                                            shader_error_to_lsp_error(&err),
                                                            err.to_string(),
                                                        )
                                                    }
                                                }
                                                _ => self.connection.send_response_error(
                                                    req_id,
                                                    shader_error_to_lsp_error(&err),
                                                    err.to_string(),
                                                ),
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    RecvTimeoutError::Disconnected => {
                        return Ok(());
                    }
                },
            }
        }
    }
    fn debug(&self, dbg: &impl Debug) -> String {
        if self.config.is_verbose() {
            format!("{:#?}", dbg)
        } else {
            "{}".into()
        }
    }
    fn get_cachable_file(&self, uri: &Url) -> Result<&ServerFileCache, ServerLanguageError> {
        let main_file =
            self.watched_files
                .get_file(&uri)
                .ok_or(ServerLanguageError::FileNotWatched(
                    uri.to_file_path().unwrap(),
                ))?;
        debug_assert!(
            main_file.is_cachable_file(),
            "File {} is not a cachable file.",
            uri
        );
        Ok(main_file)
    }
    fn on_request(
        &mut self,
        req: lsp_server::Request,
    ) -> Result<AsyncMessage, ServerLanguageError> {
        // Simply parse the request and delay them.
        let async_request =
            match req.method.as_str() {
                DocumentDiagnosticRequest::METHOD => AsyncMessage::DocumentDiagnosticRequest(
                    AsyncRequest::new(req.id, serde_json::from_value(req.params)?),
                ),
                GotoDefinition::METHOD => AsyncMessage::GotoDefinition(AsyncRequest::new(
                    req.id,
                    serde_json::from_value(req.params)?,
                )),
                Completion::METHOD => AsyncMessage::Completion(AsyncRequest::new(
                    req.id,
                    serde_json::from_value(req.params)?,
                )),
                SignatureHelpRequest::METHOD => AsyncMessage::SignatureHelpRequest(
                    AsyncRequest::new(req.id, serde_json::from_value(req.params)?),
                ),
                HoverRequest::METHOD => AsyncMessage::HoverRequest(AsyncRequest::new(
                    req.id,
                    serde_json::from_value(req.params)?,
                )),
                InlayHintRequest::METHOD => AsyncMessage::InlayHintRequest(AsyncRequest::new(
                    req.id,
                    serde_json::from_value(req.params)?,
                )),
                FoldingRangeRequest::METHOD => AsyncMessage::FoldingRangeRequest(
                    AsyncRequest::new(req.id, serde_json::from_value(req.params)?),
                ),
                WorkspaceSymbolRequest::METHOD => AsyncMessage::WorkspaceSymbolRequest(
                    AsyncRequest::new(req.id, serde_json::from_value(req.params)?),
                ),
                DocumentSymbolRequest::METHOD => AsyncMessage::DocumentSymbolRequest(
                    AsyncRequest::new(req.id, serde_json::from_value(req.params)?),
                ),
                SemanticTokensFullRequest::METHOD => AsyncMessage::SemanticTokensFullRequest(
                    AsyncRequest::new(req.id, serde_json::from_value(req.params)?),
                ),
                Formatting::METHOD => AsyncMessage::Formatting(AsyncRequest::new(
                    req.id,
                    serde_json::from_value(req.params)?,
                )),
                RangeFormatting::METHOD => AsyncMessage::RangeFormatting(AsyncRequest::new(
                    req.id,
                    serde_json::from_value(req.params)?,
                )),
                // Debug request
                DumpAstRequest::METHOD => AsyncMessage::DumpAstRequest(AsyncRequest::new(
                    req.id,
                    serde_json::from_value(req.params)?,
                )),
                DumpDependencyRequest::METHOD => AsyncMessage::DumpDependencyRequest(
                    AsyncRequest::new(req.id, serde_json::from_value(req.params)?),
                ),
                _ => {
                    warn!("Received unhandled request: {:#?}", req);
                    return Err(ServerLanguageError::MethodNotFound(req.method));
                }
            };
        if let Some(uri) = async_request.get_uri() {
            info!("Received request {} for file {}", req.method, uri);
        } else {
            info!("Received request {}", req.method);
        }
        Ok(async_request)
    }
    fn on_response(
        &mut self,
        response: lsp_server::Response,
    ) -> Result<AsyncMessage, ServerLanguageError> {
        // Here the callback return a delayed update
        match self.connection.remove_callback(&response.id) {
            Some(callback) => match response.result {
                Some(result) => callback(self, result),
                None => Ok(AsyncMessage::None), // Received message can be empty.
            },
            None => Err(ServerLanguageError::InternalError(format!(
                "Received unhandled response: {:#?}",
                response
            ))),
        }
    }
    fn on_notification(
        &mut self,
        notification: lsp_server::Notification,
    ) -> Result<AsyncMessage, ServerLanguageError> {
        // Watch & remove file as expected and return a delayed update.
        // We still update text content & AST here as performances are OK to be done here.
        // But symbol parsing & validation is done asynchronously.
        match notification.method.as_str() {
            DidOpenTextDocument::METHOD => {
                let params: DidOpenTextDocumentParams =
                    serde_json::from_value(notification.params)?;
                let uri = clean_url(&params.text_document.uri);
                profile_scope!(
                    "Received did open text document notification for {}:{}",
                    uri,
                    self.debug(&params)
                );

                // Skip non file uri.
                if uri.scheme() != "file" {
                    // Invalid Params
                    return Err(ServerLanguageError::InvalidParams(format!(
                        "Trying to watch file with unsupported scheme : {}",
                        uri.scheme()
                    )));
                }
                let shading_language = ShadingLanguage::from_str(&params.text_document.language_id)
                    .map_err(|_| {
                        ServerLanguageError::InvalidParams(format!(
                            "Trying to watch file with unsupported langID : {}",
                            params.text_document.language_id
                        ))
                    })?;
                // Return error instead
                let language_data = self.language_data.get_mut(&shading_language).unwrap();
                let _ = self.watched_files.watch_main_file(
                    &uri,
                    shading_language.clone(),
                    &params.text_document.text,
                    &mut language_data.shader_module_parser,
                )?;
                Ok(AsyncMessage::UpdateCache(vec![AsyncCacheRequest::new(
                    uri,
                    shading_language,
                    false, // Just opened file
                )]))
            }
            DidSaveTextDocument::METHOD => {
                let params: DidSaveTextDocumentParams =
                    serde_json::from_value(notification.params)?;
                let uri = clean_url(&params.text_document.uri);
                profile_scope!(
                    "Received did save text document notification for file {}:{}",
                    uri,
                    self.debug(&params)
                );
                // File content is updated through DidChangeTextDocument.
                let cached_file = self.get_cachable_file(&uri)?;

                assert!(
                    params.text.is_none()
                        || (params.text.is_some()
                            && RefCell::borrow(&cached_file.shader_module).content
                                == *params.text.as_ref().unwrap())
                );
                // Only update cache if content changed.
                if let Some(text) = params.text {
                    if text != RefCell::borrow(&cached_file.shader_module).content {
                        let shading_language = cached_file.shading_language;
                        let language_data = self.language_data.get_mut(&shading_language).unwrap();
                        let _ = self.watched_files.update_file(
                            &uri,
                            &mut language_data.shader_module_parser,
                            None,
                            None,
                        )?;
                        Ok(AsyncMessage::UpdateCache(vec![AsyncCacheRequest::new(
                            uri,
                            shading_language,
                            true,
                        )]))
                    } else {
                        Ok(AsyncMessage::None)
                    }
                } else {
                    Ok(AsyncMessage::None)
                }
            }
            DidCloseTextDocument::METHOD => {
                let params: DidCloseTextDocumentParams =
                    serde_json::from_value(notification.params)?;
                let uri = clean_url(&params.text_document.uri);
                profile_scope!(
                    "Received did close text document notification for file {}: {}",
                    uri,
                    self.debug(&params)
                );
                let removed_urls = self.watched_files.remove_main_file(&uri)?;
                for removed_url in removed_urls {
                    self.clear_diagnostic(&removed_url);
                }
                Ok(AsyncMessage::None)
            }
            DidChangeTextDocument::METHOD => {
                let params: DidChangeTextDocumentParams =
                    serde_json::from_value(notification.params)?;
                let uri = clean_url(&params.text_document.uri);
                profile_scope!(
                    "Received did change text document notification for file {}: {}",
                    uri,
                    self.debug(&params)
                );
                let cached_file = self.get_cachable_file(&uri)?;
                let shading_language = cached_file.shading_language;
                let language_data = self.language_data.get_mut(&shading_language).unwrap();
                // Update all content before caching data.
                for content in &params.content_changes {
                    match self.watched_files.update_file(
                        &uri,
                        &mut language_data.shader_module_parser,
                        content.range,
                        Some(&content.text),
                    ) {
                        Ok(_) => {}
                        Err(err) => self.connection.send_notification_error(format!("{}", err)),
                    };
                }
                Ok(AsyncMessage::UpdateCache(vec![AsyncCacheRequest::new(
                    uri,
                    shading_language,
                    true,
                )]))
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
                Ok(AsyncMessage::None) // Its request_configuration job to return async task here.
            }
            DidChangeShaderVariant::METHOD => {
                let new_variant = self.parse_variant_params(notification.params)?;
                profile_scope!(
                    "Received did change shader variant notification for file {}: {}",
                    new_variant
                        .as_ref()
                        .map(|v| v.url.to_string())
                        .unwrap_or("None".into()),
                    self.debug(&new_variant)
                );
                if new_variant != self.watched_files.variant {
                    let updated_url = if let Some(new_variant) = &new_variant {
                        let language_data = self
                            .language_data
                            .get_mut(&new_variant.shading_language)
                            .unwrap();
                        if let Some(old_variant) = &self.watched_files.variant {
                            // Remove old variant if not used anymore.
                            if new_variant.url != old_variant.url {
                                let old_variant_url = old_variant.url.clone();
                                let old_variant_language = old_variant.shading_language;
                                // Watch new variant
                                self.watched_files.watch_variant_file(
                                    &new_variant.url,
                                    new_variant.shading_language,
                                    &mut language_data.shader_module_parser,
                                )?;
                                // Remove old one.
                                let removed_urls =
                                    self.watched_files.remove_variant_file(&old_variant_url)?;
                                for removed_url in removed_urls {
                                    self.clear_diagnostic(&removed_url);
                                }
                                vec![
                                    AsyncCacheRequest::new(
                                        new_variant.url.clone(),
                                        new_variant.shading_language,
                                        false, // Only context changed
                                    ),
                                    AsyncCacheRequest::new(
                                        old_variant_url,
                                        old_variant_language,
                                        false, // Only context changed
                                    ),
                                ]
                            } else {
                                // Simply update.
                                vec![AsyncCacheRequest::new(
                                    new_variant.url.clone(),
                                    old_variant.shading_language,
                                    false, // Only context changed
                                )]
                            }
                        } else {
                            // Watch new variant
                            self.watched_files.watch_variant_file(
                                &new_variant.url,
                                new_variant.shading_language,
                                &mut language_data.shader_module_parser,
                            )?;
                            vec![AsyncCacheRequest::new(
                                new_variant.url.clone(),
                                new_variant.shading_language,
                                false, // Only context changed
                            )]
                        }
                    } else if let Some(old_variant) = &self.watched_files.variant {
                        // Remove old variant if not used anymore.
                        let old_variant_url = old_variant.url.clone();
                        let old_variant_language = old_variant.shading_language;
                        let removed_urls =
                            self.watched_files.remove_variant_file(&old_variant_url)?;
                        for removed_url in removed_urls {
                            self.clear_diagnostic(&removed_url);
                        }
                        vec![AsyncCacheRequest::new(
                            old_variant_url,
                            old_variant_language,
                            false, // Only context changed
                        )]
                    } else {
                        unreachable!();
                    };
                    // Set new variant.
                    self.watched_files.variant = new_variant;
                    Ok(AsyncMessage::UpdateCache(updated_url))
                } else {
                    Ok(AsyncMessage::None)
                }
            }
            DidChangeWorkspaceFolders::METHOD => {
                let params: DidChangeWorkspaceFoldersParams =
                    serde_json::from_value(notification.params)?;
                profile_scope!("Received {}: {}", notification.method, self.debug(&params));
                for removed in params.event.removed {
                    if let Some(position) = self
                        .watched_files
                        .workspace_folder
                        .iter()
                        .position(|f| *f == removed.uri)
                    {
                        self.watched_files.workspace_folder.remove(position);
                        info!(
                            "Removing workspace folder {} at {}",
                            removed.name, removed.uri
                        );
                    }
                }
                for added in params.event.added {
                    info!("Adding workspace folder {} at {}", added.name, added.uri);
                    self.watched_files.workspace_folder.push(added.uri);
                }
                Ok(AsyncMessage::None)
            }
            LogTrace::METHOD => {
                warn!("Log trace should only be sent by server.");
                Ok(AsyncMessage::None)
            }
            SetTrace::METHOD => {
                let params: SetTraceParams = serde_json::from_value(notification.params)?;
                profile_scope!("Received {}: {}", notification.method, self.debug(&params));
                // Config will override this though...
                self.config.set_trace(match params.value {
                    lsp_types::TraceValue::Off => ServerTrace::new(ServerTraceLevel::Off),
                    lsp_types::TraceValue::Messages => ServerTrace::new(ServerTraceLevel::Messages),
                    lsp_types::TraceValue::Verbose => ServerTrace::new(ServerTraceLevel::Verbose),
                });
                Ok(AsyncMessage::None)
            }
            Cancel::METHOD => Err(ServerLanguageError::LastRequestCanceled),
            _ => {
                warn!(
                    "Received unhandled notification {}: {}",
                    notification.method,
                    self.debug(&notification)
                );
                // This is a notification. We can safely ignore it.
                Ok(AsyncMessage::None)
            }
        }
    }
}

/// Run the server with given config and transport.
pub fn run(
    config: ServerConfig,
    transport: Transport,
    shading_language: HashSet<ShadingLanguage>,
    initialization_errors: Vec<String>,
) {
    let mut server = match ServerLanguage::new(config, transport, shading_language) {
        Ok(server) => server,
        Err(error) => {
            error!("Server failed to create transport: {:#?}", error);
            return;
        }
    };

    match server.initialize() {
        Ok(_) => info!("Server initialization successfull"),
        Err(value) => error!("Failed initalization: {:#?}", value),
    }
    if !initialization_errors.is_empty() {
        server.connection.send_notification_error(format!(
            "Error during server initialization: \n{}",
            initialization_errors.join("\n")
        ));
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
