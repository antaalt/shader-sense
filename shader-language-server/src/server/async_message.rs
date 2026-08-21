use lsp_server::RequestId;
use lsp_types::{
    request::{
        Completion, DocumentDiagnosticRequest, DocumentSymbolRequest, FoldingRangeRequest,
        Formatting, GotoDefinition, HoverRequest, InlayHintRequest, RangeFormatting, Request,
        SemanticTokensFullRequest, SignatureHelpRequest, WorkspaceSymbolRequest,
    },
    CompletionParams, DocumentDiagnosticParams, DocumentFormattingParams,
    DocumentRangeFormattingParams, DocumentSymbolParams, FoldingRangeParams, GotoDefinitionParams,
    HoverParams, InlayHintParams, SemanticTokensParams, SignatureHelpParams, Url,
    WorkspaceSymbolParams,
};
use shader_sense::shader::ShadingLanguage;

use crate::server::{
    clean_url,
    debug::{DumpAstParams, DumpAstRequest, DumpDependencyParams, DumpDependencyRequest},
    provider::compilation::{CompilationRequest, CompilationRequestParams},
};

pub struct AsyncRequest<R: Request> {
    pub req_id: RequestId,
    pub params: R::Params,
}
#[derive(PartialEq, Eq, Hash)]
pub struct AsyncCacheRequest {
    pub url: Url,
    pub shading_language: ShadingLanguage,
    pub dirty: bool,
    //version: u32,
}
pub enum AsyncMessage {
    None,
    // All notification and content changes are processed intantaneously.
    // Cache request are enqueued with version.
    UpdateCache(Vec<AsyncCacheRequest>),

    // All request available.
    DocumentSymbolRequest(AsyncRequest<DocumentSymbolRequest>),
    WorkspaceSymbolRequest(AsyncRequest<WorkspaceSymbolRequest>),
    RangeFormatting(AsyncRequest<RangeFormatting>),
    Formatting(AsyncRequest<Formatting>),
    SemanticTokensFullRequest(AsyncRequest<SemanticTokensFullRequest>),
    FoldingRangeRequest(AsyncRequest<FoldingRangeRequest>),
    InlayHintRequest(AsyncRequest<InlayHintRequest>),
    HoverRequest(AsyncRequest<HoverRequest>),
    SignatureHelpRequest(AsyncRequest<SignatureHelpRequest>),
    Completion(AsyncRequest<Completion>),
    GotoDefinition(AsyncRequest<GotoDefinition>),
    DocumentDiagnosticRequest(AsyncRequest<DocumentDiagnosticRequest>),
    CompilationRequest(AsyncRequest<CompilationRequest>),
    // Debug
    DumpDependencyRequest(AsyncRequest<DumpDependencyRequest>),
    DumpAstRequest(AsyncRequest<DumpAstRequest>),
}

impl AsyncCacheRequest {
    pub fn new(url: Url, shading_language: ShadingLanguage, dirty: bool) -> Self {
        Self {
            url,
            shading_language,
            dirty,
            //version: 0,
        }
    }
}
impl AsyncMessage {
    pub fn is_update(&self) -> bool {
        matches!(self, Self::None | Self::UpdateCache(_))
    }
    pub fn is_request(&self) -> bool {
        !self.is_update()
    }
    pub fn get_request_id(&self) -> &RequestId {
        assert!(self.is_request());
        match self {
            AsyncMessage::DocumentSymbolRequest(async_request) => &async_request.req_id,
            AsyncMessage::WorkspaceSymbolRequest(async_request) => &async_request.req_id,
            AsyncMessage::RangeFormatting(async_request) => &async_request.req_id,
            AsyncMessage::Formatting(async_request) => &async_request.req_id,
            AsyncMessage::SemanticTokensFullRequest(async_request) => &async_request.req_id,
            AsyncMessage::FoldingRangeRequest(async_request) => &async_request.req_id,
            AsyncMessage::InlayHintRequest(async_request) => &async_request.req_id,
            AsyncMessage::HoverRequest(async_request) => &async_request.req_id,
            AsyncMessage::SignatureHelpRequest(async_request) => &async_request.req_id,
            AsyncMessage::Completion(async_request) => &async_request.req_id,
            AsyncMessage::GotoDefinition(async_request) => &async_request.req_id,
            AsyncMessage::DocumentDiagnosticRequest(async_request) => &async_request.req_id,
            AsyncMessage::CompilationRequest(async_request) => &async_request.req_id,
            AsyncMessage::DumpDependencyRequest(async_request) => &async_request.req_id,
            AsyncMessage::DumpAstRequest(async_request) => &async_request.req_id,
            // These variants do not have a RequestId
            AsyncMessage::None | AsyncMessage::UpdateCache(_) => {
                unreachable!("Should not be reached. Update AsyncMessage::is_update accordingly.");
            }
        }
    }
    pub fn get_request_method(&self) -> &'static str {
        assert!(!self.is_update());
        match self {
            AsyncMessage::DocumentSymbolRequest(_) => DocumentSymbolRequest::METHOD,
            AsyncMessage::WorkspaceSymbolRequest(_) => WorkspaceSymbolRequest::METHOD,
            AsyncMessage::RangeFormatting(_) => RangeFormatting::METHOD,
            AsyncMessage::Formatting(_) => Formatting::METHOD,
            AsyncMessage::SemanticTokensFullRequest(_) => SemanticTokensFullRequest::METHOD,
            AsyncMessage::FoldingRangeRequest(_) => FoldingRangeRequest::METHOD,
            AsyncMessage::InlayHintRequest(_) => InlayHintRequest::METHOD,
            AsyncMessage::HoverRequest(_) => HoverRequest::METHOD,
            AsyncMessage::SignatureHelpRequest(_) => SignatureHelpRequest::METHOD,
            AsyncMessage::Completion(_) => Completion::METHOD,
            AsyncMessage::GotoDefinition(_) => GotoDefinition::METHOD,
            AsyncMessage::DocumentDiagnosticRequest(_) => DocumentDiagnosticRequest::METHOD,
            AsyncMessage::CompilationRequest(_) => CompilationRequest::METHOD,
            AsyncMessage::DumpDependencyRequest(_) => DumpDependencyRequest::METHOD,
            AsyncMessage::DumpAstRequest(_) => DumpAstRequest::METHOD,
            // These variants do not have a method
            AsyncMessage::None | AsyncMessage::UpdateCache(_) => {
                unreachable!("Should not be reached. Update AsyncMessage::is_update accordingly.");
            }
        }
    }
    pub fn get_uri(&self) -> Option<&Url> {
        match self {
            AsyncMessage::DocumentSymbolRequest(async_request) => {
                Some(&async_request.params.text_document.uri)
            }
            AsyncMessage::RangeFormatting(async_request) => {
                Some(&async_request.params.text_document.uri)
            }
            AsyncMessage::Formatting(async_request) => {
                Some(&async_request.params.text_document.uri)
            }
            AsyncMessage::SemanticTokensFullRequest(async_request) => {
                Some(&async_request.params.text_document.uri)
            }
            AsyncMessage::FoldingRangeRequest(async_request) => {
                Some(&async_request.params.text_document.uri)
            }
            AsyncMessage::InlayHintRequest(async_request) => {
                Some(&async_request.params.text_document.uri)
            }
            AsyncMessage::HoverRequest(async_request) => Some(
                &async_request
                    .params
                    .text_document_position_params
                    .text_document
                    .uri,
            ),
            AsyncMessage::SignatureHelpRequest(async_request) => Some(
                &async_request
                    .params
                    .text_document_position_params
                    .text_document
                    .uri,
            ),
            AsyncMessage::Completion(async_request) => Some(
                &async_request
                    .params
                    .text_document_position
                    .text_document
                    .uri,
            ),
            AsyncMessage::GotoDefinition(async_request) => Some(
                &async_request
                    .params
                    .text_document_position_params
                    .text_document
                    .uri,
            ),
            AsyncMessage::DocumentDiagnosticRequest(async_request) => {
                Some(&async_request.params.text_document.uri)
            }
            AsyncMessage::DumpDependencyRequest(async_request) => {
                Some(&async_request.params.text_document.uri)
            }
            AsyncMessage::DumpAstRequest(async_request) => {
                Some(&async_request.params.text_document.uri)
            }
            AsyncMessage::CompilationRequest(async_request) => {
                Some(&async_request.params.text_document.uri)
            }
            // These variants do not have a uri
            AsyncMessage::WorkspaceSymbolRequest(_) => None,
            // These variants should not have a uri
            AsyncMessage::None | AsyncMessage::UpdateCache(_) => {
                unreachable!("Should not be reached. Update AsyncMessage::is_update accordingly.");
            }
        }
    }
}

#[allow(private_bounds)] // Trait only used in this file.
impl<R: Request> AsyncRequest<R>
where
    R::Params: ParamsDeserialization,
{
    pub fn new(req_id: RequestId, mut params: R::Params) -> Self {
        params.clean();
        Self { req_id, params }
    }
}
trait ParamsDeserialization {
    fn clean(&mut self);
}
impl ParamsDeserialization for DocumentSymbolParams {
    fn clean(&mut self) {
        self.text_document.uri = clean_url(&self.text_document.uri)
    }
}
impl ParamsDeserialization for WorkspaceSymbolParams {
    fn clean(&mut self) {}
}
impl ParamsDeserialization for DocumentRangeFormattingParams {
    fn clean(&mut self) {
        self.text_document.uri = clean_url(&self.text_document.uri)
    }
}
impl ParamsDeserialization for DocumentFormattingParams {
    fn clean(&mut self) {
        self.text_document.uri = clean_url(&self.text_document.uri)
    }
}
impl ParamsDeserialization for SemanticTokensParams {
    fn clean(&mut self) {
        self.text_document.uri = clean_url(&self.text_document.uri)
    }
}
impl ParamsDeserialization for FoldingRangeParams {
    fn clean(&mut self) {
        self.text_document.uri = clean_url(&self.text_document.uri)
    }
}
impl ParamsDeserialization for InlayHintParams {
    fn clean(&mut self) {
        self.text_document.uri = clean_url(&self.text_document.uri)
    }
}
impl ParamsDeserialization for HoverParams {
    fn clean(&mut self) {
        self.text_document_position_params.text_document.uri =
            clean_url(&self.text_document_position_params.text_document.uri)
    }
}
impl ParamsDeserialization for SignatureHelpParams {
    fn clean(&mut self) {
        self.text_document_position_params.text_document.uri =
            clean_url(&self.text_document_position_params.text_document.uri)
    }
}
impl ParamsDeserialization for CompletionParams {
    fn clean(&mut self) {
        self.text_document_position.text_document.uri =
            clean_url(&self.text_document_position.text_document.uri)
    }
}
impl ParamsDeserialization for GotoDefinitionParams {
    fn clean(&mut self) {
        self.text_document_position_params.text_document.uri =
            clean_url(&self.text_document_position_params.text_document.uri)
    }
}
impl ParamsDeserialization for DocumentDiagnosticParams {
    fn clean(&mut self) {
        self.text_document.uri = clean_url(&self.text_document.uri)
    }
}
impl ParamsDeserialization for CompilationRequestParams {
    fn clean(&mut self) {
        self.text_document.uri = clean_url(&self.text_document.uri)
    }
}
impl ParamsDeserialization for DumpAstParams {
    fn clean(&mut self) {
        self.text_document.uri = clean_url(&self.text_document.uri)
    }
}
impl ParamsDeserialization for DumpDependencyParams {
    fn clean(&mut self) {
        self.text_document.uri = clean_url(&self.text_document.uri)
    }
}
