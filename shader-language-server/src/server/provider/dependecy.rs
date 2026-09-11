use lsp_types::{request::Request, TextDocumentIdentifier, Url};
use serde::{Deserialize, Serialize};
use shader_sense::symbols::shader_module::ShaderDependencyNode;

use crate::server::{common::ServerLanguageError, ServerLanguage};

#[derive(Debug)]
pub enum DependencyTreeRequest {}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DependencyTreeParams {
    #[serde(flatten)]
    pub text_document: TextDocumentIdentifier,
}

impl Request for DependencyTreeRequest {
    type Params = DependencyTreeParams;
    type Result = Option<ShaderDependencyNode>;
    const METHOD: &'static str = "textDocument/dependencyTree";
}

impl ServerLanguage {
    pub fn recolt_dependency_tree(
        &mut self,
        uri: &Url,
    ) -> Result<ShaderDependencyNode, ServerLanguageError> {
        let cached_file = self.get_cachable_file(uri)?;
        let deps_tree = cached_file
            .data
            .as_ref()
            .unwrap()
            .symbol_cache
            .get_dependency_tree();
        Ok(deps_tree)
    }
}
