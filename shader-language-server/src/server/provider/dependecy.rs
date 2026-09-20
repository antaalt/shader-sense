use lsp_types::{request::Request, TextDocumentIdentifier, Url};
use serde::{Deserialize, Serialize};
use shader_sense::symbols::shader_module::ShaderDependencyNode;

use crate::server::{common::ServerLanguageError, ServerLanguage};

#[derive(Debug)]
pub enum DependencyTreeRequest {}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DependencyTreeNode {
    pub url: Url,
    pub includes: Vec<DependencyTreeNode>,
}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DependencyTreeParams {
    #[serde(flatten)]
    pub text_document: TextDocumentIdentifier,
}

impl Request for DependencyTreeRequest {
    type Params = DependencyTreeParams;
    type Result = DependencyTreeNode;
    const METHOD: &'static str = "textDocument/dependencyTree";
}

impl ServerLanguage {
    pub fn recolt_dependency_tree(
        &mut self,
        uri: &Url,
    ) -> Result<DependencyTreeNode, ServerLanguageError> {
        let cached_file = self.get_cachable_file(uri)?;
        let deps_tree = cached_file.get_data().symbol_cache.get_dependency_tree();
        // Convert path to URI for web support.
        fn convert(node: ShaderDependencyNode) -> DependencyTreeNode {
            DependencyTreeNode {
                url: Url::from_file_path(node.path).unwrap(),
                includes: node.includes.into_iter().map(|i| convert(i)).collect(),
            }
        }
        Ok(convert(deps_tree))
    }
}
