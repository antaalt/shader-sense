use std::cell::RefCell;

use lsp_types::{request::Request, TextDocumentIdentifier, Url};
use serde::{Deserialize, Serialize};

use crate::server::{common::ServerLanguageError, ServerLanguage};

#[derive(Debug)]
pub enum DumpAstRequest {}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DumpAstParams {
    #[serde(flatten)]
    pub text_document: TextDocumentIdentifier,
}

impl Request for DumpAstRequest {
    type Params = DumpAstParams;
    type Result = Option<String>;
    const METHOD: &'static str = "debug/dumpAst";
}

#[derive(Debug)]
pub enum DumpDependencyRequest {}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DumpDependencyParams {
    #[serde(flatten)]
    pub text_document: TextDocumentIdentifier,
}

impl Request for DumpDependencyRequest {
    type Params = DumpDependencyParams;
    type Result = Option<String>;
    const METHOD: &'static str = "debug/dumpDependency";
}

impl ServerLanguage {
    pub fn recolt_ast_dump(&mut self, uri: &Url) -> Result<String, ServerLanguageError> {
        let cached_file = self.get_cachable_file(&uri)?;
        let ast = RefCell::borrow(&cached_file.shader_module).dump_ast();
        Ok(ast)
    }
    pub fn recolt_dependency_dump(&mut self, uri: &Url) -> Result<String, ServerLanguageError> {
        // Check if its owned by a variant.
        let cached_file_uri = if let Some(variant) = &self.watched_files.variant {
            if let Some(variant_file) = self.watched_files.files.get(&variant.url) {
                if let Some(variant_data) = &variant_file.data {
                    let file_path = uri.to_file_path().unwrap();
                    if variant_data
                        .symbol_cache
                        .find_include(&mut |include| {
                            include.get_absolute_path().as_os_str() == file_path.as_os_str()
                        })
                        .is_some()
                    {
                        Some(variant.url.clone())
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };
        Ok(self
            .recolt_dependency_tree(&cached_file_uri.unwrap_or(uri.clone()))?
            .dump())
    }
}
