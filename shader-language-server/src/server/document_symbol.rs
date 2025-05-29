use lsp_types::{DocumentSymbol, SymbolKind, Url};
use shader_sense::{
    shader_error::ShaderError,
    symbols::symbols::{ShaderRange, ShaderSymbolContent, ShaderSymbolType},
};

use super::{common::shader_range_to_location, ServerLanguage};

impl ServerLanguage {
    fn convert_to_document_symbols(&self, content: &ShaderSymbolContent) -> Vec<DocumentSymbol> {
        content
            .childrens
            .iter()
            .filter(|s| {
                // Dont publish keywords & transient.
                !s.is_type(ShaderSymbolType::Keyword)
                    && !s.get_type().map_or(false, |t| t.is_transient())
                    && s.range.is_some()
            })
            .map(|symbol| {
                let full_range = match &symbol.content {
                    Some(content) => ShaderRange::join(
                        symbol
                            .range
                            .as_ref()
                            .expect("Should be filtered out")
                            .clone(),
                        content
                            .range
                            .as_ref()
                            .expect("Should be filtered out")
                            .clone(),
                    ),
                    None => symbol
                        .range
                        .as_ref()
                        .expect("Should be filtered out")
                        .clone(),
                };
                #[allow(deprecated)]
                // https://github.com/rust-lang/rust/issues/102777
                DocumentSymbol {
                    name: symbol.label.clone(),
                    detail: Some(symbol.format()),
                    kind: match symbol.get_type().unwrap() {
                        ShaderSymbolType::Types => SymbolKind::TYPE_PARAMETER,
                        ShaderSymbolType::Constants => SymbolKind::CONSTANT,
                        ShaderSymbolType::Variables => SymbolKind::VARIABLE,
                        ShaderSymbolType::Functions => SymbolKind::FUNCTION,
                        ShaderSymbolType::Macros => SymbolKind::CONSTANT,
                        ShaderSymbolType::Include => SymbolKind::FILE,
                        ShaderSymbolType::Scope => SymbolKind::NAMESPACE, // TODO:TREE: handle scope differently. Should flatten them
                        ShaderSymbolType::Keyword | ShaderSymbolType::CallExpression => {
                            unreachable!("Field should be filtered out")
                        }
                    },
                    tags: None,
                    deprecated: None,
                    // Container range
                    range: shader_range_to_location(&full_range).range,
                    // label range
                    selection_range: shader_range_to_location(
                        symbol.range.as_ref().expect("Should be filtered out"),
                    )
                    .range,
                    children: match &symbol.content {
                        Some(content) => Some(self.convert_to_document_symbols(content)),
                        None => None,
                    },
                }
            })
            .collect()
    }
    pub fn recolt_document_symbol(
        &mut self,
        uri: &Url,
    ) -> Result<Vec<DocumentSymbol>, ShaderError> {
        let cached_file = self.watched_files.get_file(uri).unwrap();
        let symbols = cached_file.get_data().symbol_cache.get_symbol_tree();
        Ok(self.convert_to_document_symbols(&symbols.get_content()))
    }
}
