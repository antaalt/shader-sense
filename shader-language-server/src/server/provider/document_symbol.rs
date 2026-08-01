use lsp_types::{DocumentSymbol, SymbolKind, Url};
use shader_sense::symbols::symbols::{ShaderScope, ShaderSymbolMode, ShaderSymbolType};

use crate::server::common::{shader_range_to_location, ServerLanguageError};
use crate::server::ServerLanguage;

impl ServerLanguage {
    pub fn recolt_document_symbol(
        &mut self,
        uri: &Url,
    ) -> Result<Vec<DocumentSymbol>, ServerLanguageError> {
        let cached_file = self.get_cachable_file(&uri)?;
        let symbols = cached_file
            .get_data()
            .symbol_cache
            .get_local_symbols()
            .iter()
            .filter(|symbol| {
                // Dont publish keywords & transient.
                !symbol.is_type(ShaderSymbolType::Keyword)
                    && !symbol.label.trim().is_empty()
                    && !symbol.is_transient()
                    && match &symbol.mode {
                        ShaderSymbolMode::Runtime(_) => true,
                        _ => false,
                    }
            })
            .map(|symbol| {
                let label_runtime = symbol.mode.unwrap_runtime();
                // Content expected to englobe label.
                let content_range = match &label_runtime.scope {
                    Some(scope) => ShaderScope::join(scope.clone(), label_runtime.range.clone()),
                    None => label_runtime.range.clone(),
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
                        ShaderSymbolType::Keyword | ShaderSymbolType::CallExpression => {
                            unreachable!("Field should be filtered out")
                        }
                    },
                    tags: None,
                    deprecated: None,
                    range: shader_range_to_location(
                        &content_range.into_file(label_runtime.file_path.clone()),
                    )
                    .range,
                    selection_range: shader_range_to_location(
                        &label_runtime
                            .range
                            .clone()
                            .into_file(label_runtime.file_path.clone()),
                    )
                    .range,
                    children: None, // TODO: Should use a tree instead.
                }
            })
            .collect::<Vec<DocumentSymbol>>();
        Ok(symbols)
    }
}
