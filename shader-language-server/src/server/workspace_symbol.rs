use lsp_types::{SymbolInformation, SymbolKind};
use shader_sense::{shader_error::ShaderError, symbols::symbols::ShaderSymbolType};

use super::{common::shader_range_to_location, ServerLanguage};

impl ServerLanguage {
    pub fn recolt_workspace_symbol(&mut self) -> Result<Vec<SymbolInformation>, ShaderError> {
        // Might want to take active variants instead...
        // We dont browse workspace here, only opened files...
        let symbols = self
            .watched_files
            .files
            .iter()
            .filter(|(_uri, cached_file)| cached_file.is_main_file())
            .map(|(uri, cached_file)| {
                let shading_language = cached_file.shading_language;
                let symbols = self.watched_files.get_all_symbols(
                    uri,
                    &self.language_data.get(&shading_language).unwrap().language,
                );
                symbols
                    .iter_root()
                    .filter(|symbol| {
                        // For workspace, only publish function, types & macros
                        let ty = symbol.get_type();
                        match ty {
                            Some(ty) => {
                                (ty == ShaderSymbolType::Functions
                                    || ty == ShaderSymbolType::Types
                                    || ty == ShaderSymbolType::Macros)
                                    && symbol.range.is_some()
                            }
                            None => false, // skip when missing type
                        }
                    })
                    .map(|symbol| {
                        #[allow(deprecated)]
                        // https://github.com/rust-lang/rust/issues/102777
                        SymbolInformation {
                            name: symbol.label.clone(),
                            kind: match symbol.get_type() {
                                Some(ty) => match ty {
                                    ShaderSymbolType::Types => SymbolKind::TYPE_PARAMETER,
                                    ShaderSymbolType::Functions => SymbolKind::FUNCTION,
                                    ShaderSymbolType::Macros => SymbolKind::CONSTANT,
                                    _ => unreachable!("Should be filtered out"),
                                },
                                None => unreachable!("Should be filtered out"),
                            },
                            tags: None,
                            deprecated: None,
                            location: shader_range_to_location(
                                symbol.range.as_ref().expect("Should be filtered out"),
                            ),
                            container_name: Some(shading_language.to_string()),
                        }
                    })
                    .collect::<Vec<SymbolInformation>>()
            })
            .collect::<Vec<Vec<SymbolInformation>>>()
            .concat();
        Ok(symbols)
    }
}
