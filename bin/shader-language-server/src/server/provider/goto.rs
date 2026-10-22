use std::cell::RefCell;

use shader_sense::{
    position::{ShaderFilePosition, ShaderRange},
    shader_error::ShaderError,
    symbols::symbols::{ShaderSymbolData, ShaderSymbolMode},
};

use lsp_types::{GotoDefinitionResponse, Position, Url};

use crate::server::common::{shader_range_to_lsp_range, ServerLanguageError};
use crate::server::ServerLanguage;

impl ServerLanguage {
    pub fn recolt_goto(
        &mut self,
        uri: &Url,
        position: Position,
    ) -> Result<Option<GotoDefinitionResponse>, ServerLanguageError> {
        let cached_file = self.get_cachable_file(&uri)?;
        let language_data = self.get_language_data(&cached_file.shading_language)?;
        let shader_position = ShaderFilePosition::new(
            cached_file.file_path.clone(),
            position.line as u32,
            position.character as u32,
        );
        let symbol_list = self.watched_files.get_all_symbols(uri);
        match language_data.symbol_provider.get_word_range_at_position(
            &RefCell::borrow(&cached_file.shader_module),
            &shader_position.position,
        ) {
            Ok(word) => {
                let matching_symbols =
                    word.find_symbol_from_parent(cached_file.file_path.clone(), &symbol_list);
                Ok(Some(GotoDefinitionResponse::Link(
                    matching_symbols
                        .iter()
                        .filter_map(|symbol| {
                            if let ShaderSymbolData::Include { target } = &symbol.data {
                                match &symbol.mode {
                                    // _runtime.range here should be equal to selected_range.
                                    ShaderSymbolMode::Runtime(_runtime) => {
                                        Some(lsp_types::LocationLink {
                                            origin_selection_range: Some(
                                                shader_range_to_lsp_range(&word.get_range()),
                                            ),
                                            target_uri: Url::from_file_path(&target).unwrap(),
                                            target_range: shader_range_to_lsp_range(
                                                &ShaderRange::zero(),
                                            ),
                                            target_selection_range: shader_range_to_lsp_range(
                                                &ShaderRange::zero(),
                                            ),
                                        })
                                    }
                                    _ => None,
                                }
                            } else {
                                match &symbol.mode {
                                    ShaderSymbolMode::Runtime(runtime) => {
                                        Some(lsp_types::LocationLink {
                                            origin_selection_range: Some(
                                                shader_range_to_lsp_range(&word.get_range()),
                                            ),
                                            target_uri: Url::from_file_path(&runtime.file_path)
                                                .unwrap(),
                                            target_range: shader_range_to_lsp_range(&runtime.range),
                                            target_selection_range: shader_range_to_lsp_range(
                                                &runtime.range,
                                            ),
                                        })
                                    }
                                    _ => None,
                                }
                            }
                        })
                        .collect(),
                )))
            }
            Err(err) => {
                if let ShaderError::NoSymbol = err {
                    Ok(None)
                } else {
                    Err(err.into())
                }
            }
        }
    }
}
