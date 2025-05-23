use std::cell::RefCell;

use shader_sense::{
    shader_error::ShaderError,
    symbols::symbols::{ShaderPosition, ShaderRange, ShaderSymbolData},
};

use lsp_types::{GotoDefinitionResponse, Position, Url};

use super::{common::shader_range_to_lsp_range, ServerLanguage};

impl ServerLanguage {
    pub fn recolt_goto(
        &mut self,
        uri: &Url,
        position: Position,
    ) -> Result<Option<GotoDefinitionResponse>, ShaderError> {
        let cached_file = self.watched_files.get_file(uri).unwrap();
        let language_data = self
            .language_data
            .get(&cached_file.shading_language)
            .unwrap();
        let file_path = uri.to_file_path().unwrap();
        let shader_position = ShaderPosition {
            file_path: file_path.clone(),
            line: position.line as u32,
            pos: position.character as u32,
        };
        let all_symbol_list = self
            .watched_files
            .get_all_symbols(uri, &language_data.language);
        match language_data.symbol_provider.get_word_range_at_position(
            &RefCell::borrow(&cached_file.shader_module),
            &shader_position,
        ) {
            Ok((word, word_range)) => {
                let symbol_list = all_symbol_list.find_symbols_defined_at(&shader_position);
                Ok(Some(GotoDefinitionResponse::Link(
                    symbol_list
                        .iter()
                        .filter_map(|symbol| {
                            if symbol.label == word {
                                if let ShaderSymbolData::Link { target } = &symbol.data {
                                    match &symbol.range {
                                        // _range here should be equal to selected_range.
                                        Some(_range) => Some(lsp_types::LocationLink {
                                            origin_selection_range: Some(
                                                shader_range_to_lsp_range(&word_range),
                                            ),
                                            target_uri: Url::from_file_path(&target.file_path)
                                                .unwrap(),
                                            target_range: shader_range_to_lsp_range(
                                                &ShaderRange::new(target.clone(), target.clone()),
                                            ),
                                            target_selection_range: shader_range_to_lsp_range(
                                                &ShaderRange::new(target.clone(), target.clone()),
                                            ),
                                        }),
                                        None => None,
                                    }
                                } else {
                                    match &symbol.range {
                                        Some(range) => Some(lsp_types::LocationLink {
                                            origin_selection_range: Some(
                                                shader_range_to_lsp_range(&word_range),
                                            ),
                                            target_uri: Url::from_file_path(&range.start.file_path)
                                                .unwrap(),
                                            target_range: shader_range_to_lsp_range(range),
                                            target_selection_range: shader_range_to_lsp_range(
                                                range,
                                            ),
                                        }),
                                        None => None,
                                    }
                                }
                            } else {
                                None
                            }
                        })
                        .collect(),
                )))
            }
            Err(err) => {
                if let ShaderError::NoSymbol = err {
                    Ok(None)
                } else {
                    Err(err)
                }
            }
        }
    }
}
