use std::cell::RefCell;

use lsp_types::{Hover, HoverContents, MarkupContent, Position, Url};

use shader_sense::{shader_error::ShaderError, symbols::symbols::ShaderPosition};

use super::{common::shader_range_to_lsp_range, ServerLanguage};

impl ServerLanguage {
    pub fn recolt_hover(
        &mut self,
        uri: &Url,
        position: Position,
    ) -> Result<Option<Hover>, ShaderError> {
        let cached_file = self.watched_files.get_file(uri).unwrap();
        let file_path = uri.to_file_path().unwrap();
        let shader_position = ShaderPosition {
            file_path: file_path.clone(),
            line: position.line as u32,
            pos: position.character as u32,
        };
        let language_data = self
            .language_data
            .get(&cached_file.shading_language)
            .unwrap();
        match language_data.symbol_provider.get_word_range_at_position(
            &RefCell::borrow(&cached_file.shader_module),
            &shader_position,
        ) {
            // word_range should be the same as symbol range
            Ok((word, word_range)) => match self.watched_files.get_file(uri) {
                Some(target_cached_file) => {
                    let all_symbol_list = self
                        .watched_files
                        .get_all_symbols(uri, &language_data.language);
                    let symbol_list = all_symbol_list.find_symbols_defined_at(&shader_position);
                    match symbol_list.iter().find(|s| s.label == word) {
                        Some(symbol) => {
                            let label = symbol.format();
                            let description = symbol.description.clone();
                            let link = match &symbol.link {
                                Some(link) => format!("[Online documentation]({})", link),
                                None => "".into(),
                            };
                            let location = match &symbol.range {
                                Some(range) => format!(
                                    "Defined in {}, line {}",
                                    if range.start.file_path == file_path {
                                        "this file".into()
                                    } else {
                                        range.start.file_path.file_name().unwrap().to_string_lossy()
                                    },
                                    range.start.line + 1
                                ),
                                None => "".into(),
                            };

                            Ok(Some(Hover {
                                contents: HoverContents::Markup(MarkupContent {
                                    kind: lsp_types::MarkupKind::Markdown,
                                    value: format!(
                                        "```{}\n{}\n```\n{}{}\n{}\n\n{}",
                                        target_cached_file.shading_language.to_string(),
                                        label,
                                        if symbol_list.len() > 1 {
                                            format!("(+{} symbol)\n\n", symbol_list.len() - 1)
                                        } else {
                                            "".into()
                                        },
                                        description,
                                        location,
                                        link
                                    ),
                                }),
                                // Range of hovered element.
                                range: if word_range.start.file_path == *file_path {
                                    Some(shader_range_to_lsp_range(&word_range))
                                } else {
                                    None
                                },
                            }))
                        }
                        None => Ok(None),
                    }
                }
                None => Ok(None),
            },
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
