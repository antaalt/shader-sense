use std::cell::RefCell;

use lsp_types::{Hover, HoverContents, MarkupContent, Position, Url};

use shader_sense::position::ShaderFilePosition;
use shader_sense::shader_error::ShaderError;
use shader_sense::symbols::symbols::{ShaderSymbolData, ShaderSymbolMode};

use crate::server::common::{shader_range_to_lsp_range, ServerLanguageError};
use crate::server::ServerLanguage;

impl ServerLanguage {
    pub fn recolt_hover(
        &mut self,
        uri: &Url,
        position: Position,
    ) -> Result<Option<Hover>, ServerLanguageError> {
        let cached_file = self.get_cachable_file(&uri)?;
        let file_path = uri.to_file_path().unwrap();
        let shader_position = ShaderFilePosition::new(
            file_path.clone(),
            position.line as u32,
            position.character as u32,
        );
        let language_data = self
            .language_data
            .get(&cached_file.shading_language)
            .unwrap();
        match language_data.symbol_provider.get_word_range_at_position(
            &RefCell::borrow(&cached_file.shader_module),
            &shader_position.position,
        ) {
            // word_range should be the same as symbol range
            Ok(word) => {
                let symbol_list = self.watched_files.get_all_symbols(uri);
                let matching_symbols =
                    word.find_symbol_from_parent(file_path.clone(), &symbol_list);
                if matching_symbols.len() == 0 {
                    Ok(None)
                } else {
                    let symbol = &matching_symbols[0];
                    let label = symbol.format();
                    let (description, link) = match &symbol.mode {
                        ShaderSymbolMode::Intrinsic(intrinsic) => {
                            let description = intrinsic.description.clone();
                            let link = match &intrinsic.link {
                                Some(link) => format!("[Online documentation]({})", link),
                                None => "".into(),
                            };
                            (description, link)
                        }
                        ShaderSymbolMode::RuntimeContext(_) => match &symbol.data {
                            ShaderSymbolData::Macro {
                                value,
                                parameters: _,
                            } => {
                                let description = if !value.is_empty() {
                                    format!("Config macro. Expanding to \n```\n{}\n```", value)
                                } else {
                                    format!("Config macro.")
                                };
                                (description, "".into())
                            }
                            _ => ("".into(), "".into()),
                        },
                        ShaderSymbolMode::Runtime(_) => match &symbol.data {
                            ShaderSymbolData::Include { target } => {
                                let description = format!("Including file {}", target.display());
                                (description, "".into())
                            }
                            ShaderSymbolData::Macro {
                                value,
                                parameters: _,
                            } => {
                                let description = if !value.is_empty() {
                                    format!(
                                        "Preprocessor macro. Expanding to \n```\n{}\n```",
                                        value
                                    )
                                } else {
                                    format!("Preprocessor macro.")
                                };
                                (description, "".into())
                            }
                            _ => ("".into(), "".into()),
                        },
                    };
                    let location = match &symbol.mode {
                        ShaderSymbolMode::Runtime(runtime) => format!(
                            "Defined in {}, line {}",
                            if runtime.file_path.as_os_str() == file_path.as_os_str() {
                                "this file".into()
                            } else {
                                runtime.file_path.file_name().unwrap().to_string_lossy()
                            },
                            runtime.range.start.line + 1
                        ),
                        _ => "".into(),
                    };

                    Ok(Some(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: lsp_types::MarkupKind::Markdown,
                            value: format!(
                                "```{}\n{}\n```\n{}{}\n\n{}\n\n{}",
                                cached_file.shading_language.to_string(),
                                label,
                                if matching_symbols.len() > 1 {
                                    format!("(+{} symbol)\n\n", matching_symbols.len() - 1)
                                } else {
                                    "".into()
                                },
                                description,
                                location,
                                link
                            ),
                        }),
                        // Range of hovered element.
                        range: Some(shader_range_to_lsp_range(&word.get_range())),
                    }))
                }
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
