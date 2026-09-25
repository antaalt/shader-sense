use std::{cell::RefCell, ffi::OsStr};

use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionItemLabelDetails, MarkupContent, Position, Url,
};

use shader_sense::{
    position::{ShaderFilePosition, ShaderPosition},
    shader::ShadingLanguage,
    shader_error::ShaderError,
    symbols::symbols::{ShaderSymbol, ShaderSymbolData, ShaderSymbolMode, ShaderSymbolType},
};

use crate::server::{common::ServerLanguageError, ServerLanguage};

impl ServerLanguage {
    pub fn recolt_completion(
        &mut self,
        uri: &Url,
        position: Position,
        trigger_character: Option<String>,
    ) -> Result<Vec<CompletionItem>, ServerLanguageError> {
        let cached_file = self.get_cachable_file(&uri)?;
        let language_data = self.get_language_data(&cached_file.shading_language)?;
        let mut symbol_list = self.watched_files.get_all_symbols(uri);
        let content = &RefCell::borrow(&cached_file.shader_module).content;
        let shader_position = {
            let position = ShaderFilePosition::new(
                cached_file.file_path.clone(),
                position.line,
                position.character,
            );
            let position_byte_offset = position.position.to_byte_offset(content)?;
            // Get UTF8 offset of trigger character
            let trigger_offset = match &trigger_character {
                Some(trigger) => match trigger.as_str() {
                    ":" => {
                        if content[..position_byte_offset].ends_with("::") {
                            2
                        } else {
                            return Ok(vec![]); // No completion for single ":"
                        }
                    }
                    "." => 1,
                    _ => trigger.len(),
                },
                None => 0,
            };
            // Remove offset
            let byte_offset = position_byte_offset - trigger_offset;
            debug_assert!(content.is_char_boundary(byte_offset));
            if byte_offset == 0 {
                ShaderPosition::from_byte_offset(content, byte_offset)?
            } else {
                let mut new_byte_offset = byte_offset;
                // Check if the previous character is ')' or ']' for getting function call / array label position
                let prev = &content[..byte_offset];
                let ranges_to_skip = [('(', ')'), ('[', ']')];
                for range_to_skip in ranges_to_skip {
                    let mut chars = prev.char_indices().rev();
                    if let Some((_, character)) = chars.next() {
                        if character == range_to_skip.1 {
                            let mut depth = 1;
                            for (idx, ch) in chars {
                                if ch == range_to_skip.1 {
                                    depth += 1
                                } else if ch == range_to_skip.0 {
                                    depth -= 1;
                                    if depth == 0 {
                                        new_byte_offset = idx;
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
                ShaderPosition::from_byte_offset(content, new_byte_offset)?
            }
        };
        let shader_file_position =
            ShaderFilePosition::from(cached_file.file_path.clone(), shader_position);
        symbol_list.retain_scoped_symbol(&shader_file_position);
        match trigger_character {
            Some(_) => {
                match language_data.symbol_provider.get_word_range_at_position(
                    &RefCell::borrow(&cached_file.shader_module),
                    &shader_file_position.position,
                ) {
                    Ok(word) => {
                        let symbols = word
                            .find_symbol_from_parent(cached_file.file_path.clone(), &symbol_list);
                        // TODO: should select right ones based on types and context
                        if symbols.is_empty() {
                            Ok(vec![])
                        } else {
                            // TODO: Could handle all symbols here
                            let symbol_type = &symbols[0];
                            let ty = match &symbol_type.data {
                                ShaderSymbolData::Variables { ty, count: _ } => {
                                    symbol_list.find_type_symbol(ty)
                                }
                                ShaderSymbolData::Functions { signatures } => {
                                    symbol_list.find_type_symbol(&signatures[0].returnType)
                                }
                                ShaderSymbolData::Parameter {
                                    context: _,
                                    ty,
                                    count: _,
                                } => symbol_list.find_type_symbol(ty),
                                ShaderSymbolData::Method {
                                    context: _,
                                    signatures,
                                } => symbol_list.find_type_symbol(&signatures[0].returnType),
                                ShaderSymbolData::Enum { values: _ } => Some(symbol_type),
                                _ => return Ok(vec![]),
                            };
                            let completion_items = match ty {
                                Some(ty) => match &ty.data {
                                    ShaderSymbolData::Enum { values } => {
                                        values.iter().map(|v| {
                                            let description = v.description.clone();
                                            let signature = if let Some(value) = &v.value {
                                                format!("{}::{} = {}", ty.label, v.label, value)
                                            } else {
                                                format!("{}::{}", ty.label, v.label)
                                            };
                                            let shading_language = cached_file.shading_language.to_string();
                                            let position = if let Some(range) = &v.range {
                                                format!(
                                                    "{}:{}:{}",
                                                    cached_file
                                                        .file_path
                                                        .file_name()
                                                        .unwrap_or(OsStr::new("file"))
                                                        .to_string_lossy(),
                                                    range.start.line,
                                                    range.start.pos
                                                )
                                            } else {
                                                "".to_string()
                                            };
                                            CompletionItem {
                                                kind: Some(CompletionItemKind::ENUM_MEMBER),
                                                label: v.label.clone(),
                                                detail: None,
                                                label_details: Some(CompletionItemLabelDetails {
                                                    detail: None,
                                                    description: Some(signature.clone())
                                                }),
                                                filter_text: Some(v.label.clone()),
                                                documentation: Some(lsp_types::Documentation::MarkupContent(MarkupContent {
                                                    kind: lsp_types::MarkupKind::Markdown,
                                                    value: format!("```{shading_language}\n{signature}\n```\n{description}\n\n{position}"),
                                                })),
                                                ..Default::default()
                                            }
                                        }).collect()
                                    },
                                    ShaderSymbolData::Struct {
                                        constructors: _,
                                        members,
                                        methods,
                                    } => {
                                        let mut members_and_methods: Vec<ShaderSymbol> = Vec::new();
                                        members_and_methods.extend(
                                            members
                                                .iter()
                                                .map(|m| {
                                                    m.as_symbol(
                                                        ty.mode
                                                            .map_runtime()
                                                            .map(|s| s.file_path.clone()),
                                                    )
                                                })
                                                .collect::<Vec<ShaderSymbol>>(),
                                        );
                                        members_and_methods.extend(
                                            methods
                                                .iter()
                                                .map(|m| {
                                                    m.as_symbol(
                                                        ty.mode
                                                            .map_runtime()
                                                            .map(|s| s.file_path.clone()),
                                                    )
                                                })
                                                .collect::<Vec<ShaderSymbol>>(),
                                        );
                                        members_and_methods
                                            .into_iter()
                                            .map(|s| {
                                                convert_completion_item(
                                                    cached_file.shading_language,
                                                    &s,
                                                )
                                            })
                                            .collect()
                                    }
                                    _ => vec![],
                                },
                                None => vec![],
                            };
                            Ok(completion_items)
                        }
                    }
                    Err(err) => {
                        if let ShaderError::NoSymbol = err {
                            Ok(vec![])
                        } else {
                            Err(err.into())
                        }
                    }
                }
            }
            None => Ok(symbol_list
                .iter()
                .filter(|symbol| !symbol.is_type(ShaderSymbolType::CallExpression))
                .map(|symbol| convert_completion_item(cached_file.shading_language, symbol))
                .collect::<Vec<CompletionItem>>()),
        }
    }
}

fn convert_completion_item(
    shading_language: ShadingLanguage,
    shader_symbol: &ShaderSymbol,
) -> CompletionItem {
    let completion_kind = match shader_symbol.get_type() {
        ShaderSymbolType::Types => CompletionItemKind::TYPE_PARAMETER,
        ShaderSymbolType::Constants => CompletionItemKind::CONSTANT,
        ShaderSymbolType::Variables => CompletionItemKind::VARIABLE,
        ShaderSymbolType::Functions => CompletionItemKind::FUNCTION,
        ShaderSymbolType::Keyword => CompletionItemKind::KEYWORD,
        ShaderSymbolType::Macros => CompletionItemKind::CONSTANT,
        ShaderSymbolType::Include => CompletionItemKind::FILE,
        ShaderSymbolType::CallExpression => {
            unreachable!("Field should be filtered out.")
        }
    };
    let doc_link = if let ShaderSymbolMode::Intrinsic(intrinsic) = &shader_symbol.mode {
        if let Some(link) = &intrinsic.link {
            debug_assert!(
                !link.is_empty(),
                "Link for {} is empty but not None.",
                shader_symbol.label
            );
            format!("\n[Online documentation]({})", link)
        } else {
            "".to_string()
        }
    } else {
        "".to_string()
    };
    let doc_signature = if let ShaderSymbolData::Functions { signatures } = &shader_symbol.data {
        // TODO: should not hide variants
        let parameters = signatures[0]
            .parameters
            .iter()
            .map(|p| format!("- `{} {}` {}", p.ty, p.label, p.description))
            .collect::<Vec<String>>();
        let parameters_markdown = if parameters.is_empty() {
            "".into()
        } else {
            format!("**Parameters:**\n\n{}", parameters.join("\n\n"))
        };
        format!(
            "\n**Return type:**\n\n`{}` {}\n\n{}",
            signatures[0].returnType, signatures[0].description, parameters_markdown
        )
    } else {
        "".to_string()
    };
    let position = if let ShaderSymbolMode::Runtime(runtime) = &shader_symbol.mode {
        format!(
            "{}:{}:{}",
            runtime
                .file_path
                .file_name()
                .unwrap_or(OsStr::new("file"))
                .to_string_lossy(),
            runtime.range.start.line,
            runtime.range.start.pos
        )
    } else {
        "".to_string()
    };
    let shading_language = shading_language.to_string();
    let description = if let ShaderSymbolMode::Intrinsic(intrinsic) = &shader_symbol.mode {
        let mut description = intrinsic.description.clone();
        let max_len = 500;
        if description.len() > max_len {
            description.truncate(max_len);
            description.push_str("...");
        }
        description
    } else {
        "".into()
    };

    let signature = shader_symbol.format();
    CompletionItem {
        kind: Some(completion_kind),
        label: shader_symbol.label.clone(),
        detail: None,
        label_details: Some(CompletionItemLabelDetails {
            detail: None,
            description: match &shader_symbol.data {
                ShaderSymbolData::Functions { signatures } => {
                    Some(if signatures.len() > 1 {
                        format!("{} (+ {})", signatures[0].format(shader_symbol.label.as_str()), signatures.len() - 1)
                    } else {
                        signatures[0].format(shader_symbol.label.as_str())
                    })
                },
                ShaderSymbolData::Method { context, signatures } => {
                    Some(if signatures.len() > 1 {
                        format!("{} (+ {})", signatures[0].format_with_context(shader_symbol.label.as_str(), context), signatures.len() - 1)
                    } else {
                        signatures[0].format(shader_symbol.label.as_str())
                    })
                },
                _ => Some(shader_symbol.format())
            }
        }),
        filter_text: Some(shader_symbol.label.clone()),
        documentation: Some(lsp_types::Documentation::MarkupContent(MarkupContent {
            kind: lsp_types::MarkupKind::Markdown,
            value: format!("```{shading_language}\n{signature}\n```\n{description}\n\n{doc_signature}\n\n{position}\n{doc_link}"),
        })),
        ..Default::default()
    }
}
