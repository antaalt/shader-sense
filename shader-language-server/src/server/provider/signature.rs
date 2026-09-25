use std::{cell::RefCell, ops::Range};

use lsp_types::{
    MarkupContent, ParameterInformation, ParameterLabel, Position, SignatureHelp,
    SignatureInformation, Url,
};

use shader_sense::{
    position::{ShaderFilePosition, ShaderFileRange, ShaderPosition},
    shader_error::ShaderError,
    symbols::symbols::{ShaderSymbolData, ShaderSymbolMode},
};

use crate::server::{common::ServerLanguageError, ServerLanguage};

impl ServerLanguage {
    pub fn recolt_signature(
        &mut self,
        uri: &Url,
        position: Position,
    ) -> Result<Option<SignatureHelp>, ServerLanguageError> {
        let cached_file = self.get_cachable_file(&uri)?;
        let shader_position = ShaderFilePosition::new(
            cached_file.file_path.clone(),
            position.line as u32,
            position.character as u32,
        );
        let language_data = self.get_language_data(&cached_file.shading_language)?;
        let mut symbol_list = self.watched_files.get_all_symbols(uri);
        symbol_list.retain_scoped_symbol(&shader_position);
        let content = &RefCell::borrow(&cached_file.shader_module).content;
        let function_parameter = self.get_function_parameter_at_position(content, position);
        let (word_range, parameter_index) =
            if let Some((function_label_range, parameter_index)) = function_parameter {
                let function_label_range = ShaderFileRange::new(
                    cached_file.file_path.clone(),
                    ShaderPosition::from_byte_offset(content, function_label_range.start)?,
                    ShaderPosition::from_byte_offset(content, function_label_range.end)?,
                );
                let word_range = language_data.symbol_provider.get_word_range_at_position(
                    &RefCell::borrow(&cached_file.shader_module),
                    &function_label_range.start(),
                );
                if let Some(parameter_index) = parameter_index {
                    (word_range, parameter_index)
                } else {
                    (word_range, 0)
                }
            } else {
                (Err(ShaderError::NoSymbol), 0)
            };
        match word_range {
            Ok(word) => {
                let matching_symbols =
                    word.find_symbol_from_parent(cached_file.file_path.clone(), &symbol_list);
                if matching_symbols.len() == 0 {
                    Ok(None)
                } else {
                    // We need to find one to fill
                    let signatures = matching_symbols
                        .iter()
                        .filter_map(|shader_symbol| {
                            let signatures = match &shader_symbol.data {
                                ShaderSymbolData::Types { constructors } => constructors,
                                ShaderSymbolData::Struct {
                                    constructors,
                                    members: _,
                                    methods: _,
                                } => constructors,
                                ShaderSymbolData::Functions { signatures } => signatures,
                                ShaderSymbolData::Method {
                                    context: _,
                                    signatures,
                                } => signatures,
                                _ => return None,
                            };
                            Some(
                                signatures
                                    .iter()
                                    .map(|signature| SignatureInformation {
                                        label: signature.format(shader_symbol.label.as_str()),
                                        documentation: Some(
                                            lsp_types::Documentation::MarkupContent(
                                                MarkupContent {
                                                    kind: lsp_types::MarkupKind::Markdown,
                                                    value: if let ShaderSymbolMode::Intrinsic(
                                                        intrinsic,
                                                    ) = &shader_symbol.mode
                                                    {
                                                        intrinsic.description.clone()
                                                    } else {
                                                        "".into()
                                                    },
                                                },
                                            ),
                                        ),
                                        parameters: Some(
                                            signature
                                                .parameters
                                                .iter()
                                                .map(|e| ParameterInformation {
                                                    label: ParameterLabel::Simple(e.label.clone()),
                                                    documentation: Some(
                                                        lsp_types::Documentation::MarkupContent(
                                                            MarkupContent {
                                                                kind:
                                                                    lsp_types::MarkupKind::Markdown,
                                                                value: e.description.clone(),
                                                            },
                                                        ),
                                                    ),
                                                })
                                                .collect(),
                                        ),
                                        active_parameter: None,
                                    })
                                    .collect::<Vec<SignatureInformation>>(),
                            )
                        })
                        .collect::<Vec<Vec<SignatureInformation>>>()
                        .concat();
                    if signatures.is_empty() {
                        Ok(None)
                    } else {
                        Ok(Some(SignatureHelp {
                            signatures: signatures,
                            active_signature: None,
                            active_parameter: Some(parameter_index), // TODO: check out of bounds.
                        }))
                    }
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
    fn get_function_parameter_at_position(
        &self,
        shader: &str,
        position: Position,
    ) -> Option<(Range<usize>, Option<u32>)> {
        let line = shader.lines().nth(position.line as usize)?;
        for capture in self.regex_parameter_cache.captures_iter(line) {
            let function_name = capture.get(1).unwrap();
            let parenthesis = capture.get(2).unwrap();
            let parameter_index = if position.character >= parenthesis.start() as u32
                && position.character <= parenthesis.end() as u32
            {
                let parameters = line[parenthesis.start()..parenthesis.end()].to_string();
                let parameters = parameters.split(',');
                let pos_in_parameters = position.character as usize - parenthesis.start();
                // Compute parameter index
                let mut parameter_index = 0;
                let mut parameter_offset = 0;
                for parameter in parameters {
                    parameter_offset += parameter.len() + 1; // Add 1 for removed comma
                    if parameter_offset > pos_in_parameters {
                        break;
                    }
                    parameter_index += 1;
                }
                Some(parameter_index)
            } else {
                None
            };
            let byte_offset = line.as_ptr() as usize - shader.as_ptr() as usize;
            if position.character >= function_name.start() as u32
                && position.character <= parenthesis.end() as u32
            {
                return Some((
                    ((byte_offset + function_name.start())..(byte_offset + function_name.end())),
                    parameter_index,
                ));
            }
        }
        // No signature
        None
    }
}
