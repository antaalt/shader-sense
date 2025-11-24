use std::cell::RefCell;
use std::num::NonZero;

use log::warn;
use lru::LruCache;
use lsp_types::{SemanticToken, SemanticTokens, SemanticTokensResult, Url};
use shader_sense::symbols::symbols::ShaderSymbolMode;
use shader_sense::{position::ShaderPosition, symbols::symbols::ShaderSymbolData};

use crate::server::common::ServerLanguageError;
use crate::server::ServerLanguage;

impl ServerLanguage {
    fn get_regex<'a>(
        label: &String,
        regex_cache: &'a mut LruCache<String, regex::Regex>,
    ) -> &'a regex::Regex {
        regex_cache.get_or_insert(label.clone(), || {
            regex::Regex::new(format!("\\b({})\\b", regex::escape(label)).as_str()).unwrap()
        })
    }
    fn find_macros(&mut self, uri: &Url) -> Vec<SemanticToken> {
        let cached_file = self.watched_files.files.get(uri).unwrap();
        let symbols = self.watched_files.get_all_symbols(&uri);
        let file_path = uri.to_file_path().unwrap();
        let content = &RefCell::borrow(&cached_file.shader_module).content;
        symbols
            .macros
            .iter()
            .map(|symbol| {
                let byte_offset_start = match &symbol.mode {
                    ShaderSymbolMode::Runtime(runtime) => {
                        if runtime.file_path.as_os_str() == file_path.as_os_str() {
                            runtime.range.start.to_byte_offset(content).unwrap()
                        } else {
                            match cached_file
                                .data
                                .as_ref()
                                .unwrap()
                                .symbol_cache
                                .find_direct_includer(&runtime.file_path)
                            {
                                Some(include) => {
                                    include.get_range().start.to_byte_offset(content).unwrap()
                                }
                                None => 0, // Included from another file, but not found...
                            }
                        }
                    }
                    _ => 0, // Not runtime means no range means its everywhere
                };
                // Need to ignore comment aswell... Might need tree sitter instead.
                // Looking for preproc_arg & identifier might be enough.
                // Need to check for regions too...
                let reg = Self::get_regex(&symbol.label, &mut self.regex_cache);
                let word_byte_offsets: Vec<usize> = reg
                    .captures_iter(&content[byte_offset_start..])
                    .map(|e| e.get(0).unwrap().range().start + byte_offset_start)
                    .collect();
                word_byte_offsets
                    .iter()
                    .filter_map(|byte_offset| {
                        match ShaderPosition::from_byte_offset(&content, *byte_offset) {
                            Ok(position) => {
                                Some(SemanticToken {
                                    delta_line: position.line,
                                    delta_start: position.pos,
                                    length: symbol.label.len() as u32,
                                    token_type: 0, // SemanticTokenType::MACRO, view registration
                                    token_modifiers_bitset: 0,
                                })
                            }
                            Err(_) => None,
                        }
                    })
                    .collect()
            })
            .collect::<Vec<Vec<SemanticToken>>>()
            .concat()
    }
    fn find_enum(&mut self, uri: &Url) -> Vec<SemanticToken> {
        let cached_file = self.watched_files.files.get(uri).unwrap();
        let symbols = self.watched_files.get_all_symbols(&uri);
        let file_path = uri.to_file_path().unwrap();
        let content = &RefCell::borrow(&cached_file.shader_module).content;
        symbols
            .types
            .iter()
            .filter_map(|symbol| match &symbol.data {
                ShaderSymbolData::Enum { values } => {
                    let byte_offset_start = match &symbol.mode {
                        ShaderSymbolMode::Runtime(runtime) => {
                            if runtime.file_path.as_os_str() == file_path.as_os_str() {
                                runtime.range.start.to_byte_offset(content).unwrap()
                            } else {
                                match cached_file
                                    .data
                                    .as_ref()
                                    .unwrap()
                                    .symbol_cache
                                    .find_direct_includer(&runtime.file_path)
                                {
                                    Some(include) => {
                                        include.get_range().start.to_byte_offset(content).unwrap()
                                    }
                                    None => 0, // Included from another file, but not found...
                                }
                            }
                        }
                        _ => 0, // Not runtime means no range means its everywhere
                    };
                    let mut tokens = Vec::new();
                    // Add enum label aswell.
                    let reg = Self::get_regex(&symbol.label, &mut self.regex_cache);
                    let word_byte_offsets: Vec<usize> = reg
                        .captures_iter(&content[byte_offset_start..])
                        .map(|c| c.get(0).unwrap().range().start + byte_offset_start)
                        .collect();
                    tokens.extend(
                        word_byte_offsets
                            .iter()
                            .filter_map(|byte_offset| {
                                match ShaderPosition::from_byte_offset(&content, *byte_offset) {
                                    Ok(position) => {
                                        Some(SemanticToken {
                                            delta_line: position.line,
                                            delta_start: position.pos,
                                            length: symbol.label.len() as u32,
                                            token_type: 3, // SemanticTokenType::ENUM, view registration
                                            token_modifiers_bitset: 0,
                                        })
                                    }
                                    Err(_) => None,
                                }
                            })
                            .collect::<Vec<SemanticToken>>(),
                    );
                    // Collect enum member now.
                    for value in values {
                        let reg = Self::get_regex(&value.label, &mut self.regex_cache);
                        let word_byte_offsets: Vec<usize> = reg
                            .captures_iter(&content[byte_offset_start..])
                            .map(|e| e.get(0).unwrap().range().start + byte_offset_start)
                            .collect();
                        tokens.extend(
                            word_byte_offsets
                                .iter()
                                .filter_map(|byte_offset| {
                                    match ShaderPosition::from_byte_offset(&content, *byte_offset) {
                                        Ok(position) => {
                                            Some(SemanticToken {
                                                delta_line: position.line,
                                                delta_start: position.pos,
                                                length: value.label.len() as u32,
                                                token_type: 2, // SemanticTokenType::ENUM_MEMBER, view registration
                                                token_modifiers_bitset: 0,
                                            })
                                        }
                                        Err(_) => None,
                                    }
                                })
                                .collect::<Vec<SemanticToken>>(),
                        );
                    }
                    Some(tokens)
                }
                _ => None,
            })
            .collect::<Vec<Vec<SemanticToken>>>()
            .concat()
    }
    fn find_parameters_variables(&mut self, uri: &Url) -> Vec<SemanticToken> {
        let cached_file = self.watched_files.files.get(uri).unwrap();
        let symbols = self.watched_files.get_all_symbols(&uri);
        let file_path = uri.to_file_path().unwrap();
        let content = &RefCell::borrow(&cached_file.shader_module).content;
        symbols
            .functions
            .iter()
            .map(|symbol| {
                let mut tokens = Vec::new();
                // If we own a scope and have a range.
                if let ShaderSymbolMode::Runtime(runtime) = &symbol.mode {
                    if let Some(scope) = &runtime.scope {
                        if runtime.file_path.as_os_str() == file_path.as_os_str() {
                            let content_start = scope.start.to_byte_offset(&content).unwrap();
                            let content_end = scope.end.to_byte_offset(&content).unwrap();
                            match &symbol.data {
                                ShaderSymbolData::Functions { signatures } => {
                                    assert!(
                                        signatures.len() == 1,
                                        "Should have only one signature"
                                    );
                                    for parameter in &signatures[0].parameters {
                                        match &parameter.range {
                                            Some(range) => tokens.push(SemanticToken {
                                                delta_line: range.start.line,
                                                delta_start: range.start.pos,
                                                length: parameter.label.len() as u32,
                                                token_type: 1, // SemanticTokenType::PARAMETER, view registration
                                                token_modifiers_bitset: 0,
                                            }),
                                            None => continue, // Should not happen for local symbol, but skip it to be sure...
                                        }
                                        // Push occurences in scope
                                        let reg = Self::get_regex(
                                            &parameter.label,
                                            &mut self.regex_cache,
                                        );
                                        let word_byte_offsets: Vec<usize> = reg
                                            .captures_iter(&content[content_start..content_end])
                                            .map(|e| {
                                                e.get(0).unwrap().range().start + content_start
                                            })
                                            .collect();
                                        tokens.extend(
                                            word_byte_offsets
                                                .iter()
                                                .filter_map(|byte_offset| {
                                                    if *byte_offset > 0
                                                        && &content[byte_offset - 1..*byte_offset]
                                                            == "."
                                                    {
                                                        None // Skip struct param with same name.
                                                    } else {
                                                        match ShaderPosition::from_byte_offset(
                                                            &content,
                                                            *byte_offset,
                                                        ) {
                                                            Ok(position) => {
                                                                Some(SemanticToken {
                                                                    delta_line: position.line,
                                                                    delta_start: position.pos,
                                                                    length: parameter.label.len()
                                                                        as u32,
                                                                    token_type: 1, // SemanticTokenType::PARAMETER, view registration
                                                                    token_modifiers_bitset: 0,
                                                                })
                                                            }
                                                            Err(_) => None,
                                                        }
                                                    }
                                                })
                                                .collect::<Vec<SemanticToken>>(),
                                        );
                                    }
                                }
                                _ => {} // Nothing to push
                            }
                        } else {
                            // Nothing to push
                        }
                    } else {
                        // Nothing to push
                    }
                } else {
                    // Nothing to push
                }
                tokens
            })
            .collect::<Vec<Vec<SemanticToken>>>()
            .concat()
    }
    pub fn recolt_semantic_tokens(
        &mut self,
        uri: &Url,
    ) -> Result<SemanticTokensResult, ServerLanguageError> {
        // Ensure valid file input.
        let _cached_file = self.get_cachable_file(&uri)?;
        // Find occurences of tokens to paint
        let mut tokens = Vec::new();
        tokens.extend(self.find_macros(uri));
        tokens.extend(self.find_parameters_variables(uri));
        tokens.extend(self.find_enum(uri));

        // Increase cache size if we couldnt fit all tokens.
        if tokens.len() > self.regex_cache.cap().get() {
            let scale_factor = 1.2; // Allocate a bit more than the max.
            let max_size = 2000; // Avoid using too much memory.
            if tokens.len() <= max_size {
                let new_cap =
                    std::cmp::min((tokens.len() as f32 * scale_factor) as usize, max_size);
                warn!(
                    "Too many tokens found for single file {} ({}). Extending regex cache size to {}.",
                    uri,
                    tokens.len(),
                    new_cap
                );
                let old_cache = std::mem::replace(
                    &mut self.regex_cache,
                    LruCache::new(NonZero::new(new_cap).unwrap()),
                );
                for (old_key, old_regex) in old_cache {
                    self.regex_cache.put(old_key, old_regex);
                }
            } else {
                warn!(
                    "Too many tokens found for single file {} ({}), maximum limit {} reached.",
                    uri,
                    tokens.len(),
                    max_size
                );
            }
        }

        // Sort by positions
        tokens.sort_by(|lhs, rhs| {
            (&lhs.delta_line, &lhs.delta_start).cmp(&(&rhs.delta_line, &rhs.delta_start))
        });
        // Compute delta from position
        let mut delta_line = 0;
        let mut delta_pos = 0;
        for token in &mut tokens {
            // Reset pos on new line.
            if token.delta_line != delta_line {
                delta_pos = 0;
            }
            let line = token.delta_line;
            let pos = token.delta_start;
            token.delta_line = line - delta_line;
            token.delta_start = pos - delta_pos;
            delta_line = line;
            delta_pos = pos;
        }
        Ok(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: tokens,
        }))
    }
}
