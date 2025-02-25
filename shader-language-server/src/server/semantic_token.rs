use std::cell::RefCell;

use lsp_types::{SemanticToken, SemanticTokens, SemanticTokensResult, Url};
use shader_sense::{shader_error::ShaderError, symbols::symbols::ShaderPosition};

use super::{ServerFileCacheHandle, ServerLanguage};

impl ServerLanguage {
    pub fn recolt_semantic_tokens(
        &mut self,
        uri: &Url,
        cached_file: ServerFileCacheHandle,
    ) -> Result<SemanticTokensResult, ShaderError> {
        // For now, only handle macros as we cant resolve them with textmate.
        let shading_language = RefCell::borrow(&cached_file).shading_language;
        let symbols = self.watched_files.get_all_symbols(
            &uri,
            &cached_file,
            self.language_data
                .get(&shading_language)
                .unwrap()
                .symbol_provider
                .as_ref(),
        );
        let file_path = uri.to_file_path().unwrap();
        let content = &RefCell::borrow(&cached_file).symbol_tree.content;
        // Find occurences of macros to paint them.
        let mut tokens = symbols
            .macros
            .iter()
            .map(|symbol| {
                let offset_start = match &symbol.range {
                    Some(range) => {
                        if range.start.file_path == file_path {
                            range.start.to_byte_offset(content)
                        } else {
                            // TODO: should check where include is to get offset.
                            0 // Included from another file.
                        }
                    }
                    None => 0, // No range means its everywhere
                };
                // TODO: regex instead of this to match word (ex: half4 will be caught for half...)
                // Need to ignore comment aswell... Might need tree sitter instead.
                // Looking for preproc_arg & identifier might be enough.
                // Need to check for regions too...
                let occurences: Vec<(usize, &str)> = content.match_indices(&symbol.label).collect();
                occurences
                    .iter()
                    .filter_map(|(offset, label)| {
                        let position =
                            ShaderPosition::from_byte_offset(&content, *offset, &file_path);
                        if offset_start > *offset {
                            None
                        } else {
                            Some(SemanticToken {
                                delta_line: position.line,
                                delta_start: position.pos,
                                length: label.len() as u32,
                                token_type: 0, // SemanticTokenType::MACRO, view registration
                                token_modifiers_bitset: 0,
                            })
                        }
                    })
                    .collect()
            })
            .collect::<Vec<Vec<SemanticToken>>>()
            .concat();
        // Sort by positions
        tokens.sort_by(|lhs, rhs| {
            (&lhs.delta_line, &lhs.delta_start).cmp(&(&rhs.delta_line, &rhs.delta_start))
        });
        // Compute delta from position
        let mut delta_line = 0;
        let mut delta_pos = 0;
        tokens = tokens
            .iter_mut()
            .map(|token| {
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
                token.clone() // TODO: should not need clone here.
            })
            .collect();
        Ok(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: tokens,
        }))
    }
}
