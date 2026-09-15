use std::cell::RefCell;

use lsp_types::{FoldingRange, FoldingRangeKind, Url};

use crate::server::{common::ServerLanguageError, ServerLanguage};

impl ServerLanguage {
    pub fn recolt_folding_range(
        &mut self,
        uri: &Url,
    ) -> Result<Vec<FoldingRange>, ServerLanguageError> {
        let cached_file = self.get_cachable_file(&uri)?;
        // Adding regions
        let mut folding_ranges: Vec<FoldingRange> = cached_file
            .get_data()
            .symbol_cache
            .get_preprocessor()
            .regions
            .iter()
            .map(|region| FoldingRange {
                start_line: region.range.start.line,
                start_character: Some(region.range.start.pos),
                end_line: region.range.end.line,
                end_character: Some(region.range.end.pos),
                kind: Some(FoldingRangeKind::Region),
                collapsed_text: None,
            })
            .collect();
        // Adding scopes from file
        let language_data = self.get_language_data(&cached_file.shading_language)?;
        let scopes = language_data
            .symbol_provider
            .query_file_scopes(&RefCell::borrow(&cached_file.shader_module));
        let mut folded_scopes: Vec<FoldingRange> = scopes
            .iter()
            .map(|s| FoldingRange {
                start_line: s.start.line,
                start_character: Some(s.start.pos),
                end_line: s.end.line,
                end_character: Some(s.end.pos),
                kind: Some(FoldingRangeKind::Region),
                collapsed_text: None,
            })
            .collect();
        // Adding struct to scopes.
        //cached_file.data.get_symbols().iter().map(|e| e.0.iter().map(|e| match &e.data {
        //    // We dont have its range stored here...
        //    shader_sense::symbols::symbols::ShaderSymbolData::Struct { members, methods } => todo!(),
        //}));
        folding_ranges.append(&mut folded_scopes);
        Ok(folding_ranges)
    }
}
