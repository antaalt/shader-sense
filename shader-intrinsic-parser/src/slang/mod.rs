use shader_sense::symbols::symbol_list::ShaderSymbolList;

use crate::common::IntrinsicParser;

pub struct SlangIntrinsicParser {}

impl IntrinsicParser for SlangIntrinsicParser {
    fn cache(&self, _cache_path: &str) {}
    fn parse(&self, _cache_path: &str) -> ShaderSymbolList {
        ShaderSymbolList::default() // TODO:
    }
}
