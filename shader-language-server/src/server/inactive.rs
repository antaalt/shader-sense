use std::cell::RefCell;

use lsp_types::Url;
use shader_sense::{
    shader_error::ShaderError,
    symbols::symbols::{ShaderRange, ShaderSymbolList},
};

use super::{server_file_cache::ServerFileCacheHandle, server_language_data::ServerLanguageData};

impl ServerLanguageData {
    pub fn recolt_inactive(
        &mut self,
        _uri: &Url,
        cached_file: &ServerFileCacheHandle,
        symbol_cache: Option<&ShaderSymbolList>,
    ) -> Result<Vec<ShaderRange>, ShaderError> {
        // https://github.com/microsoft/language-server-protocol/issues/1938
        let cached_file = RefCell::borrow(cached_file);
        match self
            .symbol_provider
            .query_inactive_regions(&cached_file.symbol_tree, symbol_cache)
        {
            Ok(ranges) => Ok(ranges),
            Err(error) => Err(error),
        }
    }
}
