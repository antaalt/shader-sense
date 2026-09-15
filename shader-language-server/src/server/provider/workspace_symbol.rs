use lsp_types::{SymbolInformation, SymbolKind};
use shader_sense::symbols::symbols::ShaderSymbolMode;
use shader_sense::{shader_error::ShaderError, symbols::symbols::ShaderSymbolType};

use crate::server::common::shader_range_to_location;
use crate::server::ServerLanguage;

impl ServerLanguage {
    pub fn recolt_workspace_symbol(&mut self) -> Result<Vec<SymbolInformation>, ShaderError> {
        // Might want to take active variants instead...
        // We dont browse workspace here, only opened files...
        let symbols = self
            .watched_files
            .files
            .iter()
            .filter(|(_uri, cached_file)| cached_file.is_cachable_file())
            .map(|(uri, cached_file)| {
                let shading_language = cached_file.shading_language;
                let symbols = self.watched_files.get_all_symbols(uri);
                symbols
                    .iter()
                    .filter(|symbol| {
                        let ty = symbol.get_type();
                        // For workspace, only publish function, types & macros
                        (ty == ShaderSymbolType::Functions
                            || ty == ShaderSymbolType::Types
                            || ty == ShaderSymbolType::Macros)
                            && match &symbol.mode {
                                ShaderSymbolMode::Runtime(runtime) => {
                                    runtime.scope_stack.is_empty()
                                }
                                _ => false,
                            }
                    })
                    .map(|symbol| {
                        let runtime = symbol.mode.unwrap_runtime();
                        #[allow(deprecated)]
                        // https://github.com/rust-lang/rust/issues/102777
                        SymbolInformation {
                            name: symbol.label.clone(),
                            kind: match symbol.get_type() {
                                ShaderSymbolType::Types => SymbolKind::TYPE_PARAMETER,
                                ShaderSymbolType::Functions => SymbolKind::FUNCTION,
                                ShaderSymbolType::Macros => SymbolKind::CONSTANT,
                                _ => unreachable!("Should be filtered out"),
                            },
                            tags: None,
                            deprecated: None,
                            location: shader_range_to_location(
                                &runtime.range.clone().into_file(runtime.file_path.clone()),
                            ),
                            container_name: Some(shading_language.to_string()),
                        }
                    })
                    .collect::<Vec<SymbolInformation>>()
            })
            .collect::<Vec<Vec<SymbolInformation>>>()
            .concat();
        Ok(symbols)
    }
}
