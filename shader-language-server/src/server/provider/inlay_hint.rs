use lsp_types::{InlayHint, InlayHintKind, InlayHintLabel, Range, Url};

use shader_sense::symbols::symbols::{ShaderSymbolData, ShaderSymbolMode, ShaderSymbolType};

use crate::server::common::{
    lsp_range_to_shader_range, shader_position_to_lsp_position, ServerLanguageError,
};
use crate::server::ServerLanguage;

impl ServerLanguage {
    pub fn recolt_inlay_hint(
        &mut self,
        uri: &Url,
        lsp_range: &Range,
    ) -> Result<Vec<InlayHint>, ServerLanguageError> {
        // Ensure main file.
        let cached_file = self.get_cachable_file(&uri)?;
        // Get all symbols
        let symbols = self.watched_files.get_all_symbols(uri);
        let valid_range = lsp_range_to_shader_range(lsp_range);
        let inlay_hints = symbols
            .iter()
            .filter(|s| {
                s.is_type(ShaderSymbolType::CallExpression)
                    && match &s.mode {
                        ShaderSymbolMode::Runtime(runtime) => {
                            if runtime.file_path.as_os_str() == cached_file.file_path.as_os_str() {
                                valid_range.contain_bounds(&runtime.range)
                            } else {
                                false // Skip call not in main file
                            }
                        }
                        _ => false, // Should not happen with local symbols
                    }
            })
            .map(|s| match &s.data {
                ShaderSymbolData::CallExpression {
                    label,
                    range,
                    parameters,
                } => {
                    // Find label from expression.
                    // TODO: this add all includes no matter the position.
                    // Should filter them but cannot access include in SymbolsList. Need SymbolTree
                    let symbols = symbols.find_symbols_at(
                        &label,
                        &range.start.clone().into_file(cached_file.file_path.clone()),
                    );
                    for symbol in symbols {
                        // NOTE: inlay hints have a limit of 43 char per line in vscode, after which, they are truncated.
                        // https://github.com/microsoft/vscode/pull/201190
                        let functions = match &symbol.data {
                            ShaderSymbolData::Functions { signatures } => signatures,
                            ShaderSymbolData::Struct {
                                constructors,
                                members: _,
                                methods: _,
                            } => constructors,
                            ShaderSymbolData::Types { constructors } => constructors,
                            _ => &vec![],
                        };
                        match functions
                            .iter()
                            // TODO: could solve parameter type to pick correct signature.
                            .find(|s| s.parameters.len() == parameters.len())
                        {
                            Some(signature) => {
                                return parameters
                                    .iter()
                                    .enumerate()
                                    .map(|(i, (_, range))| InlayHint {
                                        position: shader_position_to_lsp_position(&range.start),
                                        label: InlayHintLabel::String(format!(
                                            "{}:",
                                            signature.parameters[i].label
                                        )),
                                        kind: Some(InlayHintKind::PARAMETER),
                                        text_edits: None,
                                        tooltip: None,
                                        padding_left: None,
                                        padding_right: Some(true),
                                        data: None,
                                    })
                                    .collect::<Vec<InlayHint>>();
                            }
                            None => continue,
                        };
                    }
                    return vec![];
                }
                _ => unreachable!("Should not be reached"),
            })
            .collect::<Vec<Vec<InlayHint>>>()
            .concat();
        Ok(inlay_hints)
    }
}
