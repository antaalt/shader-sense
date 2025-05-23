use std::{
    cell::RefCell,
    io::{BufRead, BufReader},
};

use lsp_types::{
    MarkupContent, ParameterInformation, ParameterLabel, Position, SignatureHelp,
    SignatureInformation, Url,
};
use regex::Regex;

use shader_sense::{
    shader_error::ShaderError,
    symbols::symbols::{ShaderPosition, ShaderSymbol, ShaderSymbolData},
};

use super::ServerLanguage;

impl ServerLanguage {
    pub fn recolt_signature(
        &mut self,
        uri: &Url,
        position: Position,
    ) -> Result<Option<SignatureHelp>, ShaderError> {
        let cached_file = self.watched_files.get_file(uri).unwrap();
        let language_data = self
            .language_data
            .get(&cached_file.shading_language)
            .unwrap();
        // TODO: rely on symbol provider for stronger result.
        // Should simply get symbol & read parameters. Need to get parameter index though...
        let all_symbol_list = self
            .watched_files
            .get_all_symbols(uri, &language_data.language);
        let item_parameter = get_function_parameter_at_position(
            &RefCell::borrow(&cached_file.shader_module).content,
            position,
        );

        let file_path = uri.to_file_path().unwrap();
        let completion = all_symbol_list.find_symbols_defined_at(&ShaderPosition {
            file_path: file_path.clone(),
            line: position.line as u32,
            pos: position.character as u32,
        });
        let (shader_symbols, parameter_index): (Vec<&ShaderSymbol>, u32) =
            if let (Some(item_label), Some(parameter_index)) = item_parameter {
                (
                    completion
                        .into_iter()
                        .filter(|s| s.label == item_label)
                        .collect(),
                    parameter_index,
                )
            } else {
                (Vec::new(), 0)
            };
        let signatures: Vec<SignatureInformation> = shader_symbols
            .iter()
            .filter_map(|shader_symbol| {
                let functions = match &shader_symbol.data {
                    ShaderSymbolData::Functions { signatures } => signatures,
                    ShaderSymbolData::Struct {
                        constructors,
                        members: _,
                        methods: _,
                    } => constructors,
                    ShaderSymbolData::Types { constructors } => constructors,
                    _ => return None,
                };
                Some(
                    functions
                        .iter()
                        .map(|signature| SignatureInformation {
                            label: signature.format(shader_symbol.label.as_str()),
                            documentation: Some(lsp_types::Documentation::MarkupContent(
                                MarkupContent {
                                    kind: lsp_types::MarkupKind::Markdown,
                                    value: shader_symbol.description.clone(),
                                },
                            )),
                            parameters: Some(
                                signature
                                    .parameters
                                    .iter()
                                    .map(|e| ParameterInformation {
                                        label: ParameterLabel::Simple(e.label.clone()),
                                        documentation: Some(
                                            lsp_types::Documentation::MarkupContent(
                                                MarkupContent {
                                                    kind: lsp_types::MarkupKind::Markdown,
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
// This could be replaced by an API for inlay parameters.
// Function definition should store range aswell for each parameter.
// Issue is that it then rely on updated symbols which might take some time to process.
// With this, we can update completion instantly in a multithread context, using only content.
fn get_function_parameter_at_position(
    shader: &String,
    position: Position,
) -> (Option<String>, Option<u32>) {
    let reader = BufReader::new(shader.as_bytes());
    let line = reader
        .lines()
        .nth(position.line as usize)
        .expect("Text position is out of bounds")
        .expect("Could not read line");
    // Check this regex is working for all lang.
    let regex =
        Regex::new("\\b([a-zA-Z_][a-zA-Z0-9_]*)(\\(.*?)(\\))").expect("Failed to init regex");
    for capture in regex.captures_iter(line.as_str()) {
        let file_name = capture.get(1).expect("Failed to get function name");
        let parenthesis = capture.get(2).expect("Failed to get paranthesis");
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
        if position.character >= file_name.start() as u32
            && position.character <= parenthesis.end() as u32
        {
            return (
                Some(line[file_name.start()..file_name.end()].to_string()),
                parameter_index,
            );
        }
    }
    // No signature
    (None, None)
}
