use crate::shader::ShaderStage;

use crate::symbols::symbol_parser::SymbolTreeFilter;
use crate::symbols::symbols::ShaderSymbolList;

pub fn get_hlsl_filters() -> Vec<Box<dyn SymbolTreeFilter>> {
    vec![Box::new(HlslStageFilter {})]
}

struct HlslStageFilter {}

impl SymbolTreeFilter for HlslStageFilter {
    fn filter_symbols(&self, shader_symbols: &mut ShaderSymbolList, file_name: &String) {
        match ShaderStage::from_file_name(file_name) {
            Some(shader_stage) => {
                *shader_symbols = ShaderSymbolList {
                    types: shader_symbols
                        .types
                        .drain(..)
                        .filter(|value| {
                            value.stages.contains(&shader_stage) || value.stages.is_empty()
                        })
                        .collect(),
                    constants: shader_symbols
                        .constants
                        .drain(..)
                        .filter(|value| {
                            value.stages.contains(&shader_stage) || value.stages.is_empty()
                        })
                        .collect(),
                    variables: shader_symbols
                        .variables
                        .drain(..)
                        .filter(|value| {
                            value.stages.contains(&shader_stage) || value.stages.is_empty()
                        })
                        .collect(),
                    functions: shader_symbols
                        .functions
                        .drain(..)
                        .filter(|value| {
                            value.stages.contains(&shader_stage) || value.stages.is_empty()
                        })
                        .collect(),
                    keywords: shader_symbols
                        .keywords
                        .drain(..)
                        .filter(|value| {
                            value.stages.contains(&shader_stage) || value.stages.is_empty()
                        })
                        .collect(),
                }
            }
            None => {
                // No filtering
            }
        }
    }
}
