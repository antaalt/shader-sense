use std::path::Path;

use crate::{
    include::IncludeHandler,
    symbols::{
        symbol_parser::{get_name, SymbolTreePreprocessorParser},
        symbols::{
            ShaderPreprocessor, ShaderPreprocessorDefine, ShaderPreprocessorInclude, ShaderRange,
            ShaderSymbolParams,
        },
    },
};

pub fn get_hlsl_preprocessor_parser() -> Vec<Box<dyn SymbolTreePreprocessorParser>> {
    vec![
        Box::new(HlslIncludeTreePreprocessorParser {}),
        Box::new(HlslDefineTreePreprocessorParser {}),
    ]
}
struct HlslIncludeTreePreprocessorParser {}

impl SymbolTreePreprocessorParser for HlslIncludeTreePreprocessorParser {
    fn get_query(&self) -> String {
        // TODO: string_content unsupported on tree_sitter 0.20.9
        /*r#"(preproc_include
            (#include)
            path: (string_literal
                (string_content) @include
            )
        )"#*/
        r#"(preproc_include
            (#include)
            path: (string_literal) @include
        )"#
        .into()
    }
    fn process_match(
        &self,
        matches: tree_sitter::QueryMatch,
        file_path: &Path,
        shader_content: &str,
        symbol_params: &ShaderSymbolParams,
        preprocessor: &mut ShaderPreprocessor,
    ) {
        let include_node = matches.captures[0].node;
        let range = ShaderRange::from_range(include_node.range(), file_path.into());
        let mut include_handler = IncludeHandler::new(
            file_path,
            symbol_params.includes.clone(),
            symbol_params.path_remapping.clone(),
        );
        let relative_path = get_name(shader_content, include_node);
        let relative_path = &relative_path[1..relative_path.len() - 1]; // TODO: use string_content instead

        // Only add symbol if path can be resolved.
        match include_handler.search_path_in_includes(Path::new(relative_path)) {
            Some(absolute_path) => {
                preprocessor.includes.push(ShaderPreprocessorInclude::new(
                    relative_path.into(),
                    absolute_path,
                    range,
                ));
            }
            None => {}
        }
    }
}
struct HlslDefineTreePreprocessorParser {}

impl SymbolTreePreprocessorParser for HlslDefineTreePreprocessorParser {
    fn get_query(&self) -> String {
        r#"(preproc_def
            (#define)
            name: (identifier) @define.label
            value: (preproc_arg)? @define.value
        )"#
        .into()
    }
    fn process_match(
        &self,
        matches: tree_sitter::QueryMatch,
        file_path: &Path,
        shader_content: &str,
        _symbol_params: &ShaderSymbolParams,
        symbols: &mut ShaderPreprocessor,
    ) {
        let identifier_node = matches.captures[0].node;
        let range = ShaderRange::from_range(identifier_node.range(), file_path.into());
        let name = get_name(shader_content, identifier_node).into();
        let value = if matches.captures.len() > 1 {
            Some(get_name(shader_content, matches.captures[1].node).trim())
        } else {
            None
        };
        // TODO: check exist & first one / last one. Need regions aswell... Duplicate with position as key ?
        symbols.defines.push(ShaderPreprocessorDefine::new(
            name,
            range,
            value.map(|s| s.into()),
        ));
    }
}
