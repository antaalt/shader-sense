use std::path::Path;

use crate::{
    position::{ShaderFileRange, ShaderRange},
    symbols::{
        prepocessor::{
            ShaderPreprocessor, ShaderPreprocessorContext, ShaderPreprocessorDefine,
            ShaderPreprocessorInclude,
        },
        symbol_parser::{get_name, SymbolTreePreprocessorParser},
    },
};

pub fn get_glsl_preprocessor_parser() -> Vec<Box<dyn SymbolTreePreprocessorParser>> {
    vec![
        Box::new(GlslIncludeTreePreprocessorParser {}),
        Box::new(GlslDefineTreePreprocessorParser {}),
        Box::new(GlslDefineFuncTreePreprocessorParser {}),
    ]
}
struct GlslIncludeTreePreprocessorParser {}

impl SymbolTreePreprocessorParser for GlslIncludeTreePreprocessorParser {
    fn get_query(&self) -> String {
        r#"(preproc_include
            (#include)
            path: [(string_literal)(system_lib_string)] @include
        )"#
        .into()
    }
    fn process_match(
        &self,
        symbol_match: &tree_sitter::QueryMatch,
        file_path: &Path,
        shader_content: &str,
        preprocessor: &mut ShaderPreprocessor,
        context: &mut ShaderPreprocessorContext,
    ) {
        let include_node = symbol_match.captures[0].node;
        let range =
            ShaderFileRange::from(file_path.into(), ShaderRange::from(include_node.range()));
        let relative_path = get_name(shader_content, include_node);
        let relative_path = &relative_path[1..relative_path.len() - 1];

        // Only add symbol if path can be resolved.
        match context.search_path_in_includes(Path::new(relative_path)) {
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
struct GlslDefineTreePreprocessorParser {}

impl SymbolTreePreprocessorParser for GlslDefineTreePreprocessorParser {
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
        symbol_match: &tree_sitter::QueryMatch,
        file_path: &Path,
        shader_content: &str,
        symbols: &mut ShaderPreprocessor,
        _context: &mut ShaderPreprocessorContext,
    ) {
        let identifier_node = symbol_match.captures[0].node;
        let range =
            ShaderFileRange::from(file_path.into(), ShaderRange::from(identifier_node.range()));
        let name = get_name(shader_content, identifier_node).into();
        let value = if symbol_match.captures.len() > 1 {
            Some(get_name(shader_content, symbol_match.captures[1].node).trim())
        } else {
            None
        };
        // TODO: check exist & first one / last one. Need regions aswell... Duplicate with position as key ?
        symbols.defines.push(ShaderPreprocessorDefine::new(
            name,
            range,
            value.map(|s| s.into()),
            None,
        ));
    }
}

struct GlslDefineFuncTreePreprocessorParser {}

impl SymbolTreePreprocessorParser for GlslDefineFuncTreePreprocessorParser {
    fn get_query(&self) -> String {
        r#"(preproc_function_def
            (#define)
            name: (identifier) @define.name
            parameters: (preproc_params 
                (identifier) @define.param
            )
            value: (preproc_arg) @define.value
        )"#
        .into()
    }
    fn process_match(
        &self,
        symbol_match: &tree_sitter::QueryMatch,
        file_path: &Path,
        shader_content: &str,
        symbols: &mut ShaderPreprocessor,
        _context: &mut ShaderPreprocessorContext,
    ) {
        let identifier_node = symbol_match.captures[0].node;
        let range =
            ShaderFileRange::from(file_path.into(), ShaderRange::from(identifier_node.range()));
        let name = get_name(shader_content, identifier_node).into();
        assert!(symbol_match.captures.len() >= 2);
        let arguments = symbol_match.captures[1..symbol_match.captures.len() - 1]
            .iter()
            .map(|c| get_name(shader_content, c.node).trim().into())
            .collect::<Vec<String>>();
        let value = get_name(
            shader_content,
            symbol_match.captures[symbol_match.captures.len() - 1].node,
        )
        .trim();
        symbols.defines.push(ShaderPreprocessorDefine::new(
            name,
            range,
            Some(value.into()),
            Some(arguments),
        ));
    }
}
