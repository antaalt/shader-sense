//! Parser specific for WGSL & WESL
mod wgsl_parser;
mod wgsl_regions;
mod wgsl_word;
use wgsl_parser::get_wgsl_parsers;
use wgsl_regions::WgslRegionFinder;
use wgsl_word::WgslSymbolWordProvider;

use super::symbol_provider::SymbolProvider;

pub(super) fn create_wgsl_symbol_provider(
    tree_sitter_language: &tree_sitter::Language,
) -> SymbolProvider {
    SymbolProvider::new(
        tree_sitter_language,
        get_wgsl_parsers(),
        vec![],
        Box::new(WgslRegionFinder {}),
        Box::new(WgslSymbolWordProvider {}),
    )
}

#[cfg(test)]
mod tests {
    use std::path::{Path, PathBuf};

    use crate::{
        include::canonicalize,
        position::{ShaderFilePosition, ShaderPosition},
        shader::{ShaderParams, ShadingLanguage, WgslShadingLanguageTag},
        symbols::{
            shader_module::ShaderModule,
            shader_module_parser::ShaderModuleParser,
            symbol_list::ShaderSymbolList,
            symbol_provider::{default_include_callback, SymbolProvider},
            symbols::ShaderSymbolData,
        },
    };

    fn get_symbols(file_path: &Path) -> (ShaderModule, ShaderSymbolList) {
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        let mut shader_module_parser =
            ShaderModuleParser::from_shading_language(ShadingLanguage::Wgsl);
        let symbol_provider = SymbolProvider::from_shading_language(ShadingLanguage::Wgsl);
        let shader_module = shader_module_parser
            .create_module(file_path, &shader_content)
            .unwrap();
        let symbols = symbol_provider
            .query_symbols(
                &shader_module,
                ShaderParams::default(),
                &mut default_include_callback::<WgslShadingLanguageTag>,
                None,
            )
            .unwrap();
        let symbols = symbols.get_all_symbols().into();
        (shader_module, symbols)
    }

    #[test]
    fn test_wgsl_symbols() {
        let file_path = canonicalize(Path::new("./test/wgsl/symbols.wesl")).unwrap();
        let (_, symbols) = get_symbols(&file_path);
        // Functions with their parameters
        let function = symbols
            .functions
            .iter()
            .find(|f| f.label == "computeColor")
            .unwrap();
        match &function.data {
            ShaderSymbolData::Functions { signatures } => {
                assert_eq!(signatures[0].returnType, "Color");
                let parameters: Vec<(&str, &str)> = signatures[0]
                    .parameters
                    .iter()
                    .map(|p| (p.label.as_str(), p.ty.as_str()))
                    .collect();
                assert_eq!(
                    parameters,
                    vec![("input", "VertexOutput"), ("scale", "f32")]
                );
            }
            _ => panic!("Not a function"),
        }
        assert!(symbols.functions.iter().any(|f| f.label == "fs_main"));
        // Structs with their members & alias
        let material = symbols
            .types
            .iter()
            .find(|t| t.label == "Material")
            .unwrap();
        match &material.data {
            ShaderSymbolData::Struct {
                constructors,
                members,
                methods: _,
            } => {
                let members: Vec<(&str, &str)> = members
                    .iter()
                    .map(|m| (m.parameters.label.as_str(), m.parameters.ty.as_str()))
                    .collect();
                assert_eq!(members, vec![("albedo", "vec3f"), ("roughness", "f32")]);
                assert_eq!(constructors[0].parameters.len(), 2);
            }
            _ => panic!("Not a struct"),
        }
        assert!(symbols.types.iter().any(|t| t.label == "VertexOutput"));
        assert!(symbols.types.iter().any(|t| t.label == "Color"));
        // Variables, with type explicit or inferred from literals.
        for (label, expected_ty) in [
            ("PI", "f32"),
            ("exposure", "f32"),
            ("material", "Material"),
            ("albedoTexture", "texture_2d<f32>"),
            ("base", ""),
            ("result", "vec3f"),
            ("scopedValue", ""),
            ("scale", "f32"),
        ] {
            let variable = symbols
                .variables
                .iter()
                .find(|v| v.label == label)
                .unwrap_or_else(|| panic!("Variable {} not found", label));
            match &variable.data {
                ShaderSymbolData::Variables { ty, count: _ } => {
                    assert_eq!(ty, expected_ty, "Wrong type for {}", label)
                }
                _ => panic!("Not a variable"),
            }
        }
        // Call expressions, including constructors.
        for label in ["computeColor", "Color"] {
            assert!(
                symbols.call_expression.iter().any(|c| c.label == label),
                "Call expression {} not found",
                label
            );
        }
    }

    #[test]
    fn test_wgsl_scopes() {
        let file_path = canonicalize(Path::new("./test/wgsl/symbols.wesl")).unwrap();
        let (_, symbols) = get_symbols(&file_path);
        let check_scope = |line: u32, pos: u32, visibles: &[&str], not_visibles: &[&str]| {
            let symbol_list = symbols.as_ref();
            let scoped_symbols = symbol_list.filter_scoped_symbol(&ShaderFilePosition::new(
                PathBuf::from(&file_path),
                line,
                pos,
            ));
            for visible in visibles {
                assert!(
                    scoped_symbols.variables.iter().any(|v| v.label == *visible),
                    "Variable {} should be visible at {}:{}",
                    visible,
                    line,
                    pos
                );
            }
            for not_visible in not_visibles {
                assert!(
                    !scoped_symbols
                        .variables
                        .iter()
                        .any(|v| v.label == *not_visible),
                    "Variable {} should not be visible at {}:{}",
                    not_visible,
                    line,
                    pos
                );
            }
        };
        // Inside the if block of computeColor.
        check_scope(
            24,
            8,
            &[
                "scopedValue",
                "result",
                "base",
                "input",
                "scale",
                "PI",
                "material",
            ],
            &[],
        );
        // In computeColor, outside the if block.
        check_scope(26, 4, &["result", "base", "scale"], &["scopedValue"]);
        // In fs_main.
        check_scope(
            31,
            4,
            &["input", "exposure"],
            &["scopedValue", "base", "scale"],
        );
    }

    #[test]
    fn test_wgsl_words() {
        let file_path = canonicalize(Path::new("./test/wgsl/symbols.wesl")).unwrap();
        let (shader_module, symbols) = get_symbols(&file_path);
        let symbol_provider = SymbolProvider::from_shading_language(ShadingLanguage::Wgsl);
        let get_word = |line: u32, pos: u32| {
            symbol_provider
                .get_word_range_at_position(&shader_module, &ShaderPosition::new(line, pos))
                .unwrap()
        };
        // material.albedo
        let word = get_word(20, 26);
        assert_eq!(word.get_word(), "albedo");
        assert_eq!(word.get_parent().unwrap().get_word(), "material");
        // Cursor right after material
        let word = get_word(20, 23);
        assert_eq!(word.get_word(), "material");
        assert!(word.get_parent().is_none());
        // input.normal.y
        let word = get_word(23, 39);
        assert_eq!(word.get_word(), "y");
        assert_eq!(word.get_parent().unwrap().get_word(), "normal");
        assert_eq!(
            word.get_parent().unwrap().get_parent().unwrap().get_word(),
            "input"
        );
        // computeColor(...)
        let word = get_word(31, 13);
        assert_eq!(word.get_word(), "computeColor");
        assert!(word.get_parent().is_none());
        // Resolve input.normal as a member of VertexOutput.
        let word = get_word(23, 34);
        let found = word.find_symbol_from_parent(file_path.clone(), &symbols.as_ref());
        assert_eq!(found.len(), 1, "{:#?}", found);
        assert_eq!(found[0].label, "normal");
        match &found[0].data {
            ShaderSymbolData::Parameter {
                context,
                ty,
                count: _,
            } => {
                assert_eq!(context, "VertexOutput");
                assert_eq!(ty, "vec3f");
            }
            _ => panic!("Not a member"),
        }
    }
}
