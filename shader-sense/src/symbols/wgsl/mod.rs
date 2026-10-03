//! Parser specific for WGSL & WESL
mod wgsl_import;
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
    use std::{
        collections::HashMap,
        path::{Path, PathBuf},
    };

    use crate::{
        include::canonicalize,
        position::{ShaderFilePosition, ShaderPosition, ShaderRange},
        shader::{
            ShaderCompilationParams, ShaderContextParams, ShaderParams, ShadingLanguage,
            WgslCompilationParams, WgslShadingLanguageTag,
        },
        symbols::{
            shader_module::{ShaderModule, ShaderSymbols},
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
    fn test_wesl_regions() {
        let file_path = canonicalize(Path::new("./test/wesl/macros.wesl")).unwrap();
        let shader_content = std::fs::read_to_string(&file_path).unwrap();
        let mut shader_module_parser =
            ShaderModuleParser::from_shading_language(ShadingLanguage::Wgsl);
        let symbol_provider = SymbolProvider::from_shading_language(ShadingLanguage::Wgsl);
        let shader_module = shader_module_parser
            .create_module(&file_path, &shader_content)
            .unwrap();
        // Features are read from defines, undefined ones are disabled.
        let symbols = symbol_provider
            .query_symbols(
                &shader_module,
                ShaderParams {
                    context: ShaderContextParams {
                        defines: HashMap::from([
                            ("debug".into(), "1".into()),
                            ("debug_mode".into(), "1".into()),
                            ("legacy_implementation".into(), "true".into()),
                            ("is_web_version".into(), "0".into()),
                        ]),
                        ..Default::default()
                    },
                    ..Default::default()
                },
                &mut default_include_callback::<WgslShadingLanguageTag>,
                None,
            )
            .unwrap();
        let expected_regions = vec![
            // @if(textured) on a global variable.
            ((1, 13), (2, 54), false),
            // @if(debug) on a block of declarations.
            ((6, 10), (20, 1), true),
            // @if(debug_mode && raytracing_enabled) on a struct member.
            ((26, 39), (27, 16), false),
            // @if(legacy_implementation || (is_web_version && xyz_not_supported)) on a statement.
            ((32, 69), (33, 29), true),
            // @if(!legacy_implementation && !(is_web_version && xyz_not_supported)) on a statement.
            ((34, 71), (35, 29), false),
            // @compute @if(feature), attribute order does not matter.
            ((40, 21), (40, 35), false),
            ((42, 12), (42, 35), false),
            // @if(feature1), feature are not declarations.
            ((46, 13), (48, 1), false),
            // @if / @elif / @else chain, only first true branch is active.
            ((51, 13), (51, 32), false),
            ((52, 12), (52, 31), true),
            ((53, 5), (53, 24), false),
        ];
        let regions = &symbols.preprocessor.regions;
        println!("{:#?}", regions);
        assert_eq!(regions.len(), expected_regions.len());
        for (index, (region, (start, end, is_active))) in
            regions.iter().zip(expected_regions).enumerate()
        {
            let expected_range = ShaderRange::new(
                ShaderPosition::new(start.0, start.1),
                ShaderPosition::new(end.0, end.1),
            );
            assert_eq!(
                region.range, expected_range,
                "Wrong range for region {}",
                index
            );
            assert_eq!(
                region.is_active, is_active,
                "Wrong state for region {}",
                index
            );
        }
        // Symbols in inactive regions are filtered.
        let symbols = symbols.get_all_symbols();
        let has_variable = |label: &str| symbols.variables.iter().any(|v| v.label == label);
        assert!(has_variable("debug_buffer"));
        assert!(has_variable("MAX_DEBUG_OUTPUT"));
        assert!(!has_variable("my_texture"));
        assert!(symbols.functions.iter().any(|f| f.label == "debug_write"));
        let results: Vec<String> = symbols
            .call_expression
            .iter()
            .map(|call| call.label.clone())
            .collect();
        assert!(results.contains(&"legacy_impl".into()));
        assert!(!results.contains(&"modern_impl".into()));
        let ray = symbols.types.iter().find(|t| t.label == "Ray").unwrap();
        match &ray.data {
            ShaderSymbolData::Struct { members, .. } => {
                // Members are part of the struct symbol, so they are not filtered by regions.
                assert_eq!(members.len(), 3);
            }
            _ => panic!("Not a struct"),
        }
    }

    fn query_wesl_import_symbols(file_name: &str) -> ShaderSymbols {
        let package_root = canonicalize(Path::new("./test/wesl")).unwrap();
        let file_path = package_root.join(file_name);
        let shader_content = std::fs::read_to_string(&file_path).unwrap();
        let mut shader_module_parser =
            ShaderModuleParser::from_shading_language(ShadingLanguage::Wgsl);
        let symbol_provider = SymbolProvider::from_shading_language(ShadingLanguage::Wgsl);
        let shader_module = shader_module_parser
            .create_module(&file_path, &shader_content)
            .unwrap();
        symbol_provider
            .query_symbols(
                &shader_module,
                ShaderParams {
                    compilation: ShaderCompilationParams {
                        wgsl: WgslCompilationParams {
                            package_root: Some(package_root.clone()),
                            packages: HashMap::from([(
                                "external".into(),
                                package_root.join("external"),
                            )]),
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                &mut default_include_callback::<WgslShadingLanguageTag>,
                None,
            )
            .unwrap()
    }

    #[test]
    fn test_wesl_imports() {
        let symbols = query_wesl_import_symbols("main.wesl");
        // One include per imported module, with nested imports of lighting (super::util::math).
        let includes: Vec<&str> = symbols
            .preprocessor
            .includes
            .iter()
            .map(|include| include.get_relative_path().as_str())
            .collect();
        assert_eq!(
            includes,
            vec![
                "package::lighting::Light",
                "package::util::math::saturate_color",
                "external::helpers::to_srgb"
            ]
        );
        assert!(symbols.preprocessor.diagnostics.is_empty());
        let symbols = symbols.get_all_symbols();
        assert!(symbols.types.iter().any(|t| t.label == "Light"));
        for function in ["shade", "saturate_color", "to_srgb"] {
            assert!(
                symbols.functions.iter().any(|f| f.label == function),
                "Function {} not imported",
                function
            );
        }
        // Imported by lighting.wesl through super::
        assert!(symbols.variables.iter().any(|v| v.label == "PI"));
    }

    #[test]
    fn test_wesl_imports_missing() {
        let symbols = query_wesl_import_symbols("error_import.wesl");
        assert!(symbols.preprocessor.includes.is_empty());
        assert_eq!(symbols.preprocessor.diagnostics.len(), 1);
        let diagnostic = &symbols.preprocessor.diagnostics[0];
        assert!(diagnostic.error.contains("package::missing::foo"));
        assert_eq!(diagnostic.range.range.start.line, 1);
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
