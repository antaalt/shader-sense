//! Handle symbol inspection with [`tree_sitter`]

mod glsl;
mod hlsl;
pub mod intrinsics;
pub mod prepocessor;
pub mod shader_module;
pub mod shader_module_parser;
pub mod symbol_list;
mod symbol_parser;
pub mod symbol_provider;
pub mod symbols;
mod wgsl;

#[cfg(test)]
mod tests {
    use std::{
        collections::HashSet,
        path::{Path, PathBuf},
    };

    use regex::Regex;

    use crate::{
        include::{canonicalize, IncludeHandler},
        position::{ShaderFilePosition, ShaderFileRange, ShaderPosition},
        shader::{
            GlslShadingLanguageTag, HlslShadingLanguageTag, ShaderCompilationParams, ShaderParams,
            ShaderStage, ShadingLanguage, ShadingLanguageTag, WgslShadingLanguageTag,
        },
        shader_error::ShaderError,
        symbols::{
            intrinsics::ShaderIntrinsics, shader_module_parser::ShaderModuleParser,
            symbol_list::ShaderSymbolList, symbols::ShaderSymbolData,
        },
    };

    use super::symbol_provider::{default_include_callback, SymbolProvider};

    pub fn find_file_dependencies(
        include_handler: &mut IncludeHandler,
        shader_content: &String,
    ) -> Vec<PathBuf> {
        let include_regex = Regex::new("\\#include\\s+\"([\\w\\s\\\\/\\.\\-]+)\"").unwrap();
        let dependencies_paths: Vec<&str> = include_regex
            .captures_iter(&shader_content)
            .map(|c| c.get(1).unwrap().as_str())
            .collect();
        dependencies_paths
            .iter()
            .filter_map(|dependency| include_handler.search_path_in_includes(Path::new(dependency)))
            .collect::<Vec<PathBuf>>()
    }
    pub fn find_dependencies(
        include_handler: &mut IncludeHandler,
        shader_content: &String,
    ) -> HashSet<(String, PathBuf)> {
        let dependencies_path = find_file_dependencies(include_handler, shader_content);
        let dependencies = dependencies_path
            .into_iter()
            .map(|e| (std::fs::read_to_string(&e).unwrap(), e))
            .collect::<Vec<(String, PathBuf)>>();

        // Use hashset to avoid computing dependencies twice.
        let mut recursed_dependencies = HashSet::new();
        for dependency in &dependencies {
            recursed_dependencies.extend(find_dependencies(include_handler, &dependency.0));
        }
        recursed_dependencies.extend(dependencies);

        recursed_dependencies
    }

    fn get_all_preprocessed_symbols<T: ShadingLanguageTag>(
        shader_module_parser: &mut ShaderModuleParser,
        symbol_provider: &SymbolProvider,
        file_path: &Path,
        shader_content: &String,
    ) -> Result<ShaderSymbolList, ShaderError> {
        let mut include_handler = IncludeHandler::main_without_config(&file_path);
        let deps = find_dependencies(&mut include_handler, &shader_content);
        let mut all_symbols = ShaderIntrinsics::get(T::get_language())
            .get_intrinsics_symbol(&ShaderCompilationParams::default())
            .to_owned();
        let shader_module = shader_module_parser
            .create_module(file_path, shader_content)
            .unwrap();
        let symbols = symbol_provider
            .query_symbols(
                &shader_module,
                ShaderParams::default(),
                &mut default_include_callback::<T>,
                None,
            )
            .unwrap();
        let symbols = symbols.get_all_symbols();
        all_symbols.append(symbols.into());
        for dep in deps {
            let shader_module = shader_module_parser.create_module(&dep.1, &dep.0).unwrap();
            let symbols = symbol_provider
                .query_symbols(
                    &shader_module,
                    ShaderParams::default(),
                    &mut default_include_callback::<T>,
                    None,
                )
                .unwrap();
            let symbols = symbols.get_all_symbols();
            all_symbols.append(symbols.into());
        }
        Ok(all_symbols)
    }

    #[test]
    fn intrinsics_glsl_ok() {
        // Ensure parsing of intrinsics is OK
        let _ = ShaderSymbolList::parse_from_json(String::from(include_str!(
            "glsl/glsl-intrinsics.json"
        )));
    }
    #[test]
    fn intrinsics_hlsl_ok() {
        // Ensure parsing of intrinsics is OK
        let _ = ShaderSymbolList::parse_from_json(String::from(include_str!(
            "hlsl/hlsl-intrinsics.json"
        )));
    }
    #[test]
    fn intrinsics_hlsl_modifiers() {
        let intrinsics = ShaderSymbolList::parse_from_json(String::from(include_str!(
            "hlsl/hlsl-intrinsics.json"
        )));
        let get_modifiers = |label: &str| -> Vec<(String, Option<String>)> {
            let function = intrinsics
                .functions
                .iter()
                .find(|f| f.label == label)
                .unwrap_or_else(|| panic!("Function {} not found", label));
            match &function.data {
                ShaderSymbolData::Functions { signatures } => signatures[0]
                    .parameters
                    .iter()
                    .map(|p| (p.label.clone(), p.modifier.clone()))
                    .collect(),
                _ => panic!("{} is not a function", label),
            }
        };
        assert_eq!(
            get_modifiers("sincos"),
            vec![
                ("x".into(), None),
                ("s".into(), Some("out".into())),
                ("c".into(), Some("out".into()))
            ]
        );
        // Raytracing intrinsics are functions.
        let trace_ray = get_modifiers("TraceRay");
        assert_eq!(
            trace_ray.last(),
            Some(&("Payload".into(), Some("inout".into())))
        );
    }
    #[test]
    fn intrinsics_wgsl_ok() {
        // Ensure parsing of intrinsics is OK
        let _ = ShaderSymbolList::parse_from_json(String::from(include_str!(
            "wgsl/wgsl-intrinsics.json"
        )));
    }
    #[test]
    fn create_glsl_module_ok() {
        let mut parser = ShaderModuleParser::glsl();
        let path = Path::new("./test/glsl/ok.frag.glsl");
        let _module = parser.create_module(path, &std::fs::read_to_string(path).unwrap());
    }
    #[test]
    fn create_hlsl_module_ok() {
        let mut parser = ShaderModuleParser::hlsl();
        let path = Path::new("./test/hlsl/ok.hlsl");
        let _module = parser.create_module(path, &std::fs::read_to_string(path).unwrap());
    }
    #[test]
    fn create_wgsl_module_ok() {
        let mut parser = ShaderModuleParser::wgsl();
        let path = Path::new("./test/wgsl/ok.wgsl");
        let _module = parser.create_module(path, &std::fs::read_to_string(path).unwrap());
    }
    #[test]
    fn symbols_glsl_ok() {
        // Ensure parsing of symbols is OK
        let file_path = Path::new("./test/glsl/include-level.comp.glsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        let mut shader_module_parser =
            ShaderModuleParser::from_shading_language(ShadingLanguage::Glsl);
        let symbol_provider = SymbolProvider::from_shading_language(ShadingLanguage::Glsl);
        let shader_module = shader_module_parser
            .create_module(file_path, &shader_content)
            .unwrap();
        let symbols = symbol_provider
            .query_symbols(
                &shader_module,
                ShaderParams::default(),
                &mut default_include_callback::<GlslShadingLanguageTag>,
                None,
            )
            .unwrap();
        let symbols = symbols.get_all_symbols();
        assert!(!symbols.functions.is_empty());
    }
    #[test]
    fn symbols_hlsl_ok() {
        // Ensure parsing of symbols is OK
        let file_path = Path::new("./test/hlsl/include-level.hlsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        let mut shader_module_parser =
            ShaderModuleParser::from_shading_language(ShadingLanguage::Hlsl);
        let symbol_provider = SymbolProvider::from_shading_language(ShadingLanguage::Hlsl);
        let shader_module = shader_module_parser
            .create_module(file_path, &shader_content)
            .unwrap();
        let symbols = symbol_provider
            .query_symbols(
                &shader_module,
                ShaderParams::default(),
                &mut default_include_callback::<HlslShadingLanguageTag>,
                None,
            )
            .unwrap();
        let symbols = symbols.get_all_symbols();
        assert!(!symbols.functions.is_empty());
    }
    #[test]
    fn symbols_wgsl_ok() {
        // Ensure parsing of symbols is OK
        let file_path = Path::new("./test/wgsl/ok.wgsl");
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
        let symbols = symbols.get_all_symbols();
        assert!(symbols.functions.is_empty());
    }
    #[test]
    fn symbol_scope_glsl_ok() {
        let file_path = canonicalize(Path::new("./test/glsl/scopes.frag.glsl")).unwrap();
        let shader_content = std::fs::read_to_string(&file_path).unwrap();
        let mut shader_module_parser =
            ShaderModuleParser::from_shading_language(ShadingLanguage::Glsl);
        let symbol_provider = SymbolProvider::from_shading_language(ShadingLanguage::Glsl);
        let preprocessed_symbol_list = get_all_preprocessed_symbols::<GlslShadingLanguageTag>(
            &mut shader_module_parser,
            &symbol_provider,
            &file_path,
            &shader_content,
        )
        .unwrap();
        let symbol_list = preprocessed_symbol_list.as_ref();
        let symbols = symbol_list.filter_scoped_symbol(&ShaderFilePosition::new(
            PathBuf::from(file_path),
            16,
            0,
        ));
        let variables_visibles: Vec<String> = vec![
            "scopeRoot".into(),
            "scope1".into(),
            "scopeGlobal".into(),
            "level1".into(),
        ];
        let variables_not_visibles: Vec<String> = vec!["scope2".into(), "testData".into()];
        for variable_visible in variables_visibles {
            assert!(
                symbols
                    .variables
                    .iter()
                    .any(|e| e.label == variable_visible),
                "Failed to find variable {} {:#?}",
                variable_visible,
                symbols.variables
            );
        }
        for variable_not_visible in variables_not_visibles {
            assert!(
                !symbols
                    .variables
                    .iter()
                    .any(|e| e.label == variable_not_visible),
                "Found variable {}",
                variable_not_visible
            );
        }
    }
    #[test]
    fn uniform_glsl_ok() {
        // Ensure parsing of symbols is OK
        let file_path = Path::new("./test/glsl/uniforms.frag.glsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        let mut shader_module_parser =
            ShaderModuleParser::from_shading_language(ShadingLanguage::Glsl);
        let symbol_provider = SymbolProvider::from_shading_language(ShadingLanguage::Glsl);
        let shader_module = shader_module_parser
            .create_module(file_path, &shader_content)
            .unwrap();
        let symbols = symbol_provider
            .query_symbols(
                &shader_module,
                ShaderParams::default(),
                &mut default_include_callback::<GlslShadingLanguageTag>,
                None,
            )
            .unwrap();
        let symbols = symbols.get_all_symbols();
        assert!(symbols
            .types
            .iter()
            .find(|e| e.label == "MatrixHidden")
            .is_some());
        assert!(symbols
            .variables
            .iter()
            .find(|e| e.label == "u_accessor"
                && match &e.data {
                    ShaderSymbolData::Variables { ty, count: _ } => ty == "MatrixHidden",
                    _ => false,
                })
            .is_some());
        assert!(symbols
            .variables
            .iter()
            .find(|e| e.label == "u_modelviewGlobal")
            .is_some());
        assert!(symbols
            .variables
            .iter()
            .find(|e| e.label == "u_modelviewHidden")
            .is_none());
    }
    #[test]
    fn test_position_conversion() {
        fn test_to_byte_offset(
            shader_content: &str,
            expected_content: &str,
            position: &ShaderPosition,
        ) -> usize {
            let byte_offset = position.to_byte_offset(&shader_content).unwrap();
            if expected_content.len() > 0 {
                let content_from_offset = &shader_content[byte_offset..];
                assert!(content_from_offset.len() >= expected_content.len());
                assert!(
                    content_from_offset == expected_content,
                    "Offseted content {:?} with offset {} is incorrect.",
                    &shader_content[byte_offset..],
                    byte_offset
                );
            } else {
                assert!(byte_offset == shader_content.len());
            }
            byte_offset
        }
        fn test_back_to_position(
            shader_content: &str,
            expected_position: &ShaderPosition,
            byte_offset: usize,
        ) {
            let converted_position =
                ShaderPosition::from_byte_offset(&shader_content, byte_offset).unwrap();
            let converted_byte_offset = converted_position.to_byte_offset(&shader_content).unwrap();
            assert!(converted_position == *expected_position, "Position {:#?} with byte offset {} is different from converted position: {:#?} with byte offset {}", expected_position, byte_offset, converted_position, converted_byte_offset);
        }

        // Testing file
        let utf8_file_path = Path::new("./test/hlsl/utf8.hlsl");
        let utf8_shader_content = std::fs::read_to_string(utf8_file_path).unwrap();
        // End of line are enforced to \n through gitattributes for hlsl / glsl / wgsl in this repo.
        let test_data = vec![
            ("\n}", ShaderPosition::new(5, 0), &utf8_shader_content),
            ("", ShaderPosition::new(6, 1), &utf8_shader_content),
            (
                "id main() {\n\n}",
                ShaderPosition::new(4, 2),
                &utf8_shader_content,
            ),
            (
                "にちは世界!\n\nvoid main() {\n\n}",
                ShaderPosition::new(2, 5),
                &utf8_shader_content,
            ),
        ];
        for (index, (expected_content, position, shader_content)) in test_data.iter().enumerate() {
            println!("Testing conversion {} for {:?}", index, position);
            println!(
                "Content: {:?} (len {})",
                shader_content,
                shader_content.len()
            );
            let byte_offset = test_to_byte_offset(&shader_content, expected_content, &position);
            println!("Found byte_offset {}", byte_offset);
            test_back_to_position(&shader_content, &position, byte_offset);
        }
    }
    #[test]
    fn test_end_range() {
        let file_path = canonicalize(&Path::new("./test/hlsl/utf8.hlsl")).unwrap();
        let shader_content = std::fs::read_to_string(&file_path).unwrap();
        let range = ShaderFileRange::whole(file_path.clone(), &shader_content);
        println!("File range: {:#?}", range);
        let end_byte_offset = range.range.end.to_byte_offset(&shader_content).unwrap();
        assert!(end_byte_offset == shader_content.len());
    }
    #[test]
    fn test_intrinsic_filtering() {
        let intrinsics = ShaderIntrinsics::get(ShadingLanguage::Hlsl);
        // Check with frag stage set
        let intrinsics_frag = intrinsics.get_intrinsics_symbol(&ShaderCompilationParams {
            shader_stage: Some(ShaderStage::Fragment),
            ..Default::default()
        });
        assert!(
            intrinsics_frag.find_symbol("clip").is_some(),
            "clip() should be available from fragment shader."
        );
        // Check without stage set
        let intrinsics_common =
            intrinsics.get_intrinsics_symbol(&ShaderCompilationParams::default());
        assert!(
            intrinsics_common.find_symbol("clip").is_some(),
            "clip() should be available if no shader given."
        );
        // Check with vert stage set
        let intrinsics_vert = intrinsics.get_intrinsics_symbol(&ShaderCompilationParams {
            shader_stage: Some(ShaderStage::Vertex),
            ..Default::default()
        });
        assert!(
            intrinsics_vert.find_symbol("clip").is_none(),
            "clip() should not be available from vertex shader."
        );
    }
    #[test]
    fn test_macro_expansion_struct() {
        let file_path = Path::new("./test/hlsl/macro-struct.hlsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        let mut shader_module_parser =
            ShaderModuleParser::from_shading_language(ShadingLanguage::Hlsl);
        let symbol_provider = SymbolProvider::from_shading_language(ShadingLanguage::Hlsl);
        let shader_module = shader_module_parser
            .create_module(file_path, &shader_content)
            .unwrap();
        let symbols = symbol_provider
            .query_symbols(
                &shader_module,
                ShaderParams {
                    compilation: ShaderCompilationParams {
                        experimental_macro_expansion: true,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                &mut default_include_callback::<HlslShadingLanguageTag>,
                None,
            )
            .unwrap();
        let symbols = symbols.get_all_symbols();
        assert!(symbols
            .types
            .iter()
            .find(|t| t.label == "ProceduralTestValue0")
            .is_some());
        assert!(symbols
            .types
            .iter()
            .find(|t| t.label == "ProceduralTestValue1")
            .is_some());
        assert!(symbols
            .types
            .iter()
            .find(|t| t.label == "TestMacro")
            .is_some());
    }

    // Tests for experimental macro expansion, written for the expected behaviour of the feature.
    // macro-expansion.hlsl declares symbols through function-like macros, with a nested one,
    // and token pasting. gFallback is used in PSMain but never declared.

    fn query_macro_expansion_symbols() -> (
        PathBuf,
        crate::symbols::shader_module::ShaderModule,
        ShaderSymbolList,
    ) {
        let file_path = canonicalize(Path::new("./test/hlsl/macro-expansion.hlsl")).unwrap();
        let shader_content = std::fs::read_to_string(&file_path).unwrap();
        let mut shader_module_parser =
            ShaderModuleParser::from_shading_language(ShadingLanguage::Hlsl);
        let symbol_provider = SymbolProvider::from_shading_language(ShadingLanguage::Hlsl);
        let shader_module = shader_module_parser
            .create_module(&file_path, &shader_content)
            .unwrap();
        let compilation = ShaderCompilationParams {
            experimental_macro_expansion: true,
            ..Default::default()
        };
        let symbols = symbol_provider
            .query_symbols(
                &shader_module,
                ShaderParams {
                    compilation: compilation.clone(),
                    ..Default::default()
                },
                &mut default_include_callback::<HlslShadingLanguageTag>,
                None,
            )
            .unwrap();
        // Same symbols as the server: intrinsics & file symbols, with inactive regions filtered.
        let mut all_symbols = ShaderIntrinsics::get(ShadingLanguage::Hlsl)
            .get_intrinsics_symbol(&compilation)
            .to_owned();
        all_symbols.append(symbols.get_all_symbols().into());
        (file_path, shader_module, all_symbols)
    }

    fn find_unique<'a>(
        symbols: &'a Vec<crate::symbols::symbols::ShaderSymbol>,
        label: &str,
    ) -> &'a crate::symbols::symbols::ShaderSymbol {
        let found: Vec<_> = symbols.iter().filter(|s| s.label == label).collect();
        assert_eq!(
            found.len(),
            1,
            "Expected a single symbol {}, found {:#?}",
            label,
            found
        );
        found[0]
    }

    fn assert_defined_at(
        symbol: &crate::symbols::symbols::ShaderSymbol,
        file_path: &Path,
        line: u32,
    ) {
        let runtime = symbol
            .mode
            .map_runtime()
            .unwrap_or_else(|| panic!("{} is not a runtime symbol", symbol.label));
        assert_eq!(
            runtime.file_path, file_path,
            "{} is defined in the wrong file",
            symbol.label
        );
        assert_eq!(
            runtime.range.start.line, line,
            "{} should be defined at line {}, found {:?}",
            symbol.label, line, runtime.range
        );
    }

    #[test]
    fn test_macro_expansion_symbols() {
        let (_, _, symbols) = query_macro_expansion_symbols();
        // Variables declared through macros, with their expanded type.
        for (label, expected_ty) in [
            // TODO: No template <Material> for ConstantBuffer
            ("gMaterial", "ConstantBuffer"),
            ("gNested", "ConstantBuffer"), // Nested macro CBUFFER_SLOT0 -> CBUFFER
            ("gAlbedo", "Texture2D"),
            ("gAlbedoSampler", "SamplerState"), // Token pasting Name##Sampler
            ("gBatch", "ConstantBuffer"),
        ] {
            match &find_unique(&symbols.variables, label).data {
                ShaderSymbolData::Variables { ty, count: _ } => {
                    assert_eq!(ty, expected_ty, "Wrong type for {}", label)
                }
                data => panic!("{} is not a variable: {:#?}", label, data),
            }
        }
        // Undeclared variable must not exist.
        assert!(
            !symbols.variables.iter().any(|v| v.label == "gFallback"),
            "gFallback is never declared"
        );
        // Struct declared through token pasting Name##Data.
        match &find_unique(&symbols.types, "VertexData").data {
            ShaderSymbolData::Struct { members, .. } => {
                let members: Vec<(&str, &str)> = members
                    .iter()
                    .map(|m| (m.parameters.label.as_str(), m.parameters.ty.as_str()))
                    .collect();
                assert_eq!(members, vec![("value", "float4")]);
            }
            data => panic!("VertexData is not a struct: {:#?}", data),
        }
    }

    #[test]
    fn test_macro_expansion_locations() {
        // Expanded symbols should be located at the macro call in the file, not inside the expanded text.
        let (file_path, _, symbols) = query_macro_expansion_symbols();
        for (label, line) in [
            ("gMaterial", 20),
            ("gAlbedo", 22),
            ("gAlbedoSampler", 22),
            ("gNested", 23),
            ("gBatch", 24),
        ] {
            assert_defined_at(find_unique(&symbols.variables, label), &file_path, line);
        }
        let vertex_data = find_unique(&symbols.types, "VertexData");
        assert_defined_at(vertex_data, &file_path, 21);
        match &vertex_data.data {
            ShaderSymbolData::Struct { members, .. } => {
                let range = members[0].parameters.range.as_ref().unwrap();
                assert_eq!(
                    range.start.line, 21,
                    "VertexData::value should be at line 21"
                );
            }
            _ => panic!("VertexData is not a struct"),
        }
    }

    #[test]
    fn test_macro_expansion_definitions() {
        // Go to definition from PSMain, as textDocument/definition does.
        let (file_path, shader_module, symbols) = query_macro_expansion_symbols();
        let symbol_provider = SymbolProvider::from_shading_language(ShadingLanguage::Hlsl);
        let symbol_list = symbols.as_ref();
        // (position of the word, expected symbol label, expected definition line)
        // TODO: Cannot view member of ConstantBuffer<DataType> as it need specific treatment.
        let expected_definitions = [
            ((29, 15), "gMaterial", 20),
            //((29, 25), "roughness", 16), // gMaterial.roughness, member of Material
            //((30, 25), "metallic", 17),  // gMaterial.metallic
            ((34, 5), "VertexData", 21),
            ((35, 15), "v", 34),
            ((35, 19), "value", 21), // v.value, member of VertexData
            ((38, 4), "gAlbedo", 22),
            ((38, 20), "gAlbedoSampler", 22),
            ((45, 14), "gBatch", 24),
            //((45, 22), "roughness", 16), // gBatch.roughness, member of Material
        ];
        for ((line, pos), label, definition_line) in expected_definitions {
            let word = symbol_provider
                .get_word_range_at_position(&shader_module, &ShaderPosition::new(line, pos))
                .unwrap_or_else(|err| panic!("No word found at {}:{}: {:?}", line, pos, err));
            assert_eq!(word.get_word(), label, "Wrong word at {}:{}", line, pos);
            let found = word.find_symbol_from_parent(file_path.clone(), &symbol_list);
            assert_eq!(
                found.len(),
                1,
                "Expected a single definition for {} at {}:{}, found {:#?}",
                label,
                line,
                pos,
                found
            );
            assert_eq!(found[0].label, label);
            assert_defined_at(&found[0], &file_path, definition_line);
        }
        // gFallback is never declared, so it has no definition.
        let word = symbol_provider
            .get_word_range_at_position(&shader_module, &ShaderPosition::new(42, 15))
            .unwrap();
        assert_eq!(word.get_word(), "gFallback");
        assert!(word
            .find_symbol_from_parent(file_path.clone(), &symbol_list)
            .is_empty());
    }

    #[test]
    fn test_dependency_tree() {
        let file_path = Path::new("./test/glsl/include-level.comp.glsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        let mut shader_module_parser =
            ShaderModuleParser::from_shading_language(ShadingLanguage::Glsl);
        let symbol_provider = SymbolProvider::from_shading_language(ShadingLanguage::Glsl);
        let shader_module = shader_module_parser
            .create_module(file_path, &shader_content)
            .unwrap();
        let symbols = symbol_provider
            .query_symbols(
                &shader_module,
                ShaderParams {
                    compilation: ShaderCompilationParams {
                        experimental_macro_expansion: true,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                &mut default_include_callback::<GlslShadingLanguageTag>,
                None,
            )
            .unwrap();
        let dependency_tree = symbols.get_dependency_tree();
        assert_eq!(dependency_tree.path, canonicalize(&file_path).unwrap());
        assert!(dependency_tree.includes.len() == 1);
        let dependency_tree = &dependency_tree.includes[0];
        assert_eq!(
            dependency_tree.path,
            canonicalize(&Path::new("./test/glsl/inc0/level0.glsl")).unwrap()
        );
        assert!(dependency_tree.includes.len() == 1);
        let dependency_tree = &dependency_tree.includes[0];
        assert_eq!(
            dependency_tree.path,
            canonicalize(&Path::new("./test/glsl/inc0/inc1/level1.glsl")).unwrap()
        );
        assert!(dependency_tree.includes.len() == 0);
    }
}
