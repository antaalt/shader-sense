mod glsl;
mod hlsl;
pub mod shader_language;
mod symbol_parser;
pub mod symbol_provider;
pub mod symbol_tree;
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
        include::IncludeHandler,
        shader::{
            GlslShadingLanguageTag, HlslShadingLanguageTag, ShadingLanguage, ShadingLanguageTag,
            WgslShadingLanguageTag,
        },
        shader_error::ShaderError,
        symbols::{
            shader_language::ShaderLanguage,
            symbol_provider::ShaderSymbolParams,
            symbols::{
                ShaderBuiltinSymbol, ShaderPosition, ShaderRange, ShaderSymbolContent,
                ShaderSymbolData, ShaderSymbolType,
            },
        },
    };

    use super::{
        symbol_parser::ShaderSymbolTreeBuilder,
        symbol_provider::{default_include_callback, SymbolProvider},
        symbols::{ShaderSignature, ShaderSymbol, ShaderSymbolTree},
    };

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

    fn get_all_symbols<T: ShadingLanguageTag>(
        language: &mut ShaderLanguage,
        symbol_provider: &SymbolProvider,
        file_path: &Path,
        shader_content: &String,
    ) -> Result<ShaderSymbolTree, ShaderError> {
        let mut include_handler = IncludeHandler::main_without_config(&file_path);
        let deps = find_dependencies(&mut include_handler, &shader_content);
        let mut all_symbols = ShaderSymbolTree::default();
        let symbol_tree = language.create_module(file_path, shader_content).unwrap();
        let symbols = symbol_provider
            .query_symbols(
                &symbol_tree,
                ShaderSymbolParams::default(),
                &mut default_include_callback::<T>,
                None,
            )
            .unwrap();
        let symbols = symbols.get_symbol_tree();
        all_symbols.append(symbols.clone());
        for dep in deps {
            let symbol_tree = language.create_module(&dep.1, &dep.0).unwrap();
            let symbols = symbol_provider
                .query_symbols(
                    &symbol_tree,
                    ShaderSymbolParams::default(),
                    &mut default_include_callback::<T>,
                    None,
                )
                .unwrap();
            let symbols = symbols.get_symbol_tree();
            all_symbols.append(symbols.clone());
        }
        all_symbols.append_builtins(language.get_intrinsics_symbol().clone());
        Ok(all_symbols)
    }

    #[test]
    fn intrinsics_glsl_ok() {
        // Ensure parsing of intrinsics is OK
        let _ = ShaderBuiltinSymbol::parse_from_json(String::from(include_str!(
            "glsl/glsl-intrinsics.json"
        )));
    }
    #[test]
    fn intrinsics_hlsl_ok() {
        // Ensure parsing of intrinsics is OK
        let _ = ShaderBuiltinSymbol::parse_from_json(String::from(include_str!(
            "hlsl/hlsl-intrinsics.json"
        )));
    }
    #[test]
    fn intrinsics_wgsl_ok() {
        // Ensure parsing of intrinsics is OK
        let _ = ShaderBuiltinSymbol::parse_from_json(String::from(include_str!(
            "wgsl/wgsl-intrinsics.json"
        )));
    }
    #[test]
    fn symbols_glsl_ok() {
        // Ensure parsing of symbols is OK
        let file_path = Path::new("./test/glsl/include-level.comp.glsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        let mut language = ShaderLanguage::new(ShadingLanguage::Glsl);
        let symbol_provider = language.create_symbol_provider();
        let symbol_tree = language.create_module(file_path, &shader_content).unwrap();
        let symbols = symbol_provider
            .query_symbols(
                &symbol_tree,
                ShaderSymbolParams::default(),
                &mut default_include_callback::<GlslShadingLanguageTag>,
                None,
            )
            .unwrap();
        let symbols = symbols.get_symbol_tree();
        assert!(symbols
            .iter_all()
            .find(|s| s.is_type(ShaderSymbolType::Functions))
            .is_some());
    }
    #[test]
    fn symbols_hlsl_ok() {
        // Ensure parsing of symbols is OK
        let file_path = Path::new("./test/hlsl/include-level.hlsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        let mut language = ShaderLanguage::new(ShadingLanguage::Hlsl);
        let symbol_provider = language.create_symbol_provider();
        let symbol_tree = language.create_module(file_path, &shader_content).unwrap();
        let symbols = symbol_provider
            .query_symbols(
                &symbol_tree,
                ShaderSymbolParams::default(),
                &mut default_include_callback::<HlslShadingLanguageTag>,
                None,
            )
            .unwrap();
        let symbols = symbols.get_symbol_tree();
        assert!(symbols
            .iter_all()
            .find(|s| s.is_type(ShaderSymbolType::Functions))
            .is_some());
    }
    #[test]
    fn symbols_wgsl_ok() {
        // Ensure parsing of symbols is OK
        let file_path = Path::new("./test/wgsl/ok.wgsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        let mut language = ShaderLanguage::new(ShadingLanguage::Wgsl);
        let symbol_provider = language.create_symbol_provider();
        let symbol_tree = language.create_module(file_path, &shader_content).unwrap();
        let symbols = symbol_provider
            .query_symbols(
                &symbol_tree,
                ShaderSymbolParams::default(),
                &mut default_include_callback::<WgslShadingLanguageTag>,
                None,
            )
            .unwrap();
        let symbols = symbols.get_symbol_tree();
        assert!(symbols
            .iter_all()
            .find(|s| s.is_type(ShaderSymbolType::Functions))
            .is_none());
    }
    #[test]
    fn symbol_tree_builder() {
        fn make_symbol(
            label: &str,
            label_range: ShaderRange,
            data: ShaderSymbolData,
            content: Option<ShaderSymbolContent>,
        ) -> ShaderSymbol {
            ShaderSymbol {
                label: label.into(),
                description: "dummy".into(),
                version: "version".into(),
                stages: vec![],
                link: None,
                data: data,
                content: content,
                range: Some(label_range),
            }
        }
        let mut builder = ShaderSymbolTreeBuilder::new(&|_| -> bool {
            true // no filtering
        });
        let file_path = Path::new("./test/virtual.hlsl");
        builder.add_children(
            make_symbol(
                "test",
                ShaderRange::new(
                    ShaderPosition::new(file_path.into(), 0, 0),
                    ShaderPosition::new(file_path.into(), 0, 10),
                ),
                ShaderSymbolData::Functions {
                    signatures: vec![ShaderSignature {
                        returnType: "void".into(),
                        description: "void".into(),
                        parameters: vec![],
                    }],
                },
                Some(ShaderSymbolContent {
                    range: Some(ShaderRange::new(
                        ShaderPosition::new(file_path.into(), 0, 10),
                        ShaderPosition::new(file_path.into(), 10, 0),
                    )),
                    childrens: vec![], // Child auto set.
                }),
            ),
            true,
        );
        builder.add_children(
            make_symbol(
                "content",
                ShaderRange::new(
                    ShaderPosition::new(file_path.into(), 3, 0),
                    ShaderPosition::new(file_path.into(), 3, 10),
                ),
                ShaderSymbolData::Functions {
                    signatures: vec![ShaderSignature {
                        returnType: "void".into(),
                        description: "void".into(),
                        parameters: vec![],
                    }],
                },
                None,
            ),
            false,
        );
        let tree = builder.get_shader_symbol_tree();
        println!("Tree: {:#?}", tree);
        // Test global count
        let iter_count = tree.iter_root();
        let root_count = iter_count.count();
        assert!(
            root_count == 1,
            "Expected 1 root element, found {}",
            root_count
        );
        // Test all count
        let iter_count = tree.iter_all();
        let global_count = iter_count.count();
        assert!(
            global_count == 2,
            "Expected 2 global element, found {}",
            root_count
        );
        // Test hierarchy
        let mut iter = tree.iter_all();
        let root = iter.next().unwrap();
        assert!(
            root.label == "test",
            "Test label is invalid: {}",
            root.label
        );
        assert!(
            root.content.as_ref().unwrap().childrens.len() > 0,
            "Root do not have child."
        );
        let child = iter.next().unwrap();
        assert!(
            child.label == "content",
            "content label is invalid: {}",
            child.label
        );
    }
    #[test]
    fn symbol_scope_glsl_ok() {
        let file_path = Path::new("./test/glsl/scopes.frag.glsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        let mut language = ShaderLanguage::new(ShadingLanguage::Glsl);
        let symbol_provider = language.create_symbol_provider();
        let symbols = get_all_symbols::<GlslShadingLanguageTag>(
            &mut language,
            &symbol_provider,
            file_path,
            &shader_content,
        )
        .unwrap();
        let symbols = symbols.find_symbols_defined_at(&ShaderPosition {
            file_path: PathBuf::from(file_path),
            line: 16,
            pos: 0,
        });
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
                    .iter()
                    .any(|e| e.label == variable_visible && e.is_type(ShaderSymbolType::Variables)),
                "Failed to find variable {} {:#?}",
                variable_visible,
                symbols
            );
        }
        for variable_not_visible in variables_not_visibles {
            assert!(
                !symbols
                    .iter()
                    .any(|e| e.label == variable_not_visible
                        && e.is_type(ShaderSymbolType::Variables)),
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
        let mut language = ShaderLanguage::new(ShadingLanguage::Glsl);
        let symbol_provider = language.create_symbol_provider();
        let symbol_tree = language.create_module(file_path, &shader_content).unwrap();
        let symbols = symbol_provider
            .query_symbols(
                &symbol_tree,
                ShaderSymbolParams::default(),
                &mut default_include_callback::<GlslShadingLanguageTag>,
                None,
            )
            .unwrap();
        let symbols = symbols.get_symbol_tree();
        assert!(symbols
            .iter_all()
            .find(|e| e.label == "MatrixHidden" && e.is_type(ShaderSymbolType::Types))
            .is_some());
        assert!(symbols
            .iter_all()
            .find(|e| e.label == "u_accessor"
                && match &e.data {
                    ShaderSymbolData::Variables { ty, count: _ } => ty == "MatrixHidden",
                    _ => false,
                })
            .is_some());
        assert!(symbols
            .iter_all()
            .find(|e| e.label == "u_modelviewGlobal" && e.is_type(ShaderSymbolType::Variables))
            .is_some());
        assert!(symbols
            .iter_all()
            .find(|e| e.label == "u_modelviewHidden" && e.is_type(ShaderSymbolType::Variables))
            .is_none());
    }
}
