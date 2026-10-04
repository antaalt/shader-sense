//! Parser specific for HLSL
mod hlsl_parser;
mod hlsl_preprocessor;
mod hlsl_regions;
mod hlsl_word;

use hlsl_parser::get_hlsl_parsers;
use hlsl_preprocessor::get_hlsl_preprocessor_parser;

// For glsl
pub use hlsl_regions::HlslSymbolRegionFinder;
pub use hlsl_word::HlslSymbolWordProvider;

use crate::shader::ShadingLanguage;

use super::symbol_provider::SymbolProvider;

pub(super) fn create_hlsl_symbol_provider() -> SymbolProvider {
    SymbolProvider::new(
        ShadingLanguage::Hlsl,
        get_hlsl_parsers(),
        get_hlsl_preprocessor_parser(),
        Box::new(HlslSymbolRegionFinder::new(ShadingLanguage::Hlsl)),
        Box::new(hlsl_word::HlslSymbolWordProvider {}),
    )
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use crate::{
        position::{ShaderPosition, ShaderRange},
        shader::{
            GlslShadingLanguageTag, HlslShadingLanguageTag, ShaderCompilationParams, ShaderParams,
            ShaderStage, ShadingLanguage, ShadingLanguageTag,
        },
        symbols::{
            hlsl::hlsl_word::HlslSymbolWordProvider,
            prepocessor::ShaderRegion,
            shader_module_parser::ShaderModuleParser,
            symbol_parser::SymbolWordProvider,
            symbol_provider::{default_include_callback, SymbolProvider},
        },
    };

    #[test]
    fn test_hlsl_regions() {
        let shader_module_parser = ShaderModuleParser::from_shading_language(ShadingLanguage::Hlsl);
        let symbol_provider = SymbolProvider::from_shading_language(ShadingLanguage::Hlsl);
        test_regions::<HlslShadingLanguageTag>(shader_module_parser, symbol_provider);
    }
    #[test]
    fn test_glsl_regions() {
        let shader_module_parser = ShaderModuleParser::from_shading_language(ShadingLanguage::Glsl);
        let symbol_provider = SymbolProvider::from_shading_language(ShadingLanguage::Glsl);
        test_regions::<GlslShadingLanguageTag>(shader_module_parser, symbol_provider);
    }

    fn test_regions<T: ShadingLanguageTag>(
        mut shader_module_parser: ShaderModuleParser,
        symbol_provider: SymbolProvider,
    ) {
        let file_path = Path::new("./test/hlsl/regions.hlsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        let shader_module = shader_module_parser
            .create_module(file_path, &shader_content)
            .unwrap();
        let symbols = symbol_provider
            .query_symbols(
                &shader_module,
                ShaderParams {
                    compilation: ShaderCompilationParams {
                        entry_point: Some("main".into()),
                        shader_stage: Some(ShaderStage::Compute),
                        ..Default::default()
                    },
                    ..Default::default()
                },
                &mut default_include_callback::<T>,
                None,
            )
            .unwrap();
        let set_region =
            |start_line: u32, start_pos: u32, end_line: u32, end_pos: u32, active: bool| {
                ShaderRegion {
                    range: ShaderRange::new(
                        ShaderPosition::new(start_line, start_pos),
                        ShaderPosition::new(end_line, end_pos),
                    ),
                    is_active: active,
                }
            };
        let expected_regions = vec![
            // elif
            set_region(7, 21, 8, 16, true),   // 00
            set_region(9, 32, 10, 16, false), // 01
            set_region(11, 5, 12, 16, false), // 02
            // ifdef true
            set_region(15, 24, 16, 16, true), // 03
            set_region(17, 5, 18, 16, false), // 04
            // ifndef
            set_region(21, 25, 22, 16, false), // 05
            set_region(23, 5, 24, 16, true),   // 06
            // ifdef false
            set_region(27, 28, 28, 16, false), // 07
            // if 0
            set_region(31, 5, 32, 16, false), // 08
            // if parenthesized
            set_region(36, 50, 37, 16, false), // 09
            // if binary
            set_region(41, 43, 42, 16, false), // 10
            // if unary
            set_region(46, 22, 47, 16, false), // 11
            // unary defined expression
            set_region(51, 66, 52, 16, false), // 12
            // region depending on region not defined
            set_region(56, 25, 57, 35, false), // 13
            set_region(59, 28, 60, 34, false), // 14
            // region depending on region defined
            set_region(64, 21, 65, 29, true), // 15
            set_region(67, 22, 68, 16, true), // 16
            // macro included before
            set_region(72, 26, 73, 34, false), // 17
            // macro defined after
            set_region(77, 18, 78, 34, false), // 18
            // macro included after
            set_region(82, 31, 83, 34, false), // 19
            // macro only for compute
            set_region(87, 51, 88, 34, false), // 20
        ];
        assert!(
            symbols.preprocessor.regions.len() == expected_regions.len(),
            "Expecting {} regions, found {}",
            expected_regions.len(),
            symbols.preprocessor.regions.len()
        );
        for region_index in 0..symbols.preprocessor.regions.len() {
            println!(
                "region {}: {:#?}",
                region_index, symbols.preprocessor.regions[region_index]
            );
            assert!(
                symbols.preprocessor.regions[region_index].range.start
                    == expected_regions[region_index].range.start,
                "Failed start assert for region {}",
                region_index
            );
            assert!(
                symbols.preprocessor.regions[region_index].range.end
                    == expected_regions[region_index].range.end,
                "Failed end assert for region {}",
                region_index
            );
            assert!(
                symbols.preprocessor.regions[region_index].is_active
                    == expected_regions[region_index].is_active,
                "Failed active assert for region {}",
                region_index
            );
        }
    }

    #[test]
    fn test_words() {
        let file_path = "./test/hlsl/struct.hlsl";
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        let word_provider = HlslSymbolWordProvider {};
        let mut shader_module_parser =
            ShaderModuleParser::from_shading_language(ShadingLanguage::Hlsl);
        let shader_module = shader_module_parser
            .create_module(Path::new(file_path), &shader_content)
            .unwrap();

        // container
        let word = word_provider
            .find_word_at_position_in_node(
                &shader_module,
                shader_module.tree.root_node(),
                &ShaderPosition::new(23, 17),
            )
            .unwrap();
        assert!(word.get_word() == "container");
        assert!(word.get_parent().is_none());

        // container.method()
        let word = word_provider
            .find_word_at_position_in_node(
                &shader_module,
                shader_module.tree.root_node(),
                &ShaderPosition::new(23, 27),
            )
            .unwrap();
        assert!(word.get_word() == "method");
        assert!(word.get_parent().unwrap().get_word() == "container");

        // container.method().test2
        let word = word_provider
            .find_word_at_position_in_node(
                &shader_module,
                shader_module.tree.root_node(),
                &ShaderPosition::new(23, 44),
            )
            .unwrap();
        assert!(word.get_word() == "test2");
        assert!(word.get_parent().unwrap().get_word() == "method");
        assert!(word.get_parent().unwrap().get_parent().unwrap().get_word() == "container");

        // container.testArray[0].oui
        let word = word_provider
            .find_word_at_position_in_node(
                &shader_module,
                shader_module.tree.root_node(),
                &ShaderPosition::new(24, 44),
            )
            .unwrap();
        assert!(word.get_word() == "oui");
        assert!(word.get_parent().unwrap().get_word() == "testArray");
        assert!(word.get_parent().unwrap().get_parent().unwrap().get_word() == "container");
    }

    #[test]
    fn test_namespace() {
        use crate::{
            include::canonicalize,
            symbols::{symbol_list::ShaderSymbolList, symbols::ShaderScope},
        };
        let file_path = canonicalize(Path::new("./test/hlsl/namespace.hlsl")).unwrap();
        let shader_content = std::fs::read_to_string(&file_path).unwrap();
        let mut shader_module_parser =
            ShaderModuleParser::from_shading_language(ShadingLanguage::Hlsl);
        let symbol_provider = SymbolProvider::from_shading_language(ShadingLanguage::Hlsl);
        let shader_module = shader_module_parser
            .create_module(&file_path, &shader_content)
            .unwrap();

        // Namespace scopes are retrieved with their name, inside their curly braces.
        let namespace_scopes: Vec<ShaderScope> = symbol_provider
            .query_file_scopes(&shader_module)
            .into_iter()
            .filter(|scope| scope.namespace.is_some())
            .collect();
        let expected_scopes = vec![
            ShaderScope::new_namespace(
                ShaderRange::new(ShaderPosition::new(1, 16), ShaderPosition::new(5, 0)),
                "Test".into(),
            ),
            // Same namespace reopened.
            ShaderScope::new_namespace(
                ShaderRange::new(ShaderPosition::new(11, 16), ShaderPosition::new(16, 0)),
                "Test".into(),
            ),
        ];
        assert_eq!(namespace_scopes, expected_scopes);

        let symbols = symbol_provider
            .query_symbols(
                &shader_module,
                ShaderParams::default(),
                &mut default_include_callback::<HlslShadingLanguageTag>,
                None,
            )
            .unwrap();
        let symbols: ShaderSymbolList = symbols.get_all_symbols().into();

        // Functions declared in the namespace hold it in their scope stack.
        let get_namespaces = |label: &str| -> Vec<String> {
            let function = symbols
                .functions
                .iter()
                .find(|f| f.label == label)
                .unwrap_or_else(|| panic!("Function {} not found", label));
            function
                .mode
                .map_runtime()
                .unwrap()
                .scope_stack
                .iter()
                .filter_map(|scope| scope.namespace.clone())
                .collect()
        };
        assert_eq!(get_namespaces("test"), vec!["Test"]);
        assert_eq!(get_namespaces("other"), vec!["Test"]);
        assert!(get_namespaces("main").is_empty());
        assert!(get_namespaces("qualified").is_empty());

        // Namespace is used to resolve symbols.
        let symbol_list = symbols.as_ref();
        let find_definition = |line: u32, pos: u32, expected_word: &str| {
            let word = symbol_provider
                .get_word_range_at_position(&shader_module, &ShaderPosition::new(line, pos))
                .unwrap_or_else(|err| panic!("No word at {}:{}: {:?}", line, pos, err));
            assert_eq!(
                word.get_word(),
                expected_word,
                "Wrong word at {}:{}",
                line,
                pos
            );
            word.find_symbol_from_parent(file_path.clone(), &symbol_list)
        };
        let assert_resolves_to_test = |found: Vec<_>, context: &str| {
            let found: Vec<&crate::symbols::symbols::ShaderSymbol> = found.iter().collect();
            assert_eq!(
                found.len(),
                1,
                "{}: expected Test::test, found {:#?}",
                context,
                found
            );
            assert_eq!(found[0].label, "test");
            assert_eq!(
                found[0].mode.map_runtime().unwrap().range.start.line,
                2,
                "{}: should resolve to Test::test at line 2",
                context
            );
        };
        // Unqualified call outside of the namespace does not resolve.
        assert!(
            find_definition(8, 5, "test").is_empty(),
            "test is not visible outside of namespace Test without qualification"
        );
        // TODO: Unqualified call in the same namespace, reopened, should resolve.
        // Symbol visibility only checks the scope range, so test is only visible in the first
        // `namespace Test { }` block. It should compare the namespace name instead of its range.
        //assert_resolves_to_test(find_definition(14, 9, "test"), "Call in reopened namespace");
        let _ = assert_resolves_to_test;
        // Qualified call is parsed with its namespace as parent.
        let word = symbol_provider
            .get_word_range_at_position(&shader_module, &ShaderPosition::new(19, 11))
            .unwrap();
        assert_eq!(word.get_parent().map(|p| p.get_word()), Some("Test"));
        // TODO: Qualified call should resolve.
        // There is no accessor for namespace yet: find_symbol_from_parent looks for a symbol named
        // Test as root, which does not exist as namespaces are only scopes. It should instead look
        // for test in symbols whose scope stack holds the namespace Test.
        //assert_resolves_to_test(find_definition(19, 11, "test"), "Qualified call Test::test");
    }
}
