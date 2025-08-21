mod hlsl_parser;
mod hlsl_preprocessor;
mod hlsl_regions;
mod hlsl_word;

use hlsl_parser::get_hlsl_parsers;
use hlsl_preprocessor::get_hlsl_preprocessor_parser;

// For glsl
pub use hlsl_regions::HlslSymbolRegionFinder;

use super::symbol_provider::SymbolProvider;

pub(super) fn create_hlsl_symbol_provider(
    tree_sitter_language: tree_sitter::Language,
) -> SymbolProvider {
    SymbolProvider::new(
        tree_sitter_language.clone(),
        get_hlsl_parsers(),
        get_hlsl_preprocessor_parser(),
        Box::new(HlslSymbolRegionFinder::new(tree_sitter_language.clone())),
        Box::new(hlsl_word::HlslSymbolWordProvider {}),
    )
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use crate::{
        position::{ShaderFileRange, ShaderPosition},
        shader::{
            GlslShadingLanguageTag, HlslShadingLanguageTag, ShaderParams, ShadingLanguage,
            ShadingLanguageTag,
        },
        symbols::{
            shader_module_parser::ShaderModuleParser,
            symbol_provider::{default_include_callback, SymbolProvider},
            symbols::ShaderRegion,
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
                ShaderParams::default(),
                &mut default_include_callback::<T>,
                None,
            )
            .unwrap();
        let set_region =
            |start_line: u32, start_pos: u32, end_line: u32, end_pos: u32, active: bool| {
                ShaderRegion {
                    range: ShaderFileRange::new(
                        file_path.into(),
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
                symbols.preprocessor.regions[region_index].range.range.start
                    == expected_regions[region_index].range.range.start,
                "Failed start assert for region {}",
                region_index
            );
            assert!(
                symbols.preprocessor.regions[region_index].range.range.end
                    == expected_regions[region_index].range.range.end,
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
}
