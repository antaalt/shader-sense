use std::path::Path;

use shader_sense::{
    shader::{GlslShadingLanguageTag, ShadingLanguage, ShadingLanguageTag},
    symbols::{
        shader_language::ShaderLanguage,
        symbol_provider::{default_include_callback, ShaderSymbolParams},
    },
    validator::{create_validator, validator::ValidationParams},
};

fn validate_file(shading_language: ShadingLanguage, shader_path: &Path) {
    // Validator intended to validate a file using standard API.
    let mut validator = create_validator(shading_language);
    let shader_content = std::fs::read_to_string(shader_path).unwrap();
    match validator.validate_shader(
        &shader_content,
        shader_path,
        &ValidationParams::default(),
        &mut |path: &Path| Some(std::fs::read_to_string(path).unwrap()),
    ) {
        Ok(diagnostic_list) => println!(
            "Validated file and return following diagnostics: {:#?}",
            diagnostic_list
        ),
        Err(err) => println!("Failed to validate file: {:#?}", err),
    }
}

fn query_all_symbol<T: ShadingLanguageTag>(shader_path: &Path) {
    // SymbolProvider intended to gather file symbol at runtime by inspecting the AST.
    let mut language = ShaderLanguage::new(T::get_language());
    let symbol_provider = language.create_symbol_provider();
    let shader_content = std::fs::read_to_string(shader_path).unwrap();
    match language.create_module(shader_path, &shader_content) {
        Ok(symbol_tree) => {
            let symbols = symbol_provider
                .query_symbols(
                    &symbol_tree,
                    ShaderSymbolParams::default(),
                    &mut default_include_callback::<GlslShadingLanguageTag>,
                    None,
                )
                .unwrap();
            let symbol_tree = symbols.get_symbol_tree();
            println!("Found symbols: {:#?}", symbol_tree);
        }
        Err(err) => println!("Failed to create ast: {:#?}", err),
    }
}

fn main() {
    let shader_path = Path::new("./test/glsl/ok.frag.glsl");
    validate_file(ShadingLanguage::Glsl, shader_path);
    query_all_symbol::<GlslShadingLanguageTag>(shader_path);
}
