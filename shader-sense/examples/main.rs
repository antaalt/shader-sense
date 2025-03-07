use std::path::Path;

use shader_sense::{
    include::IncludeHandler,
    shader::ShadingLanguage,
    symbols::{create_symbol_provider, symbol_tree::SymbolTree, symbols::ShaderSymbolParams},
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
        Ok((diagnostic_list, dependencies)) => println!(
            "Validated file and return following diagnostics: {:#?}\n With dependencies: {:#?}",
            diagnostic_list, dependencies
        ),
        Err(err) => println!("Failed to validate file: {:#?}", err),
    }
}

fn query_all_symbol(shading_language: ShadingLanguage, shader_path: &Path) {
    // SymbolProvider intended to gather file symbol at runtime by inspecting the AST.
    let mut symbol_provider = create_symbol_provider(shading_language);
    let shader_content = std::fs::read_to_string(shader_path).unwrap();
    match SymbolTree::new(symbol_provider.as_mut(), shader_path, &shader_content) {
        Ok(symbol_tree) => {
            let preprocessor = symbol_provider
                .query_preprocessor(
                    &symbol_tree,
                    &ShaderSymbolParams::default(),
                    &mut IncludeHandler::default(shader_path),
                )
                .unwrap();
            let symbol_list = symbol_provider
                .query_file_symbols(&symbol_tree, &preprocessor)
                .unwrap();
            println!("Found symbols: {:#?}", symbol_list);
        }
        Err(err) => println!("Failed to create ast: {:#?}", err),
    }
}

fn main() {
    let shader_path = Path::new("./test/glsl/ok.frag.glsl");
    validate_file(ShadingLanguage::Glsl, shader_path);
    query_all_symbol(ShadingLanguage::Glsl, shader_path);
}
