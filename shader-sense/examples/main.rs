use std::path::Path;

use shader_sense::{
    shader::{GlslShadingLanguageTag, ShaderParams, ShadingLanguageTag},
    symbols::{
        shader_module_parser::ShaderModuleParser,
        symbol_provider::{default_include_callback, SymbolProvider},
    },
    validator::validator::Validator,
};

fn validate_file<T: ShadingLanguageTag>(shader_path: &Path, shader_content: &str) {
    // Validator intended to validate a file using standard API.
    let validator = Validator::from_shading_language(T::get_language());
    match validator.validate_shader(
        shader_content,
        shader_path,
        &ShaderParams::default(),
        &mut |path: &Path| Some(std::fs::read_to_string(path).unwrap()),
    ) {
        Ok((_blob, diagnostic_list)) => println!(
            "Validated file and return following diagnostics: {:#?}",
            diagnostic_list
        ),
        Err(err) => println!("Failed to validate file: {:#?}", err),
    }
}

fn query_all_symbol<T: ShadingLanguageTag>(shader_path: &Path, shader_content: &str) {
    // SymbolProvider intended to gather file symbol at runtime by inspecting the AST.
    let mut shader_module_parser = ShaderModuleParser::from_shading_language(T::get_language());
    let symbol_provider = SymbolProvider::from_shading_language(T::get_language());
    match shader_module_parser.create_module(shader_path, shader_content) {
        Ok(shader_module) => {
            let symbols = symbol_provider
                .query_symbols(
                    &shader_module,
                    ShaderParams::default(),
                    &mut default_include_callback::<T>,
                    None,
                )
                .unwrap();
            let symbol_list = symbols.get_all_symbols();
            println!("Found symbols: {:#?}", symbol_list);
        }
        Err(err) => println!("Failed to create ast: {:#?}", err),
    }
}

const SHADER: &str = r#"
#version 450
void symbol(uint i) {}
void symbol(float b) {}
void main() {
    vec4 frags = gl_FragCoord;
}
"#;

fn main() {
    let shader_path = Path::new("dummy/shader.frag.glsl");
    validate_file::<GlslShadingLanguageTag>(shader_path, SHADER);
    query_all_symbol::<GlslShadingLanguageTag>(shader_path, SHADER);
}
