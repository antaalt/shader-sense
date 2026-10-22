//! Command line interface for generating intrinsic JSON database for shader-sense.

use std::{collections::HashSet, path::Path};

use common::get_intrinsic_parser;
use shader_sense::shader::ShadingLanguage;

mod common;
mod glsl;
mod hlsl;
mod wgsl;

fn usage() {
    println!(
        r"Usage options:
    --parse-glsl : Parse glsl doc & generate glsl-intrinsics.json file.
    --parse-hlsl : Parse hlsl doc & generate hlsl-intrinsics.json file.
    --parse-wgsl : Parse wgsl doc & generate wgsl-intrinsics.json file.
    --all : Parse all docs.
    "
    );
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let language_to_parse = if args.len() == 1 {
        vec![
            ShadingLanguage::Glsl,
            ShadingLanguage::Hlsl,
            ShadingLanguage::Wgsl,
        ] // All
    } else {
        let mut language_to_parse = HashSet::new();
        for arg in &args[1..] {
            // Skip first executable
            match arg.as_str() {
                "--parse-glsl" => {
                    language_to_parse.insert(ShadingLanguage::Glsl);
                }
                "--parse-hlsl" => {
                    language_to_parse.insert(ShadingLanguage::Hlsl);
                }
                "--parse-wgsl" => {
                    language_to_parse.insert(ShadingLanguage::Wgsl);
                }
                "--all" => {
                    language_to_parse.insert(ShadingLanguage::Glsl);
                    language_to_parse.insert(ShadingLanguage::Hlsl);
                    language_to_parse.insert(ShadingLanguage::Wgsl);
                }
                "--help" => {
                    usage();
                    return;
                }
                invalid_arg => {
                    println!("{}", format!("Invalid arg: {}.", invalid_arg));
                    usage();
                    return;
                }
            };
        }
        language_to_parse.into_iter().collect()
    };
    for shading_language in language_to_parse {
        println!("Parsing {}", shading_language.to_string());
        let parser = get_intrinsic_parser(shading_language);
        let cache_path = format!("./.cache/{}/", shading_language.to_string());
        if !Path::new(&cache_path).is_dir() {
            parser.cache(&cache_path);
        }
        let intrinsic_symbols = parser.parse(&cache_path);

        println!("Saving result...");
        let json = serde_json::to_string(&intrinsic_symbols).expect("Failed to serialize JSON");
        std::fs::write(
            format!("{}-intrinsics.json", shading_language.to_string()),
            json,
        )
        .expect("Failed to write JSON");
        println!("Done with {} !", shading_language.to_string());
    }
}
