use validator::Validator;

use crate::shader::ShadingLanguage;

#[cfg(not(target_os = "wasi"))]
pub mod dxc;
pub mod glslang;
pub mod naga;
pub mod validator;

pub fn create_validator(shading_language: ShadingLanguage) -> Box<dyn Validator> {
    match shading_language {
        ShadingLanguage::Wgsl => Box::new(naga::Naga::new()),
        #[cfg(not(target_os = "wasi"))]
        ShadingLanguage::Hlsl => Box::new(dxc::Dxc::new().unwrap()),
        #[cfg(target_os = "wasi")]
        ShadingLanguage::Hlsl => Box::new(glslang::Glslang::hlsl()),
        ShadingLanguage::Glsl => Box::new(glslang::Glslang::glsl()),
    }
}

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, path::Path};

    use super::validator::*;
    use super::*;

    #[test]
    fn glsl_ok() {
        let mut validator = create_validator(ShadingLanguage::Glsl);
        let file_path = Path::new("./test/glsl/ok.frag.glsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ValidationParams::default(),
            &mut default_include_callback,
        ) {
            Ok(result) => {
                println!("Diagnostic should be empty: {:#?}", result);
                assert!(result.is_empty())
            }
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn glsl_include_config() {
        let mut validator = create_validator(ShadingLanguage::Glsl);
        let file_path = Path::new("./test/glsl/include-config.frag.glsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ValidationParams {
                includes: vec!["./test/glsl/inc0/".into()],
                ..Default::default()
            },
            &mut default_include_callback,
        ) {
            Ok(result) => {
                println!("Diagnostic should be empty: {:#?}", result);
                assert!(result.is_empty())
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    fn glsl_include_level() {
        let mut validator = create_validator(ShadingLanguage::Glsl);
        let file_path = Path::new("./test/glsl/include-level.comp.glsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ValidationParams {
                includes: vec!["./test/glsl/inc0/".into()],
                ..Default::default()
            },
            &mut default_include_callback,
        ) {
            Ok(result) => {
                println!("Diagnostic should be empty: {:#?}", result);
                assert!(result.is_empty())
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    fn glsl_no_stage() {
        let mut validator = create_validator(ShadingLanguage::Glsl);
        let file_path = Path::new("./test/glsl/nostage.glsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ValidationParams {
                includes: vec!["./test/glsl/inc0/".into()],
                ..Default::default()
            },
            &mut default_include_callback,
        ) {
            Ok(result) => {
                println!("Diagnostic should be empty: {:#?}", result);
                assert!(result.is_empty())
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    fn glsl_macro() {
        let mut validator = create_validator(ShadingLanguage::Glsl);
        let file_path = Path::new("./test/glsl/macro.frag.glsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ValidationParams {
                defines: HashMap::from([("CUSTOM_MACRO".into(), "42".into())]),
                ..Default::default()
            },
            &mut default_include_callback,
        ) {
            Ok(result) => {
                println!("Diagnostic should be empty: {:#?}", result);
                assert!(result.is_empty())
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    fn glsl_error_parsing() {
        let mut validator = create_validator(ShadingLanguage::Glsl);
        let file_path = Path::new("./test/glsl/error-parsing.frag.glsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ValidationParams::default(),
            &mut default_include_callback,
        ) {
            Ok(result) => {
                let diags = result.diagnostics;
                println!("Diagnostic should not be empty: {:#?}", diags);
                assert!(diags[0].range.start.file_path.exists());
                assert_eq!(diags[0].error, String::from(" '#include' : Could not process include directive for header name: ./level1.glsl\n"));
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    fn hlsl_ok() {
        let mut validator = create_validator(ShadingLanguage::Hlsl);
        let file_path = Path::new("./test/hlsl/ok.hlsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ValidationParams::default(),
            &mut default_include_callback,
        ) {
            Ok(result) => {
                println!("Diagnostic should be empty: {:#?}", result);
                assert!(result.is_empty())
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    fn hlsl_include_config() {
        let mut validator = create_validator(ShadingLanguage::Hlsl);
        let file_path = Path::new("./test/hlsl/include-config.hlsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ValidationParams {
                includes: vec!["./test/hlsl/inc0/".into()],
                ..Default::default()
            },
            &mut default_include_callback,
        ) {
            Ok(result) => {
                println!("Diagnostic should be empty: {:#?}", result);
                assert!(result.is_empty())
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    fn hlsl_include_parent_folder() {
        let mut validator = create_validator(ShadingLanguage::Hlsl);
        let file_path = Path::new("./test/hlsl/folder/folder-file.hlsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ValidationParams {
                includes: vec!["./test/hlsl/".into()],
                ..Default::default()
            },
            &mut default_include_callback,
        ) {
            Ok(result) => {
                println!("Diagnostic should be empty: {:#?}", result);
                assert!(result.is_empty())
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    fn hlsl_include_level() {
        let mut validator = create_validator(ShadingLanguage::Hlsl);
        let file_path = Path::new("./test/hlsl/include-level.hlsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ValidationParams {
                includes: vec!["./test/hlsl/inc0/".into()],
                ..Default::default()
            },
            &mut default_include_callback,
        ) {
            Ok(result) => {
                println!("Diagnostic should be empty: {:#?}", result);
                assert!(result.is_empty())
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    fn hlsl_macro() {
        let mut validator = create_validator(ShadingLanguage::Hlsl);
        let file_path = Path::new("./test/hlsl/macro.hlsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ValidationParams {
                defines: HashMap::from([("CUSTOM_MACRO".into(), "42".into())]),
                ..Default::default()
            },
            &mut default_include_callback,
        ) {
            Ok(result) => {
                println!("Diagnostic should be empty: {:#?}", result);
                assert!(result.is_empty())
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    #[cfg(not(target_os = "wasi"))] // Somehow glslang fail to enable 16bit types... Disabled for now.
    fn hlsl_16bits_types_ok() {
        let mut validator = create_validator(ShadingLanguage::Hlsl);
        let file_path = Path::new("./test/hlsl/16bit-types.hlsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ValidationParams {
                hlsl_enable16bit_types: true,
                ..Default::default()
            },
            &mut default_include_callback,
        ) {
            Ok(result) => {
                println!("Diagnostic should be empty: {:#?}", result);
                assert!(result.is_empty())
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    #[cfg(not(target_os = "wasi"))] // Default behaviour of glslang, so ignore
    fn hlsl_spirv_ok() {
        let mut validator = create_validator(ShadingLanguage::Hlsl);
        let file_path = Path::new("./test/hlsl/spirv-shader.hlsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        // Check warning
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ValidationParams {
                hlsl_spirv: false,
                ..Default::default()
            },
            &mut default_include_callback,
        ) {
            Ok(result) => {
                println!("Diagnostic should not be empty: {:#?}", result);
                assert!(!result.is_empty())
            }
            Err(err) => panic!("{}", err),
        };
        // Check no warning
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ValidationParams {
                hlsl_spirv: true,
                ..Default::default()
            },
            &mut default_include_callback,
        ) {
            Ok(result) => {
                println!("Diagnostic should be empty: {:#?}", result);
                assert!(result.is_empty())
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    fn wgsl_ok() {
        let mut validator = create_validator(ShadingLanguage::Wgsl);
        let file_path = Path::new("./test/wgsl/ok.wgsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ValidationParams::default(),
            &mut default_include_callback,
        ) {
            Ok(result) => {
                println!("Diagnostic should be empty: {:#?}", result);
                assert!(result.is_empty())
            }
            Err(err) => panic!("{}", err),
        };
    }
}
