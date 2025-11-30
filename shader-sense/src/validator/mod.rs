//! Handle shader validation with standard API

#[cfg(not(target_os = "wasi"))]
pub mod dxc;
pub mod glslang;
pub mod naga;
pub mod slang;
pub mod validator;

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, path::Path};

    use crate::shader::{
        ShaderCompilationParams, ShaderContextParams, ShaderParams, ShaderStage, ShadingLanguage,
    };

    use super::validator::*;
    use super::*;

    #[cfg(not(target_os = "wasi"))]
    fn find_test_dxc() -> Option<std::path::PathBuf> {
        use std::path::PathBuf;
        // Dxc copied here by build system for convenience.
        // Manifest dir is not in workspace, but command executed from workspace, so hardcode the path here.
        // Should be handled cleanly, maybe by copying dll with a build.rs file.
        Some(PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..\\target\\debug"))
    }
    fn create_test_validator(shading_language: ShadingLanguage) -> Box<dyn ValidatorImpl> {
        // Do not use Validator::from_shading_language to enforce dxc on PC.
        match shading_language {
            ShadingLanguage::Wgsl => Box::new(naga::Naga::new()),
            #[cfg(not(target_os = "wasi"))]
            ShadingLanguage::Hlsl => Box::new(dxc::Dxc::new(find_test_dxc()).unwrap()),
            #[cfg(target_os = "wasi")]
            ShadingLanguage::Hlsl => Box::new(glslang::Glslang::hlsl()),
            ShadingLanguage::Glsl => Box::new(glslang::Glslang::glsl()),
            ShadingLanguage::Slang => Box::new(slang::Slang::new()),
        }
    }

    #[test]
    fn glsl_ok() {
        let validator = create_test_validator(ShadingLanguage::Glsl);
        let file_path = Path::new("./test/glsl/ok.frag.glsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ShaderParams::default(),
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
        let validator = create_test_validator(ShadingLanguage::Glsl);
        let file_path = Path::new("./test/glsl/include-config.frag.glsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ShaderParams {
                context: ShaderContextParams {
                    includes: vec!["./test/glsl/inc0/".into()],
                    ..Default::default()
                },
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
        let validator = create_test_validator(ShadingLanguage::Glsl);
        let file_path = Path::new("./test/glsl/include-level.comp.glsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ShaderParams {
                context: ShaderContextParams {
                    includes: vec!["./test/glsl/inc0/".into()],
                    ..Default::default()
                },
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
        let validator = create_test_validator(ShadingLanguage::Glsl);
        let file_path = Path::new("./test/glsl/nostage.glsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ShaderParams {
                context: ShaderContextParams {
                    includes: vec!["./test/glsl/inc0/".into()],
                    ..Default::default()
                },
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
        let validator = create_test_validator(ShadingLanguage::Glsl);
        let file_path = Path::new("./test/glsl/macro.frag.glsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ShaderParams {
                context: ShaderContextParams {
                    defines: HashMap::from([("CUSTOM_MACRO".into(), "42".into())]),
                    ..Default::default()
                },
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
        let validator = create_test_validator(ShadingLanguage::Glsl);
        let file_path = Path::new("./test/glsl/error-parsing.frag.glsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ShaderParams::default(),
            &mut default_include_callback,
        ) {
            Ok(result) => {
                let diags = result.diagnostics;
                println!("Diagnostic should not be empty: {:#?}", diags);
                assert!(diags[0].range.file_path.exists());
                assert_eq!(diags[0].error, String::from(" '#include' : Could not process include directive for header name: ./level1.glsl\n"));
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    fn hlsl_ok() {
        let validator = create_test_validator(ShadingLanguage::Hlsl);
        let file_path = Path::new("./test/hlsl/ok.hlsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ShaderParams::default(),
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
        let validator = create_test_validator(ShadingLanguage::Hlsl);
        let file_path = Path::new("./test/hlsl/include-config.hlsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ShaderParams {
                context: ShaderContextParams {
                    includes: vec!["./test/hlsl/inc0/".into()],
                    ..Default::default()
                },
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
        let validator = create_test_validator(ShadingLanguage::Hlsl);
        let file_path = Path::new("./test/hlsl/folder/folder-file.hlsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ShaderParams {
                context: ShaderContextParams {
                    includes: vec!["./test/hlsl/".into()],
                    ..Default::default()
                },
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
        let validator = create_test_validator(ShadingLanguage::Hlsl);
        let file_path = Path::new("./test/hlsl/include-level.hlsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ShaderParams {
                context: ShaderContextParams {
                    includes: vec!["./test/hlsl/inc0/".into()],
                    ..Default::default()
                },
                compilation: ShaderCompilationParams {
                    entry_point: Some("compute".into()),
                    shader_stage: Some(ShaderStage::Compute),
                    ..Default::default()
                },
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
        let validator = create_test_validator(ShadingLanguage::Hlsl);
        let file_path = Path::new("./test/hlsl/macro.hlsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ShaderParams {
                context: ShaderContextParams {
                    defines: HashMap::from([("CUSTOM_MACRO".into(), "42".into())]),
                    ..Default::default()
                },
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
        use crate::shader::HlslCompilationParams;

        let validator = create_test_validator(ShadingLanguage::Hlsl);
        let file_path = Path::new("./test/hlsl/16bit-types.hlsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ShaderParams {
                compilation: ShaderCompilationParams {
                    hlsl: HlslCompilationParams {
                        enable16bit_types: true,
                        ..Default::default()
                    },
                    ..Default::default()
                },
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
        use crate::shader::HlslCompilationParams;

        let validator = create_test_validator(ShadingLanguage::Hlsl);
        let file_path = Path::new("./test/hlsl/spirv-shader.hlsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        // Check warning
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ShaderParams {
                compilation: ShaderCompilationParams {
                    hlsl: HlslCompilationParams {
                        spirv: false,
                        ..Default::default()
                    },
                    ..Default::default()
                },
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
            &ShaderParams {
                compilation: ShaderCompilationParams {
                    hlsl: HlslCompilationParams {
                        spirv: true,
                        ..Default::default()
                    },
                    ..Default::default()
                },
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
    fn glsl_stages() {
        #[rustfmt::skip] // Keep them inline
        let stages = vec![
            ("graphics.vert.glsl", "VSMain", ShaderStage::Vertex),
            ("graphics.frag.glsl", "PSMain", ShaderStage::Fragment),
            ("graphics.geom.glsl", "GSMain", ShaderStage::Geometry),
            ("graphics.tesc.glsl", "TCSMain", ShaderStage::TesselationControl),
            ("graphics.tese.glsl", "TESMain", ShaderStage::TesselationEvaluation),
            ("compute.comp.glsl", "CSMain", ShaderStage::Compute),
            ("mesh.task.glsl", "TSMain", ShaderStage::Task),
            ("mesh.mesh.glsl", "MSMain", ShaderStage::Mesh),
            ("raytracing.rgen.glsl", "RayGenMain", ShaderStage::RayGeneration,),
            ("raytracing.rint.glsl", "IntersectionMain", ShaderStage::Intersect),
            ("raytracing.rmiss.glsl", "MissMain", ShaderStage::Miss),
            ("raytracing.rahit.glsl", "AnyHitMain", ShaderStage::AnyHit),
            ("raytracing.rchit.glsl", "ClosestHitMain", ShaderStage::ClosestHit),
            ("raytracing.rcall.glsl", "CallableMain", ShaderStage::Callable),
        ];
        let validator = create_test_validator(ShadingLanguage::Glsl);
        for (file_name, entry_point, shader_stage) in stages {
            let file_path = Path::new("./test/glsl/stages/").join(file_name);
            let shader_content = std::fs::read_to_string(&file_path).unwrap();
            match validator.validate_shader(
                &shader_content,
                &file_path,
                &ShaderParams {
                    compilation: ShaderCompilationParams {
                        entry_point: Some(entry_point.into()),
                        shader_stage: Some(shader_stage),
                        ..Default::default()
                    },
                    ..Default::default()
                },
                &mut default_include_callback,
            ) {
                Ok(result) => {
                    println!(
                        "Diagnostic should be empty for stage {:?}: {:#?}",
                        shader_stage, result
                    );
                    assert!(result.is_empty())
                }
                Err(err) => panic!("{}", err),
            };
        }
    }

    #[test]
    fn hlsl_stages() {
        #[rustfmt::skip] // Keep them inline
        let stages = vec![
            ("graphics.hlsl", "VSMain", ShaderStage::Vertex),
            ("graphics.hlsl", "PSMain", ShaderStage::Fragment),
            #[cfg(not(target_os= "wasi"))] // TODO: Find why its failing on WASI.
            ("graphics.hlsl", "GSMain", ShaderStage::Geometry),
            ("graphics.hlsl", "HSMain", ShaderStage::TesselationControl),
            ("graphics.hlsl", "DSMain", ShaderStage::TesselationEvaluation),
            ("compute.hlsl", "CSMain", ShaderStage::Compute),
            ("mesh.hlsl", "ASMain", ShaderStage::Task),
            ("mesh.hlsl", "MSMain", ShaderStage::Mesh),
            ("raytracing.hlsl", "RayGenMain", ShaderStage::RayGeneration),
            ("raytracing.hlsl", "IntersectionMain", ShaderStage::Intersect),
            ("raytracing.hlsl", "MissMain", ShaderStage::Miss),
            ("raytracing.hlsl", "AnyHitMain", ShaderStage::AnyHit),
            ("raytracing.hlsl", "ClosestHitMain", ShaderStage::ClosestHit),
            ("raytracing.hlsl", "CallableMain", ShaderStage::Callable),
        ];
        let validator = create_test_validator(ShadingLanguage::Hlsl);
        for (file_name, entry_point, shader_stage) in stages {
            let file_path = Path::new("./test/hlsl/stages/").join(file_name);
            let shader_content = std::fs::read_to_string(&file_path).unwrap();
            // Check for WASI test.
            if validator.support(shader_stage) {
                match validator.validate_shader(
                    &shader_content,
                    &file_path,
                    &ShaderParams {
                        compilation: ShaderCompilationParams {
                            entry_point: Some(entry_point.into()),
                            shader_stage: Some(shader_stage),
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    &mut default_include_callback,
                ) {
                    Ok(result) => {
                        println!(
                            "Diagnostic should be empty for stage {:?}: {:#?}",
                            shader_stage, result
                        );
                        assert!(result.is_empty())
                    }
                    Err(err) => panic!("{}", err),
                };
            }
        }
    }

    #[test]
    fn wgsl_stages() {
        // Wgsl only support three main stages.
        // Mesh shader stage: https://github.com/gfx-rs/wgpu/issues/7197
        // Raytracing shader stage: https://github.com/gfx-rs/wgpu/issues/6762
        // Geometry shader deprecated
        // Tesselation shader deprecated ?
        #[rustfmt::skip] // Keep them inline
        let stages = vec![
            ("graphics.wgsl", "VSMain", ShaderStage::Vertex),
            ("graphics.wgsl", "PSMain", ShaderStage::Fragment),
            ("compute.wgsl", "CSMain", ShaderStage::Compute),
        ];
        let validator = create_test_validator(ShadingLanguage::Wgsl);
        for (file_name, entry_point, shader_stage) in stages {
            let file_path = Path::new("./test/wgsl/stages/").join(file_name);
            let shader_content = std::fs::read_to_string(&file_path).unwrap();
            match validator.validate_shader(
                &shader_content,
                &file_path,
                &ShaderParams {
                    compilation: ShaderCompilationParams {
                        entry_point: Some(entry_point.into()),
                        shader_stage: Some(shader_stage),
                        ..Default::default()
                    },
                    ..Default::default()
                },
                &mut default_include_callback,
            ) {
                Ok(result) => {
                    println!(
                        "Diagnostic should be empty for stage {:?}: {:#?}",
                        shader_stage, result
                    );
                    assert!(result.is_empty())
                }
                Err(err) => panic!("{}", err),
            };
        }
    }

    #[test]
    fn wgsl_ok() {
        let validator = create_test_validator(ShadingLanguage::Wgsl);
        let file_path = Path::new("./test/wgsl/ok.wgsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ShaderParams::default(),
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
