//! Handle shader validation with standard API

#[cfg(not(target_os = "wasi"))]
pub mod dxc;
pub mod glslang;
pub mod validator;
pub mod wesl;

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, path::Path};

    use shader_sense_core::include::canonicalize;
    use shader_sense_core::shader::{
        CompilationResult, GlslCompilationParams, GlslProfile, GlslProfileVersion,
        GlslSpirvVersion, GlslTargetClient, ShaderCompilationParams, ShaderContextParams,
        ShaderParams, ShaderStage, ShadingLanguage, WgslCompilationParams,
    };
    use shader_sense_core::shader_error::ShaderDiagnosticList;
    use shader_sense_core::test_path;

    use super::validator::*;
    use super::*;

    #[cfg(not(target_os = "wasi"))]
    fn find_test_dxc() -> Option<std::path::PathBuf> {
        use std::path::PathBuf;
        // Dxc copied here by build system for convenience.
        // Manifest dir is not in workspace, but command executed from workspace, so hardcode the path here.
        // Should be handled cleanly, maybe by copying dll with a build.rs file.
        Some(PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..\\..\\target\\debug"))
    }
    fn create_test_validator(shading_language: ShadingLanguage) -> Box<dyn ValidatorImpl> {
        // Do not use Validator::from_shading_language to enforce dxc on PC.
        match shading_language {
            ShadingLanguage::Wgsl => Box::new(wesl::Wesl::new()),
            #[cfg(not(target_os = "wasi"))]
            ShadingLanguage::Hlsl => Box::new(dxc::Dxc::new(find_test_dxc()).unwrap()),
            #[cfg(target_os = "wasi")]
            ShadingLanguage::Hlsl => Box::new(glslang::Glslang::hlsl()),
            ShadingLanguage::Glsl => Box::new(glslang::Glslang::glsl()),
        }
    }

    #[test]
    fn glsl_ok() {
        let validator = create_test_validator(ShadingLanguage::Glsl);
        let file_path = &test_path("glsl/ok.frag.glsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ShaderParams::default(),
            &mut default_include_callback,
        ) {
            Ok((_blob, diagnostics)) => {
                println!("Diagnostic should be empty: {:#?}", diagnostics);
                assert!(diagnostics.is_empty())
            }
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn glsl_include_config() {
        let validator = create_test_validator(ShadingLanguage::Glsl);
        let file_path = &test_path("glsl/include-config.frag.glsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ShaderParams {
                context: ShaderContextParams {
                    includes: vec![test_path("glsl/inc0/")],
                    ..Default::default()
                },
                ..Default::default()
            },
            &mut default_include_callback,
        ) {
            Ok((_blob, diagnostic_list)) => {
                println!("Diagnostic should be empty: {:#?}", diagnostic_list);
                assert!(diagnostic_list.is_empty())
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    fn glsl_include_level() {
        let validator = create_test_validator(ShadingLanguage::Glsl);
        let file_path = &test_path("glsl/include-level.comp.glsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ShaderParams {
                context: ShaderContextParams {
                    includes: vec![test_path("glsl/inc0/")],
                    ..Default::default()
                },
                ..Default::default()
            },
            &mut default_include_callback,
        ) {
            Ok((_blob, diagnostic_list)) => {
                println!("Diagnostic should be empty: {:#?}", diagnostic_list);
                assert!(diagnostic_list.is_empty())
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    fn glsl_no_stage() {
        let validator = create_test_validator(ShadingLanguage::Glsl);
        let file_path = &test_path("glsl/nostage.glsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ShaderParams {
                context: ShaderContextParams {
                    includes: vec![test_path("glsl/inc0/")],
                    ..Default::default()
                },
                ..Default::default()
            },
            &mut default_include_callback,
        ) {
            Ok((_blob, diagnostic_list)) => {
                println!("Diagnostic should be empty: {:#?}", diagnostic_list);
                assert!(diagnostic_list.is_empty())
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    fn glsl_macro() {
        let validator = create_test_validator(ShadingLanguage::Glsl);
        let file_path = &test_path("glsl/macro.frag.glsl");
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
            Ok((_blob, diagnostic_list)) => {
                println!("Diagnostic should be empty: {:#?}", diagnostic_list);
                assert!(diagnostic_list.is_empty())
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    fn glsl_error_parsing() {
        let validator = create_test_validator(ShadingLanguage::Glsl);
        let file_path = test_path("glsl/error-parsing.frag.glsl");
        let shader_content = std::fs::read_to_string(&file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            &file_path,
            &ShaderParams::default(),
            &mut default_include_callback,
        ) {
            Ok((_blob, diagnostic_list)) => {
                let diags = diagnostic_list.diagnostics;
                println!("Diagnostic should not be empty: {:#?}", diags);
                assert!(diags[0].range.file_path.exists());
                assert_eq!(diags[0].error, String::from(" '#include' : Could not process include directive for header name: ./level1.glsl\n"));
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    fn glsl_no_preamble() {
        let validator = create_test_validator(ShadingLanguage::Glsl);
        let file_path = canonicalize(&test_path("glsl/dependent-include.frag.glsl")).unwrap();
        let shader_content = std::fs::read_to_string(&file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            &file_path,
            &ShaderParams::default(),
            &mut default_include_callback,
        ) {
            Ok((_blob, diagnostic_list)) => {
                println!("Diagnostic should not be empty: {:#?}", diagnostic_list);
                assert!(!diagnostic_list.is_empty())
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    fn glsl_preamble() {
        let validator = create_test_validator(ShadingLanguage::Glsl);
        let file_path = &test_path("glsl/dependent-include.frag.glsl");
        let preamble = &test_path("glsl/helpers/preamble.glsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        let preamble_content = std::fs::read_to_string(preamble).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ShaderParams {
                compilation: ShaderCompilationParams {
                    glsl: GlslCompilationParams {
                        preamble_path: Some(preamble.into()),
                        preamble_content: Some(preamble_content),
                        ..Default::default()
                    },
                    ..Default::default()
                },
                ..Default::default()
            },
            &mut default_include_callback,
        ) {
            Ok((_blob, diagnostic_list)) => {
                println!("Diagnostic should be empty: {:#?}", diagnostic_list);
                assert!(diagnostic_list.is_empty())
            }
            Err(err) => panic!("{}", err),
        };
    }

    fn validate_with_preamble(
        shading_language: ShadingLanguage,
        file_path: &Path,
        preamble_path: &Path,
        preamble_content: Option<String>,
    ) -> ShaderDiagnosticList {
        let validator = create_test_validator(shading_language);
        let file_path = canonicalize(file_path).unwrap();
        let preamble_path = canonicalize(preamble_path).unwrap();
        let shader_content = std::fs::read_to_string(&file_path).unwrap();
        let preamble_content =
            preamble_content.unwrap_or(std::fs::read_to_string(&preamble_path).unwrap());
        let mut params = ShaderParams::default();
        let compilation = &mut params.compilation;
        let (path, content) = match shading_language {
            ShadingLanguage::Hlsl => (
                &mut compilation.hlsl.preamble_path,
                &mut compilation.hlsl.preamble_content,
            ),
            ShadingLanguage::Glsl => (
                &mut compilation.glsl.preamble_path,
                &mut compilation.glsl.preamble_content,
            ),
            ShadingLanguage::Wgsl => (
                &mut compilation.wgsl.preamble_path,
                &mut compilation.wgsl.preamble_content,
            ),
        };
        *path = Some(preamble_path);
        *content = Some(preamble_content);
        match validator.validate_shader(
            &shader_content,
            &file_path,
            &params,
            &mut default_include_callback,
        ) {
            Ok((_blob, diagnostic_list)) => diagnostic_list,
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn glsl_preamble_trailing_newline() {
        let diagnostic_list = validate_with_preamble(
            ShadingLanguage::Glsl,
            &test_path("glsl/dependent-include.frag.glsl"),
            &test_path("glsl/helpers/preamble.glsl"),
            Some("#version 450\n\n#define CUSTOM_MACRO 1\n".into()),
        );
        println!("Diagnostic should be empty: {:#?}", diagnostic_list);
        assert!(diagnostic_list.is_empty());
    }

    #[test]
    fn hlsl_preamble() {
        let diagnostic_list = validate_with_preamble(
            ShadingLanguage::Hlsl,
            &test_path("hlsl/preamble.hlsl"),
            &test_path("hlsl/helpers/preamble.hlsl"),
            None,
        );
        println!("Diagnostic should be empty: {:#?}", diagnostic_list);
        assert!(diagnostic_list.is_empty());
    }

    #[test]
    fn hlsl_preamble_error_in_file() {
        let file_path = &test_path("hlsl/preamble-error.hlsl");
        let diagnostic_list = validate_with_preamble(
            ShadingLanguage::Hlsl,
            file_path,
            &test_path("hlsl/helpers/preamble.hlsl"),
            None,
        );
        println!("Diagnostic should be in file: {:#?}", diagnostic_list);
        let diagnostic = &diagnostic_list.diagnostics[0];
        assert!(diagnostic.range.file_path == canonicalize(file_path).unwrap());
        assert!(diagnostic.range.start().line == 5);
    }

    #[test]
    fn hlsl_preamble_error_in_preamble() {
        let preamble_path = &test_path("hlsl/helpers/preamble-error.hlsl");
        let diagnostic_list = validate_with_preamble(
            ShadingLanguage::Hlsl,
            &test_path("hlsl/ok.hlsl"),
            preamble_path,
            None,
        );
        println!("Diagnostic should be in preamble: {:#?}", diagnostic_list);
        let diagnostic = &diagnostic_list.diagnostics[0];
        assert!(diagnostic.range.file_path == canonicalize(preamble_path).unwrap());
        assert!(diagnostic.range.start().line == 1);
    }

    #[test]
    fn wgsl_preamble() {
        let diagnostic_list = validate_with_preamble(
            ShadingLanguage::Wgsl,
            &test_path("wgsl/preamble.wgsl"),
            &test_path("wgsl/helpers/preamble.wgsl"),
            None,
        );
        println!("Diagnostic should be empty: {:#?}", diagnostic_list);
        assert!(diagnostic_list.is_empty());
    }

    #[test]
    fn wgsl_preamble_error_in_preamble() {
        let preamble_path = &test_path("wgsl/helpers/preamble-error.wgsl");
        let diagnostic_list = validate_with_preamble(
            ShadingLanguage::Wgsl,
            &test_path("wgsl/ok.wgsl"),
            preamble_path,
            None,
        );
        println!("Diagnostic should be in preamble: {:#?}", diagnostic_list);
        let diagnostic = &diagnostic_list.diagnostics[0];
        assert!(diagnostic.range.file_path == canonicalize(preamble_path).unwrap());
        assert!(diagnostic.range.start().line == 5);
    }

    #[test]
    fn glsl_opengl() {
        let validator = create_test_validator(ShadingLanguage::Glsl);
        let file_path = &test_path("glsl/target-client/opengl.vert.glsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ShaderParams {
                compilation: ShaderCompilationParams {
                    entry_point: None,
                    shader_stage: Some(ShaderStage::Vertex),
                    glsl: GlslCompilationParams {
                        client: GlslTargetClient::OpenGL450,
                        spirv: GlslSpirvVersion::None,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                context: ShaderContextParams::default(),
            },
            &mut default_include_callback,
        ) {
            Ok((_blob, diagnostic_list)) => {
                println!("Diagnostic should be empty: {:#?}", diagnostic_list);
                assert!(diagnostic_list.is_empty())
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    fn glsl_vulkan() {
        let validator = create_test_validator(ShadingLanguage::Glsl);
        let file_path = &test_path("glsl/target-client/vulkan.vert.glsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ShaderParams {
                compilation: ShaderCompilationParams {
                    entry_point: Some("main".into()), // TODO: should not require this and induce main as entry point if not set for compile
                    shader_stage: Some(ShaderStage::Vertex),
                    glsl: GlslCompilationParams {
                        client: GlslTargetClient::Vulkan1_3,
                        spirv: GlslSpirvVersion::SPIRV1_6,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                context: ShaderContextParams::default(),
            },
            &mut default_include_callback,
        ) {
            Ok((blob, diagnostic_list)) => {
                assert!(
                    diagnostic_list.is_empty(),
                    "Diagnostic should be empty: {:#?}",
                    diagnostic_list
                );
                assert!(
                    match &blob {
                        CompilationResult::Spirv(blob) => !blob.is_empty(),
                        _ => false, // Only expect SpirV
                    },
                    "Blob should not be empty: {:?}",
                    blob
                );
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    fn glsl_webgl_1() {
        let validator = create_test_validator(ShadingLanguage::Glsl);
        let file_path = &test_path("glsl/target-client/webgl1.vert.glsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ShaderParams {
                compilation: ShaderCompilationParams {
                    entry_point: None,
                    shader_stage: Some(ShaderStage::Vertex),
                    glsl: GlslCompilationParams {
                        client: GlslTargetClient::None,
                        spirv: GlslSpirvVersion::None,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                context: ShaderContextParams::default(),
            },
            &mut default_include_callback,
        ) {
            Ok((_blob, diagnostic_list)) => {
                assert!(
                    diagnostic_list.is_empty(),
                    "Diagnostic should be empty: {:#?}",
                    diagnostic_list
                );
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    fn glsl_webgl_2() {
        let validator = create_test_validator(ShadingLanguage::Glsl);
        let file_path = &test_path("glsl/target-client/webgl2.vert.glsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ShaderParams {
                compilation: ShaderCompilationParams {
                    entry_point: None,
                    shader_stage: Some(ShaderStage::Vertex),
                    glsl: GlslCompilationParams {
                        client: GlslTargetClient::None,
                        spirv: GlslSpirvVersion::None,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                context: ShaderContextParams::default(),
            },
            &mut default_include_callback,
        ) {
            Ok((_blob, diagnostic_list)) => {
                assert!(
                    diagnostic_list.is_empty(),
                    "Diagnostic should be empty: {:#?}",
                    diagnostic_list
                );
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    fn glsl_version_profile() {
        let validator = create_test_validator(ShadingLanguage::Glsl);
        let file_path = &test_path("glsl/target-client/vulkan.vert.glsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        let validate_with_profile = |profile: GlslProfileVersion| {
            validator.validate_shader(
                &shader_content,
                file_path,
                &ShaderParams {
                    compilation: ShaderCompilationParams {
                        entry_point: Some("main".into()), // TODO: should not require this and induce main as entry point if not set for compile
                        shader_stage: Some(ShaderStage::Vertex),
                        glsl: GlslCompilationParams {
                            client: GlslTargetClient::Vulkan1_3,
                            spirv: GlslSpirvVersion::SPIRV1_6,
                            version: Some(profile),
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    context: ShaderContextParams::default(),
                },
                &mut default_include_callback,
            )
        };
        match validate_with_profile(GlslProfileVersion {
            version: 100,
            profile: GlslProfile::Core,
        }) {
            Ok(_) => panic!("Validation should fail"),
            Err(_err) => {} // Expected error InvalidProfile as InternalError
        };
        match validate_with_profile(GlslProfileVersion {
            version: 460,
            profile: GlslProfile::Core,
        }) {
            Ok((_blob, diagnostic_list)) => {
                assert!(
                    diagnostic_list.is_empty(),
                    "Diagnostic should be empty: {:#?}",
                    diagnostic_list
                );
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    fn hlsl_ok() {
        let validator = create_test_validator(ShadingLanguage::Hlsl);
        let file_path = &test_path("hlsl/ok.hlsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ShaderParams::default(),
            &mut default_include_callback,
        ) {
            Ok((blob, diagnostic_list)) => {
                assert!(
                    diagnostic_list.is_empty(),
                    "Diagnostic should be empty: {:#?}",
                    diagnostic_list
                );
                assert!(
                    match &blob {
                        #[cfg(not(target_os = "wasi"))]
                        CompilationResult::Dxil(blob) => !blob.is_empty(),
                        #[cfg(target_os = "wasi")]
                        CompilationResult::None => true, // No compilation result on wasi.
                        _ => false, // Only expect Dxil
                    },
                    "Blob should not be empty: {:?}",
                    blob
                );
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    fn hlsl_include_config() {
        let validator = create_test_validator(ShadingLanguage::Hlsl);
        let file_path = &test_path("hlsl/include-config.hlsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ShaderParams {
                context: ShaderContextParams {
                    includes: vec![test_path("hlsl/inc0/")],
                    ..Default::default()
                },
                ..Default::default()
            },
            &mut default_include_callback,
        ) {
            Ok((_blob, diagnostic_list)) => {
                println!("Diagnostic should be empty: {:#?}", diagnostic_list);
                assert!(diagnostic_list.is_empty())
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    fn hlsl_include_parent_folder() {
        let validator = create_test_validator(ShadingLanguage::Hlsl);
        let file_path = &test_path("hlsl/folder/folder-file.hlsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ShaderParams {
                context: ShaderContextParams {
                    includes: vec![test_path("hlsl/")],
                    ..Default::default()
                },
                ..Default::default()
            },
            &mut default_include_callback,
        ) {
            Ok((_blob, diagnostic_list)) => {
                println!("Diagnostic should be empty: {:#?}", diagnostic_list);
                assert!(diagnostic_list.is_empty())
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    fn hlsl_include_level() {
        let validator = create_test_validator(ShadingLanguage::Hlsl);
        let file_path = &test_path("hlsl/include-level.hlsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ShaderParams {
                context: ShaderContextParams {
                    includes: vec![test_path("hlsl/inc0/")],
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
            Ok((_blob, diagnostic_list)) => {
                println!("Diagnostic should be empty: {:#?}", diagnostic_list);
                assert!(diagnostic_list.is_empty())
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    fn hlsl_macro() {
        let validator = create_test_validator(ShadingLanguage::Hlsl);
        let file_path = &test_path("hlsl/macro.hlsl");
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
            Ok((_blob, diagnostic_list)) => {
                println!("Diagnostic should be empty: {:#?}", diagnostic_list);
                assert!(diagnostic_list.is_empty())
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    #[cfg(not(target_os = "wasi"))] // Somehow glslang fail to enable 16bit types... Disabled for now.
    fn hlsl_16bits_types_ok() {
        use shader_sense_core::shader::HlslCompilationParams;

        let validator = create_test_validator(ShadingLanguage::Hlsl);
        let file_path = &test_path("hlsl/16bit-types.hlsl");
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
            Ok((_blob, diagnostic_list)) => {
                println!("Diagnostic should be empty: {:#?}", diagnostic_list);
                assert!(diagnostic_list.is_empty())
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    #[cfg(not(target_os = "wasi"))] // Default behaviour of glslang, so ignore
    fn hlsl_spirv_ok() {
        use shader_sense_core::shader::HlslCompilationParams;

        let validator = create_test_validator(ShadingLanguage::Hlsl);
        let file_path = &test_path("hlsl/spirv-shader.hlsl");
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
            Ok((_blob, diagnostic_list)) => {
                println!("Diagnostic should not be empty: {:#?}", diagnostic_list);
                assert!(!diagnostic_list.is_empty())
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
            Ok((_blob, diagnostic_list)) => {
                println!("Diagnostic should be empty: {:#?}", diagnostic_list);
                assert!(diagnostic_list.is_empty())
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
            let file_path = &test_path("glsl/stages/").join(file_name);
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
                Ok((_blob, diagnostic_list)) => {
                    assert!(
                        diagnostic_list.is_empty(),
                        "Diagnostic should be empty for stage {:?}: {:#?}",
                        shader_stage,
                        diagnostic_list
                    )
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
            let file_path = &test_path("hlsl/stages/").join(file_name);
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
                    Ok((_blob, diagnostic_list)) => {
                        println!(
                            "Diagnostic should be empty for stage {:?}: {:#?}",
                            shader_stage, diagnostic_list
                        );
                        assert!(diagnostic_list.is_empty())
                    }
                    Err(err) => panic!("{}", err),
                };
            }
        }
    }

    #[test]
    #[cfg(not(target_os = "wasi"))] // Glslang HLSL do not support pragma once
    fn hlsl_pragma_once() {
        let validator = create_test_validator(ShadingLanguage::Hlsl);
        let file_path = &test_path("hlsl/pragma-once/main.hlsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            file_path,
            &ShaderParams {
                compilation: ShaderCompilationParams {
                    entry_point: Some("main".into()),
                    shader_stage: Some(ShaderStage::Compute),
                    ..Default::default()
                },
                ..Default::default()
            },
            &mut default_include_callback,
        ) {
            Ok((_blob, diagnostic_list)) => {
                println!("Diagnostic should be empty: {:#?}", diagnostic_list);
                assert!(diagnostic_list.is_empty())
            }
            Err(err) => panic!("{}", err),
        };
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
            let file_path = canonicalize(&&test_path("wgsl/stages/").join(file_name)).unwrap();
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
                Ok((_blob, diagnostic_list)) => {
                    println!(
                        "Diagnostic should be empty for stage {:?}: {:#?}",
                        shader_stage, diagnostic_list
                    );
                    assert!(diagnostic_list.is_empty())
                }
                Err(err) => panic!("{}", err),
            };
        }
    }

    #[test]
    fn wgsl_ok() {
        let validator = create_test_validator(ShadingLanguage::Wgsl);
        let file_path = canonicalize(&test_path("wgsl/ok.wgsl")).unwrap();
        let shader_content = std::fs::read_to_string(&file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            &file_path,
            &ShaderParams::default(),
            &mut default_include_callback,
        ) {
            Ok((_blob, diagnostic_list)) => {
                println!("Diagnostic should be empty: {:#?}", diagnostic_list);
                assert!(diagnostic_list.is_empty())
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    fn wgsl_error() {
        let validator = create_test_validator(ShadingLanguage::Wgsl);
        let file_path = canonicalize(&test_path("wgsl/error.wgsl")).unwrap();
        let shader_content = std::fs::read_to_string(&file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            &file_path,
            &ShaderParams::default(),
            &mut default_include_callback,
        ) {
            Ok((_blob, diagnostic_list)) => {
                println!("Diagnostic should not be empty: {:#?}", diagnostic_list);
                assert!(!diagnostic_list.is_empty());
                // Missing entry point attribute should be mapped back on the function declaration.
                let range = &diagnostic_list.diagnostics[0].range;
                assert_eq!(range.file_path, file_path);
                assert_eq!(range.range.start.line, 1);
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    fn wgsl_error_nested() {
        let validator = create_test_validator(ShadingLanguage::Wgsl);
        let file_path = canonicalize(&test_path("wgsl/error-nested.wgsl")).unwrap();
        let shader_content = std::fs::read_to_string(&file_path).unwrap();
        match validator.validate_shader(
            &shader_content,
            &file_path,
            &ShaderParams::default(),
            &mut default_include_callback,
        ) {
            Ok((_blob, diagnostic_list)) => {
                println!("Diagnostic should not be empty: {:#?}", diagnostic_list);
                assert!(!diagnostic_list.is_empty());
                // Error should be mapped back on the statement inside the else block.
                let range = &diagnostic_list.diagnostics[0].range;
                assert_eq!(range.file_path, file_path);
                assert_eq!(range.range.start.line, 9);
            }
            Err(err) => panic!("{}", err),
        };
    }

    fn validate_wesl_import(file_name: &str) -> ShaderDiagnosticList {
        let validator = create_test_validator(ShadingLanguage::Wgsl);
        let package_root = canonicalize(&test_path("wesl")).unwrap();
        let file_path = package_root.join(file_name);
        let shader_content = std::fs::read_to_string(&file_path).unwrap();
        let params = ShaderParams {
            compilation: ShaderCompilationParams {
                wgsl: WgslCompilationParams {
                    package_root: Some(package_root.clone()),
                    packages: HashMap::from([("external".into(), package_root.join("external"))]),
                    ..Default::default()
                },
                ..Default::default()
            },
            ..Default::default()
        };
        match validator.validate_shader(
            &shader_content,
            &file_path,
            &params,
            &mut default_include_callback,
        ) {
            Ok((_blob, diagnostic_list)) => {
                println!("Diagnostics for {}: {:#?}", file_name, diagnostic_list);
                diagnostic_list
            }
            Err(err) => panic!("{}", err),
        }
    }

    fn validate_wesl_features(defines: &[(&str, &str)]) -> ShaderDiagnosticList {
        let validator = create_test_validator(ShadingLanguage::Wgsl);
        let file_path = canonicalize(&test_path("wesl/features.wesl")).unwrap();
        let shader_content = std::fs::read_to_string(&file_path).unwrap();
        let params = ShaderParams {
            context: ShaderContextParams {
                defines: defines
                    .iter()
                    .map(|(name, value)| (name.to_string(), value.to_string()))
                    .collect(),
                ..Default::default()
            },
            ..Default::default()
        };
        match validator.validate_shader(
            &shader_content,
            &file_path,
            &params,
            &mut default_include_callback,
        ) {
            Ok((_blob, diagnostic_list)) => diagnostic_list,
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn wesl_features() {
        // Defined features are enabled, with any value but 0 or false.
        assert!(validate_wesl_features(&[("debug", "")]).is_empty());
        assert!(validate_wesl_features(&[("debug", "1"), ("textured", "1")]).is_empty());
        // Undefined or disabled features remove debug_only, which is used by main.
        assert!(!validate_wesl_features(&[]).is_empty());
        assert!(!validate_wesl_features(&[("debug", "0")]).is_empty());
        assert!(!validate_wesl_features(&[("debug", "False")]).is_empty());
    }

    #[test]
    fn wesl_import_ok() {
        // Imports from package, from super & from an external package.
        assert!(validate_wesl_import("main.wesl").is_empty());
    }

    #[test]
    fn wesl_import_missing_module() {
        let diagnostic_list = validate_wesl_import("error_import.wesl");
        assert!(!diagnostic_list.is_empty());
        // Unresolved module should point to its import statement.
        let range = &diagnostic_list.diagnostics[0].range;
        assert!(range.file_path.ends_with("error_import.wesl"));
        assert_eq!(range.range.start.line, 1);
        assert_eq!(range.range.start.pos, 0);
        assert_eq!(range.range.end.pos, 29);
    }

    #[test]
    fn wesl_import_error_in_module() {
        // Error in imported module should be reported in that module.
        let diagnostic_list = validate_wesl_import("error_module.wesl");
        assert!(!diagnostic_list.is_empty());
        let range = &diagnostic_list.diagnostics[0].range;
        assert!(range.file_path.ends_with("broken.wesl"));
        assert_eq!(range.range.start.line, 1);
    }

    #[test]
    fn wesl_import_unknown_package() {
        let validator = create_test_validator(ShadingLanguage::Wgsl);
        let file_path = canonicalize(&test_path("wesl/main.wesl")).unwrap();
        let shader_content = std::fs::read_to_string(&file_path).unwrap();
        // No external package declared.
        match validator.validate_shader(
            &shader_content,
            &file_path,
            &ShaderParams::default(),
            &mut default_include_callback,
        ) {
            Ok((_blob, diagnostic_list)) => {
                println!("{:#?}", diagnostic_list);
                assert!(!diagnostic_list.is_empty());
                assert!(diagnostic_list.diagnostics[0].error.contains("external"));
            }
            Err(err) => panic!("{}", err),
        };
    }

    #[test]
    fn wgsl_spirv_conversion() {
        let file_path = &test_path("wgsl/ok.wgsl");
        let shader_content = std::fs::read_to_string(file_path).unwrap();
        let spirv = match wesl::wgsl_to_spirv(&shader_content) {
            Ok(spirv) => spirv,
            Err(err) => panic!("{}", err),
        };
        assert!(!spirv.is_empty());
        match wesl::spirv_to_wgsl(&spirv) {
            // Generated wgsl is not the same as the input one, but should hold the same entry point.
            Ok(wgsl) => assert!(wgsl.contains("vs_main"), "Unexpected wgsl output: {}", wgsl),
            Err(err) => panic!("{}", err),
        };
    }
}
