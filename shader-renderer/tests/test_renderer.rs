use std::path::Path;
use std::time::SystemTime;

use image::{ImageBuffer, RgbaImage};
use shader_renderer::renderer::{self};
use shader_renderer::server::notification::ResizeTargetNotificationParams;
use shader_renderer::server::{self, notification::UpdateShaderNotificationParams};
use shader_sense::shader::{
    ShaderCompilationParams, ShaderContextParams, ShaderParams, ShaderStage, ShadingLanguage,
};
use shader_sense::validator::validator::{CompilationResult, Validator};

use crate::test_server::{TestFile, TestServer};

mod test_server;

/// Default include callback for [`Validator::validate_shader`]
pub fn default_include_callback(path: &Path) -> Option<String> {
    Some(std::fs::read_to_string(path).unwrap())
}

fn spirv8_to_32(bytes: Vec<u8>) -> Vec<u32> {
    assert!(bytes.len() % 4 == 0);
    let len = bytes.len() / 4;
    let mut vec = Vec::from(bytes);
    let ptr = vec.as_mut_ptr() as *mut u32;
    std::mem::forget(vec);
    unsafe { Vec::from_raw_parts(ptr, len, len) }
}

fn load_spirv(file: &TestFile, shader_stage: ShaderStage) -> Vec<u32> {
    let spirv_path = {
        let mut path = file.file_path.clone();
        path.add_extension("spv");
        path
    };
    // Check if cached or generate and cache.
    fn get_time(path: &Path) -> SystemTime {
        std::fs::metadata(&path).unwrap().modified().unwrap()
    }
    // Check file was updated.
    let modification_time = get_time(&file.file_path);
    if std::fs::exists(&spirv_path).unwrap() && modification_time < get_time(&spirv_path) {
        println!("Loading spirv from cache at {}", spirv_path.display());
        let bytes = std::fs::read(spirv_path).unwrap();
        spirv8_to_32(bytes)
    } else {
        println!("Generating spirv from {}", file.file_path.display());
        let validator = Validator::glsl();
        let (compilation, diagnostics) = validator
            .validate_shader(
                &file.content,
                &file.file_path,
                &ShaderParams {
                    context: ShaderContextParams::default(),
                    compilation: ShaderCompilationParams {
                        entry_point: Some("main".into()),
                        shader_stage: Some(shader_stage),
                        ..Default::default()
                    },
                },
                &mut default_include_callback,
            )
            .unwrap();
        if let CompilationResult::Spirv(spirv) = compilation {
            println!("Saving generated spirv to cache at {}", spirv_path.display());
            std::fs::write(spirv_path, &spirv).unwrap();
            spirv8_to_32(spirv)
        } else {
            panic!("Compilation result is not SPIRV: {:?} \nDiagnostics: {:?}", compilation, diagnostics);
        }
    }
}

#[test]
fn test_graphic_pipeline() {
    const WIDTH: u32 = 1280;
    const HEIGHT: u32 = 720;

    let mut server = TestServer::desktop().unwrap();

    let fragment = TestFile::new(
        Path::new("./tests/assets/frag.glsl"),
        ShadingLanguage::Glsl,
    );
    let vertex = TestFile::new(
        Path::new("./tests/assets/vert.glsl"),
        ShadingLanguage::Glsl,
    );
    server.send_notification::<server::notification::ResizeTargetNotification>(
        &ResizeTargetNotificationParams {
            width: WIDTH,
            height: HEIGHT,
        },
    );
    server.send_notification::<server::notification::UpdateShaderNotification>(
        &UpdateShaderNotificationParams {
            shader_stage: ShaderStage::Vertex,
            shader: Some(renderer::Shader {
                shading_language: ShadingLanguage::Glsl,
                stage: ShaderStage::Vertex,
                entry_point: "main".into(),
                source: renderer::ShaderSource::Spirv(load_spirv(&vertex, ShaderStage::Vertex)),
            }),
        },
    );
    // TODO: set dummy shaders that are always present if we remove them.
    server.send_notification::<server::notification::UpdateShaderNotification>(
        &UpdateShaderNotificationParams {
            shader_stage: ShaderStage::Fragment,
            shader: Some(renderer::Shader {
                shading_language: ShadingLanguage::Glsl,
                stage: ShaderStage::Fragment,
                entry_point: "main".into(),
                source: renderer::ShaderSource::Spirv(load_spirv(&fragment, ShaderStage::Fragment)),
            }),
        },
    );
    server.send_request::<server::request::RenderRequest>(&(), |result| {
        assert_eq!(result.data.len(), (WIDTH * HEIGHT * 4) as usize);
        assert_eq!(result.data[0], 255);
        assert_eq!(result.data[1], 255);
        assert_eq!(result.data[2], 0);
        assert_eq!(result.data[3], 255);
        log::info!("Result: {:?}", result.data.len());
        let img: RgbaImage = ImageBuffer::from_raw(WIDTH, HEIGHT, result.data).unwrap();
        img.save("output.png").unwrap();
    });
}
