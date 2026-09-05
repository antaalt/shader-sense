use std::collections::HashMap;
use std::path::Path;

use image::{ImageBuffer, RgbaImage};
use shader_renderer::renderer::{self};
use shader_renderer::server::notification::ResizeTargetNotificationParams;
use shader_renderer::server::request::UpdateShaderRequestParams;
use shader_renderer::server::{self};
use shader_sense::shader::{ShaderStage, ShadingLanguage};

use crate::test_server::{TestFile, TestServer};

mod test_server;

/// Default include callback for [`Validator::validate_shader`]
pub fn default_include_callback(path: &Path) -> Option<String> {
    Some(std::fs::read_to_string(path).unwrap())
}

#[test]
fn test_graphic_pipeline() {
    const WIDTH: u32 = 1280;
    const HEIGHT: u32 = 720;

    let mut server = TestServer::desktop().unwrap();

    let fragment = TestFile::new(Path::new("./tests/assets/frag.glsl"), ShadingLanguage::Glsl);
    let vertex = TestFile::new(Path::new("./tests/assets/vert.glsl"), ShadingLanguage::Glsl);
    server.send_notification::<server::notification::ResizeTargetNotification>(
        &ResizeTargetNotificationParams {
            width: WIDTH,
            height: HEIGHT,
        },
    );
    server.send_request::<server::request::UpdateShaderRequest>(
        &UpdateShaderRequestParams {
            shader_stage: ShaderStage::Vertex,
            shader: Some(renderer::shader::Shader {
                shading_language: ShadingLanguage::Glsl,
                stage: ShaderStage::Vertex,
                entry_point: "main".into(),
                file_path: vertex.file_path,
                content: vertex.content,
                defines: HashMap::new(),
                includes: Vec::new(),
            }),
        },
        |_| {},
    );
    server.send_request::<server::request::UpdateShaderRequest>(
        &UpdateShaderRequestParams {
            shader_stage: ShaderStage::Fragment,
            shader: Some(renderer::shader::Shader {
                shading_language: ShadingLanguage::Glsl,
                stage: ShaderStage::Fragment,
                entry_point: "main".into(),
                file_path: fragment.file_path,
                content: fragment.content,
                defines: HashMap::new(),
                includes: Vec::new(),
            }),
        },
        |_| {},
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
