use std::{collections::HashMap, path::PathBuf};

use log::info;
use serde::{Deserialize, Serialize};
use shader_sense::{
    shader::{
        ShaderCompilationParams, ShaderContextParams, ShaderParams, ShaderStage, ShadingLanguage,
    },
    validator::validator::{default_include_callback, CompilationResult, Validator},
};
use wgpu::{
    wgt::CreateShaderModuleDescriptorPassthrough, PassthroughShaderEntryPoint, ShaderModule,
    ShaderModuleDescriptor,
};

use crate::renderer::{error::RendererError, Renderer};

#[derive(Debug, Serialize, Deserialize)]
pub enum ShaderCompilation {
    Spirv(Vec<u32>),
    Dxil(Vec<u8>),
    Wgsl(String),
    Glsl(String),
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
// Send shader path to server, along variant info
pub struct Shader {
    // TODO: this should have reflection data ? Or compute them at runtime instead.
    pub file_path: PathBuf,
    pub shading_language: ShadingLanguage,
    pub stage: ShaderStage,
    pub entry_point: String,
    pub content: String,
    pub defines: HashMap<String, String>,
    pub includes: Vec<PathBuf>,
}

impl Shader {
    pub fn stage(&self) -> ShaderStage {
        self.stage
    }
    pub fn content(&self) -> &String {
        &self.content
    }
    pub fn shading_language(&self) -> &ShadingLanguage {
        &self.shading_language
    }
    pub fn entry_point(&self) -> &str {
        &self.entry_point
    }

    fn cast_vec8_to_32(bytes: Vec<u8>) -> Vec<u32> {
        assert!(bytes.len() % 4 == 0);
        let len = bytes.len() / 4;
        let mut vec = Vec::from(bytes);
        let ptr = vec.as_mut_ptr() as *mut u32;
        std::mem::forget(vec);
        unsafe { Vec::from_raw_parts(ptr, len, len) }
    }

    fn compile_shader(&self) -> Result<ShaderCompilation, RendererError> {
        let validator = Validator::glsl();
        let (compilation, _diagnostics) = validator
            .validate_shader(
                &self.content,
                &self.file_path,
                &ShaderParams {
                    context: ShaderContextParams {
                        defines: self.defines.clone(),
                        includes: self.includes.clone(),
                        path_remapping: HashMap::new(),
                    },
                    compilation: ShaderCompilationParams {
                        entry_point: Some(self.entry_point.clone()),
                        shader_stage: Some(self.stage),
                        ..Default::default()
                    },
                },
                &mut default_include_callback,
            )
            .unwrap();
        match compilation {
            CompilationResult::None => Err(RendererError::InternalError(
                "Shader compilation failed.".into(),
            )),
            CompilationResult::Spirv(spirv) => {
                Ok(ShaderCompilation::Spirv(Self::cast_vec8_to_32(spirv)))
            }
            CompilationResult::Dxil(dxil) => Ok(ShaderCompilation::Dxil(dxil)),
            CompilationResult::Wgsl(wgsl) => Ok(ShaderCompilation::Wgsl(wgsl)),
        }
    }

    pub fn create_shader_module(&self, renderer: &Renderer) -> Result<ShaderModule, RendererError> {
        let compilation = self.compile_shader()?;
        if matches!(
            compilation,
            ShaderCompilation::Dxil(_) | ShaderCompilation::Glsl(_)
        ) && !renderer
            .device
            .features()
            .contains(wgpu::Features::PASSTHROUGH_SHADERS)
        {
            return Err(RendererError::InternalError(format!(
                "Device does not support passthrough shaders, required for {:?} sources. Compile the shader to SPIRV or WGSL instead.",
                self.shading_language
            )));
        }
        info!(
            "Creating shader module for {:?} stage with entry point {}",
            self.stage, self.entry_point
        );
        renderer.catch_validation_error(
            &format!("create the {:?} shader module", self.stage),
            |renderer| match &compilation {
                // Ensure validation
                ShaderCompilation::Spirv(_) | ShaderCompilation::Wgsl(_) => renderer
                    .device
                    .create_shader_module(ShaderModuleDescriptor {
                        label: Some(&self.stage.to_string()),
                        source: match &compilation {
                            ShaderCompilation::Spirv(spirv) => {
                                wgpu::ShaderSource::SpirV(std::borrow::Cow::Borrowed(spirv))
                            }
                            ShaderCompilation::Wgsl(wgsl) => {
                                wgpu::ShaderSource::Wgsl(std::borrow::Cow::Borrowed(wgsl))
                            }
                            _ => unreachable!(),
                        },
                    }),
                // Unsafe.
                ShaderCompilation::Dxil(_) | ShaderCompilation::Glsl(_) => unsafe {
                    renderer.device.create_shader_module_passthrough(
                        CreateShaderModuleDescriptorPassthrough {
                            label: Some(&self.stage.to_string()),
                            entry_points: std::borrow::Cow::Borrowed(&[
                                PassthroughShaderEntryPoint {
                                    name: std::borrow::Cow::Borrowed(&self.entry_point),
                                    workgroup_size: (1, 1, 1), // Only for metal
                                },
                            ]),
                            dxil: if let ShaderCompilation::Dxil(dxil) = &compilation {
                                Some(std::borrow::Cow::Borrowed(dxil))
                            } else {
                                None
                            },
                            glsl: if let ShaderCompilation::Glsl(glsl) = &compilation {
                                Some(std::borrow::Cow::Borrowed(glsl))
                            } else {
                                None
                            },
                            ..Default::default()
                        },
                    )
                },
            },
        )
    }
}
