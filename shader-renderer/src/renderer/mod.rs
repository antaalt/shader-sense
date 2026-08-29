use std::{collections::HashMap, num::NonZeroU64, time::Duration};

use serde::{Deserialize, Serialize};
use shader_sense::shader::{ShaderStage, ShadingLanguage};
use wgpu::{
    util::DeviceExt,
    wgt::{BufferDescriptor, CreateShaderModuleDescriptorPassthrough, TextureDescriptor},
    BufferBinding, BufferUsages, Color, ColorTargetState, ColorWrites, ComputePipeline,
    ExperimentalFeatures, Extent3d, MeshPipelineDescriptor, MultisampleState, Operations, Origin3d,
    PassthroughShaderEntryPoint, PipelineCompilationOptions, PrimitiveState, PrimitiveTopology,
    RenderPassColorAttachment, RenderPipeline, ShaderModule, ShaderModuleDescriptor,
    TexelCopyBufferInfo, TexelCopyBufferLayout, TexelCopyTextureInfoBase, VertexState,
};

use crate::renderer::error::RendererError;

pub mod error;

#[derive(Serialize, Deserialize)]
pub enum ShaderSource {
    Spirv(Vec<u32>),
    Dxil(Vec<u8>),
    Wgsl(String),
    Glsl(String),
}

#[derive(Serialize, Deserialize)]
// Send shader path to server, along variant info
pub struct Shader {
    shading_language: ShadingLanguage,
    stage: ShaderStage,
    entry_point: String,
    source: ShaderSource,
}

pub struct BindGroupLayoutEntry {}

pub struct BindGroupLayout {
    pub entries: Vec<BindGroupLayoutEntry>,
}
pub struct BindGroupEntry {
    // TODO: data
    buffer: wgpu::Buffer,
}
pub struct BindGroup {
    pub entries: Vec<BindGroupEntry>,
}

pub struct BindGroupSlot {
    bind_group: BindGroup,
    bind_group_layout: BindGroupLayout,
}

impl BindGroupSlot {
    fn convert_bind_group_layout(&self, device: &wgpu::Device) -> wgpu::BindGroupLayout {
        device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some("BindGroupLayout".into()),
            entries: &self
                .bind_group_layout
                .entries
                .iter()
                .map(|e| wgpu::BindGroupLayoutEntry {
                    binding: 0,
                    visibility: wgpu::ShaderStages::VERTEX | wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Buffer {
                        ty: wgpu::BufferBindingType::Storage { read_only: true },
                        // This is the size of a single element in the buffer.
                        min_binding_size: Some(NonZeroU64::new(4).unwrap()),
                        has_dynamic_offset: false,
                    },
                    count: None,
                })
                .collect::<Vec<wgpu::BindGroupLayoutEntry>>(),
        })
    }
    fn convert_bind_group(&self, device: &wgpu::Device) -> wgpu::BindGroup {
        device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("BindGroup".into()),
            layout: &self.convert_bind_group_layout(device),
            entries: &self
                .bind_group
                .entries
                .iter()
                .map(|bind_group_entry| wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::Buffer(BufferBinding {
                        buffer: &bind_group_entry.buffer,
                        offset: 0,
                        size: None,
                    }),
                })
                .collect::<Vec<wgpu::BindGroupEntry>>(),
        })
    }
}

pub struct Renderer {
    width: u32,
    height: u32,
    instance: wgpu::Instance,
    adapter: wgpu::Adapter,
    device: wgpu::Device,
    queue: wgpu::Queue,
    active_shaders: HashMap<ShaderStage, Shader>,
    headless_surface: wgpu::Texture,
    headless_surface_view: wgpu::TextureView,
    read_back_buffer: wgpu::Buffer,
    graphic_pipeline: Option<RenderPipeline>,
    compute_pipeline: Option<ComputePipeline>,
    bind_group: Option<Vec<BindGroupSlot>>,
}

impl Renderer {
    const SURFACE_FORMAT: wgpu::TextureFormat = wgpu::TextureFormat::Rgba8Unorm;

    pub fn new(width: u32, height: u32) -> Self {
        let instance = wgpu::Instance::new(wgpu::InstanceDescriptor {
            backends: Default::default(), // TODO: Dxil vs spirv vs wgsl need different backend...
            flags: Default::default(),
            memory_budget_thresholds: Default::default(),
            backend_options: Default::default(),
            display: None,
        });

        // Select adapter
        let adapter =
            pollster::block_on(instance.request_adapter(&wgpu::RequestAdapterOptions::default()))
                .expect("Failed to create adapter");

        // Print out some basic information about the adapter.
        println!("Running on Adapter: {:#?}", adapter.get_info());

        // Check to see if the adapter supports compute shaders. While WebGPU guarantees support for
        // compute shaders, wgpu supports a wider range of devices through the use of "downlevel" devices.
        let downlevel_capabilities = adapter.get_downlevel_capabilities();
        if !downlevel_capabilities
            .flags
            .contains(wgpu::DownlevelFlags::COMPUTE_SHADERS)
        {
            panic!("Adapter does not support compute shaders");
        }
        let (device, queue) = pollster::block_on(adapter.request_device(&wgpu::DeviceDescriptor {
            label: Some("Device".into()),
            required_features: wgpu::Features::empty(),
            required_limits: wgpu::Limits::downlevel_defaults(),
            memory_hints: wgpu::MemoryHints::MemoryUsage,
            trace: wgpu::Trace::Off,
            experimental_features: ExperimentalFeatures::disabled(),
        }))
        .expect("Failed to create device");

        let headless_surface = device.create_texture(&TextureDescriptor {
            label: Some("HeadlessSurface".into()),
            size: wgpu::Extent3d {
                width,
                height,
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: Self::SURFACE_FORMAT,
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            view_formats: &[Self::SURFACE_FORMAT],
        });
        let headless_surface_view =
            headless_surface.create_view(&wgpu::wgt::TextureViewDescriptor {
                label: Some("HeadlessViewSurface".into()),
                format: Some(Self::SURFACE_FORMAT),
                dimension: Some(wgpu::TextureViewDimension::D2),
                usage: Some(wgpu::TextureUsages::RENDER_ATTACHMENT),
                aspect: wgpu::TextureAspect::All,
                base_mip_level: 0,
                mip_level_count: None,
                base_array_layer: 0,
                array_layer_count: None,
            });
        let read_back_buffer = device.create_buffer(&BufferDescriptor {
            label: Some("ReadbackBuffer".into()),
            size: (width * height * 4) as u64,
            usage: BufferUsages::MAP_READ,
            mapped_at_creation: false,
        });
        Self {
            width,
            height,
            instance,
            adapter,
            device,
            queue,
            active_shaders: HashMap::new(),
            headless_surface,
            headless_surface_view,
            read_back_buffer,
            graphic_pipeline: None,
            compute_pipeline: None,
            bind_group: None,
        }
    }
    pub fn resize(&self, width: u32, height: u32) {
        todo!()
    }
    pub fn remove_shader(&self, shader_stage: ShaderStage) {
        todo!()
    }
    pub fn set_shader(&mut self, shader: Shader) {
        // TODO: need to reflect shader and create bind group here. Or pass reflection result as JSON.
        // Would be nice to pass reflection result as client NEEDS them to map everything.
        // But it could be generated here via spirv tools (and naga and what about dxil ??)
        // and returned via a response.
        if let Some(old_shader) = self.active_shaders.insert(shader.stage, shader) {
            // TODO: something ?
        }
        let graphic_pipeline = self.create_graphic_pipeline();
    }
    pub fn create_shader_module(&self, shader: &Shader) -> ShaderModule {
        match &shader.source {
            // Ensure validation
            ShaderSource::Spirv(_) | ShaderSource::Wgsl(_) => {
                self.device.create_shader_module(ShaderModuleDescriptor {
                    label: Some(&shader.stage.to_string()),
                    source: match &shader.source {
                        ShaderSource::Spirv(spirv) => {
                            wgpu::ShaderSource::SpirV(std::borrow::Cow::Borrowed(spirv))
                        }
                        ShaderSource::Wgsl(wgsl) => {
                            wgpu::ShaderSource::Wgsl(std::borrow::Cow::Borrowed(wgsl))
                        }
                        _ => unreachable!(),
                    },
                })
            }
            // Unsafe.
            ShaderSource::Dxil(_) | ShaderSource::Glsl(_) => unsafe {
                self.device.create_shader_module_passthrough(
                    CreateShaderModuleDescriptorPassthrough {
                        label: Some(&shader.stage.to_string()),
                        entry_points: std::borrow::Cow::Borrowed(&[PassthroughShaderEntryPoint {
                            name: std::borrow::Cow::Borrowed(&shader.entry_point),
                            workgroup_size: (1, 1, 1), // Only for metal
                        }]),
                        dxil: if let ShaderSource::Dxil(dxil) = &shader.source {
                            Some(std::borrow::Cow::Borrowed(dxil))
                        } else {
                            None
                        },
                        glsl: if let ShaderSource::Glsl(glsl) = &shader.source {
                            Some(std::borrow::Cow::Borrowed(glsl))
                        } else {
                            None
                        },
                        ..Default::default()
                    },
                )
            },
        }
    }
    /*fn update_bind_group(&mut self) {
        let input_data_buffer = self
            .device
            .create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: None,
                contents: &[],
                usage: wgpu::BufferUsages::STORAGE,
            });

        // Now we create a buffer to store the output data.
        let output_data_buffer = self.device.create_buffer(&wgpu::BufferDescriptor {
            label: None,
            size: input_data_buffer.size(),
            usage: wgpu::BufferUsages::STORAGE | wgpu::BufferUsages::COPY_SRC,
            mapped_at_creation: false,
        });
        let bind_group = self.device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: None,
            layout: &bind_group_layout,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: input_data_buffer.as_entire_binding(),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: output_data_buffer.as_entire_binding(),
                },
            ],
        });
    }*/
    fn create_graphic_pipeline(&mut self) -> Result<RenderPipeline, RendererError> {
        let bind_group_layout = if let Some(bind_group) = &self.bind_group {
            bind_group
                .into_iter()
                .map(|bind_group| bind_group.convert_bind_group_layout(&self.device))
                .collect::<Vec<wgpu::BindGroupLayout>>()
        } else {
            vec![] // No layout
        };
        let pipeline_layout = self
            .device
            .create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                label: None,
                bind_group_layouts: &bind_group_layout
                    .iter()
                    .map(|layout| Some(layout))
                    .collect::<Vec<Option<&wgpu::BindGroupLayout>>>(),
                immediate_size: 0,
            });
        let (vertex_shader, vertex_entry_point) =
            if let Some(vertex) = self.active_shaders.get(&ShaderStage::Vertex) {
                (
                    self.create_shader_module(vertex),
                    vertex.entry_point.clone(),
                )
            } else {
                return Err(RendererError::InternalError("No vertex shader set".into()));
            };
        let fragment_shader =
            if let Some(fragment) = self.active_shaders.get(&ShaderStage::Fragment) {
                Some((
                    self.create_shader_module(fragment),
                    fragment.entry_point.clone(),
                ))
            } else {
                None
            };
        let color_target_state = vec![Some(ColorTargetState {
            format: Self::SURFACE_FORMAT,
            blend: None,
            write_mask: ColorWrites::all(),
        })];
        Ok(self
            .device
            .create_render_pipeline(&wgpu::RenderPipelineDescriptor {
                label: Some("RenderPipeline".into()),
                layout: Some(&pipeline_layout),
                vertex: VertexState {
                    module: &vertex_shader,
                    entry_point: Some(&vertex_entry_point),
                    compilation_options: PipelineCompilationOptions {
                        constants: &[],
                        zero_initialize_workgroup_memory: false,
                    },
                    buffers: &[],
                },
                primitive: PrimitiveState {
                    topology: PrimitiveTopology::TriangleList,
                    strip_index_format: None,
                    front_face: wgpu::FrontFace::Ccw,
                    cull_mode: None,
                    unclipped_depth: false,
                    polygon_mode: wgpu::PolygonMode::Fill,
                    conservative: false,
                },
                depth_stencil: None,
                multisample: MultisampleState::default(),
                fragment: fragment_shader.as_ref().map(
                    |(fragment_shader, fragment_entry_point)| wgpu::FragmentState {
                        module: fragment_shader,
                        entry_point: Some(fragment_entry_point),
                        compilation_options: PipelineCompilationOptions {
                            constants: &[],
                            zero_initialize_workgroup_memory: false,
                        },
                        targets: &color_target_state,
                    },
                ),
                multiview_mask: None,
                cache: None,
            }))
    }
    fn create_mesh_pipeline(
        &mut self,
        bind_group_layout: Vec<&wgpu::BindGroupLayout>,
    ) -> Result<RenderPipeline, RendererError> {
        let pipeline_layout = self
            .device
            .create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                label: None,
                bind_group_layouts: &bind_group_layout
                    .into_iter()
                    .map(|layout| Some(layout))
                    .collect::<Vec<Option<&wgpu::BindGroupLayout>>>(),
                immediate_size: 0,
            });
        Ok(self.device.create_mesh_pipeline(&MeshPipelineDescriptor {
            label: Some("MeshPipeline".into()),
            layout: Some(&pipeline_layout),
            task: todo!(),
            mesh: todo!(),
            primitive: todo!(),
            depth_stencil: todo!(),
            multisample: todo!(),
            fragment: todo!(),
            multiview: todo!(),
            cache: todo!(),
        }))
    }
    fn create_compute_pipeline(&mut self) -> Result<ComputePipeline, RendererError> {
        // The pipeline layout describes the bind groups that a pipeline expects
        let pipeline_layout = self
            .device
            .create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                label: None,
                bind_group_layouts: &[None],
                immediate_size: 0,
            });
        match self.active_shaders.get(&ShaderStage::Compute) {
            Some(compute_shader) => {
                let compute_module = self.create_shader_module(compute_shader);
                Ok(self
                    .device
                    .create_compute_pipeline(&wgpu::ComputePipelineDescriptor {
                        label: None,
                        layout: Some(&pipeline_layout),
                        module: &compute_module,
                        entry_point: Some(&compute_shader.entry_point),
                        compilation_options: wgpu::PipelineCompilationOptions::default(),
                        cache: None,
                    }))
            }
            None => Err(RendererError::InternalError("No compute shader set".into())),
        }
    }

    pub fn render(&self) -> Result<Vec<u8>, RendererError> {
        if self.graphic_pipeline.is_none() {
            return Err(RendererError::InternalError(
                "No graphic pipeline set".into(),
            ));
        }
        let bind_groups = if let Some(bind_group) = &self.bind_group {
            bind_group
                .into_iter()
                .map(|bind_group| bind_group.convert_bind_group(&self.device))
                .collect::<Vec<wgpu::BindGroup>>()
        } else {
            vec![] // No bind group
        };
        let mut encoder = self
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });

        let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            label: Some("RenderPass".into()),
            color_attachments: &[Some(RenderPassColorAttachment {
                view: &self.headless_surface_view,
                depth_slice: None,
                resolve_target: None,
                ops: Operations {
                    load: wgpu::LoadOp::Clear(Color::BLACK),
                    store: wgpu::StoreOp::Store,
                },
            })],
            depth_stencil_attachment: None,
            timestamp_writes: None,
            occlusion_query_set: None,
            multiview_mask: None,
        });
        render_pass.set_pipeline(self.graphic_pipeline.as_ref().unwrap());
        for (index, bind_group) in bind_groups.iter().enumerate() {
            render_pass.set_bind_group(index as u32, bind_group, &[]);
        }
        render_pass.draw(0..6, 0..1);
        drop(render_pass);

        encoder.copy_texture_to_buffer(
            TexelCopyTextureInfoBase {
                texture: &self.headless_surface,
                mip_level: 0,
                origin: Origin3d::ZERO,
                aspect: wgpu::TextureAspect::All,
            },
            TexelCopyBufferInfo {
                buffer: &self.read_back_buffer,
                layout: TexelCopyBufferLayout {
                    offset: 0,
                    bytes_per_row: None,
                    rows_per_image: None,
                },
            },
            Extent3d {
                width: self.width,
                height: self.height,
                depth_or_array_layers: 1,
            },
        );

        let command_buffer = encoder.finish();

        let submission_index = self.queue.submit([command_buffer]);

        let buffer_slice = self.read_back_buffer.slice(..);
        buffer_slice.map_async(wgpu::MapMode::Read, |_| {
            // mapping will be finished with poll. Not in WebGpu though...
        });

        self.device
            .poll(wgpu::PollType::Wait {
                submission_index: Some(submission_index),
                timeout: Some(Duration::from_secs(1)),
            })
            .unwrap();

        // We can now read the data from the buffer.
        let data = buffer_slice.get_mapped_range().unwrap();
        Ok(data.to_vec())
    }
    pub fn compute(&self) -> Result<Vec<u8>, RendererError> {
        if self.compute_pipeline.is_none() {
            return Err(RendererError::InternalError(
                "No compute pipeline set".into(),
            ));
        }
        let bind_groups = if let Some(bind_group) = &self.bind_group {
            bind_group
                .into_iter()
                .map(|bind_group| bind_group.convert_bind_group(&self.device))
                .collect::<Vec<wgpu::BindGroup>>()
        } else {
            vec![] // No bind group
        };
        let mut encoder = self
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });

        let mut compute_pass = encoder.begin_compute_pass(&wgpu::ComputePassDescriptor {
            label: None,
            timestamp_writes: None,
        });

        compute_pass.set_pipeline(self.compute_pipeline.as_ref().unwrap());
        for (index, bind_group) in bind_groups.iter().enumerate() {
            compute_pass.set_bind_group(index as u32, bind_group, &[]);
        }

        compute_pass.dispatch_workgroups(1, 1, 1);

        drop(compute_pass);

        let command_buffer = encoder.finish();

        let submission_index = self.queue.submit([command_buffer]);

        self.device
            .poll(wgpu::PollType::Wait {
                submission_index: Some(submission_index),
                timeout: Some(Duration::from_secs(1)),
            })
            .unwrap();
        Ok(vec![])
    }
    pub fn raytrace(&self) {
        todo!()
    }
}
