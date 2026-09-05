use std::{collections::HashMap, num::NonZeroU64, time::Duration};

use log::{error, info, warn};
use shader_sense::shader::ShaderStage;
use wgpu::{
    wgt::{BufferDescriptor, TextureDescriptor},
    Backends, BufferBinding, BufferUsages, Color, ColorTargetState, ColorWrites, ComputePipeline,
    ExperimentalFeatures, Extent3d, InstanceFlags, MeshPipelineDescriptor, MultisampleState,
    Operations, Origin3d, PipelineCompilationOptions, PrimitiveState, PrimitiveTopology,
    RenderPassColorAttachment, RenderPipeline, ShaderModule, ShaderModuleDescriptor,
    TexelCopyBufferInfo, TexelCopyBufferLayout, TexelCopyTextureInfoBase, VertexState,
};

use crate::renderer::{error::RendererError, shader::Shader};

pub mod error;
pub mod shader;

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
                .map(|_| wgpu::BindGroupLayoutEntry {
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

struct ShaderCompilationResult {
    module: ShaderModule,
    entry_point: String,
}

pub struct Renderer {
    width: u32,
    height: u32,
    instance: wgpu::Instance,
    adapter: wgpu::Adapter,
    device: wgpu::Device,
    queue: wgpu::Queue,
    active_shaders: HashMap<ShaderStage, ShaderCompilationResult>,
    default_shaders: HashMap<ShaderStage, ShaderCompilationResult>,
    headless_surface: wgpu::Texture,
    headless_surface_view: wgpu::TextureView,
    read_back_buffer: wgpu::Buffer,
    graphic_pipeline: Option<RenderPipeline>,
    compute_pipeline: Option<ComputePipeline>,
    bind_group: Option<Vec<BindGroupSlot>>,
}

impl Renderer {
    const SURFACE_FORMAT: wgpu::TextureFormat = wgpu::TextureFormat::Rgba8Unorm;
    const SURFACE_BYTES_PER_TEXEL: u32 = 4;

    /// Pitch of a row of the render target once copied to the read back buffer.
    /// A texture to buffer copy requires its rows to be aligned, so the read back
    /// data may be padded & the client needs to take this pitch into account.
    pub fn get_bytes_per_row(width: u32) -> u32 {
        let bytes_per_row = width * Self::SURFACE_BYTES_PER_TEXEL;
        bytes_per_row.next_multiple_of(wgpu::COPY_BYTES_PER_ROW_ALIGNMENT)
    }

    /// Create the render target & the buffer used to read it back on the CPU.
    fn create_target(
        device: &wgpu::Device,
        width: u32,
        height: u32,
    ) -> (wgpu::Texture, wgpu::TextureView, wgpu::Buffer) {
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
            // COPY_SRC is required to read the rendered image back.
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::COPY_SRC,
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
            size: (Self::get_bytes_per_row(width) * height) as u64,
            // COPY_DST is required as destination of the texture to buffer copy.
            usage: BufferUsages::MAP_READ | BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
        (headless_surface, headless_surface_view, read_back_buffer)
    }

    pub fn new(width: u32, height: u32) -> Self {
        let instance = wgpu::Instance::new(wgpu::InstanceDescriptor {
            backends: Backends::default(), // TODO: Dxil vs spirv vs wgsl need different backend...
            flags: InstanceFlags::default(),
            memory_budget_thresholds: Default::default(),
            backend_options: Default::default(),
            display: None,
        });

        // Select adapter
        let adapter =
            pollster::block_on(instance.request_adapter(&wgpu::RequestAdapterOptions::default()))
                .expect("Failed to create adapter");

        // Print out some basic information about the adapter.
        // Never print to stdout: it is the transport used by the stdio connection.
        info!("Running on Adapter: {:#?}", adapter.get_info());

        // Check to see if the adapter supports compute shaders. While WebGPU guarantees support for
        // compute shaders, wgpu supports a wider range of devices through the use of "downlevel" devices.
        let downlevel_capabilities = adapter.get_downlevel_capabilities();
        if !downlevel_capabilities
            .flags
            .contains(wgpu::DownlevelFlags::COMPUTE_SHADERS)
        {
            panic!("Adapter does not support compute shaders");
        }
        // DXIL & GLSL sources are created through create_shader_module_passthrough, which needs
        // this feature. Only request it when available so we still run for SPIRV & WGSL sources.
        let required_features = adapter.features() & wgpu::Features::PASSTHROUGH_SHADERS;
        if !required_features.contains(wgpu::Features::PASSTHROUGH_SHADERS) {
            warn!("Adapter does not support passthrough shaders. Only SPIRV & WGSL sources will be supported.");
        }
        let (device, queue) = pollster::block_on(adapter.request_device(&wgpu::DeviceDescriptor {
            label: Some("Device".into()),
            required_features,
            // Keep downlevel defaults for compatibility, but allow the resolution the adapter supports
            // so that a large render target is not rejected.
            required_limits: wgpu::Limits::downlevel_defaults().using_resolution(adapter.limits()),
            memory_hints: wgpu::MemoryHints::MemoryUsage,
            trace: wgpu::Trace::Off,
            experimental_features: ExperimentalFeatures::disabled(),
        }))
        .expect("Failed to create device");

        let (headless_surface, headless_surface_view, read_back_buffer) =
            Self::create_target(&device, width, height);
        let default_shaders = Self::load_default_shaders(&device);
        Self {
            width,
            height,
            instance,
            adapter,
            device,
            queue,
            active_shaders: HashMap::new(),
            default_shaders,
            headless_surface,
            headless_surface_view,
            read_back_buffer,
            graphic_pipeline: None,
            compute_pipeline: None,
            bind_group: None,
        }
    }
    fn load_default_shaders(
        device: &wgpu::Device,
    ) -> HashMap<ShaderStage, ShaderCompilationResult> {
        HashMap::from([
            (
                ShaderStage::Vertex,
                ShaderCompilationResult {
                    module: device.create_shader_module(ShaderModuleDescriptor {
                        label: Some("DefaultVertexShader"),
                        source: wgpu::ShaderSource::Wgsl(std::borrow::Cow::Borrowed(include_str!(
                            "default_shaders/vertex.wgsl"
                        ))),
                    }),
                    entry_point: "main".into(),
                },
            ),
            (
                ShaderStage::Fragment,
                ShaderCompilationResult {
                    module: device.create_shader_module(ShaderModuleDescriptor {
                        label: Some("DefaultFragmentShader"),
                        source: wgpu::ShaderSource::Wgsl(std::borrow::Cow::Borrowed(include_str!(
                            "default_shaders/fragment.wgsl"
                        ))),
                    }),
                    entry_point: "main".into(),
                },
            ),
        ])
    }
    pub fn resize(&mut self, width: u32, height: u32) {
        if self.width == width && self.height == height {
            return;
        }
        info!("Resizing render target to {}x{}", width, height);
        let (headless_surface, headless_surface_view, read_back_buffer) =
            Self::create_target(&self.device, width, height);
        self.headless_surface = headless_surface;
        self.headless_surface_view = headless_surface_view;
        self.read_back_buffer = read_back_buffer;
        self.width = width;
        self.height = height;
        // Pipelines do not depend on the target size, so they are kept as is.
    }
    pub fn remove_shader(&mut self, shader_stage: ShaderStage) {
        if self.active_shaders.remove(&shader_stage).is_some() {
            self.invalidate_pipelines();
        }
    }
    pub fn set_shader(&mut self, shader: Shader) -> Result<(), RendererError> {
        // TODO: need to reflect shader and create bind group here. Or pass reflection result as JSON.
        // Would be nice to pass reflection result as client NEEDS them to map everything.
        // But it could be generated here via spirv tools (and naga and what about dxil ??)
        // and returned via a response.
        self.active_shaders.insert(
            shader.stage,
            ShaderCompilationResult {
                module: shader.create_shader_module(self)?,
                entry_point: shader.entry_point,
            },
        );
        self.invalidate_pipelines();
        Ok(())
    }
    /// Drop the pipelines so that they are recreated with the current shaders on next use.
    ///
    /// Pipelines are not rebuilt here: a client binding the stages of a pipeline one by one would
    /// else fail on every incomplete state & rebuild the pipeline as many times as it has stages.
    fn invalidate_pipelines(&mut self) {
        self.graphic_pipeline = None;
        self.compute_pipeline = None;
    }
    /// Run a wgpu operation with a validation error scope, so an invalid shader is reported back to
    /// the client instead of reaching the uncaptured error handler, which panics.
    ///
    /// Wrap the smallest step possible: an error is reported to the innermost scope that catches it,
    /// so a scope around a whole pipeline creation cannot tell a bad shader module from a bad pipeline.
    fn catch_validation_error<T, F: FnOnce(&Self) -> T>(
        &self,
        step: &str,
        callback: F,
    ) -> Result<T, RendererError> {
        let scope = self.device.push_error_scope(wgpu::ErrorFilter::Validation);
        let value = callback(self);
        match pollster::block_on(scope.pop()) {
            Some(error) => {
                error!("Failed to {}: {}", step, error);
                Err(RendererError::ValidationError(format!(
                    "Failed to {}: {}",
                    step, error
                )))
            }
            None => Ok(value),
        }
    }
    /// Get the graphic pipeline for the currently bound shaders, creating it if needed.
    fn ensure_graphic_pipeline(&mut self) -> Result<&RenderPipeline, RendererError> {
        if self.graphic_pipeline.is_none() {
            // Creating a pipeline from a shader the client sent us is expected to fail.
            let pipeline = self.create_graphic_pipeline()?;
            self.graphic_pipeline = Some(pipeline);
        }
        Ok(self.graphic_pipeline.as_ref().unwrap())
    }
    /// Get the compute pipeline for the currently bound shaders, creating it if needed.
    fn ensure_compute_pipeline(&mut self) -> Result<&ComputePipeline, RendererError> {
        if self.compute_pipeline.is_none() {
            let pipeline = self.create_compute_pipeline()?;
            self.compute_pipeline = Some(pipeline);
        }
        Ok(self.compute_pipeline.as_ref().unwrap())
    }
    fn create_graphic_pipeline(&self) -> Result<RenderPipeline, RendererError> {
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
                label: Some("GraphicPipelineLayout"),
                bind_group_layouts: &bind_group_layout
                    .iter()
                    .map(|layout| Some(layout))
                    .collect::<Vec<Option<&wgpu::BindGroupLayout>>>(),
                immediate_size: 0,
            });
        let (vertex_shader, vertex_entry_point) =
            if let Some(vertex) = self.active_shaders.get(&ShaderStage::Vertex) {
                (vertex.module.clone(), vertex.entry_point.clone())
            } else {
                info!("Using default vertex shader");
                let vertex_shader = self
                    .default_shaders
                    .get(&ShaderStage::Vertex)
                    .expect("No default vertex shader");
                (
                    vertex_shader.module.clone(),
                    vertex_shader.entry_point.clone(),
                )
            };
        let (fragment_shader, fragment_entry_point) =
            if let Some(fragment) = self.active_shaders.get(&ShaderStage::Fragment) {
                (fragment.module.clone(), fragment.entry_point.clone())
            } else {
                info!("Using default fragment shader");
                let fragment_shader = self
                    .default_shaders
                    .get(&ShaderStage::Fragment)
                    .expect("No default fragment shader");
                (
                    fragment_shader.module.clone(),
                    fragment_shader.entry_point.clone(),
                )
            };
        let color_target_state = vec![Some(ColorTargetState {
            format: Self::SURFACE_FORMAT,
            blend: None,
            write_mask: ColorWrites::all(),
        })];
        self.catch_validation_error("create the render pipeline", |renderer| {
            renderer
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
                    fragment: Some(wgpu::FragmentState {
                        module: &fragment_shader,
                        entry_point: Some(&fragment_entry_point),
                        compilation_options: PipelineCompilationOptions {
                            constants: &[],
                            zero_initialize_workgroup_memory: false,
                        },
                        targets: &color_target_state,
                    }),
                    multiview_mask: None,
                    cache: None,
                })
        })
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
    fn create_compute_pipeline(&self) -> Result<ComputePipeline, RendererError> {
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
                self.catch_validation_error("create the compute pipeline", |renderer| {
                    renderer
                        .device
                        .create_compute_pipeline(&wgpu::ComputePipelineDescriptor {
                            label: None,
                            layout: Some(&pipeline_layout),
                            module: &compute_shader.module,
                            entry_point: Some(&compute_shader.entry_point),
                            compilation_options: wgpu::PipelineCompilationOptions::default(),
                            cache: None,
                        })
                })
            }
            None => Err(RendererError::InternalError("No compute shader set".into())),
        }
    }

    /// Render a frame with the currently bound shaders & read it back.
    ///
    /// Rows of the returned image are padded up to [`Self::get_bytes_per_row`].
    pub fn render(&mut self) -> Result<Vec<u8>, RendererError> {
        self.ensure_graphic_pipeline()?;
        // Rendering a shader the client sent us is expected to fail.
        self.catch_validation_error("render the frame", |renderer| renderer.render_frame())?
    }
    fn render_frame(&self) -> Result<Vec<u8>, RendererError> {
        let graphic_pipeline = self
            .graphic_pipeline
            .as_ref()
            .ok_or_else(|| RendererError::InternalError("No graphic pipeline set".into()))?;
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
        render_pass.set_pipeline(graphic_pipeline);
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
                    // A copy with more than one row requires an explicit aligned pitch.
                    bytes_per_row: Some(Self::get_bytes_per_row(self.width)),
                    rows_per_image: Some(self.height),
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
        let (sender, receiver) = std::sync::mpsc::channel();
        buffer_slice.map_async(wgpu::MapMode::Read, move |result| {
            // mapping will be finished with poll. Not in WebGpu though...
            let _ = sender.send(result);
        });

        self.device
            .poll(wgpu::PollType::Wait {
                submission_index: Some(submission_index),
                timeout: Some(Duration::from_secs(5)),
            })
            .map_err(|err| {
                RendererError::InternalError(format!("Failed to wait for the GPU: {}", err))
            })?;
        match receiver.try_recv() {
            Ok(Ok(())) => {}
            Ok(Err(err)) => {
                return Err(RendererError::InternalError(format!(
                    "Failed to map the read back buffer: {}",
                    err
                )))
            }
            Err(_) => {
                return Err(RendererError::InternalError(
                    "Read back buffer was not mapped once the GPU was done".into(),
                ))
            }
        }

        // We can now read the data from the buffer.
        let data = buffer_slice.get_mapped_range().map_err(|err| {
            RendererError::InternalError(format!("Failed to read the read back buffer: {}", err))
        })?;
        let image = data.to_vec();
        // The buffer needs to be unmapped before it can be used by another render.
        drop(data);
        self.read_back_buffer.unmap();
        Ok(image)
    }
    pub fn compute(&mut self) -> Result<Vec<u8>, RendererError> {
        self.ensure_compute_pipeline()?;
        self.catch_validation_error("dispatch the compute pass", |renderer| {
            renderer.dispatch_compute()
        })?
    }
    fn dispatch_compute(&self) -> Result<Vec<u8>, RendererError> {
        let compute_pipeline = self
            .compute_pipeline
            .as_ref()
            .ok_or_else(|| RendererError::InternalError("No compute pipeline set".into()))?;
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

        compute_pass.set_pipeline(compute_pipeline);
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
                timeout: Some(Duration::from_secs(5)),
            })
            .map_err(|err| {
                RendererError::InternalError(format!("Failed to wait for the GPU: {}", err))
            })?;
        Ok(vec![])
    }
    pub fn raytrace(&self) {
        todo!()
    }
}
