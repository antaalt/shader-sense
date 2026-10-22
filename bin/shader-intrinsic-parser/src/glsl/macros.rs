use shader_sense::{
    shader::ShaderStageMask,
    symbols::{
        symbol_list::ShaderSymbolList,
        symbols::{
            GlslRequirementParameter, RequirementParameter, ShaderSymbol, ShaderSymbolData,
            ShaderSymbolIntrinsic, ShaderSymbolMode,
        },
    },
};

use crate::glsl::GlslIntrinsicParser;

impl GlslIntrinsicParser {
    pub fn add_macros(&self, symbols: &mut ShaderSymbolList) {
        // These values are taken from glslang code, so need to keep track of new values to be correct.
        // Maybe glslang can drop these values somewhere to be parsed though...
        // https://github.com/KhronosGroup/glslang/blob/main/glslang/MachineIndependent/Versions.cpp
        // TODO: should add VULKAN GL_SPIRV and other defined macros. Need to add requirements.
        fn add_macro_with_req(
            symbols: &mut ShaderSymbolList,
            name: &str,
            value: &str,
            req: GlslRequirementParameter,
        ) {
            symbols.macros.push(ShaderSymbol {
                label: name.into(),
                mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                    "".into(), 
                    Some("https://github.com/microsoft/DirectXShaderCompiler/wiki/Predefined-Version-Macros".into())
                )),
                requirement: Some(RequirementParameter::Glsl(req)),
                data: ShaderSymbolData::Macro {
                    value: value.into(),
                    parameters: vec![],
                },
            });
        }
        add_macro_with_req(
            symbols,
            "GL_VERTEX_SHADER".into(),
            "1",
            GlslRequirementParameter {
                stages: Some(ShaderStageMask::VERTEX),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "GL_TESSELLATION_CONTROL_SHADER".into(),
            "1",
            GlslRequirementParameter {
                stages: Some(ShaderStageMask::TESSELATION_CONTROL),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "GL_TESSELLATION_EVALUATION_SHADER".into(),
            "1",
            GlslRequirementParameter {
                stages: Some(ShaderStageMask::TESSELATION_EVALUATION),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "GL_GEOMETRY_SHADER".into(),
            "1",
            GlslRequirementParameter {
                stages: Some(ShaderStageMask::GEOMETRY),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "GL_FRAGMENT_SHADER".into(),
            "1",
            GlslRequirementParameter {
                stages: Some(ShaderStageMask::FRAGMENT),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "GL_COMPUTE_SHADER".into(),
            "1",
            GlslRequirementParameter {
                stages: Some(ShaderStageMask::COMPUTE),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "GL_RAY_GENERATION_SHADER_EXT".into(),
            "1",
            GlslRequirementParameter {
                stages: Some(ShaderStageMask::RAY_GENERATION),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "GL_INTERSECTION_SHADER_EXT".into(),
            "1",
            GlslRequirementParameter {
                stages: Some(ShaderStageMask::INTERSECT),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "GL_ANY_HIT_SHADER_EXT".into(),
            "1",
            GlslRequirementParameter {
                stages: Some(ShaderStageMask::ANY_HIT),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "GL_CLOSEST_HIT_SHADER_EXT".into(),
            "1",
            GlslRequirementParameter {
                stages: Some(ShaderStageMask::CLOSEST_HIT),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "GL_MISS_SHADER_EXT".into(),
            "1",
            GlslRequirementParameter {
                stages: Some(ShaderStageMask::MISS),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "GL_CALLABLE_SHADER_EXT".into(),
            "1",
            GlslRequirementParameter {
                stages: Some(ShaderStageMask::CALLABLE),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "GL_TASK_SHADER_EXT".into(),
            "1",
            GlslRequirementParameter {
                stages: Some(ShaderStageMask::TASK),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "GL_MESH_SHADER_EXT".into(),
            "1",
            GlslRequirementParameter {
                stages: Some(ShaderStageMask::MESH),
                ..Default::default()
            },
        );
    }
}
