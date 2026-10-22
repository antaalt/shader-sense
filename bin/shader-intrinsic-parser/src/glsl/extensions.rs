use shader_sense::{
    shader::ShaderStageMask,
    symbols::{
        symbol_list::ShaderSymbolList,
        symbols::{
            GlslRequirementParameter, RequirementParameter, ShaderMember, ShaderParameter,
            ShaderSignature, ShaderSymbol, ShaderSymbolArray, ShaderSymbolData,
            ShaderSymbolIntrinsic, ShaderSymbolMode,
        },
    },
};

use super::GlslIntrinsicParser;

impl GlslIntrinsicParser {
    pub fn get_extensions(&self, symbol_list: &mut ShaderSymbolList) {
        // Could parse all extensions from https://github.com/KhronosGroup/GLSL/tree/main
        // But asciidoctor not really parse friendly...
        self.get_glsl_ext_mesh_shader(symbol_list);
    }
    fn get_glsl_ext_mesh_shader(&self, symbol_list: &mut ShaderSymbolList) {
        // From https://github.com/KhronosGroup/GLSL/blob/main/extensions/ext/GLSL_EXT_mesh_shader.txt
        symbol_list.variables.push(ShaderSymbol {
            label: "EmitMeshTasksEXT".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "".into(),
                Some("https://github.com/KhronosGroup/GLSL/blob/main/extensions/ext/GLSL_EXT_mesh_shader.txt".into()))
            ),
            requirement: Some(RequirementParameter::Glsl(GlslRequirementParameter {
                extension: Some("GLSL_EXT_mesh_shader".into()),
                stages: Some(ShaderStageMask::TASK),
                ..Default::default()
            })),
            data: ShaderSymbolData::Functions { signatures: vec![
                ShaderSignature {
                    returnType: "void".into(),
                    description: "Emit the given number of mesh task for next stage on each axis.".into(),
                    parameters: vec![
                        ShaderParameter{
                            ty: "uint".into(),
                            label: "x".into(),
                            count: None,
                            description: "".into(),
                            range: None,
                            modifier: None
                        },
                        ShaderParameter{
                            ty: "uint".into(),
                            label: "y".into(),
                            count: None,
                            description: "".into(),
                            range: None,
                            modifier: None
                        },
                        ShaderParameter{
                            ty: "uint".into(),
                            label: "z".into(),
                            count: None,
                            description: "".into(),
                            range: None,
                            modifier: None
                        },
                    ]
                }]
            },
        });
        symbol_list.variables.push(ShaderSymbol {
            label: "SetMeshOutputsEXT".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "".into(),
                Some("https://github.com/KhronosGroup/GLSL/blob/main/extensions/ext/GLSL_EXT_mesh_shader.txt".into()))
            ),
            requirement: Some(RequirementParameter::Glsl(GlslRequirementParameter {
                extension: Some("GLSL_EXT_mesh_shader".into()),
                stages: Some(ShaderStageMask::MESH),
                ..Default::default()
            })),
            data: ShaderSymbolData::Functions { signatures: vec![
                ShaderSignature {
                    returnType: "void".into(),
                    description: "Set the output values for current thread.".into(),
                    parameters: vec![
                        ShaderParameter{
                            ty: "uint".into(),
                            label: "vertexCount".into(),
                            count: None,
                            description: "Number of vertex to output for current thread.".into(),
                            range: None,
                            modifier: None
                        },
                        ShaderParameter{
                            ty: "uint".into(),
                            label: "primitiveCount".into(),
                            count: None,
                            description: "Number of primitive to output for current thread.".into(),
                            range: None,
                            modifier: None
                        },
                    ]
                }]
            },
        });

        // Per vertex
        symbol_list.types.push(ShaderSymbol {
            label: "gl_MeshPerVertexEXT".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "".into(),
                Some("https://github.com/KhronosGroup/GLSL/blob/main/extensions/ext/GLSL_EXT_mesh_shader.txt".into()))
            ),
            requirement: Some(RequirementParameter::Glsl(GlslRequirementParameter {
                extension: Some("GLSL_EXT_mesh_shader".into()),
                stages: Some(ShaderStageMask::TASK | ShaderStageMask::MESH),
                ..Default::default()
            })),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![
                    ShaderMember {
                        context: "gl_MeshPerVertexEXT".into(),
                        parameters: ShaderParameter {
                            ty: "vec4".into(),
                            label: "gl_Position".into(),
                            count: None,
                            description: "".into(),
                            range: None,
                            modifier: None
                        }
                    },
                    ShaderMember {
                        context: "gl_MeshPerVertexEXT".into(),
                        parameters: ShaderParameter {
                            ty: "float".into(),
                            label: "gl_PointSize".into(),
                            count: None,
                            description: "".into(),
                            range: None,
                            modifier: None
                        }
                    },
                    ShaderMember {
                        context: "gl_MeshPerVertexEXT".into(),
                        parameters: ShaderParameter {
                            ty: "float".into(),
                            label: "gl_ClipDistance".into(),
                            count: Some(ShaderSymbolArray::Unsized),
                            description: "".into(),
                            range: None,
                            modifier: None
                        }
                    },
                    ShaderMember {
                        context: "gl_MeshPerVertexEXT".into(),
                        parameters: ShaderParameter {
                            ty: "float".into(),
                            label: "gl_CullDistance".into(),
                            count: Some(ShaderSymbolArray::Unsized),
                            description: "".into(),
                            range: None,
                            modifier: None
                        }
                    },
                ],
                methods: vec![]
            },
        });
        symbol_list.variables.push(ShaderSymbol {
            label: "gl_MeshVerticesEXT".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "".into(),
                Some("https://github.com/KhronosGroup/GLSL/blob/main/extensions/ext/GLSL_EXT_mesh_shader.txt".into()))
            ),
            requirement: Some(RequirementParameter::Glsl(GlslRequirementParameter {
                extension: Some("GLSL_EXT_mesh_shader".into()),
                stages: Some(ShaderStageMask::TASK | ShaderStageMask::MESH),
                ..Default::default()
            })),
            data: ShaderSymbolData::Variables  {
                ty: "gl_MeshPerVertexEXT".into(), 
                count: Some(ShaderSymbolArray::Unsized),
            },
        });
        // Per primitives
        symbol_list.types.push(ShaderSymbol {
            label: "gl_MeshPerPrimitiveEXT".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "".into(),
                Some("https://github.com/KhronosGroup/GLSL/blob/main/extensions/ext/GLSL_EXT_mesh_shader.txt".into()))
            ),
            requirement: Some(RequirementParameter::Glsl(GlslRequirementParameter {
                extension: Some("GLSL_EXT_mesh_shader".into()),
                stages: Some(ShaderStageMask::TASK | ShaderStageMask::MESH),
                ..Default::default()
            })),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![
                    ShaderMember {
                        context: "gl_MeshPerPrimitiveEXT".into(),
                        parameters: ShaderParameter {
                            ty: "int".into(),
                            label: "gl_PrimitiveID".into(),
                            count: None,
                            description: "".into(),
                            range: None,
                            modifier: None
                        }
                    },
                    ShaderMember {
                        context: "gl_MeshPerPrimitiveEXT".into(),
                        parameters: ShaderParameter {
                            ty: "int".into(),
                            label: "gl_Layer".into(),
                            count: None,
                            description: "".into(),
                            range: None,
                            modifier: None
                        }
                    },
                    ShaderMember {
                        context: "gl_MeshPerPrimitiveEXT".into(),
                        parameters: ShaderParameter {
                            ty: "int".into(),
                            label: "gl_ViewportIndex".into(),
                            count: Some(ShaderSymbolArray::Unsized),
                            description: "".into(),
                            range: None,
                            modifier: None
                        }
                    },
                    ShaderMember {
                        context: "gl_MeshPerPrimitiveEXT".into(),
                        parameters: ShaderParameter {
                            ty: "bool".into(),
                            label: "gl_CullPrimitiveEXT".into(),
                            count: Some(ShaderSymbolArray::Unsized),
                            description: "".into(),
                            range: None,
                            modifier: None
                        }
                    },
                    ShaderMember {
                        context: "gl_MeshPerPrimitiveEXT".into(),
                        parameters: ShaderParameter {
                            ty: "int".into(),
                            label: "gl_PrimitiveShadingRateEXT".into(),
                            count: Some(ShaderSymbolArray::Unsized),
                            description: "".into(),
                            range: None,
                            modifier: None
                        }
                    },
                ],
                methods: vec![]
            },
        });
        symbol_list.variables.push(ShaderSymbol {
            label: "gl_MeshPrimitivesEXT".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "".into(),
                Some("https://github.com/KhronosGroup/GLSL/blob/main/extensions/ext/GLSL_EXT_mesh_shader.txt".into()))
            ),
            requirement: Some(RequirementParameter::Glsl(GlslRequirementParameter {
                extension: Some("GLSL_EXT_mesh_shader".into()),
                stages: Some(ShaderStageMask::TASK | ShaderStageMask::MESH),
                ..Default::default()
            })),
            data: ShaderSymbolData::Variables {
                ty: "gl_MeshPerPrimitiveEXT".into(),
                count: Some(ShaderSymbolArray::Unsized),
            },
        });
        // Variables
        symbol_list.variables.push(ShaderSymbol {
            label: "gl_MeshVerticesEXT".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "".into(),
                Some("https://github.com/KhronosGroup/GLSL/blob/main/extensions/ext/GLSL_EXT_mesh_shader.txt".into()))
            ),
            requirement: Some(RequirementParameter::Glsl(GlslRequirementParameter {
                extension: Some("GLSL_EXT_mesh_shader".into()),
                stages: Some(ShaderStageMask::TASK | ShaderStageMask::MESH),
                ..Default::default()
            })),
            data: ShaderSymbolData::Variables {
                ty: "gl_PrimitivePointIndicesEXT".into(), 
                count: Some(ShaderSymbolArray::Unsized),
            },
        });
        symbol_list.variables.push(ShaderSymbol {
            label: "gl_PrimitiveLineIndicesEXT".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "".into(),
                Some("https://github.com/KhronosGroup/GLSL/blob/main/extensions/ext/GLSL_EXT_mesh_shader.txt".into()))
            ),
            requirement: Some(RequirementParameter::Glsl(GlslRequirementParameter {
                extension: Some("GLSL_EXT_mesh_shader".into()),
                stages: Some(ShaderStageMask::TASK | ShaderStageMask::MESH),
                ..Default::default()
            })),
            data: ShaderSymbolData::Variables {
                ty: "uvec2".into(), 
                count: Some(ShaderSymbolArray::Unsized),
            },
        });
        symbol_list.variables.push(ShaderSymbol {
            label: "gl_PrimitiveTriangleIndicesEXT".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "".into(),
                Some("https://github.com/KhronosGroup/GLSL/blob/main/extensions/ext/GLSL_EXT_mesh_shader.txt".into()))
            ),
            requirement: Some(RequirementParameter::Glsl(GlslRequirementParameter {
                extension: Some("GLSL_EXT_mesh_shader".into()),
                stages: Some(ShaderStageMask::TASK | ShaderStageMask::MESH),
                ..Default::default()
            })),
            data: ShaderSymbolData::Variables {
                ty: "uvec3".into(), 
                count: Some(ShaderSymbolArray::Unsized),
            },
        });
    }
}
