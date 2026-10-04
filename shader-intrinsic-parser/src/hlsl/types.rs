use shader_sense::{
    shader::{HlslShaderModel, ShaderStageMask},
    symbols::{
        symbol_list::ShaderSymbolList,
        symbols::{
            HlslRequirementParameter, RequirementParameter, ShaderMethod, ShaderParameter,
            ShaderSignature, ShaderSymbol, ShaderSymbolData, ShaderSymbolIntrinsic,
            ShaderSymbolMode,
        },
    },
};

use super::HlslIntrinsicParser;

pub fn new_hlsl_scalar(
    label: &str,
    description: &str,
    min_shader_model: HlslShaderModel,
) -> ShaderSymbol {
    ShaderSymbol {
        label: label.into(),
        mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
            description.into(),
            Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-scalar".into())
        )),
        requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
            min_shader_model: Some(min_shader_model),
            ..Default::default()
        })),
        data: ShaderSymbolData::Types {
            constructors: vec![ShaderSignature {
                returnType: "".into(),
                description: format!("Constructor for type {}", label),
                parameters: vec![ShaderParameter {
                    ty: label.into(),
                    label: "value".into(),
                    count: None,
                    description: "".into(),
                    range: None,
                    modifier: None,
                }],
            }],
        },
    }
}

impl HlslIntrinsicParser {
    pub fn add_types(&self, symbols: &mut ShaderSymbolList) {
        fn get_texture_object_methods(context: &str) -> Vec<ShaderMethod> {
            // https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-texture2d
            // https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-to-gather
            let mut methods = Vec::new();
            if context != "Texture2DMS" && context != "Texture2DMSArray" {
                methods.push(ShaderMethod {
                    context: context.into(),
                    label: "CalculateLevelOfDetail".into(),
                    signature: ShaderSignature {
                        returnType: "float".into(),
                        description: "Calculates the level of detail.".into(),
                        parameters: vec![
                            ShaderParameter {
                                ty: "sampler".into(),
                                label: "s".into(),
                                description: "A Sampler state. This is an object declared in an effect file that contains state assignments.".into(),
                                count: None,
                                range: None,
                                modifier: None,
                            },
                            ShaderParameter {
                                ty: match context {
                                    "Texture1D" | "Texture1DArray" => "float",
                                    "Texture2D" | "Texture2DArray" => "float2",
                                    "Texture3D" | "TextureCube" | "TextureCubeArray" => "float3",
                                    str => unreachable!("Reached {}", str)
                                }.into(),
                                label: "location".into(),
                                description: "The linear interpolation value or values, which is a floating-point number between 0.0 and 1.0 inclusive. The number of components is dependent on the texture-object type. ".into(),
                                count: None,
                                range: None,
                                modifier: None,
                            }
                        ]
                    },
                    range: None,
                });
                methods.push(ShaderMethod {
                    context: context.into(),
                    label: "CalculateLevelOfDetailUnclamped".into(),
                    signature: ShaderSignature {
                        returnType: "float".into(),
                        description: "Calculates the LOD without clamping the result.".into(),
                        parameters: vec![
                            ShaderParameter {
                                ty: "sampler".into(),
                                label: "s".into(),
                                description: "A Sampler state. This is an object declared in an effect file that contains state assignments.".into(),
                                count: None,
                                range: None,
                                modifier: None,
                            },
                            ShaderParameter {
                                ty: match context {
                                    "Texture1D" | "Texture1DArray" => "float",
                                    "Texture2D" | "Texture2DArray" => "float2",
                                    "Texture3D" | "TextureCube" | "TextureCubeArray" => "float3",
                                    str => unreachable!("Reached {}", str)
                                }.into(),
                                label: "location".into(),
                                description: "The linear interpolation value or values, which is a floating-point number between 0.0 and 1.0 inclusive. The number of components is dependent on the texture-object type. ".into(),
                                count: None,
                                range: None,
                                modifier: None,
                            }
                        ]
                    },
                    range: None,
                });
            }
            match context {
                "Texture2D" | "Texture2DArray" | "TextureCube" | "TextureCubeArray" => {
                    let mut method = ShaderMethod {
                        context: context.into(),
                        label: "Gather".into(),
                        signature: ShaderSignature {
                            returnType: "void".into(),
                            description: "Gets the four samples (red component only) that would be used for bilinear interpolation when sampling a texture.".into(),
                            parameters: vec![
                                ShaderParameter {
                                    ty: "sampler".into(),
                                    label: "s".into(),
                                    description: "A Sampler state. This is an object declared in an effect file that contains state assignments.".into(),
                                    count: None,
                                    range: None,
                                    modifier: None,
                                },
                                ShaderParameter {
                                    ty: match context {
                                        "Texture2D" => "float2",
                                        "Texture2DArray" | "TextureCube" => "float3",
                                        "TextureCubeArray" => "float4",
                                        _ => unreachable!(),
                                    }.into(),
                                    label: "location".into(),
                                    description: "The texture coordinates. The argument type is dependent on the texture-object type. ".into(),
                                    count: None,
                                    range: None,
                                    modifier: None,
                                }
                            ]
                        },
                        range: None,
                    };
                    if context == "Texture2D" || context == "Texture2DArray" {
                        method.signature.parameters.push(ShaderParameter {
                            ty: match context {
                                "Texture2D" | "Texture2DArray" => "int2",
                                _ => unreachable!(),
                            }.into(),
                            label: "offset".into(),
                            description: "An optional texture coordinate offset, which can be used for any texture-object type; the offset is applied to the location before sampling. The argument type is dependent on the texture-object type. For shaders targeting Shader Model 5.0 and above, the 6 least significant bits of each offset value is honored as a signed value, yielding [-32..31] range. For previous shader model shaders, offsets need to be immediate integers between -8 and 7.".into(),
                            count: None,
                            range: None,
                            modifier: None,
                        });
                    }
                    methods.push(method);
                }
                _ => {} // not supported
            }
            // TODO: https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-to-getdimensions
            let (dimensions, has_layers, has_mips) = match context {
                "Texture1D" => (1, false, true),
                "Texture1DArray" => (1, true, true),
                "Texture2D" => (2, false, true),
                "Texture2DArray" => (2, true, true),
                "Texture3D" => (3, false, true),
                "Texture3DArray" => (3, true, true),
                "TextureCube" => (2, false, true),
                "TextureCubeArray" => (2, true, true),
                "Texture2DMS" => (2, false, false),
                "Texture2DMSArray" => (2, true, false),
                _ => unreachable!(),
            };
            let mut base_get = ShaderMethod {
                context: context.into(),
                label: "GetDimensions".into(),
                signature: ShaderSignature {
                    returnType: "void".into(),
                    description: "".into(),
                    parameters: vec![ShaderParameter {
                        ty: "uint".into(),
                        label: "width".into(),
                        description: "The resource width, in texels.".into(),
                        count: None,
                        range: None,
                        modifier: Some("out".into()),
                    }],
                },
                range: None,
            };
            if dimensions > 1 {
                base_get.signature.parameters.push(ShaderParameter {
                    ty: "uint".into(),
                    label: "height".into(),
                    description: "The resource height, in texels.".into(),
                    count: None,
                    range: None,
                    modifier: Some("out".into()),
                });
            }
            if dimensions > 2 {
                base_get.signature.parameters.push(ShaderParameter {
                    ty: "uint".into(),
                    label: "depth".into(),
                    description: "The resource depth, in texels.".into(),
                    count: None,
                    range: None,
                    modifier: Some("out".into()),
                });
            }
            if has_layers {
                base_get.signature.parameters.push(ShaderParameter {
                    ty: "uint".into(),
                    label: "elements".into(),
                    description: "The height of the texture.".into(),
                    count: None,
                    range: None,
                    modifier: Some("out".into()),
                });
            }
            if has_mips {
                // Push version without mips.
                methods.push(base_get.clone());
                let mip_level = ShaderParameter {
                    ty: "uint".into(),
                    label: "mipLevel".into(),
                    description:
                        "Optional. Mipmap level (must be specified if NumberOfLevels is used)."
                            .into(),
                    count: None,
                    range: None,
                    modifier: None,
                };
                let nb_level = ShaderParameter {
                    ty: "uint".into(),
                    label: "numberOfLevel".into(),
                    description: "The number of mipmap levels (requires MipLevel also).".into(),
                    count: None,
                    range: None,
                    modifier: Some("out".into()),
                };
                base_get.signature.parameters = [
                    vec![mip_level],
                    base_get.signature.parameters,
                    vec![nb_level],
                ]
                .concat();
                // Push version with mips.
                methods.push(base_get);
            }
            match context {
                "Texture2DMS" | "Texture2DMSArray" => methods.push(ShaderMethod {
                    context: context.into(),
                    label: "GetSamplePosition".into(),
                    signature: ShaderSignature {
                        returnType: "float2".into(),
                        description: "Gets the position of the specified sample.".into(),
                        parameters: vec![ShaderParameter {
                            ty: "sampler".into(),
                            label: "s".into(),
                            description: "The zero-based sample index.".into(),
                            count: None,
                            range: None,
                            modifier: None,
                        }],
                    },
                    range: None,
                }),
                _ => {}
            }
            if context != "TextureCube" && context != "TextureCubeArray" {
                // Load
                let mut method = ShaderMethod {
                    context: context.into(),
                    label: "Load".into(),
                    signature: ShaderSignature {
                        returnType: "float4".into(),
                        description: "Reads texel data without any filtering or sampling.".into(),
                        parameters: vec![
                            ShaderParameter {
                                ty: match context {
                                    "Buffer" => "int",
                                    "Texture1D" | "Texture2DMS" => "int2",
                                    "Texture1DArray" | "Texture2D" | "Texture2DMSArray" => "int3",
                                    "Texture2DArray" | "Texture3D" => "int4",
                                    str => unreachable!("Reached {}", str)
                                }.into(),
                                label: "location".into(),
                                description: "The texture coordinates; the last component specifies the mipmap level. This method uses a 0-based coordinate system and not a 0.0-1.0 UV system. The argument type is dependent on the texture-object type.".into(),
                                count: None,
                                range: None,
                                modifier: None,
                            }
                        ]
                    },
                    range: None,
                };
                match context {
                    "Texture2DMS" | "Texture2DMSArray" => method.signature.parameters.push(ShaderParameter {
                        ty: "int".into(),
                        label: "sampleIndex".into(),
                        description: "A sampling index. Required for multi-sample textures. Not supported for other textures.".into(),
                        count: None,
                        range: None,
                        modifier: None,
                    }),
                    _ => {}
                }
                method.signature.parameters.push(ShaderParameter {
                    ty: match context {
                        "Texture1D" | "Texture1DArray" => "int",
                        "Texture2D" | "Texture2DArray" | "Texture2DMS" | "Texture2DMSArray" => "int2",
                        "Texture3D" => "int3",
                        _ => unreachable!()
                    }.into(),
                    label: "sampleIndex".into(),
                    description: "A sampling index. Required for multi-sample textures. Not supported for other textures.".into(),
                    count: None,
                    range: None,
                    modifier: None,
                });
                methods.push(method);
            }
            if context != "Texture2DMS" && context != "Texture2DMSArray" {
                // Sample | SampleBias | SampleCmp | SampleCmpLevelZero | SampleGrad | SampleLevel.
                let variants = vec![
                    ("Sample", "Samples a texture."),
                    ("SampleBias", "Samples a texture, after applying the input bias to the mipmap level."), // Add a bias before offset
                    ("SampleCmp", "Samples a texture and compares a single component against the specified comparison value."), // Add a CompareValue before offset
                    ("SampleCmpLevelZero", "Samples a texture and compares the result to a comparison value. This function is identical to calling SampleCmp on mipmap level 0 only."), // Add a CompareValue before offset
                    ("SampleGrad", "Samples a texture using a gradient to influence the way the sample location is calculated."), // Add a CompareValue before offset
                    ("SampleLevel", "Samples a texture using a mipmap-level offset. This function is similar to Sample except that it uses the LOD level (in the last component of the location parameter) to choose the mipmap level. For example, a 2D texture uses the first two components for uv coordinates and the third component for the mipmap level."), // Add a lod before offset
                ];
                for (variant, variant_description) in variants {
                    let mut method = ShaderMethod {
                        context: context.into(),
                        label: variant.into(),
                        signature: ShaderSignature {
                            returnType: "float4".into(),
                            description:variant_description.into(),
                            parameters: vec![
                                ShaderParameter {
                                    ty: "sampler".into(),
                                    label: "s".into(),
                                    description: "A Sampler state. This is an object declared in an effect file that contains state assignments.".into(),
                                    count: None,
                                    range: None,
                                    modifier: None,
                                },
                                ShaderParameter {
                                    ty: match context {
                                        "Texture1D" => "float",
                                        "Texture1DArray" | "Texture2D" => "float2",
                                        "Texture2DArray" | "Texture3D" | "TextureCube" => "float3",
                                        "TextureCubeArray" => "float4",
                                        _ => unreachable!()
                                    }.into(),
                                    label: "location".into(),
                                    description: "The texture coordinates. The argument type is dependent on the texture-object type. If the texture object is an array, the last component is the array index.".into(),
                                    count: None,
                                    range: None,
                                    modifier: None,
                                },
                            ]
                        },
                        range: None,
                    };
                    match variant {
                        "SampleBias" => method.signature.parameters.push(ShaderParameter {
                            ty: "float".into(),
                            label: "bias".into(),
                            description: "The bias value, which is a floating-point number between -16.0 and 15.99, is applied to a mip level before sampling.".into(),
                            count: None,
                            range: None,
                            modifier: None,
                        }),
                        "SampleCmp" | "SampleCmpLevelZero" => method.signature.parameters.push(ShaderParameter {
                            ty: "float".into(),
                            label: "CompareValue".into(),
                            description: "A floating-point value to use as a comparison value.".into(),
                            count: None,
                            range: None,
                            modifier: None,
                        }),
                        "SampleGrad" => {
                            method.signature.parameters.push(ShaderParameter {
                                ty: match context {
                                    "Texture1D" |  "Texture1DArray" => "float",
                                    "Texture2D" | "Texture2DArray" => "float2",
                                    "Texture3D" | "TextureCubeArray" | "TextureCube" => "float3",
                                    _ => unreachable!()
                                }.into(),
                                label: "DDX".into(),
                                description: "The rate of change of the surface geometry in the x direction. The argument type is dependent on the texture-object type.".into(),
                                count: None,
                                range: None,
                                modifier: None,
                            });
                            method.signature.parameters.push(ShaderParameter {
                                ty: match context {
                                    "Texture1D" |  "Texture1DArray" => "float",
                                    "Texture2D" | "Texture2DArray" => "float2",
                                    "Texture3D" | "TextureCubeArray" | "TextureCube" => "float3",
                                    _ => unreachable!()
                                }.into(),
                                label: "DDY".into(),
                                description: "The rate of change of the surface geometry in the y direction. The argument type is dependent on the texture-object type.".into(),
                                count: None,
                                range: None,
                                modifier: None,
                            });
                        },
                        "SampleLevel" => method.signature.parameters.push(ShaderParameter {
                            ty: "float".into(),
                            label: "LOD".into(),
                            description: "A number that specifies the mipmap level (internally clamped to the smallest map level). If the value is = 0, the zero'th (biggest map) is used. The fractional value (if supplied) is used to interpolate between two mipmap levels.".into(),
                            count: None,
                            range: None,
                            modifier: None,
                        }),
                        _ => {} // Nothing
                    }
                    match context {
                        "TextureCube" | "TextureCubeArray" => {} // not supported
                        _ => method.signature.parameters.push(ShaderParameter {
                            ty: match context {
                                "Texture1D" | "Texture1DArray" => "int",
                                "Texture2D" | "Texture2DArray" => "int2",
                                "Texture3D" => "int3",
                                _ => unreachable!()
                            }.into(),
                            label: "offset".into(),
                            description: "An optional texture coordinate offset, which can be used for any texture-object type; the offset is applied to the location before sampling. The texture offsets need to be static. The argument type is dependent on the texture-object type. For more info, see Applying texture coordinate offsets.".into(),
                            count: None,
                            range: None,
                            modifier: None,
                        }),
                    }
                    methods.push(method);
                }
            }
            methods
        }
        // sm 4.0 : Object<Type, Samples> name
        // https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-to-type
        symbols.types.push(ShaderSymbol {
            label: "Buffer".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "A buffer containing data".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-buffer".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel4),
                ..Default::default()
            })),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![
                    ShaderMethod {
                        label: "GetDimensions".into(),
                        context: "Buffer".into(),
                        signature: ShaderSignature {
                            returnType: "void".into(),
                            description: "The length, in elements, of the Buffer as set in the Unordered Resource View.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "uint".into(),
                                label: "dim".into(),
                                count: None,
                                description: "The length, in bytes, of the buffer.".into(),
                                range: None,
                                modifier: Some("out".into())
                            }]
                        },
                        range: None,
                    },
                    ShaderMethod {
                        label: "Load".into(),
                        context: "Buffer".into(),
                        signature: ShaderSignature {
                            returnType: "T".into(),
                            description: "Reads buffer data.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "int".into(),
                                label: "location".into(),
                                count: None,
                                description: "The location of the buffer.".into(),
                                range: None,
                                modifier: None
                            }]
                        },
                        range: None,
                    }
                ],
            },
        });
        symbols.types.push(ShaderSymbol {
            label: "Texture1D".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "A 1D texture type.".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-texture1d".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel4),
                ..Default::default()
            })),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: get_texture_object_methods("Texture1D"),
            },
        });
        symbols.types.push(ShaderSymbol {
            label: "Texture1DArray".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "A 1D texture array type.".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-texture1darray".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel4),
                ..Default::default()
            })),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: get_texture_object_methods("Texture1DArray"),
            },
        });
        symbols.types.push(ShaderSymbol {
            label: "Texture2D".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "A 2D texture type.".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-texture2d".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel4),
                ..Default::default()
            })),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: get_texture_object_methods("Texture2D"),
            },
        });
        symbols.types.push(ShaderSymbol {
            label: "Texture2DArray".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "A 2D texture array type.".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-texture2darray".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel4),
                ..Default::default()
            })),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: get_texture_object_methods("Texture2DArray"),
            },
        });
        symbols.types.push(ShaderSymbol {
            label: "Texture3D".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "A 3D texture type.".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-texture3d".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel4),
                ..Default::default()
            })),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: get_texture_object_methods("Texture3D"),
            },
        });
        symbols.types.push(ShaderSymbol {
            label: "TextureCube".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "A cube texture type.".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-texturecube".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel4),
                ..Default::default()
            })),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: get_texture_object_methods("TextureCube"),
            },
        });
        symbols.types.push(ShaderSymbol {
            label: "TextureCubeArray".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "A cube texture array type.".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-texturecubearray".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel4_1),
                ..Default::default()
            })),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: get_texture_object_methods("TextureCubeArray"),
            },
        });
        symbols.types.push(ShaderSymbol {
            label: "Texture2DMS".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "A 2D multisampled texture type.".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-texture2dms".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel4_1),
                ..Default::default()
            })),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: get_texture_object_methods("Texture2DMS"),
            },
        });
        symbols.types.push(ShaderSymbol {
            label: "Texture2DMSArray".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "A 2D multisampled texture type.".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-Texture2DMSArray".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel4_1),
                ..Default::default()
            })),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: get_texture_object_methods("Texture2DMSArray"),
            },
        });
        // sm 5.0 : Object<Type, Samples> name
        // https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/d3d11-graphics-reference-sm5-objects
        symbols.types.push(ShaderSymbol {
            label: "AppendStructuredBuffer".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Output buffer that appears as a stream the shader may append to. Only structured buffers can take T types that are structures.".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-AppendStructuredBuffer".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![
                    ShaderMethod {
                        label: "Append".into(),
                        context: "AppendStructuredBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "void".into(),
                            description: "Appends a value to the end of the buffer.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "int".into(),
                                label: "value".into(),
                                count: None,
                                description: "The input value.".into(),
                                range: None,
                                modifier: None
                            }]
                        },
                        range: None,
                    },
                    ShaderMethod {
                        label: "GetDimensions".into(),
                        context: "AppendStructuredBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "void".into(),
                            description: "Returns the dimensions of the resource.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "uint".into(),
                                label: "numStructs".into(),
                                count: None,
                                description: "The number of structures in the resource.".into(),
                                range: None,
                                modifier: Some("out".into())
                            }, ShaderParameter {
                                ty: "uint".into(),
                                label: "stride".into(),
                                count: None,
                                description: "The number of bytes in each element.".into(),
                                range: None,
                                modifier: Some("out".into())
                            }]
                        },
                        range: None,
                    }
                ],
            },
        });
        symbols.types.push(ShaderSymbol {
            label: "ByteAddressBuffer".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "A read-only buffer that is indexed in bytes.".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-ByteAddressBuffer".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![
                    ShaderMethod {
                        label: "GetDimensions".into(),
                        context: "ByteAddressBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "void".into(),
                            description: "Gets the length of the buffer.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "uint".into(),
                                label: "dim".into(),
                                count: None,
                                description: "The length, in bytes, of the buffer.".into(),
                                range: None,
                                modifier: Some("out".into())
                            }]
                        },
                        range: None,
                    },
                    ShaderMethod {
                        label: "Load".into(),
                        context: "ByteAddressBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "uint".into(),
                            description: "Gets one value.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "int".into(),
                                label: "address".into(),
                                count: None,
                                description: "The input address in bytes, which must be a multiple of 4.".into(),
                                range: None,
                                modifier: None
                            }]
                        },
                        range: None,
                    },
                    ShaderMethod {
                        label: "Load2".into(),
                        context: "ByteAddressBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "uint2".into(),
                            description: "Gets two values.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "int".into(),
                                label: "address".into(),
                                count: None,
                                description: "The input address in bytes, which must be a multiple of 4.".into(),
                                range: None,
                                modifier: None
                            }]
                        },
                        range: None,
                    },
                    ShaderMethod {
                        label: "Load3".into(),
                        context: "ByteAddressBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "uint3".into(),
                            description: "Gets three values.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "int".into(),
                                label: "address".into(),
                                count: None,
                                description: "The input address in bytes, which must be a multiple of 4.".into(),
                                range: None,
                                modifier: None
                            }]
                        },
                        range: None,
                    },
                    ShaderMethod {
                        label: "Load4".into(),
                        context: "ByteAddressBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "uint4".into(),
                            description: "Gets four values.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "int".into(),
                                label: "address".into(),
                                count: None,
                                description: "The input address in bytes, which must be a multiple of 4.".into(),
                                range: None,
                                modifier: None
                            }]
                        },
                        range: None,
                    }
                ],
            },
        });
        symbols.types.push(ShaderSymbol {
            label: "ConsumeStructuredBuffer".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "An input buffer that appears as a stream the shader may pull values from. Only structured buffers can take T types that are structures.".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-ConsumeStructuredBuffer".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![
                    ShaderMethod {
                        label: "Consume".into(),
                        context: "ConsumeStructuredBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "T".into(),
                            description: "Removes a value from the end of the buffer.".into(),
                            parameters: vec![]
                        },
                        range: None,
                    },
                    ShaderMethod {
                        label: "GetDimensions".into(),
                        context: "ConsumeStructuredBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "void".into(),
                            description: "Returns the dimensions of the resource.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "uint".into(),
                                label: "numStructs".into(),
                                count: None,
                                description: "The number of structures in the resource.".into(),
                                range: None,
                                modifier: Some("out".into())
                            }, ShaderParameter {
                                ty: "uint".into(),
                                label: "stride".into(),
                                count: None,
                                description: "The number of bytes in each element.".into(),
                                range: None,
                                modifier: Some("out".into())
                            }]
                        },
                        range: None,
                    }
                ],
            },
        });
        symbols.types.push(ShaderSymbol {
            label: "InputPatch".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Represents an array of control points that are available to the hull shader as inputs.".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-InputPatch".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                stages: Some(ShaderStageMask::TESSELATION_CONTROL),
                ..Default::default()
            })),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![], // Only [] operator
            },
        });
        symbols.types.push(ShaderSymbol {
            label: "OutputPatch".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Represents an array of output control points that are available to the hull shader's patch-constant function as well as the domain shader.".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-OutputPatch".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                stages: Some(ShaderStageMask::TESSELATION_CONTROL),
                ..Default::default()
            })),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![], // Only [] operator
            },
        });
        symbols.types.push(ShaderSymbol {
            label: "RWBuffer".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "A read/write buffer.".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-RWBuffer".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![
                    ShaderMethod {
                        label: "GetDimensions".into(),
                        context: "RWBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "void".into(),
                            description: "The length, in elements, of the Buffer as set in the Unordered Resource View.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "uint".into(),
                                label: "dim".into(),
                                count: None,
                                description: "The length, in bytes, of the buffer.".into(),
                                range: None,
                                modifier: Some("out".into())
                            }]
                        },
                        range: None,
                    },
                    ShaderMethod {
                        label: "Load".into(),
                        context: "RWBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "T".into(),
                            description: "Reads buffer data.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "int".into(),
                                label: "location".into(),
                                count: None,
                                description: "The location of the buffer.".into(),
                                range: None,
                                modifier: None
                            }]
                        },
                        range: None,
                    }
                ],
            },
        });
        symbols.types.push(ShaderSymbol {
            label: "RWByteAddressBuffer".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "A read/write buffer.".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-RWByteAddressBuffer".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![
                    ShaderMethod {
                        label: "GetDimensions".into(),
                        context: "RWBufRWByteAddressBufferfer".into(),
                        signature: ShaderSignature {
                            returnType: "void".into(),
                            description: "Gets the length of the buffer.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "uint".into(),
                                label: "dim".into(),
                                count: None,
                                description: "The length, in bytes, of the buffer.".into(),
                                range: None,
                                modifier: Some("out".into())
                            }]
                        },
                        range: None,
                    },
                    // Interlocked
                    ShaderMethod {
                        label: "InterlockedAdd".into(),
                        context: "RWByteAddressBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "void".into(),
                            description: "Adds the value, atomically.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "uint".into(),
                                label: "dest".into(),
                                count: None,
                                description: "The destination address.".into(),
                                range: None,
                                modifier: None
                            },
                            ShaderParameter {
                                ty: "uint".into(),
                                label: "value".into(),
                                count: None,
                                description: "The input value.".into(),
                                range: None,
                                modifier: None
                            },
                            ShaderParameter {
                                ty: "uint".into(),
                                label: "original_value".into(),
                                count: None,
                                description: "The original value as output.".into(),
                                range: None,
                                modifier: None
                            }]
                        },
                        range: None,
                    },
                    ShaderMethod {
                        label: "InterlockedAnd".into(),
                        context: "RWByteAddressBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "void".into(),
                            description: "Ands the value, atomically.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "uint".into(),
                                label: "dest".into(),
                                count: None,
                                description: "The destination address.".into(),
                                range: None,
                                modifier: None
                            },
                            ShaderParameter {
                                ty: "uint".into(),
                                label: "value".into(),
                                count: None,
                                description: "The input value.".into(),
                                range: None,
                                modifier: None
                            },
                            ShaderParameter {
                                ty: "uint".into(),
                                label: "original_value".into(),
                                count: None,
                                description: "The original value as output.".into(),
                                range: None,
                                modifier: None
                            }]
                        },
                        range: None,
                    },
                    ShaderMethod {
                        label: "InterlockedCompareExchange".into(),
                        context: "RWByteAddressBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "void".into(),
                            description: "Compares the input to the comparison value and exchanges the result, atomically.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "uint".into(),
                                label: "dest".into(),
                                count: None,
                                description: "The destination address.".into(),
                                range: None,
                                modifier: None
                            },
                            ShaderParameter {
                                ty: "uint".into(),
                                label: "compare_value".into(),
                                count: None,
                                description: "The comparison value.".into(),
                                range: None,
                                modifier: None
                            },
                            ShaderParameter {
                                ty: "uint".into(),
                                label: "value".into(),
                                count: None,
                                description: "The input value.".into(),
                                range: None,
                                modifier: None
                            },
                            ShaderParameter {
                                ty: "uint".into(),
                                label: "original_value".into(),
                                count: None,
                                description: "The original value as output.".into(),
                                range: None,
                                modifier: None
                            }]
                        },
                        range: None,
                    },
                    ShaderMethod {
                        label: "InterlockedCompareStore".into(),
                        context: "RWByteAddressBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "void".into(),
                            description: "Compares the input to the comparison value, atomically.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "uint".into(),
                                label: "dest".into(),
                                count: None,
                                description: "The destination address.".into(),
                                range: None,
                                modifier: None
                            },
                            ShaderParameter {
                                ty: "uint".into(),
                                label: "compare_value".into(),
                                count: None,
                                description: "The comparison value.".into(),
                                range: None,
                                modifier: None
                            },
                            ShaderParameter {
                                ty: "uint".into(),
                                label: "original_value".into(),
                                count: None,
                                description: "The original value as output.".into(),
                                range: None,
                                modifier: None
                            }]
                        },
                        range: None,
                    },
                    ShaderMethod {
                        label: "InterlockedExchange".into(),
                        context: "RWByteAddressBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "void".into(),
                            description: "Exchanges a value, atomically.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "uint".into(),
                                label: "dest".into(),
                                count: None,
                                description: "The destination address.".into(),
                                range: None,
                                modifier: None
                            },
                            ShaderParameter {
                                ty: "uint".into(),
                                label: "value".into(),
                                count: None,
                                description: "The input value.".into(),
                                range: None,
                                modifier: None
                            },
                            ShaderParameter {
                                ty: "uint".into(),
                                label: "original_value".into(),
                                count: None,
                                description: "The original value as output.".into(),
                                range: None,
                                modifier: None
                            }]
                        },
                        range: None,
                    },
                    ShaderMethod {
                        label: "InterlockedMax".into(),
                        context: "RWByteAddressBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "void".into(),
                            description: "Finds the maximum value, atomically.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "uint".into(),
                                label: "dest".into(),
                                count: None,
                                description: "The destination address.".into(),
                                range: None,
                                modifier: None
                            },
                            ShaderParameter {
                                ty: "uint".into(),
                                label: "value".into(),
                                count: None,
                                description: "The input value.".into(),
                                range: None,
                                modifier: None
                            },
                            ShaderParameter {
                                ty: "uint".into(),
                                label: "original_value".into(),
                                count: None,
                                description: "The original value as output.".into(),
                                range: None,
                                modifier: None
                            }]
                        },
                        range: None,
                    },
                    ShaderMethod {
                        label: "InterlockedMin".into(),
                        context: "RWByteAddressBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "void".into(),
                            description: "Finds the minimum value, atomically.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "uint".into(),
                                label: "dest".into(),
                                count: None,
                                description: "The destination address.".into(),
                                range: None,
                                modifier: None
                            },
                            ShaderParameter {
                                ty: "uint".into(),
                                label: "value".into(),
                                count: None,
                                description: "The input value.".into(),
                                range: None,
                                modifier: None
                            },
                            ShaderParameter {
                                ty: "uint".into(),
                                label: "original_value".into(),
                                count: None,
                                description: "The original value as output.".into(),
                                range: None,
                                modifier: None
                            }]
                        },
                        range: None,
                    },
                    ShaderMethod {
                        label: "InterlockedOr".into(),
                        context: "RWByteAddressBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "void".into(),
                            description: "Performs an atomic OR on the value.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "uint".into(),
                                label: "dest".into(),
                                count: None,
                                description: "The destination address.".into(),
                                range: None,
                                modifier: None
                            },
                            ShaderParameter {
                                ty: "uint".into(),
                                label: "value".into(),
                                count: None,
                                description: "The input value.".into(),
                                range: None,
                                modifier: None
                            },
                            ShaderParameter {
                                ty: "uint".into(),
                                label: "original_value".into(),
                                count: None,
                                description: "The original value as output.".into(),
                                range: None,
                                modifier: None
                            }]
                        },
                        range: None,
                    },
                    ShaderMethod {
                        label: "InterlockedXor".into(),
                        context: "RWByteAddressBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "void".into(),
                            description: "Performs an atomic XOR on the value.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "uint".into(),
                                label: "dest".into(),
                                count: None,
                                description: "The destination address.".into(),
                                range: None,
                                modifier: None
                            },
                            ShaderParameter {
                                ty: "uint".into(),
                                label: "value".into(),
                                count: None,
                                description: "The input value.".into(),
                                range: None,
                                modifier: None
                            },
                            ShaderParameter {
                                ty: "uint".into(),
                                label: "original_value".into(),
                                count: None,
                                description: "The original value as output.".into(),
                                range: None,
                                modifier: None
                            }]
                        },
                        range: None,
                    },
                    // Load
                    ShaderMethod {
                        label: "Load".into(),
                        context: "RWByteAddressBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "uint".into(),
                            description: "Gets one value.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "uint".into(),
                                label: "address".into(),
                                count: None,
                                description: "The input address in bytes, which must be a multiple of 4.".into(),
                                range: None,
                                modifier: None
                            }]
                        },
                        range: None,
                    },
                    ShaderMethod {
                        label: "Load2".into(),
                        context: "RWByteAddressBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "uint2".into(),
                            description: "Gets two values.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "uint".into(),
                                label: "address".into(),
                                count: None,
                                description: "The input address in bytes, which must be a multiple of 4.".into(),
                                range: None,
                                modifier: None
                            }]
                        },
                        range: None,
                    },
                    ShaderMethod {
                        label: "Load3".into(),
                        context: "RWByteAddressBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "uint3".into(),
                            description: "Gets three values.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "uint".into(),
                                label: "address".into(),
                                count: None,
                                description: "The input address in bytes, which must be a multiple of 4.".into(),
                                range: None,
                                modifier: None
                            }]
                        },
                        range: None,
                    },
                    ShaderMethod {
                        label: "Load4".into(),
                        context: "RWByteAddressBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "uint4".into(),
                            description: "Gets four values.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "uint".into(),
                                label: "address".into(),
                                count: None,
                                description: "The input address in bytes, which must be a multiple of 4.".into(),
                                range: None,
                                modifier: None
                            }]
                        },
                        range: None,
                    },
                    // Store
                    ShaderMethod {
                        label: "Store".into(),
                        context: "RWByteAddressBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "void".into(),
                            description: "Set one value.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "uint".into(),
                                label: "address".into(),
                                count: None,
                                description: "The input address in bytes, which must be a multiple of 4.".into(),
                                range: None,
                                modifier: None
                            }, ShaderParameter {
                                ty: "uint".into(),
                                label: "values".into(),
                                count: None,
                                description: "Input value.".into(),
                                range: None,
                                modifier: None
                            }]
                        },
                        range: None,
                    },
                    ShaderMethod {
                        label: "Store2".into(),
                        context: "RWByteAddressBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "void".into(),
                            description: "Sets two values.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "uint".into(),
                                label: "address".into(),
                                count: None,
                                description: "The input address in bytes, which must be a multiple of 4.".into(),
                                range: None,
                                modifier: None
                            }, ShaderParameter {
                                ty: "uint2".into(),
                                label: "values".into(),
                                count: None,
                                description: "Two input values.".into(),
                                range: None,
                                modifier: None
                            }]
                        },
                        range: None,
                    },
                    ShaderMethod {
                        label: "Store3".into(),
                        context: "RWByteAddressBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "void".into(),
                            description: "Sets three values.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "uint".into(),
                                label: "address".into(),
                                count: None,
                                description: "The input address in bytes, which must be a multiple of 4.".into(),
                                range: None,
                                modifier: None
                            }, ShaderParameter {
                                ty: "uint3".into(),
                                label: "values".into(),
                                count: None,
                                description: "Three input values.".into(),
                                range: None,
                                modifier: None
                            }]
                        },
                        range: None,
                    },
                    ShaderMethod {
                        label: "Store4".into(),
                        context: "RWByteAddressBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "void".into(),
                            description: "Sets four values.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "uint".into(),
                                label: "address".into(),
                                count: None,
                                description: "The input address in bytes, which must be a multiple of 4.".into(),
                                range: None,
                                modifier: None
                            }, ShaderParameter {
                                ty: "uint4".into(),
                                label: "values".into(),
                                count: None,
                                description: "Four input values.".into(),
                                range: None,
                                modifier: None
                            }]
                        },
                        range: None,
                    }
                ],
            },
        });
        symbols.types.push(ShaderSymbol {
            label: "RWStructuredBuffer".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "A read/write buffer that can take a T type that is a structure.".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-RWStructuredBuffer".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![
                    ShaderMethod {
                        label: "GetDimensions".into(),
                        context: "RWStructuredBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "void".into(),
                            description: "Returns the dimensions of the resource.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "uint".into(),
                                label: "numStructs".into(),
                                count: None,
                                description: "The number of structures in the resource.".into(),
                                range: None,
                                modifier: Some("out".into())
                            }, ShaderParameter {
                                ty: "uint".into(),
                                label: "stride".into(),
                                count: None,
                                description: "The stride, in bytes, of each structure element.".into(),
                                range: None,
                                modifier: Some("out".into())
                            }]
                        },
                        range: None,
                    },
                    ShaderMethod {
                        label: "Load".into(),
                        context: "RWStructuredBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "T".into(),
                            description: "Reads buffer data.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "int".into(),
                                label: "location".into(),
                                count: None,
                                description: "The location of the buffer.".into(),
                                range: None,
                                modifier: None
                            }]
                        },
                        range: None,
                    },
                    ShaderMethod {
                        label: "DecrementCounter".into(),
                        context: "RWStructuredBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "uint".into(),
                            description: "Decrements the object's hidden counter.".into(),
                            parameters: vec![]
                        },
                        range: None,
                    },
                    ShaderMethod {
                        label: "IncrementCounter".into(),
                        context: "RWStructuredBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "uint".into(),
                            description: "Increments the object's hidden counter.".into(),
                            parameters: vec![]
                        },
                        range: None,
                    }
                ],
            },
        });
        symbols.types.push(ShaderSymbol {
            label: "RWTexture1D".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "A read/write resource.".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-RWTexture1D".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![
                    ShaderMethod {
                        label: "GetDimensions".into(),
                        context: "RWTexture1D".into(),
                        signature: ShaderSignature {
                            returnType: "void".into(),
                            description: "Returns the dimensions of the resource.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "uint".into(),
                                label: "width".into(),
                                count: None,
                                description: "The resource width, in texels".into(),
                                range: None,
                                modifier: Some("out".into())
                            }]
                        },
                        range: None,
                    },
                    ShaderMethod {
                        label: "Load".into(),
                        context: "RWTexture1D".into(),
                        signature: ShaderSignature {
                            returnType: "T".into(),
                            description: "Reads texture data.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "int".into(),
                                label: "location".into(),
                                count: None,
                                description: "The location of the texture.".into(),
                                range: None,
                                modifier: None
                            }]
                        },
                        range: None,
                    }],
            },
        });
        symbols.types.push(ShaderSymbol {
            label: "RWTexture1DArray".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "A read/write resource.".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-RWTexture1DArray".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![
                    ShaderMethod {
                        label: "GetDimensions".into(),
                        context: "RWTexture1DArray".into(),
                        signature: ShaderSignature {
                            returnType: "void".into(),
                            description: "Returns the dimensions of the resource.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "uint".into(),
                                label: "width".into(),
                                count: None,
                                description: "The resource width, in texels".into(),
                                range: None,
                                modifier: Some("out".into())
                            }, ShaderParameter {
                                ty: "uint".into(),
                                label: "elements".into(),
                                count: None,
                                description: "The number of elements in the array.".into(),
                                range: None,
                                modifier: Some("out".into())
                            }]
                        },
                        range: None,
                    },
                    ShaderMethod {
                        label: "Load".into(),
                        context: "RWTexture1DArray".into(),
                        signature: ShaderSignature {
                            returnType: "T".into(),
                            description: "Reads texture data.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "int".into(),
                                label: "location".into(),
                                count: None,
                                description: "The location of the texture.".into(),
                                range: None,
                                modifier: None
                            }]
                        },
                        range: None,
                    }
                ],
            },
        });
        symbols.types.push(ShaderSymbol {
            label: "RWTexture2D".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "A read/write resource.".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-RWTexture2D".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![
                    ShaderMethod {
                        label: "GetDimensions".into(),
                        context: "RWTexture2D".into(),
                        signature: ShaderSignature {
                            returnType: "void".into(),
                            description: "Returns the dimensions of the resource.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "uint".into(),
                                label: "width".into(),
                                count: None,
                                description: "The resource width, in texels".into(),
                                range: None,
                                modifier: Some("out".into())
                            }, ShaderParameter {
                                ty: "uint".into(),
                                label: "height".into(),
                                count: None,
                                description: "The resource height, in texels".into(),
                                range: None,
                                modifier: Some("out".into())
                            }]
                        },
                        range: None,
                    },
                    ShaderMethod {
                        label: "Load".into(),
                        context: "RWTexture2D".into(),
                        signature: ShaderSignature {
                            returnType: "T".into(),
                            description: "Reads texture data.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "int".into(),
                                label: "location".into(),
                                count: None,
                                description: "The location of the texture.".into(),
                                range: None,
                                modifier: None
                            }]
                        },
                        range: None,
                    }
                ],
            },
        });
        symbols.types.push(ShaderSymbol {
            label: "RWTexture2DArray".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "A read/write resource.".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-RWTexture2DArray".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![
                    ShaderMethod {
                        label: "GetDimensions".into(),
                        context: "RWTexture2DArray".into(),
                        signature: ShaderSignature {
                            returnType: "void".into(),
                            description: "Returns the dimensions of the resource.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "uint".into(),
                                label: "width".into(),
                                count: None,
                                description: "The resource width, in texels".into(),
                                range: None,
                                modifier: Some("out".into())
                            }, ShaderParameter {
                                ty: "uint".into(),
                                label: "height".into(),
                                count: None,
                                description: "The resource height, in texels".into(),
                                range: None,
                                modifier: Some("out".into())
                            }, ShaderParameter {
                                ty: "uint".into(),
                                label: "elements".into(),
                                count: None,
                                description: "The number of elements in the array.".into(),
                                range: None,
                                modifier: Some("out".into())
                            }]
                        },
                        range: None,
                    },
                    ShaderMethod {
                        label: "Load".into(),
                        context: "RWTexture2DArray".into(),
                        signature: ShaderSignature {
                            returnType: "T".into(),
                            description: "Reads texture data.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "int".into(),
                                label: "location".into(),
                                count: None,
                                description: "The location of the texture.".into(),
                                range: None,
                                modifier: None
                            }]
                        },
                        range: None,
                    }],
            },
        });
        symbols.types.push(ShaderSymbol {
            label: "RWTexture3D".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "A read/write resource.".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-RWTexture3D".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![
                    ShaderMethod {
                        label: "GetDimensions".into(),
                        context: "RWTexture3D".into(),
                        signature: ShaderSignature {
                            returnType: "void".into(),
                            description: "Returns the dimensions of the resource.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "uint".into(),
                                label: "width".into(),
                                count: None,
                                description: "The resource width, in texels".into(),
                                range: None,
                                modifier: Some("out".into())
                            }, ShaderParameter {
                                ty: "uint".into(),
                                label: "height".into(),
                                count: None,
                                description: "The resource height, in texels".into(),
                                range: None,
                                modifier: Some("out".into())
                            }, ShaderParameter {
                                ty: "uint".into(),
                                label: "depth".into(),
                                count: None,
                                description: "The resource depth, in texels.".into(),
                                range: None,
                                modifier: Some("out".into())
                            }]
                        },
                        range: None,
                    },
                    ShaderMethod {
                        label: "Load".into(),
                        context: "RWTexture3D".into(),
                        signature: ShaderSignature {
                            returnType: "T".into(),
                            description: "Reads texture data.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "int".into(),
                                label: "location".into(),
                                count: None,
                                description: "The location of the texture.".into(),
                                range: None,
                                modifier: None
                            }]
                        },
                        range: None,
                    }
                ],
            },
        });
        // sm 5.1
        symbols.types.push(ShaderSymbol {
            label: "StructuredBuffer".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "A read-only buffer, which can take a T type that is a structure.".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-StructuredBuffer".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![
                    ShaderMethod {
                        label: "GetDimensions".into(),
                        context: "StructuredBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "void".into(),
                            description: "Returns the dimensions of the resource.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "uint".into(),
                                label: "numStructs".into(),
                                count: None,
                                description: "The number of structures in the resource.".into(),
                                range: None,
                                modifier: Some("out".into())
                            }, ShaderParameter {
                                ty: "uint".into(),
                                label: "stride".into(),
                                count: None,
                                description: "The stride, in bytes, of each structure element.".into(),
                                range: None,
                                modifier: Some("out".into())
                            }]
                        },
                        range: None,
                    },
                    ShaderMethod {
                        label: "Load".into(),
                        context: "StructuredBuffer".into(),
                        signature: ShaderSignature {
                            returnType: "T".into(),
                            description: "Reads texture data.".into(),
                            parameters: vec![ShaderParameter {
                                ty: "int".into(),
                                label: "location".into(),
                                count: None,
                                description: "The location of the texture.".into(),
                                range: None,
                                modifier: None
                            }]
                        },
                        range: None,
                    }
                ],
            },
        });
        // The following types are alias, so copy existing types.
        // RasterizerOrderedBuffer
        symbols.types.push(
            symbols
                .types
                .iter()
                .find(|t| t.label == "RWBuffer")
                .map(|s| {
                    let mut s = s.clone();
                    s.label = "RasterizerOrderedBuffer".into();
                    s
                })
                .unwrap(),
        );
        // RasterizerOrderedByteAddressBuffer
        symbols.types.push(
            symbols
                .types
                .iter()
                .find(|t| t.label == "RWByteAddressBuffer")
                .map(|s| {
                    let mut s = s.clone();
                    s.label = "RasterizerOrderedByteAddressBuffer".into();
                    s
                })
                .unwrap(),
        );
        // RasterizerOrderedStructuredBuffer
        symbols.types.push(
            symbols
                .types
                .iter()
                .find(|t| t.label == "RWStructuredBuffer")
                .map(|s| {
                    let mut s = s.clone();
                    s.label = "RasterizerOrderedStructuredBuffer".into();
                    s
                })
                .unwrap(),
        );
        // RasterizerOrderedTexture1D
        symbols.types.push(
            symbols
                .types
                .iter()
                .find(|t| t.label == "RWTexture1D")
                .map(|s| {
                    let mut s = s.clone();
                    s.label = "RasterizerOrderedTexture1D".into();
                    s
                })
                .unwrap(),
        );
        //RasterizerOrderedTexture1DArray
        symbols.types.push(
            symbols
                .types
                .iter()
                .find(|t| t.label == "RWTexture1DArray")
                .map(|s| {
                    let mut s = s.clone();
                    s.label = "RasterizerOrderedTexture1DArray".into();
                    s
                })
                .unwrap(),
        );
        // RasterizerOrderedTexture2D
        symbols.types.push(
            symbols
                .types
                .iter()
                .find(|t| t.label == "RWTexture2D")
                .map(|s| {
                    let mut s = s.clone();
                    s.label = "RasterizerOrderedTexture2D".into();
                    s
                })
                .unwrap(),
        );
        // RasterizerOrderedTexture2DArray
        symbols.types.push(
            symbols
                .types
                .iter()
                .find(|t| t.label == "RWTexture2DArray")
                .map(|s| {
                    let mut s = s.clone();
                    s.label = "RasterizerOrderedTexture2DArray".into();
                    s
                })
                .unwrap(),
        );
        // RasterizerOrderedTexture3D
        symbols.types.push(
            symbols
                .types
                .iter()
                .find(|t| t.label == "RWTexture3D")
                .map(|s| {
                    let mut s = s.clone();
                    s.label = "RasterizerOrderedTexture3D".into();
                    s
                })
                .unwrap(),
        );

        // Manually push types as they are not in documentation
        let mut scalar_types = Vec::new();
        scalar_types.push(new_hlsl_scalar(
            "bool",
            "conditional type, values may be either true or false",
            HlslShaderModel::earliest(),
        ));
        scalar_types.push(new_hlsl_scalar(
            "int",
            "32-bit signed integer",
            HlslShaderModel::earliest(),
        ));
        scalar_types.push(new_hlsl_scalar(
            "uint",
            "32-bit unsigned integer",
            HlslShaderModel::earliest(),
        ));
        scalar_types.push(new_hlsl_scalar(
            "dword",
            "32-bit unsigned integer",
            HlslShaderModel::earliest(),
        ));
        scalar_types.push(new_hlsl_scalar(
            "half",
            "16-bit floating point value",
            HlslShaderModel::earliest(),
        ));
        scalar_types.push(new_hlsl_scalar(
            "float",
            "32-bit floating point value",
            HlslShaderModel::earliest(),
        ));
        scalar_types.push(new_hlsl_scalar(
            "double",
            "64-bit floating point value.",
            HlslShaderModel::earliest(),
        ));
        // Minimum are only supported with windows 8+
        scalar_types.push(new_hlsl_scalar(
            "min16float",
            "minimum 16-bit floating point value. Only supported on Windows 8+ only.",
            HlslShaderModel::earliest(),
        ));
        scalar_types.push(new_hlsl_scalar(
            "min10float",
            "minimum 10-bit floating point value. Only supported on Windows 8+ only.",
            HlslShaderModel::earliest(),
        ));
        scalar_types.push(new_hlsl_scalar(
            "min16int",
            "minimum 16-bit signed integer. Only supported on Windows 8+ only.",
            HlslShaderModel::earliest(),
        ));
        scalar_types.push(new_hlsl_scalar(
            "min12int",
            "minimum 12-bit signed integer. Only supported on Windows 8+ only.",
            HlslShaderModel::earliest(),
        ));
        scalar_types.push(new_hlsl_scalar(
            "min16uint",
            "minimum 16-bit unsigned integer. Only supported on Windows 8+ only.",
            HlslShaderModel::earliest(),
        ));
        scalar_types.push(new_hlsl_scalar(
            "uint64_t",
            "A 64-bit unsigned integer.",
            HlslShaderModel::ShaderModel6,
        ));
        scalar_types.push(new_hlsl_scalar(
            "int64_t",
            "A 64-bit signed integer.",
            HlslShaderModel::ShaderModel6,
        ));
        // TODO: -enable16bnit float16_t + uint16_t
        fn get_vector_component_label(index: u32) -> String {
            match index {
                0 => "x".into(),
                1 => "y".into(),
                2 => "z".into(),
                3 => "w".into(),
                _ => unreachable!(""),
            }
        }
        fn get_matrix_component_label(index_col: u32, index_row: u32) -> String {
            format!("m{}{}", index_col, index_row)
        }
        for component_col in 1..=4 {
            // Vectors
            for scalar in &scalar_types {
                let fmt = format!("{}{}", scalar.label, component_col);
                symbols.types.push(ShaderSymbol {
                    label: fmt.clone(),
                    mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                        format!("Vector with {} components of {}", component_col, scalar.label),
                        Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-vector".into()),
                    )),
                    data: ShaderSymbolData::Types { constructors: vec![
                        ShaderSignature {
                            returnType: fmt.clone(),
                            description: format!("Constructor for type {}", fmt),
                            parameters: vec![ShaderParameter {
                                ty: fmt.clone(),
                                label: "value".into(),
                                count: None,
                                description: "".into(),
                                range:None,
                                modifier: None,
                            }],
                        },
                        ShaderSignature {
                            returnType: fmt.clone(),
                            description: format!("Constructor for type {}", fmt),
                            parameters: (0..component_col).map(|parameter_index| ShaderParameter {
                                ty: scalar.label.clone(),
                                label: get_vector_component_label(parameter_index),
                                count: None,
                                description: "".into(),
                                range:None,
                                modifier: None,
                            }).collect(),
                        }
                    ]},
                    requirement: None,
                });
                /*symbols.types.push(ShaderSymbol {
                    label: format!("vector<{},{}>", scalar.label, component_col),
                    description: format!("Vector with {} components of {}", component_col, scalar.label),
                    link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-vector".into()),
                    data: ShaderSymbolData::Types { ty:fmt },
                    version: "".into(),
                    stages: vec![]
                });*/
                for component_row in 1..=4 {
                    let fmt = format!("{}{}x{}", scalar.label, component_row, component_col);
                    symbols.types.push(ShaderSymbol{
                        label: fmt.clone(),
                        mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                            format!("Matrice with {} rows and {} columns of {}", component_row, component_col, scalar.label), 
                            Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-matrix".into())
                        )),
                        data: ShaderSymbolData::Types { constructors: vec![
                            ShaderSignature {
                                returnType: fmt.clone(),
                                description: format!("Constructor for type {}", fmt),
                                parameters: vec![ShaderParameter {
                                    ty: fmt.clone(),
                                    label: "value".into(),
                                    count: None,
                                    description: "".into(),
                                    range:None,
                                    modifier: None,
                                }],
                            },
                            ShaderSignature {
                                returnType: fmt.clone(),
                                description: format!("Constructor for type {}", fmt),
                                parameters: (0..component_col).map(|col_index|
                                    (0..component_row).map(|row_index| ShaderParameter {
                                        ty: scalar.label.clone(),
                                        label: get_matrix_component_label(col_index, row_index),
                                        count: None,
                                        description: "".into(),
                                        range:None,
                                        modifier: None,
                                    }).collect::<Vec<ShaderParameter>>()
                                ).collect::<Vec<Vec<ShaderParameter>>>().concat(),
                            }
                        ] },
                        requirement: None,
                    });
                    /*symbols.types.push(ShaderSymbol{
                        label: format!("matrix<{},{},{}>", scalar.label, component_row, component_col),
                        description: format!("Matrice with {} rows and {} columns of {}", component_row, component_col, scalar.label),
                        link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-matrix".into()),
                        data: ShaderSymbolData::Types { ty:fmt },
                        version: "".into(),
                        stages: vec![]
                    });*/
                }
            }
        }
        symbols.types.append(&mut scalar_types);
    }
}
