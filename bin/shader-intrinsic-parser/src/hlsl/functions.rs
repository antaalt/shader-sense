use shader_sense::{
    shader::{HlslShaderModel, ShaderStageMask},
    symbols::{
        symbol_list::ShaderSymbolList,
        symbols::{
            HlslRequirementParameter, RequirementParameter, ShaderParameter, ShaderSignature,
            ShaderSymbol, ShaderSymbolData, ShaderSymbolIntrinsic, ShaderSymbolMode,
        },
    },
};

use super::{type_size_iter, HlslIntrinsicParser};

impl HlslIntrinsicParser {
    pub fn add_functions(&self, symbols: &mut ShaderSymbolList) {
        symbols.functions.push(ShaderSymbol {
            label: "abort".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Submits an error message to the information queue and terminates the current draw or dispatch call being executed.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/abort".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel4),
                ..Default::default()
            })),
            data: ShaderSymbolData::Functions { signatures:vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![],
            }]},
        });
        symbols.functions.push(ShaderSymbol {
            label: "abs".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the absolute value of the specified value.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-abs".into()
            ))),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
            data: ShaderSymbolData::Functions {
                signatures: type_size_iter(&["float", "int"], true, true, true)
                    .iter()
                    .map(|v| ShaderSignature {
                        returnType: v.format(),
                        description: "".into(),
                        parameters: vec![ShaderParameter {
                            count: None,
                            ty: v.format(),
                            label: "x".into(),
                            description: "The specified value.".into(),
                            range: None,
                            modifier: None,
                        }],
                    })
                    .collect(),
            },
        });
        symbols.functions.push(ShaderSymbol {
            label: "acos".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the arccosine of the specified value.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-acos".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value. Each component should be a floating-point value within the range of -1 to 1.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
        });
        symbols.functions.push(ShaderSymbol {
            label: "all".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Determines if all components of the specified value are non-zero.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-all".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float", "int", "bool"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: "bool".into(),
                description: "True if all components of the x parameter are non-zero; otherwise, false.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
        });
        symbols.functions.push(ShaderSymbol {
            label: "any".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Determines if any components of the specified value are non-zero.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-all".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float", "int", "bool"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: "bool".into(),
                description: "True if any components of the x parameter are non-zero; otherwise, false.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
        });
        symbols.functions.push(ShaderSymbol {
            label: "AllMemoryBarrier".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Blocks execution of all threads in a group until all memory accesses have been completed.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/allmemorybarrier".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
            data: ShaderSymbolData::Functions { signatures:vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![],
            }]},
        });
        symbols.functions.push(ShaderSymbol {
            label: "AllMemoryBarrierWithGroupSync".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Blocks execution of all threads in a group until all memory accesses have been completed and all threads in the group have reached this call.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/allmemorybarrierwithgroupsync".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
            data: ShaderSymbolData::Functions { signatures:vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![],
            }]},
        });
        symbols.functions.push(ShaderSymbol {
            label: "asdouble".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Reinterprets a cast value (two 32-bit values) into a double.".into(),
                Some(
                    "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/asdouble".into(),
                ),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
            data: ShaderSymbolData::Functions {
                signatures: vec![ShaderSignature {
                    returnType: "double".into(),
                    description: "The input (two 32-bit values) recast as a double.".into(),
                    parameters: vec![
                        ShaderParameter {
                            count: None,
                            ty: "uint".into(),
                            label: "lowbits".into(),
                            description: "The low 32-bit pattern of the input value.".into(),
                            range: None,
                            modifier: None,
                        },
                        ShaderParameter {
                            count: None,
                            ty: "uint".into(),
                            label: "highbits".into(),
                            description: "The high 32-bit pattern of the input value.".into(),
                            range: None,
                            modifier: None,
                        },
                    ],
                }],
            },
        });
        symbols.functions.push(ShaderSymbol {
            label: "asfloat".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Interprets the bit pattern of x as a floating-point number.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-asfloat".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel4),
                ..Default::default()
            })),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["int", "uint"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format_with_type("float"),
                description: "The input interpreted as a floating-point number.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The input value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
        });
        symbols.functions.push(ShaderSymbol {
            label: "asint".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Interprets the bit pattern of x as an integer.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-asint".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel4),
                ..Default::default()
            })),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float", "uint"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format_with_type("int"),
                description: "The input interpreted as an integer.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The input value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
        });
        symbols.functions.push(ShaderSymbol {
            label: "asuint".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Interprets the bit pattern of x as an unsigned integer.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-asuint".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel4),
                ..Default::default()
            })),
            data: ShaderSymbolData::Functions { signatures:type_size_iter(&["float", "int"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format_with_type("uint"),
                description: "The input interpreted as an unsigned integer.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The input value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).chain(std::iter::once(ShaderSignature {
                returnType: "void".into(),
                description: "Splits the bit pattern of a double into two unsigned integers.".into(),
                parameters: vec![
                    ShaderParameter {
                        count: None,
                        ty: "double".into(),
                        label: "value".into(),
                        description: "The input value.".into(),
                        range: None,
                        modifier: None,
                    },
                    ShaderParameter {
                        count: None,
                        ty: "uint".into(),
                        label: "lowbits".into(),
                        description: "The low 32-bit pattern of the input value.".into(),
                        range: None,
                        modifier: Some("out".into()),
                    },
                    ShaderParameter {
                        count: None,
                        ty: "uint".into(),
                        label: "highbits".into(),
                        description: "The high 32-bit pattern of the input value.".into(),
                        range: None,
                        modifier: Some("out".into()),
                    },
                ],
            })).collect()},
        });
        symbols.functions.push(ShaderSymbol {
            label: "asin".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the arcsine of the specified value.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-asin".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The arcsine of the x parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
        });
        symbols.functions.push(ShaderSymbol {
            label: "atan".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the arctangent of the specified value.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-atan".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The arctangent of the x parameter. This value is within the range of -π/2 to π/2.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
        });
        symbols.functions.push(ShaderSymbol {
            label: "atan2".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the arctangent of two values (x,y).".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-atan2".into()),
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The arctangent of (y,x).".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "y".into(),
                    description: "The y value.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The x value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
        });
        symbols.functions.push(ShaderSymbol {
            label: "ceil".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the smallest integer value that is greater than or equal to the specified value.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-ceil".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The smallest integer value (returned as a floating-point type) that is greater than or equal to the x parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "CheckAccessFullyMapped".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Determines whether all values from a Sample, Gather, or Load operation accessed mapped tiles in a tiled resource.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/checkaccessfullymapped".into()),
            )),
            data: ShaderSymbolData::Functions { signatures:vec![ShaderSignature {
                returnType: "bool".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "uint".into(),
                    label: "status".into(),
                    description: "The status value that is returned from a Sample, Gather, or Load operation. Because you can't access this status value directly, you need to pass it to CheckAccessFullyMapped.".into(),
                    range:None,
                    modifier: None,
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                stages: Some(ShaderStageMask::FRAGMENT | ShaderStageMask::COMPUTE),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "clamp".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Clamps the specified value to the specified minimum and maximum range.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-clamp".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "A value to clamp.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "min".into(),
                    description: " The specified minimum range.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "max".into(),
                    description: " The specified maximum range.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "clip".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Discards the current pixel if the specified value is less than zero.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-clip".into()),
            )),
            data: ShaderSymbolData::Functions { signatures:vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "bool".into(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                    range:None,
                    modifier: None,
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                stages: Some(ShaderStageMask::FRAGMENT),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "cos".into(),
            mode: ShaderSymbolMode::Intrinsic(
                ShaderSymbolIntrinsic::new(
                    "Returns the cosine of the specified value.".into(),
                    Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-cos"
                    .into()),
                ),
            ),
            data: ShaderSymbolData::Functions {
                signatures: type_size_iter(&["float"], true, true, true)
                    .iter()
                    .map(|v| ShaderSignature {
                        returnType: v.format(),
                        description: "The cosine of the x parameter.".into(),
                        parameters: vec![ShaderParameter {
                            range: None,
                            modifier: None,
                            count: None,
                            ty: v.format(),
                            label: "x".into(),
                            description: "The specified value, in radians.".into(),
                        }],
                    })
                    .collect(),
            },
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "cosh".into(),
            mode: ShaderSymbolMode::Intrinsic(
                ShaderSymbolIntrinsic::new(
                    "Returns the hyperbolic cosine of the specified value.".into(),
                    Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-cos"
                    .into()),
                ),
            ),
            data: ShaderSymbolData::Functions {
                signatures: type_size_iter(&["float"], true, true, true)
                    .iter()
                    .map(|v| ShaderSignature {
                        returnType: v.format(),
                        description: "The hyperbolic cosine of the x parameter.".into(),
                        parameters: vec![ShaderParameter {
                            range: None,
                            modifier: None,
                            count: None,
                            ty: v.format(),
                            label: "x".into(),
                            description: "The specified value, in radians.".into(),
                        }],
                    })
                    .collect(),
            },
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "countbits".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Counts the number of bits (per component) set in the input integer.".into(),
                Some(
                    "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/countbits".into(),
                ),
            )),
            data: ShaderSymbolData::Functions {
                signatures: type_size_iter(&["uint"], true, true, false)
                    .iter()
                    .map(|v| ShaderSignature {
                        returnType: v.format(),
                        description: "The number of bits.".into(),
                        parameters: vec![ShaderParameter {
                            range: None,
                            modifier: None,
                            count: None,
                            ty: v.format(),
                            label: "value".into(),
                            description: "The input value.".into(),
                        }],
                    })
                    .collect(),
            },
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "cross".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the cross product of two floating-point, 3D vectors.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-cross".into()),
            )),
            data: ShaderSymbolData::Functions { signatures:vec![ShaderSignature {
                returnType: "float3".into(),
                description: "The cross product of the x parameter and the y parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "float3".into(),
                    label: "x".into(),
                    description: "The first floating-point, 3D vector.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "float3".into(),
                    label: "y".into(),
                    description: "The second floating-point, 3D vector.".into(),
                    range:None,
                    modifier: None,
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "ddx".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the partial derivative of the specified value with respect to the screen-space x-coordinate.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-ddx".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The partial derivative of the x parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel2),
                stages: Some(ShaderStageMask::FRAGMENT),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "ddx_coarse".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Computes a low precision partial derivative with respect to the screen-space x-coordinate.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/ddx-coarse".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The low precision partial derivative of value.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                stages: Some(ShaderStageMask::FRAGMENT),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "ddx_fine".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Computes a high precision partial derivative with respect to the screen-space x-coordinate.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/ddx-fine".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The high precision partial derivative of value.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                stages: Some(ShaderStageMask::FRAGMENT),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "ddy".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the partial derivative of the specified value with respect to the screen-space y-coordinate.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-ddy".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The partial derivative of the x parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel2),
                stages: Some(ShaderStageMask::FRAGMENT),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "ddy_coarse".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Computes a low precision partial derivative with respect to the screen-space y-coordinate.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/ddx-coarse".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The low precision partial derivative of value.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                stages: Some(ShaderStageMask::FRAGMENT),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "ddy_fine".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Computes a high precision partial derivative with respect to the screen-space y-coordinate.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/ddx-fine".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The high precision partial derivative of value.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                stages: Some(ShaderStageMask::FRAGMENT),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "degrees".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Converts the specified value from radians to degrees.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-degrees".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The result of converting the x parameter from radians to degrees.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "determinant".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the determinant of the specified floating-point, square matrix.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-determinant".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], false, false, true).iter().map(|v| ShaderSignature {
                returnType: v.format_as_scalar(),
                description: "The floating-point, scalar value that represents the determinant of the m parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "DeviceMemoryBarrier".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Blocks execution of all threads in a group until all device memory accesses have been completed.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/devicememorybarrier".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "DeviceMemoryBarrierWithGroupSync".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Blocks execution of all threads in a group until all device memory accesses have been completed and all threads in the group have reached this call.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/devicememorybarrierwithgroupsync".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "distance".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns a distance scalar between two vectors.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-determinant".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], false, true, false).iter().map(|v| ShaderSignature {
                returnType: v.format_as_scalar(),
                description: "A floating-point, scalar value that represents the distance between the x parameter and the y parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The first floating-point vector to compare.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "y".into(),
                    description: "The second floating-point vector to compare.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "dot".into(),
            mode: ShaderSymbolMode::Intrinsic(
                ShaderSymbolIntrinsic::new(
                    "Returns the dot product of two vectors.".into(),
                    Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-dot"
                    .into()),
                ),
            ),
            data: ShaderSymbolData::Functions {
                signatures: type_size_iter(&["float", "int"], false, true, false)
                    .iter()
                    .map(|v| ShaderSignature {
                        returnType: v.format_as_scalar(),
                        description: "The dot product of the x parameter and the y parameter."
                            .into(),
                        parameters: vec![
                            ShaderParameter {
                                count: None,
                                ty: v.format(),
                                label: "x".into(),
                                description: "The first vector.".into(),
                                range: None,
                                modifier: None,
                            },
                            ShaderParameter {
                                count: None,
                                ty: v.format(),
                                label: "y".into(),
                                description: "The second vector.".into(),
                                range: None,
                                modifier: None,
                            },
                        ],
                    })
                    .collect(),
            },
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "dst".into(),
            mode: ShaderSymbolMode::Intrinsic(
                ShaderSymbolIntrinsic::new("Calculates a distance vector.".into(), Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-dst"
                    .into())),
            ),
            data: ShaderSymbolData::Functions {
                signatures: type_size_iter(&["float", "int"], false, true, false)
                    .iter()
                    .map(|v| ShaderSignature {
                        returnType: v.format_as_scalar(),
                        description: "The computed distance vector.".into(),
                        parameters: vec![
                            ShaderParameter {
                                count: None,
                                ty: v.format(),
                                label: "x".into(),
                                description: "The first vector.".into(),
                                range: None,
                                modifier: None,
                            },
                            ShaderParameter {
                                count: None,
                                ty: v.format(),
                                label: "y".into(),
                                description: "The second vector.".into(),
                                range: None,
                                modifier: None,
                            },
                        ],
                    })
                    .collect(),
            },
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "errorf".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Submits an error message to the information queue.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/errorf".into()),
            )),
            data: ShaderSymbolData::Functions {
                signatures: vec![ShaderSignature {
                    returnType: "void".into(),
                    description: "".into(),
                    parameters: vec![
                        ShaderParameter {
                            range: None,
                            modifier: None,
                            count: None,
                            ty: "string".into(),
                            label: "message".into(),
                            description: "The format string.".into(),
                        },
                        ShaderParameter {
                            range: None,
                            modifier: None,
                            count: None,
                            ty: "T".into(),
                            label: "...".into(),
                            description: "Optional arguments.".into(),
                        },
                    ],
                }],
            },
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel4),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "EvaluateAttributeCentroid".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Evaluates at the pixel centroid.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/evaluateattributecentroid".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "attrib".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "attrib".into(),
                    label: "value".into(),
                    description: "The input value.".into(),
                    range:None,
                    modifier: None,
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                stages: Some(ShaderStageMask::FRAGMENT),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "EvaluateAttributeAtSample".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Evaluates at the indexed sample location.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/evaluateattributeatsample".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "attrib".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "attrib".into(),
                    label: "value".into(),
                    description: "The input value.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "uint".into(),
                    label: "sampleindex".into(),
                    description: "The sample location.".into(),
                    range:None,
                    modifier: None,
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                stages: Some(ShaderStageMask::FRAGMENT),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "EvaluateAttributeSnapped".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Evaluates at the pixel centroid with an offset.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/evaluateattributesnapped".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "attrib".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "attrib".into(),
                    label: "value".into(),
                    description: "The input value.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "int2".into(),
                    label: "offset".into(),
                    description: "A 2D offset from the pixel center using a 16x16 grid.".into(),
                    range:None,
                    modifier: None,
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                stages: Some(ShaderStageMask::FRAGMENT),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "exp".into(),
            mode: ShaderSymbolMode::Intrinsic(
                ShaderSymbolIntrinsic::new(
                    "Returns the base-e exponential, or ex, of the specified value.".into(),
                    Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-exp"
                    .into()),
                ),
            ),
            data: ShaderSymbolData::Functions {
                signatures: type_size_iter(&["float"], true, true, true)
                    .iter()
                    .map(|v| ShaderSignature {
                        returnType: v.format(),
                        description: "The base-e exponential of the x parameter.".into(),
                        parameters: vec![ShaderParameter {
                            range: None,
                            modifier: None,
                            count: None,
                            ty: v.format(),
                            label: "x".into(),
                            description: "The input value.".into(),
                        }],
                    })
                    .collect(),
            },
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "exp2".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the base 2 exponential, or 2^x, of the specified value.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-exp2".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The base 2 exponential of the x parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The input value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "f16tof32".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Converts the float16 stored in the low-half of the uint to a float.".into(),
                Some(
                    "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/f16tof32".into(),
                ),
            )),
            data: ShaderSymbolData::Functions {
                signatures: type_size_iter(&["uint"], false, true, false)
                    .iter()
                    .map(|v| ShaderSignature {
                        returnType: v.format_with_type("float"),
                        description: "The converted value.".into(),
                        parameters: vec![ShaderParameter {
                            range: None,
                            modifier: None,
                            count: None,
                            ty: v.format(),
                            label: "x".into(),
                            description: "The input value.".into(),
                        }],
                    })
                    .collect(),
            },
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel4),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "f32tof16".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Converts an input into a float16 type.".into(),
                Some(
                    "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/f32tof16".into(),
                ),
            )),
            data: ShaderSymbolData::Functions {
                signatures: type_size_iter(&["float"], false, true, false)
                    .iter()
                    .map(|v| ShaderSignature {
                        returnType: v.format_with_type("uint"),
                        description: "The converted value.".into(),
                        parameters: vec![ShaderParameter {
                            range: None,
                            modifier: None,
                            count: None,
                            ty: v.format(),
                            label: "x".into(),
                            description: "The input value.".into(),
                        }],
                    })
                    .collect(),
            },
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel4),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "faceforward".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Flips the surface-normal (if needed) to face in a direction opposite to i; returns the result in n.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-faceforward".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], false, true, false).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "A floating-point, surface normal vector that is facing the view direction.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "n".into(),
                    description: "The resulting floating-point surface-normal vector.".into(),
                    range:None,
                    modifier: None,
                },ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "i".into(),
                    description: "A floating-point, incident vector that points from the view position to the shading position.".into(),
                    range:None,
                    modifier: None,
                },ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "ng".into(),
                    description: "A floating-point surface-normal vector.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "firstbithigh".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Gets the location of the first set bit starting from the highest order bit and working downward, per component.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/firstbithigh".into()),
            )),
            data: ShaderSymbolData::Functions { signatures:  type_size_iter(&["int", "uint"], true, true, false).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The location of the first set bit.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "value".into(),
                    description: "The input value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "firstbitlow".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the location of the first set bit starting from the lowest order bit and working upward, per component.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/firstbitlow".into()),
            )),
            data: ShaderSymbolData::Functions { signatures:  type_size_iter(&["int", "uint"], true, true, false).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The location of the first set bit.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "value".into(),
                    description: "The input value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "floor".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the largest integer that is less than or equal to the specified value.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-floor".into()),
            )),
            data: ShaderSymbolData::Functions { signatures:  type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The largest integer value (returned as a floating-point type) that is less than or equal to the x parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "value".into(),
                    description: "The input value.".into(),
                            range: None,
                            modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "fma".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the double-precision fused multiply-addition of a * b + c.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-fma".into()),
            )),
            data: ShaderSymbolData::Functions { signatures:  type_size_iter(&["double"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The double-precision fused multiply-addition of parameters a * b + c. The returned value must be accurate to 0.5 units of least precision (ULP).".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "a".into(),
                    description: "The first value in the fused multiply-addition.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "b".into(),
                    description: "The second value in the fused multiply-addition.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "c".into(),
                    description: "The third value in the fused multiply-addition.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "fmod".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the floating-point remainder of x/y.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-fmod".into()),
            )),
            data: ShaderSymbolData::Functions { signatures:  type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The floating-point remainder of the x parameter divided by the y parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: " The floating-point dividend.".into(),
                            range: None,
                            modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "y".into(),
                    description: "The floating-point divisor.".into(),
                            range: None,
                            modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "frac".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the fractional (or decimal) part of x; which is greater than or equal to 0 and less than 1.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-frac".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The fractional part of the x parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                            range: None,
                            modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "frexp".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the mantissa and exponent of the specified floating-point value.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-frac".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The mantissa of the x parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified floating-point value. If the x parameter is 0, this function returns 0 for both the mantissa and the exponent.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "exp".into(),
                    description: "The returned exponent of the x parameter.".into(),
                    range:None,
                    modifier: Some("out".into()),
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "fwidth".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the absolute value of the partial derivatives of the specified value.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-fwidth".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The absolute value of the partial derivatives of the x parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel2),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "GetRenderTargetSampleCount".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Gets the number of samples for a render target.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-getrendertargetsamplecount".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "uint".into(),
                description: "The number of samples.".into(),
                parameters: vec![],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel4),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "GetRenderTargetSamplePosition".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Gets the sampling position (x,y) for a given sample index.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-getrendertargetsampleposition".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "float2".into(),
                description: "The (x,y) position of the given sample.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "int".into(),
                    label: "index".into(),
                    description: "".into() ,
                    range:None,
                    modifier: None,
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel4),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "GroupMemoryBarrier".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Blocks execution of all threads in a group until all group shared accesses have been completed.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/groupmemorybarrier".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "GroupMemoryBarrierWithGroupSync".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Blocks execution of all threads in a group until all group shared accesses have been completed and all threads in the group have reached this call.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/groupmemorybarrierwithgroupsync".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "InterlockedAdd".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Performs a guaranteed atomic add of value to the dest resource variable.".into(),
                Some(
                    "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/interlockedadd"
                        .into(),
                ),
            )),
            data: ShaderSymbolData::Functions {
                signatures: vec![ShaderSignature {
                    returnType: "void".into(),
                    description: "".into(),
                    parameters: vec![
                        ShaderParameter {
                            range: None,
                            modifier: None,
                            count: None,
                            ty: "R".into(),
                            label: "dest".into(),
                            description: "The destination address.".into(),
                        },
                        ShaderParameter {
                            range: None,
                            modifier: None,
                            count: None,
                            ty: "T".into(),
                            label: "value".into(),
                            description: "The input value.".into(),
                        },
                        ShaderParameter {
                            range: None,
                            modifier: Some("out".into()),
                            count: None,
                            ty: "T".into(),
                            label: "original_value".into(),
                            description: "Optional. The original input value.".into(),
                        },
                    ],
                }],
            },
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "InterlockedAnd".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Performs a guaranteed atomic and.".into(),
                Some(
                    "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/interlockedand"
                        .into(),
                ),
            )),
            data: ShaderSymbolData::Functions {
                signatures: vec![ShaderSignature {
                    returnType: "void".into(),
                    description: "".into(),
                    parameters: vec![
                        ShaderParameter {
                            range: None,
                            modifier: None,
                            count: None,
                            ty: "R".into(),
                            label: "dest".into(),
                            description: "The destination address.".into(),
                        },
                        ShaderParameter {
                            range: None,
                            modifier: None,
                            count: None,
                            ty: "T".into(),
                            label: "value".into(),
                            description: "The input value.".into(),
                        },
                        ShaderParameter {
                            range: None,
                            modifier: Some("out".into()),
                            count: None,
                            ty: "T".into(),
                            label: "original_value".into(),
                            description: "Optional. The original input value.".into(),
                        },
                    ],
                }],
            },
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "InterlockedCompareExchange".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Atomically compares the destination with the comparison value. If they are identical, the destination is overwritten with the input value. The original value is set to the destination's original value.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/interlockedcompareexchange".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "R".into(),
                    label: "dest".into(),
                    description: "The destination address.".into() ,
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "compare_value".into(),
                    description: "The comparison value.".into() ,
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "value".into(),
                    description: "The input value.".into() ,
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "original_value".into(),
                    description: "Optional. The original input value.".into() ,
                    range:None,
                    modifier: Some("out".into()),
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "InterlockedCompareStore".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Atomically compares the destination to the comparison value. If they are identical, the destination is overwritten with the input value.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/interlockedcomparestore".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "R".into(),
                    label: "dest".into(),
                    description: "The destination address.".into() ,
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "value".into(),
                    description: "The input value.".into() ,
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "original_value".into(),
                    description: "Optional. The original input value.".into() ,
                    range:None,
                    modifier: None,
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "InterlockedExchange".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Assigns value to dest and returns the original value.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/interlockedexchange".into()),
            )),
            data: ShaderSymbolData::Functions {
                signatures: vec![ShaderSignature {
                    returnType: "void".into(),
                    description: "".into(),
                    parameters: vec![
                        ShaderParameter {
                            count: None,
                            ty: "R".into(),
                            label: "dest".into(),
                            description: "The destination address.".into(),
                            range: None,
                            modifier: None,
                        },
                        ShaderParameter {
                            count: None,
                            ty: "T".into(),
                            label: "value".into(),
                            description: "The input value.".into(),
                            range: None,
                            modifier: None,
                        },
                        ShaderParameter {
                            count: None,
                            ty: "T".into(),
                            label: "original_value".into(),
                            description: "Optional. The original input value.".into(),
                            range: None,
                            modifier: Some("out".into()),
                        },
                    ],
                }],
            },
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "InterlockedMax".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Performs a guaranteed atomic max.".into(),
                Some(
                    "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/interlockedmax"
                        .into(),
                ),
            )),
            data: ShaderSymbolData::Functions {
                signatures: vec![ShaderSignature {
                    returnType: "void".into(),
                    description: "".into(),
                    parameters: vec![
                        ShaderParameter {
                            range: None,
                            modifier: None,
                            count: None,
                            ty: "R".into(),
                            label: "dest".into(),
                            description: "The destination address.".into(),
                        },
                        ShaderParameter {
                            range: None,
                            modifier: None,
                            count: None,
                            ty: "T".into(),
                            label: "value".into(),
                            description: "The input value.".into(),
                        },
                        ShaderParameter {
                            range: None,
                            modifier: Some("out".into()),
                            count: None,
                            ty: "T".into(),
                            label: "original_value".into(),
                            description: "Optional. The original input value.".into(),
                        },
                    ],
                }],
            },
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "InterlockedMin".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Performs a guaranteed atomic min.".into(),
                Some(
                    "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/interlockedmin"
                        .into(),
                ),
            )),
            data: ShaderSymbolData::Functions {
                signatures: vec![ShaderSignature {
                    returnType: "void".into(),
                    description: "".into(),
                    parameters: vec![
                        ShaderParameter {
                            range: None,
                            modifier: None,
                            count: None,
                            ty: "R".into(),
                            label: "dest".into(),
                            description: "The destination address.".into(),
                        },
                        ShaderParameter {
                            range: None,
                            modifier: None,
                            count: None,
                            ty: "T".into(),
                            label: "value".into(),
                            description: "The input value.".into(),
                        },
                        ShaderParameter {
                            range: None,
                            modifier: Some("out".into()),
                            count: None,
                            ty: "T".into(),
                            label: "original_value".into(),
                            description: "Optional. The original input value.".into(),
                        },
                    ],
                }],
            },
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "InterlockedOr".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Performs a guaranteed atomic or.".into(),
                Some(
                    "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/interlockedor"
                        .into(),
                ),
            )),
            data: ShaderSymbolData::Functions {
                signatures: vec![ShaderSignature {
                    returnType: "void".into(),
                    description: "".into(),
                    parameters: vec![
                        ShaderParameter {
                            range: None,
                            modifier: None,
                            count: None,
                            ty: "R".into(),
                            label: "dest".into(),
                            description: "The destination address.".into(),
                        },
                        ShaderParameter {
                            range: None,
                            modifier: None,
                            count: None,
                            ty: "T".into(),
                            label: "value".into(),
                            description: "The input value.".into(),
                        },
                        ShaderParameter {
                            range: None,
                            modifier: Some("out".into()),
                            count: None,
                            ty: "T".into(),
                            label: "original_value".into(),
                            description: "Optional. The original input value.".into(),
                        },
                    ],
                }],
            },
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "InterlockedXor".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Performs a guaranteed atomic xor.".into(),
                Some(
                    "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/interlockedxor"
                        .into(),
                ),
            )),
            data: ShaderSymbolData::Functions {
                signatures: vec![ShaderSignature {
                    returnType: "void".into(),
                    description: "".into(),
                    parameters: vec![
                        ShaderParameter {
                            range: None,
                            modifier: None,
                            count: None,
                            ty: "R".into(),
                            label: "dest".into(),
                            description: "The destination address.".into(),
                        },
                        ShaderParameter {
                            range: None,
                            modifier: None,
                            count: None,
                            ty: "T".into(),
                            label: "value".into(),
                            description: "The input value.".into(),
                        },
                        ShaderParameter {
                            range: None,
                            modifier: Some("out".into()),
                            count: None,
                            ty: "T".into(),
                            label: "original_value".into(),
                            description: "Optional. The original input value.".into(),
                        },
                    ],
                }],
            },
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "isfinite".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Determines if the specified floating-point value is finite.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-isfinite".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format_with_type("bool"),
                description: "Returns a value of the same size as the input, with a value set to True if the x parameter is finite; otherwise False.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "isinf".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Determines if the specified value is infinite.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-isinf".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format_with_type("bool"),
                description: "Returns a value of the same size as the input, with a value set to True if the x parameter is +INF or -INF. Otherwise, False.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "isnan".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Determines if the specified value is NAN or QNAN.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-isnan".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format_with_type("bool"),
                description: "Returns a value of the same size as the input, with a value set to True if the x parameter is NAN or QNAN. Otherwise, False.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "ldexp".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the result of multiplying the specified value by two, raised to the power of the specified exponent.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-ldexp".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The result of multiplying the x parameter by two, raised to the power of the exp parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "exp".into(),
                    description: "The specified exponent.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "length".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the length of the specified floating-point vector.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-length".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], false, true, false).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "A floating-point scalar that represents the length of the x parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format_as_scalar(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                            range: None,
                            modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "lerp".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Performs a linear interpolation.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-lerp".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The result of the linear interpolation.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The first-floating point value.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The second-floating point value.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "s".into(),
                    description: "A value that linearly interpolates between the x parameter and the y parameter.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "lit".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns a lighting coefficient vector.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-lit".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "float4".into(),
                description: "The lighting coefficient vector.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "n_dot_l".into(),
                    description: "The dot product of the normalized surface normal and the light vector.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "n_dot_h".into(),
                    description: "The dot product of the half-angle vector and the surface normal.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "m".into(),
                    description: "A specular exponent.".into(),
                    range:None,
                    modifier: None,
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "log".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the base-e logarithm of the specified value.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-log".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The base-e logarithm of the x parameter. If the x parameter is negative, this function returns indefinite. If the x parameter is 0, this function returns -INF.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "log10".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the base-10 logarithm of the specified value.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-log10".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The base-10 logarithm of the x parameter. If the x parameter is negative, this function returns indefinite. If the x is 0, this function returns -INF.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "log2".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the base-2 logarithm of the specified value.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-log2".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The base-2 logarithm of the x parameter. If the x parameter is negative, this function returns indefinite. If the x is 0, this function returns +INF.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "mad".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Performs an arithmetic multiply/add operation on three values.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/mad".into()),
            )),
            data: ShaderSymbolData::Functions {
                signatures: type_size_iter(&["float"], true, true, false)
                    .iter()
                    .map(|v| ShaderSignature {
                        returnType: v.format(),
                        description: "The result of mvalue * avalue + bvalue.".into(),
                        parameters: vec![
                            ShaderParameter {
                                count: None,
                                ty: v.format(),
                                label: "m".into(),
                                description: "The multiplication value.".into(),
                                range: None,
                                modifier: None,
                            },
                            ShaderParameter {
                                count: None,
                                ty: v.format(),
                                label: "a".into(),
                                description: "The first addition value.".into(),
                                range: None,
                                modifier: None,
                            },
                            ShaderParameter {
                                count: None,
                                ty: v.format(),
                                label: "b".into(),
                                description: "The second addition value..".into(),
                                range: None,
                                modifier: None,
                            },
                        ],
                    })
                    .collect(),
            },
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "max".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Selects the greater of x and y.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-max".into()),
            )),
            data: ShaderSymbolData::Functions {
                signatures: type_size_iter(&["float", "int"], true, true, true)
                    .iter()
                    .map(|v| ShaderSignature {
                        returnType: v.format(),
                        description: "The x or y parameter, whichever is the largest value.".into(),
                        parameters: vec![
                            ShaderParameter {
                                count: None,
                                ty: v.format(),
                                label: "x".into(),
                                description: "The x input value.".into(),
                                range: None,
                                modifier: None,
                            },
                            ShaderParameter {
                                count: None,
                                ty: v.format(),
                                label: "y".into(),
                                description: "The y input value.".into(),
                                range: None,
                                modifier: None,
                            },
                        ],
                    })
                    .collect(),
            },
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "min".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Selects the lesser  of x and y.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-min".into()),
            )),
            data: ShaderSymbolData::Functions {
                signatures: type_size_iter(&["float", "int"], true, true, true)
                    .iter()
                    .map(|v| ShaderSignature {
                        returnType: v.format(),
                        description: "The x or y parameter, whichever is the smallest value."
                            .into(),
                        parameters: vec![
                            ShaderParameter {
                                count: None,
                                ty: v.format(),
                                label: "x".into(),
                                description: "The x input value.".into(),
                                range: None,
                                modifier: None,
                            },
                            ShaderParameter {
                                count: None,
                                ty: v.format(),
                                label: "y".into(),
                                description: "The y input value.".into(),
                                range: None,
                                modifier: None,
                            },
                        ],
                    })
                    .collect(),
            },
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "modf".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Splits the value x into fractional and integer parts, each of which has the same sign as x.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-modf".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float", "int"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The signed-fractional portion of x.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The x input value.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "ip".into(),
                    description: "The integer portion of x.".into(),
                    range:None,
                    modifier: Some("out".into()),
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "msad4".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Compares a 4-byte reference value and an 8-byte source value and accumulates a vector of 4 sums. Each sum corresponds to the masked sum of absolute differences of a different byte alignment between the reference value and the source value.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-msad4".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "uint4".into(),
                description: "A vector of 4 sums. Each sum corresponds to the masked sum of absolute differences of different byte alignments between the reference value and the source value. msad4 doesn't include a difference in the sum if that difference is masked (that is, the reference byte is 0).".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "uint".into(),
                    label: "reference".into(),
                    description: "The reference array of 4 bytes in one uint value.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "uint2".into(),
                    label: "source".into(),
                    description: "The source array of 8 bytes in two uint2 values.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "uint4".into(),
                    label: "accum".into(),
                    description: "A vector of 4 values. msad4 adds this vector to the masked sum of absolute differences of the different byte alignments between the reference value and the source value.".into(),
                    range:None,
                    modifier: None,
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "mul".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Multiplies x and y using matrix math. The inner dimension x-columns and y-rows must be equal.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-mul".into()),
            )),            // TODO: handle all overrides vec * scalar & co...
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float", "int"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The result of x times y. The result has the dimension x-rows x y-columns.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The x input value. If x is a vector, it treated as a row vector.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "y".into(),
                    description: " The y input value. If y is a vector, it treated as a column vector.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "noise".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Generates a random value using the Perlin-noise algorithm.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-noise".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], false, true, false).iter().map(|v| ShaderSignature {
                returnType: v.format_as_scalar(),
                description: "The Perlin noise value within a range between -1 and 1.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "A floating-point vector from which to generate Perlin noise.".into(),
                            range: None,
                            modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "normalize".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Normalizes the specified floating-point vector according to x / length(x).".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-normalize".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], false, true, false).iter().map(|v| ShaderSignature {
                returnType: v.format_as_scalar(),
                description: "The normalized x parameter. If the length of the x parameter is 0, the result is indefinite.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified floating-point vector.".into(),
                            range: None,
                            modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "pow".into(),
            mode: ShaderSymbolMode::Intrinsic(
                ShaderSymbolIntrinsic::new(
                    "Returns the specified value raised to the specified power.".into(),
                    Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-pow".into()),
                ),
            ),
            data: ShaderSymbolData::Functions {
                signatures: type_size_iter(&["float"], true, true, true)
                    .iter()
                    .map(|v| ShaderSignature {
                        returnType: v.format_as_scalar(),
                        description: "The x parameter raised to the power of the y parameter."
                            .into(),
                        parameters: vec![
                            ShaderParameter {
                                count: None,
                                ty: v.format(),
                                label: "x".into(),
                                description: "The specified value.".into(),
                                range: None,
                                modifier: None,
                            },
                            ShaderParameter {
                                count: None,
                                ty: v.format(),
                                label: "y".into(),
                                description: "The specified power.".into(),
                                range: None,
                                modifier: None,
                            },
                        ],
                    })
                    .collect(),
            },
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "printf".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Submits an custom shader message to the information queue.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/printf".into()),
            )),
            data: ShaderSymbolData::Functions {
                signatures: vec![ShaderSignature {
                    returnType: "void".into(),
                    description: "".into(),
                    parameters: vec![
                        ShaderParameter {
                            count: None,
                            ty: "string".into(),
                            label: "message".into(),
                            description: "The format string.".into(),
                            range: None,
                            modifier: None,
                        },
                        ShaderParameter {
                            count: None,
                            ty: "T".into(),
                            label: "...".into(),
                            description: "Optional arguments.".into(),
                            range: None,
                            modifier: None,
                        },
                    ],
                }],
            },
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel4),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "Process2DQuadTessFactorsAvg".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Generates the corrected tessellation factors for a quad patch.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/process2dquadtessfactorsavg".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "float4".into(),
                    label: "RawEdgeFactors".into(),
                    description: "The edge tessellation factors, passed into the tessellator stage.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "float2".into(),
                    label: "InsideScale".into(),
                    description: "The scale factor applied to the UV tessellation factors computed by the tessellation stage. The allowable range for InsideScale is 0.0 to 1.0.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "float4".into(),
                    label: "RoundedEdgeTessFactors".into(),
                    description: "The rounded edge-tessellation factors calculated by the tessellator stage.".into(),
                    range:None,
                    modifier: Some("out".into()),
                },
                ShaderParameter {
                    count: None,
                    ty: "float2".into(),
                    label: "RoundedInsideTessFactors".into(),
                    description: "The rounded tessellation factors calculated by the tessellator stage for inside edges.".into(),
                    range:None,
                    modifier: Some("out".into()),
                },
                ShaderParameter {
                    count: None,
                    ty: "float2".into(),
                    label: "UnroundedInsideTessFactors".into(),
                    description: "The tessellation factors calculated by the tessellator stage for inside edges.".into(),
                    range:None,
                    modifier: Some("out".into()),
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                stages: Some(ShaderStageMask::TESSELATION_CONTROL),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "Process2DQuadTessFactorsMax".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Generates the corrected tessellation factors for a quad patch.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/process2dquadtessfactorsmax".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "float4".into(),
                    label: "RawEdgeFactors".into(),
                    description: "The edge tessellation factors, passed into the tessellator stage.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "float2".into(),
                    label: "InsideScale".into(),
                    description: "The scale factor applied to the UV tessellation factors computed by the tessellation stage. The allowable range for InsideScale is 0.0 to 1.0.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "float4".into(),
                    label: "RoundedEdgeTessFactors".into(),
                    description: "The rounded edge-tessellation factors calculated by the tessellator stage.".into(),
                    range:None,
                    modifier: Some("out".into()),
                },
                ShaderParameter {
                    count: None,
                    ty: "float2".into(),
                    label: "RoundedInsideTessFactors".into(),
                    description: "The rounded tessellation factors calculated by the tessellator stage for inside edges.".into(),
                    range:None,
                    modifier: Some("out".into()),
                },
                ShaderParameter {
                    count: None,
                    ty: "float2".into(),
                    label: "UnroundedInsideTessFactors".into(),
                    description: "The tessellation factors calculated by the tessellator stage for inside edges.".into(),
                    range:None,
                    modifier: Some("out".into()),
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                stages: Some(ShaderStageMask::TESSELATION_CONTROL),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "Process2DQuadTessFactorsMin".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Generates the corrected tessellation factors for a quad patch.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/process2dquadtessfactorsmin".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "float4".into(),
                    label: "RawEdgeFactors".into(),
                    description: "The edge tessellation factors, passed into the tessellator stage.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "float2".into(),
                    label: "InsideScale".into(),
                    description: "The scale factor applied to the UV tessellation factors computed by the tessellation stage. The allowable range for InsideScale is 0.0 to 1.0.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "float4".into(),
                    label: "RoundedEdgeTessFactors".into(),
                    description: "The rounded edge-tessellation factors calculated by the tessellator stage.".into(),
                    range:None,
                    modifier: Some("out".into()),
                },
                ShaderParameter {
                    count: None,
                    ty: "float2".into(),
                    label: "RoundedInsideTessFactors".into(),
                    description: "The rounded tessellation factors calculated by the tessellator stage for inside edges.".into(),
                    range:None,
                    modifier: Some("out".into()),
                },
                ShaderParameter {
                    count: None,
                    ty: "float2".into(),
                    label: "UnroundedInsideTessFactors".into(),
                    description: "The tessellation factors calculated by the tessellator stage for inside edges.".into(),
                    range:None,
                    modifier: Some("out".into()),
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                stages: Some(ShaderStageMask::TESSELATION_CONTROL),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "ProcessIsolineTessFactors".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Generates the rounded tessellation factors for an isoline.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/processisolinetessfactors".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "RawDetailFactor".into(),
                    description: "The desired detail factor.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "RawDensityFactor".into(),
                    description: "The desired density factor.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "RoundedDetailFactor".into(),
                    description: "The rounded detail factor clamped to a range that can be used by the tessellator.".into(),
                    range:None,
                    modifier: Some("out".into()),
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "RoundedDensityFactor".into(),
                    description: "The rounded density factor clamped to a rangethat can be used by the tessellator.".into(),
                    range:None,
                    modifier: Some("out".into()),
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                stages: Some(ShaderStageMask::TESSELATION_CONTROL),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "ProcessQuadTessFactorsAvg".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Generates the corrected tessellation factors for a quad patch.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/processquadtessfactorsavg".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "float4".into(),
                    label: "RawEdgeFactors".into(),
                    description: "The edge tessellation factors, passed into the tessellator stage.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "InsideScale".into(),
                    description: "The scale factor applied to the UV tessellation factors computed by the tessellation stage. The allowable range for InsideScale is 0.0 to 1.0.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "float4".into(),
                    label: "RoundedEdgeTessFactors".into(),
                    description: "The rounded edge-tessellation factors calculated by the tessellator stage.".into(),
                    range:None,
                    modifier: Some("out".into()),
                },
                ShaderParameter {
                    count: None,
                    ty: "float2".into(),
                    label: "RoundedInsideTessFactors".into(),
                    description: "The rounded tessellation factors calculated by the tessellator stage for inside edges.".into(),
                    range:None,
                    modifier: Some("out".into()),
                },
                ShaderParameter {
                    count: None,
                    ty: "float2".into(),
                    label: "UnroundedInsideTessFactors".into(),
                    description: "The tessellation factors calculated by the tessellator stage for inside edges.".into(),
                    range:None,
                    modifier: Some("out".into()),
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                stages: Some(ShaderStageMask::TESSELATION_CONTROL),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "ProcessQuadTessFactorsMax".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Generates the corrected tessellation factors for a quad patch.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/processquadtessfactorsmax".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "float4".into(),
                    label: "RawEdgeFactors".into(),
                    description: "The edge tessellation factors, passed into the tessellator stage.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "InsideScale".into(),
                    description: "The scale factor applied to the UV tessellation factors computed by the tessellation stage. The allowable range for InsideScale is 0.0 to 1.0.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "float4".into(),
                    label: "RoundedEdgeTessFactors".into(),
                    description: "The rounded edge-tessellation factors calculated by the tessellator stage.".into(),
                    range:None,
                    modifier: Some("out".into()),
                },
                ShaderParameter {
                    count: None,
                    ty: "float2".into(),
                    label: "RoundedInsideTessFactors".into(),
                    description: "The rounded tessellation factors calculated by the tessellator stage for inside edges.".into(),
                    range:None,
                    modifier: Some("out".into()),
                },
                ShaderParameter {
                    count: None,
                    ty: "float2".into(),
                    label: "UnroundedInsideTessFactors".into(),
                    description: "The tessellation factors calculated by the tessellator stage for inside edges.".into(),
                    range:None,
                    modifier: Some("out".into()),
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                stages: Some(ShaderStageMask::TESSELATION_CONTROL),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "ProcessQuadTessFactorsMin".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Generates the corrected tessellation factors for a quad patch.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/processquadtessfactorsmin".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "float4".into(),
                    label: "RawEdgeFactors".into(),
                    description: "The edge tessellation factors, passed into the tessellator stage.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "InsideScale".into(),
                    description: "The scale factor applied to the UV tessellation factors computed by the tessellation stage. The allowable range for InsideScale is 0.0 to 1.0.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "float4".into(),
                    label: "RoundedEdgeTessFactors".into(),
                    description: "The rounded edge-tessellation factors calculated by the tessellator stage.".into(),
                    range:None,
                    modifier: Some("out".into()),
                },
                ShaderParameter {
                    count: None,
                    ty: "float2".into(),
                    label: "RoundedInsideTessFactors".into(),
                    description: "The rounded tessellation factors calculated by the tessellator stage for inside edges.".into(),
                    range:None,
                    modifier: Some("out".into()),
                },
                ShaderParameter {
                    count: None,
                    ty: "float2".into(),
                    label: "UnroundedInsideTessFactors".into(),
                    description: "The tessellation factors calculated by the tessellator stage for inside edges.".into(),
                    range:None,
                    modifier: Some("out".into()),
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                stages: Some(ShaderStageMask::TESSELATION_CONTROL),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "ProcessTriTessFactorsAvg".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Generates the corrected tessellation factors for a tri patch.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/processtritessfactorsavg".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "float4".into(),
                    label: "RawEdgeFactors".into(),
                    description: "The edge tessellation factors, passed into the tessellator stage.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "InsideScale".into(),
                    description: "The scale factor applied to the UV tessellation factors computed by the tessellation stage. The allowable range for InsideScale is 0.0 to 1.0.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "float3".into(),
                    label: "RoundedEdgeTessFactors".into(),
                    description: "The rounded edge-tessellation factors calculated by the tessellator stage.".into(),
                    range:None,
                    modifier: Some("out".into()),
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "RoundedInsideTessFactors".into(),
                    description: "The rounded tessellation factors calculated by the tessellator stage for inside edges.".into(),
                    range:None,
                    modifier: Some("out".into()),
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "UnroundedInsideTessFactors".into(),
                    description: "The tessellation factors calculated by the tessellator stage for inside edges.".into(),
                    range:None,
                    modifier: Some("out".into()),
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                stages: Some(ShaderStageMask::TESSELATION_CONTROL),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "ProcessTriTessFactorsMax".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Generates the corrected tessellation factors for a tri patch.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/processtritessfactorsmax".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "float4".into(),
                    label: "RawEdgeFactors".into(),
                    description: "The edge tessellation factors, passed into the tessellator stage.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "InsideScale".into(),
                    description: "The scale factor applied to the UV tessellation factors computed by the tessellation stage. The allowable range for InsideScale is 0.0 to 1.0.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "float3".into(),
                    label: "RoundedEdgeTessFactors".into(),
                    description: "The rounded edge-tessellation factors calculated by the tessellator stage.".into(),
                    range:None,
                    modifier: Some("out".into()),
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "RoundedInsideTessFactors".into(),
                    description: "The rounded tessellation factors calculated by the tessellator stage for inside edges.".into(),
                    range:None,
                    modifier: Some("out".into()),
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "UnroundedInsideTessFactors".into(),
                    description: "The tessellation factors calculated by the tessellator stage for inside edges.".into(),
                    range:None,
                    modifier: Some("out".into()),
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                stages: Some(ShaderStageMask::TESSELATION_CONTROL),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "ProcessTriTessFactorsMin".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Generates the corrected tessellation factors for a tri patch.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/processtritessfactorsmin".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "float4".into(),
                    label: "RawEdgeFactors".into(),
                    description: "The edge tessellation factors, passed into the tessellator stage.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "InsideScale".into(),
                    description: "The scale factor applied to the UV tessellation factors computed by the tessellation stage. The allowable range for InsideScale is 0.0 to 1.0.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "float3".into(),
                    label: "RoundedEdgeTessFactors".into(),
                    description: "The rounded edge-tessellation factors calculated by the tessellator stage.".into(),
                    range:None,
                    modifier: Some("out".into()),
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "RoundedInsideTessFactors".into(),
                    description: "The rounded tessellation factors calculated by the tessellator stage for inside edges.".into(),
                    range:None,
                    modifier: Some("out".into()),
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "UnroundedInsideTessFactors".into(),
                    description: "The tessellation factors calculated by the tessellator stage for inside edges.".into(),
                    range:None,
                    modifier: Some("out".into()),
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                stages: Some(ShaderStageMask::TESSELATION_CONTROL),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "radians".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Converts the specified value from degrees to radians.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-radians".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The x parameter converted from degrees to radians.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "rcp".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Calculates a fast, approximate, per-component reciprocal.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/rcp".into()),
            )),
            data: ShaderSymbolData::Functions {
                signatures: type_size_iter(&["float", "double"], true, true, true)
                    .iter()
                    .map(|v| ShaderSignature {
                        returnType: v.format(),
                        description: "The reciprocal of the x parameter.".into(),
                        parameters: vec![ShaderParameter {
                            range: None,
                            modifier: None,
                            count: None,
                            ty: v.format(),
                            label: "x".into(),
                            description: "The specified value.".into(),
                        }],
                    })
                    .collect(),
            },
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "reflect".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns a reflection vector using an incident ray and a surface normal.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-reflect".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], false, true, false).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "A floating-point, reflection vector.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "i".into(),
                    description: "A floating-point, incident vector.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "n".into(),
                    description: "A floating-point, normal vector.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "refract".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns a refraction vector using an entering ray, a surface normal, and a refraction index.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-refract".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], false, true, false).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "A floating-point, refraction vector. If the angle between the entering ray i and the surface normal n is too great for a given refraction index ?, the return value is (0,0,0).".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "i".into(),
                    description: "A floating-point, ray direction vector.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "n".into(),
                    description: "A floating-point, surface normal vector.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: v.format_as_scalar(),
                    label: "f".into(),
                    description: "A floating-point, refraction index scalar.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "reversebits".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Reverses the order of the bits, per component.".into(),
                Some(
                    "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/reversebits"
                        .into(),
                ),
            )),
            data: ShaderSymbolData::Functions {
                signatures: type_size_iter(&["uint"], true, true, false)
                    .iter()
                    .map(|v| ShaderSignature {
                        returnType: v.format(),
                        description: "The input value, with the bit order reversed.".into(),
                        parameters: vec![ShaderParameter {
                            range: None,
                            modifier: None,
                            count: None,
                            ty: v.format(),
                            label: "value".into(),
                            description: "The input value.".into(),
                        }],
                    })
                    .collect(),
            },
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel5),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "round".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Rounds the specified value to the nearest integer. Halfway cases are rounded to the nearest even.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-round".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The x parameter, rounded to the nearest integer within a floating-point type.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "rsqrt".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the reciprocal of the square root of the specified value.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-round".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The reciprocal of the square root of the x parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "saturate".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Clamps the specified value within the range of 0 to 1.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-saturate".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The x parameter, clamped within the range of 0 to 1.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "sign".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the sign of x.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-saturate".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float", "int"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "Returns -1 if x is less than zero; 0 if x equals zero; and 1 if x is greater than zero.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The input value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "sin".into(),
            mode: ShaderSymbolMode::Intrinsic(
                ShaderSymbolIntrinsic::new(
                    "Returns the sine of the specified value.".into(),
                    Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-sin"
                    .into()),
                ),
            ),
            data: ShaderSymbolData::Functions {
                signatures: type_size_iter(&["float"], true, true, true)
                    .iter()
                    .map(|v| ShaderSignature {
                        returnType: v.format(),
                        description: "The sine of the x parameter.".into(),
                        parameters: vec![ShaderParameter {
                            range: None,
                            modifier: None,
                            count: None,
                            ty: v.format(),
                            label: "x".into(),
                            description: "The input value.".into(),
                        }],
                    })
                    .collect(),
            },
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "sincos".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the sine and cosine of x.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-sincos".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value, in radians.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "s".into(),
                    description: "Returns the sine of x.".into(),
                    range:None,
                    modifier: Some("out".into()),
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "c".into(),
                    description: "Returns the cosine of x.".into(),
                    range:None,
                    modifier: Some("out".into()),
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "sinh".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the hyperbolic sine of the specified value.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-sinh".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The hyperbolic sine of the x parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value, in radians.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "smoothstep".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns a smooth Hermite interpolation between 0 and 1, if x is in the range [min, max].".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-smoothstep".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "Returns 0 if x is less than min; 1 if x is greater than max; otherwise, a value between 0 and 1 if x is in the range [min, max].".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "min".into(),
                    description: "The minimum range of the x parameter.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "max".into(),
                    description: "The maximum range of the x parameter.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value to be interpolated.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "sqrt".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the square root of the specified floating-point value, per component.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-sqrt".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The square root of the x parameter, per component.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified floating-point value.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "step".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Compares two values, returning 0 or 1 based on which value is greater.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-sqrt".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "1 if the x parameter is greater than or equal to the y parameter; otherwise, 0.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "y".into(),
                    description: "The first floating-point value to compare.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The second floating-point value to compare.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "tan".into(),
            mode: ShaderSymbolMode::Intrinsic(
                ShaderSymbolIntrinsic::new(
                    "Returns the tangent of the specified value.".into(),
                    Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-tan"
                    .into()),
                ),
            ),
            data: ShaderSymbolData::Functions {
                signatures: type_size_iter(&["float"], true, true, true)
                    .iter()
                    .map(|v| ShaderSignature {
                        returnType: v.format(),
                        description: "The tangent of the x parameter.".into(),
                        parameters: vec![ShaderParameter {
                            range: None,
                            modifier: None,
                            count: None,
                            ty: v.format(),
                            label: "x".into(),
                            description: "The specified value, in radians.".into(),
                        }],
                    })
                    .collect(),
            },
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "tanh".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the hyperbolic tangent of the specified value.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-tanh".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The hyperbolic tangent of the x parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value, in radians.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        for dim in 1..=4 {
            let dim_text = if dim == 4 {
                "CUBE".into()
            } else {
                format!("{}D", dim)
            };
            let dim_text_lower = dim_text.to_lowercase();
            symbols.functions.push(ShaderSymbol {
                label: format!("tex{}", dim_text),
                mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                format!("Samples a {} texture.", dim_text),
                Some(format!("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-tex{}", dim_text_lower)),
            )),
                    data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                    returnType: "float4".into(),
                    description: "The value of the texture data.".into(),
                    parameters: vec![ShaderParameter {
                    count: None,
                        ty: format!("sampler{}", dim_text),
                        label: "s".into(),
                        description: "The sampler state.".into(),
                    range:None,
                    modifier: None,
                    },
                    ShaderParameter {
                    count: None,
                        ty: format!("float{}", dim),
                        label: "t".into(),
                        description: "The texture coordinate.".into(),
                    range:None,
                    modifier: None,
                    }],
                }]},
                requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                    min_shader_model: Some(HlslShaderModel::ShaderModel1),
                    stages: Some(ShaderStageMask::FRAGMENT),
                    ..Default::default()
                })),
            });
            symbols.functions.push(ShaderSymbol {
                label: format!("tex{}", dim_text),
                mode:ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                    format!("Samples a {} texture using a gradient to select the mip level.", dim_text), 
                    Some(format!("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-tex{}-s-t-ddx-ddy", dim_text_lower))
                )),
                data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                    returnType: "float4".into(),
                    description: "The value of the texture data.".into(),
                    parameters: vec![ShaderParameter {
                    count: None,
                        ty: format!("sampler{}", dim_text),
                        label: "s".into(),
                        description: "The sampler state.".into(),
                    range:None,
                    modifier: None,
                    },
                    ShaderParameter {
                    count: None,
                        ty: format!("float{}", dim),
                        label: "t".into(),
                        description: "The texture coordinate.".into(),
                    range:None,
                    modifier: None,
                    },
                    ShaderParameter {
                    count: None,
                        ty: format!("float{}", dim),
                        label: "ddx".into(),
                        description: "Rate of change of the surface geometry in the x direction.".into(),
                    range:None,
                    modifier: None,
                    },
                    ShaderParameter {
                    count: None,
                        ty: format!("float{}", dim),
                        label: "ddy".into(),
                        description: "Rate of change of the surface geometry in the y direction.".into(),
                    range:None,
                    modifier: None,
                    }],
                }]},
                requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                    min_shader_model: Some(HlslShaderModel::ShaderModel2),
                    stages: Some(ShaderStageMask::FRAGMENT),
                    ..Default::default()
                })),
            });
            symbols.functions.push(ShaderSymbol {
                label: format!("tex{}bias", dim_text),
                mode:ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                    format!("Samples a {} texture after biasing the mip level by t.w.", dim_text), 
                    Some(format!("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-tex{}bias", dim_text_lower))
                )),
                data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                    returnType: "float4".into(),
                    description: "The value of the texture data.".into(),
                    parameters: vec![ShaderParameter {
                    count: None,
                        ty: format!("sampler{}", dim_text),
                        label: "s".into(),
                        description: "The sampler state.".into(),
                    range:None,
                    modifier: None,
                    },
                    ShaderParameter {
                    count: None,
                        ty: format!("float{}", dim),
                        label: "t".into(),
                        description: "The texture coordinate.".into(),
                    range:None,
                    modifier: None,
                    }],
                }]},
                requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                    min_shader_model: Some(HlslShaderModel::ShaderModel2),
                    stages: Some(ShaderStageMask::FRAGMENT),
                    ..Default::default()
                })),
            });
            symbols.functions.push(ShaderSymbol {
                label: format!("tex{}grad", dim_text),
                mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                format!("Samples a {} texture using a gradient to select the mip level.", dim_text),
                Some(format!("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-tex{}grad", dim_text_lower)),
            )),
                    data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                    returnType: "float4".into(),
                    description: "The value of the texture data.".into(),
                    parameters: vec![ShaderParameter {
                    count: None,
                        ty: format!("sampler{}", dim_text),
                        label: "s".into(),
                        description: "The sampler state.".into(),
                    range:None,
                    modifier: None,
                    },
                    ShaderParameter {
                    count: None,
                        ty: format!("float{}", dim),
                        label: "t".into(),
                        description: "The texture coordinate.".into(),
                    range:None,
                    modifier: None,
                    },
                    ShaderParameter {
                    count: None,
                        ty: format!("float{}", dim),
                        label: "ddx".into(),
                        description: "Rate of change of the surface geometry in the x direction.".into(),
                    range:None,
                    modifier: None,
                    },
                    ShaderParameter {
                    count: None,
                        ty: format!("float{}", dim),
                        label: "ddy".into(),
                        description: "Rate of change of the surface geometry in the y direction.".into(),
                    range:None,
                    modifier: None,
                    }],
                }]},
                requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                    min_shader_model: Some(HlslShaderModel::ShaderModel2),
                    stages: Some(ShaderStageMask::FRAGMENT),
                    ..Default::default()
                })),
            });
            symbols.functions.push(ShaderSymbol {
                label: format!("tex{}lod", dim_text),
                mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                    format!("Samples a {} texture with mipmaps. The mipmap LOD is specified in t.w.", dim_text),
                    Some(format!("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-tex{}lod", dim_text_lower)),
                )),
                data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                    returnType: "float4".into(),
                    description: "The value of the texture data.".into(),
                    parameters: vec![ShaderParameter {
                    count: None,
                        ty: format!("sampler{}", dim_text),
                        label: "s".into(),
                        description: "The sampler state.".into(),
                    range:None,
                    modifier: None,
                    },
                    ShaderParameter {
                    count: None,
                        ty: "float4".into(),
                        label: "t".into(),
                        description: "The texture coordinate.".into(),
                    range:None,
                    modifier: None,
                    }],
                }]},
                requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                    min_shader_model: Some(HlslShaderModel::ShaderModel3),
                    stages: Some(ShaderStageMask::FRAGMENT),
                    ..Default::default()
                })),
            });
            symbols.functions.push(ShaderSymbol {
                label: format!("tex{}proj", dim_text),
                mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                format!("Samples a {} texture using a projective divide; the texture coordinate is divided by t.w before the lookup takes place.", dim_text),
                Some(format!("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-tex{}proj", dim_text_lower)),
            )),
                    data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                    returnType: "float4".into(),
                    description: "The value of the texture data.".into(),
                    parameters: vec![ShaderParameter {
                    count: None,
                        ty: format!("sampler{}", dim_text),
                        label: "s".into(),
                        description: "The sampler state.".into(),
                    range:None,
                    modifier: None,
                    },
                    ShaderParameter {
                    count: None,
                        ty: "float4".into(),
                        label: "t".into(),
                        description: "The texture coordinate.".into(),
                    range:None,
                    modifier: None,
                    }],
                }]},
                requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                    min_shader_model: Some(HlslShaderModel::ShaderModel2),
                    stages: Some(ShaderStageMask::FRAGMENT),
                    ..Default::default()
                })),
            });
        }
        symbols.functions.push(ShaderSymbol {
            label: "transpose".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Transposes the specified input matrix.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-transpose".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float", "int", "bool"], false, false, true).iter().map(|v| ShaderSignature {
                returnType: "float".into(),
                description: "The transposed value of the x parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "s".into(),
                    description: "The specified matrix.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "trunc".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Truncates a floating-point value to the integer component.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-trunc".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: "float".into(),
                description: "The input value truncated to an integer component.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified input.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel1),
                ..Default::default()
            })),
        });
        // sm 6.0
        symbols.functions.push(ShaderSymbol {
            label: "QuadReadAcrossDiagonal".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the specified local value which is read from the diagonally opposite lane in this quad.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/QuadReadAcrossDiagonal".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "T".into(),
                description: "The specified local value which is read from the diagonally opposite lane in this quad.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "localValue".into(),
                    description: "The requested type.".into(),
                    range:None,
                    modifier: None,
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel6),
                stages: Some(ShaderStageMask::FRAGMENT | ShaderStageMask::COMPUTE),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "QuadReadLaneAt".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the specified source value from the lane identified by the lane ID within the current quad.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/QuadReadLaneAt".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "T".into(),
                description: "The specified source value. The result of this function is uniform across the quad. If the source lane is inactive, the results are undefined.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "localValue".into(),
                    description: "The requested type.".into(),
                    range:None,
                    modifier: None,
                }, ShaderParameter {
                    count: None,
                    ty: "uint".into(),
                    label: "quadLaneID".into(),
                    description: "The lane ID; this will be a value from 0 to 3.".into(),
                    range:None,
                    modifier: None,
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel6),
                stages: Some(ShaderStageMask::FRAGMENT | ShaderStageMask::COMPUTE),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "QuadReadAcrossX".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the specified local value read from the other lane in this quad in the X direction.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/QuadReadAcrossX".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "T".into(),
                description: "The specified local value. If the source lane is inactive, the results are undefined.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "localValue".into(),
                    description: "The requested type.".into(),
                    range:None,
                    modifier: None,
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel6),
                stages: Some(ShaderStageMask::FRAGMENT | ShaderStageMask::COMPUTE),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "QuadReadAcrossY".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the specified local value read from the other lane in this quad in the Y direction.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/QuadReadAcrossY".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "T".into(),
                description: "The specified local value. If the source lane is inactive, the results are undefined.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "localValue".into(),
                    description: "The requested type.".into(),
                    range:None,
                    modifier: None,
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel6),
                stages: Some(ShaderStageMask::FRAGMENT | ShaderStageMask::COMPUTE),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "WaveActiveAllEqual".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns true for each component of expr that is the same for every active lane in the current wave.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/WaveActiveAllEqual".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "booln".into(),
                description: "Returns true for each component of expr that is the same for every active lane in the current wave.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "expr".into(),
                    description: "The expression to evaluate. type can be a basic scalar, vector, or matrix type.".into(),
                    range:None,
                    modifier: None,
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel6),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "WaveActiveBitAnd".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the bitwise AND of all the values of the expression across all active lanes in the current wave and replicates it back to all active lanes.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/WaveActiveBitAnd".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["int"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The bitwise AND value.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "expr".into(),
                    description: "The expression to evaluate.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel6),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "WaveActiveBitOr".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the bitwise OR of all the values of <expr> across all active non-helper lanes in the current wave, and replicates it back to all active non-helper lanes.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/WaveActiveBitOr".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["int"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The bitwise OR value.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "expr".into(),
                    description: "The expression to evaluate.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel6),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "WaveActiveBitXor".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the bitwise XOR of all the values of the expression across all active lanes in the current wave and replicates it back to all active lanes.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/WaveActiveBitXor".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["int"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The bitwise XOR value.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "expr".into(),
                    description: "The expression to evaluate.".into(),
                    range:None,
                    modifier: None,
                }],
            }).collect()},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel6),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "WaveActiveCountBits".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Counts the number of boolean variables which evaluate to true across all active lanes in the current wave, and replicates the result to all lanes in the wave.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/WaveActiveCountBits".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "uint".into(),
                description: "The number of lanes for which the boolean variable evaluates to true, across all active lanes in the current wave.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "bool".into(),
                    label: "bBit".into(),
                    description: "The boolean variables to evaluate. Providing an explicit true Boolean value returns the number of active lanes.".into(),
                    range:None,
                    modifier: None,
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel6),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "WaveActiveMax".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the maximum value of the expression across all active lanes in the current wave and replicates it back to all active lanes.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/WaveActiveMax".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "T".into(),
                description: "The maximum value.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "expr".into(),
                    description: "The expression to evaluate.".into(),
                    range:None,
                    modifier: None,
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel6),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "WaveActiveMin".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the maximum value of the expression across all active lanes in the current wave and replicates it back to all active lanes.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/WaveActiveMin".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "T".into(),
                description: "The minimum value.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "expr".into(),
                    description: "The expression to evaluate.".into(),
                    range:None,
                    modifier: None,
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel6),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "WaveActiveProduct".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Multiplies the values of the expression together across all active lanes in the current wave and replicates it back to all active lanes.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/WaveActiveProduct".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "T".into(),
                description: "The product value.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "expr".into(),
                    description: "The expression to evaluate.".into(),
                    range:None,
                    modifier: None,
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel6),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "WaveActiveSum".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Sums up the value of the expression across all active lanes in the current wave and replicates it to all lanes in the current wave.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/waveallsum".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "T".into(),
                description: "The sum value.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "expr".into(),
                    description: "The expression to evaluate.".into(),
                    range:None,
                    modifier: None,
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel6),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "WaveActiveAllTrue".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns true if the expression is true in all active lanes in the current wave."
                    .into(),
                Some(
                    "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/wavealltrue"
                        .into(),
                ),
            )),
            data: ShaderSymbolData::Functions {
                signatures: vec![ShaderSignature {
                    returnType: "bool".into(),
                    description: "True if the expression is true in all lanes.".into(),
                    parameters: vec![ShaderParameter {
                        count: None,
                        ty: "bool".into(),
                        label: "expr".into(),
                        description: "The expression to evaluate.".into(),
                        range: None,
                        modifier: None,
                    }],
                }],
            },
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel6),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "WaveActiveAnyTrue".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns true if the expression is true in any of the active lanes in the current wave.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/waveanytrue".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "bool".into(),
                description: "True if the expression is true in any lane.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "bool".into(),
                    label: "expr".into(),
                    description: "The expression to evaluate.".into(),
                    range:None,
                    modifier: None,
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel6),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "WaveActiveBallot".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns a uint4 containing a bitmask of the evaluation of the Boolean expression for all active lanes in the current wave.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/waveballot".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "uint4".into(),
                description: "A uint4 containing a bitmask of the evaluation of the Boolean expression for all active lanes in the current wave. The least-significant bit corresponds to the lane with index zero. The bits corresponding to inactive lanes will be zero. The bits that are greater than or equal to WaveGetLaneCount will be zero.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "bool".into(),
                    label: "expr".into(),
                    description: "The boolean expression to evaluate.".into(),
                    range:None,
                    modifier: None,
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel6),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "WaveGetLaneCount".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the number of lanes in a wave on this architecture.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/WaveGetLaneCount".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "uint".into(),
                description: "The result will be between 4 and 128, and includes all waves: active, inactive, and/or helper lanes. The result returned from this function may vary significantly depending on the driver implementation.".into(),
                parameters: vec![],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel6),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "WaveGetLaneIndex".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the index of the current lane within the current wave.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/WaveGetLaneIndex".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "uint".into(),
                description: "The current lane index. The result will be between 0 and the result returned from WaveGetLaneCount.".into(),
                parameters: vec![],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel6),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "WaveIsFirstLane".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns true only for the active lane in the current wave with the smallest index.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/WaveIsFirstLane".into()),
            )),
            data: ShaderSymbolData::Functions {
                signatures: vec![ShaderSignature {
                    returnType: "bool".into(),
                    description:
                        "True only for the active lane in the current wave with the smallest index."
                            .into(),
                    parameters: vec![],
                }],
            },
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel6),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "WavePrefixCountBits".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the sum of all the specified boolean variables set to true across all active lanes with indices smaller than the current lane..".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/WavePrefixCountBits".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "uint".into(),
                description: "The sum of all the specified Boolean variables set to true across all active lanes with indices smaller than the current lane.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "bool".into(),
                    label: "bBit".into(),
                    description: "The specified boolean variables.".into(),
                    range:None,
                    modifier: None,
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel6),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "WavePrefixProduct".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the product of all of the values in the active lanes in this wave with indices less than this lane.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/WavePrefixProduct".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "T".into(),
                description: "The product of all the values.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "value".into(),
                    description: "The value to multiply.".into(),
                    range:None,
                    modifier: None,
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel6),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "WavePrefixSum".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the sum of all of the values in the active lanes with smaller indices than this one.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/WavePrefixSum".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "T".into(),
                description: "The sum of the values.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "value".into(),
                    description: "The value to sum up.".into(),
                    range:None,
                    modifier: None,
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel6),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "WaveReadLaneFirst".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the value of the expression for the active lane of the current wave with the smallest index.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/WaveReadLaneFirst".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "T".into(),
                description: "The resulting value is uniform across the wave.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "expr".into(),
                    description: "The expression to evaluate.".into(),
                    range:None,
                    modifier: None,
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel6),
                ..Default::default()
            })),
        });
        symbols.functions.push(ShaderSymbol {
            label: "WaveReadLaneAt".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Returns the value of the expression for the given lane index within the specified wave.".into(),
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/WaveReadLaneAt".into()),
            )),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "T".into(),
                description: "The resulting value is the result of expr. It will be uniform if laneIndex is uniform.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "expr".into(),
                    description: "The expression to evaluate.".into(),
                    range:None,
                    modifier: None,
                },
                ShaderParameter {
                    count: None,
                    ty: "uint".into(),
                    label: "laneIndex".into(),
                    description: "The index of the lane for which the expr result will be returned.".into(),
                    range:None,
                    modifier: None,
                }],
            }]},
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                min_shader_model: Some(HlslShaderModel::ShaderModel6),
                ..Default::default()
            })),
        });
    }
}
