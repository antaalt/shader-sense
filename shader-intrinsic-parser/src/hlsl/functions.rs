use shader_sense::{
    shader::ShaderStage,
    symbols::symbols::{
        ShaderBuiltinSymbol, ShaderParameter, ShaderSignature, ShaderSymbol, ShaderSymbolData,
    },
};

use super::{type_size_iter, HlslIntrinsicParser};

impl HlslIntrinsicParser {
    pub fn add_functions(&self, symbols: &mut ShaderBuiltinSymbol) {
        symbols.push(ShaderSymbol {
            label: "abort".into(),
            description: "Submits an error message to the information queue and terminates the current draw or dispatch call being executed.".into(),
            version: "sm4".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/abort".into()),
            data: ShaderSymbolData::Functions { signatures:vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![],
            }]},
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "abs".into(),
            description: "Returns the absolute value of the specified value.".into(),
            version: "sm1".into(),
            stages: vec![],
            link: Some(
                "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-abs"
                    .into(),
            ),
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
                        }],
                    })
                    .collect(),
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "acos".into(),
            description: "Returns the arccosine of the specified value.".into(),
            version: "sm1".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-acos".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value. Each component should be a floating-point value within the range of -1 to 1.".into(),
                }],
            }).collect()},
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "all".into(),
            description: "Determines if all components of the specified value are non-zero.".into(),
            version: "sm1".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-all".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float", "int", "bool"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: "bool".into(),
                description: "True if all components of the x parameter are non-zero; otherwise, false.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                }],
            }).collect()},
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "any".into(),
            description: "Determines if any components of the specified value are non-zero.".into(),
            version: "sm1".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-all".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float", "int", "bool"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: "bool".into(),
                description: "True if any components of the x parameter are non-zero; otherwise, false.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                }],
            }).collect()},
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "AllMemoryBarrier".into(),
            description: "Blocks execution of all threads in a group until all memory accesses have been completed.".into(),
            version: "sm5".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/allmemorybarrier".into()),
            data: ShaderSymbolData::Functions { signatures:vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![],
            }]},
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "AllMemoryBarrierWithGroupSync".into(),
            description: "Blocks execution of all threads in a group until all memory accesses have been completed and all threads in the group have reached this call.".into(),
            version: "sm5".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/allmemorybarrierwithgroupsync".into()),
            data: ShaderSymbolData::Functions { signatures:vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![],
            }]},
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "asdouble".into(),
            description: "Reinterprets a cast value (two 32-bit values) into a double.".into(),
            version: "sm5".into(),
            stages: vec![],
            link: Some(
                "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/asdouble".into(),
            ),
            data: ShaderSymbolData::Functions {
                signatures: vec![ShaderSignature {
                    returnType: "double".into(),
                    description: "The input (two 32-bit values) recast as a double.".into(),
                    parameters: vec![
                        ShaderParameter {
                            count: None,
                            ty: "in uint".into(),
                            label: "lowbits".into(),
                            description: "The low 32-bit pattern of the input value.".into(),
                        },
                        ShaderParameter {
                            count: None,
                            ty: "in uint".into(),
                            label: "highbits".into(),
                            description: "The high 32-bit pattern of the input value.".into(),
                        },
                    ],
                }],
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "asfloat".into(),
            description: "Interprets the bit pattern of x as a floating-point number.".into(),
            version: "sm4".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-asfloat".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["int", "uint"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format_with_type("float"),
                description: "The input interpreted as a floating-point number.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The input value.".into(),
                }],
            }).collect()},
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "asint".into(),
            description: "Interprets the bit pattern of x as an integer.".into(),
            version: "sm4".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-asint".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float", "uint"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format_with_type("int"),
                description: "The input interpreted as an integer.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The input value.".into(),
                }],
            }).collect()},
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "asuint".into(),
            description: "Interprets the bit pattern of x as an unsigned integer.".into(),
            version: "sm4".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-asuint".into()),
            data: ShaderSymbolData::Functions { signatures:type_size_iter(&["float", "int"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format_with_type("uint"),
                description: "The input interpreted as an unsigned integer.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The input value.".into(),
                }],
            }).collect()},
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "asin".into(),
            description: "Returns the arcsine of the specified value.".into(),
            version: "sm1".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-asin".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The arcsine of the x parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                }],
            }).collect()},
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "atan".into(),
            description: "Returns the arctangent of the specified value.".into(),
            version: "sm1".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-atan".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The arctangent of the x parameter. This value is within the range of -π/2 to π/2.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                }],
            }).collect()},
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "atan2".into(),
            description: "Returns the arctangent of two values (x,y).".into(),
            version: "sm1".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-atan2".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The arctangent of (y,x).".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "y".into(),
                    description: "The y value.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The x value.".into(),
                }],
            }).collect()},
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "ceil".into(),
            description: "Returns the smallest integer value that is greater than or equal to the specified value.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-ceil".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The smallest integer value (returned as a floating-point type) that is greater than or equal to the x parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "CheckAccessFullyMapped".into(),
            description: "Determines whether all values from a Sample, Gather, or Load operation accessed mapped tiles in a tiled resource.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/checkaccessfullymapped".into()),
            data: ShaderSymbolData::Functions { signatures:vec![ShaderSignature {
                returnType: "bool".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "uint".into(),
                    label: "status".into(),
                    description: "The status value that is returned from a Sample, Gather, or Load operation. Because you can't access this status value directly, you need to pass it to CheckAccessFullyMapped.".into(),
                }],
            }]},
            version: "sm5".into(),
            stages: vec![ShaderStage::Fragment, ShaderStage::Compute],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "clamp".into(),
            description: "Clamps the specified value to the specified minimum and maximum range.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-clamp".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "A value to clamp.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "min".into(),
                    description: " The specified minimum range.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "max".into(),
                    description: " The specified maximum range.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "clip".into(),
            description: "Discards the current pixel if the specified value is less than zero.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-clip".into()),
            data: ShaderSymbolData::Functions { signatures:vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "bool".into(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                }],
            }]},
            version: "sm1".into(),
            stages: vec![ShaderStage::Fragment],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "cos".into(),
            description: "Returns the cosine of the specified value.".into(),
            link: Some(
                "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-cos"
                    .into(),
            ),
            data: ShaderSymbolData::Functions {
                signatures: type_size_iter(&["float"], true, true, true)
                    .iter()
                    .map(|v| ShaderSignature {
                        returnType: v.format(),
                        description: "The cosine of the x parameter.".into(),
                        parameters: vec![ShaderParameter {
                            count: None,
                            ty: v.format(),
                            label: "x".into(),
                            description: "The specified value, in radians.".into(),
                        }],
                    })
                    .collect(),
            },
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "cosh".into(),
            description: "Returns the hyperbolic cosine of the specified value.".into(),
            link: Some(
                "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-cos"
                    .into(),
            ),
            data: ShaderSymbolData::Functions {
                signatures: type_size_iter(&["float"], true, true, true)
                    .iter()
                    .map(|v| ShaderSignature {
                        returnType: v.format(),
                        description: "The hyperbolic cosine of the x parameter.".into(),
                        parameters: vec![ShaderParameter {
                            count: None,
                            ty: v.format(),
                            label: "x".into(),
                            description: "The specified value, in radians.".into(),
                        }],
                    })
                    .collect(),
            },
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "countbits".into(),
            description: "Counts the number of bits (per component) set in the input integer."
                .into(),
            link: Some(
                "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/countbits".into(),
            ),
            data: ShaderSymbolData::Functions {
                signatures: type_size_iter(&["uint"], true, true, false)
                    .iter()
                    .map(|v| ShaderSignature {
                        returnType: v.format(),
                        description: "The number of bits.".into(),
                        parameters: vec![ShaderParameter {
                            count: None,
                            ty: v.format(),
                            label: "value".into(),
                            description: "The input value.".into(),
                        }],
                    })
                    .collect(),
            },
            version: "sm5".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "cross".into(),
            description: "Returns the cross product of two floating-point, 3D vectors.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-cross".into()),
            data: ShaderSymbolData::Functions { signatures:vec![ShaderSignature {
                returnType: "float3".into(),
                description: "The cross product of the x parameter and the y parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "float3".into(),
                    label: "x".into(),
                    description: "The first floating-point, 3D vector.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float3".into(),
                    label: "y".into(),
                    description: "The second floating-point, 3D vector.".into(),
                }],
            }]},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "ddx".into(),
            description: "Returns the partial derivative of the specified value with respect to the screen-space x-coordinate.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-ddx".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The partial derivative of the x parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                }],
            }).collect()},
            version: "sm2".into(),
            stages: vec![ShaderStage::Fragment],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "ddx_coarse".into(),
            description: "Computes a low precision partial derivative with respect to the screen-space x-coordinate.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/ddx-coarse".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The low precision partial derivative of value.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                }],
            }).collect()},
            version: "sm5".into(),
            stages: vec![ShaderStage::Fragment],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "ddx_fine".into(),
            description: "Computes a high precision partial derivative with respect to the screen-space x-coordinate.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/ddx-fine".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The high precision partial derivative of value.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                }],
            }).collect()},
            version: "sm5".into(),
            stages: vec![ShaderStage::Fragment],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "ddy".into(),
            description: "Returns the partial derivative of the specified value with respect to the screen-space y-coordinate.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-ddy".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The partial derivative of the x parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                }],
            }).collect()},
            version: "sm2".into(),
            stages: vec![ShaderStage::Fragment],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "ddy_coarse".into(),
            description: "Computes a low precision partial derivative with respect to the screen-space y-coordinate.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/ddx-coarse".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The low precision partial derivative of value.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                }],
            }).collect()},
            version: "sm5".into(),
            stages: vec![ShaderStage::Fragment],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "ddy_fine".into(),
            description: "Computes a high precision partial derivative with respect to the screen-space y-coordinate.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/ddx-fine".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The high precision partial derivative of value.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                }],
            }).collect()},
            version: "sm5".into(),
            stages: vec![ShaderStage::Fragment],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "degrees".into(),
            description: "Converts the specified value from radians to degrees.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-degrees".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The result of converting the x parameter from radians to degrees.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "determinant".into(),
            description: "Returns the determinant of the specified floating-point, square matrix.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-determinant".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], false, false, true).iter().map(|v| ShaderSignature {
                returnType: v.format_as_scalar(),
                description: "The floating-point, scalar value that represents the determinant of the m parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "DeviceMemoryBarrier".into(),
            description: "Blocks execution of all threads in a group until all device memory accesses have been completed.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/devicememorybarrier".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![],
            }]},
            version: "sm5".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "DeviceMemoryBarrierWithGroupSync".into(),
            description: "Blocks execution of all threads in a group until all device memory accesses have been completed and all threads in the group have reached this call.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/devicememorybarrierwithgroupsync".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![],
            }]},
            version: "sm5".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "distance".into(),
            description: "Returns a distance scalar between two vectors.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-determinant".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], false, true, false).iter().map(|v| ShaderSignature {
                returnType: v.format_as_scalar(),
                description: "A floating-point, scalar value that represents the distance between the x parameter and the y parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The first floating-point vector to compare.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "y".into(),
                    description: "The second floating-point vector to compare.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "dot".into(),
            description: "Returns the dot product of two vectors.".into(),
            link: Some(
                "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-dot"
                    .into(),
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
                            },
                            ShaderParameter {
                                count: None,
                                ty: v.format(),
                                label: "y".into(),
                                description: "The second vector.".into(),
                            },
                        ],
                    })
                    .collect(),
            },
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "dst".into(),
            description: "Calculates a distance vector.".into(),
            link: Some(
                "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-dst"
                    .into(),
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
                            },
                            ShaderParameter {
                                count: None,
                                ty: v.format(),
                                label: "y".into(),
                                description: "The second vector.".into(),
                            },
                        ],
                    })
                    .collect(),
            },
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "errorf".into(),
            description: "Submits an error message to the information queue.".into(),
            link: Some(
                "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/errorf".into(),
            ),
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
                        },
                        ShaderParameter {
                            count: None,
                            ty: "T".into(),
                            label: "...".into(),
                            description: "Optional arguments.".into(),
                        },
                    ],
                }],
            },
            version: "sm4".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "EvaluateAttributeCentroid".into(),
            description: "Evaluates at the pixel centroid.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/evaluateattributecentroid".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "attrib".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "attrib".into(),
                    label: "value".into(),
                    description: "The input value.".into(),
                }],
            }]},
            version: "sm5".into(),
            stages: vec![ShaderStage::Fragment],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "EvaluateAttributeAtSample".into(),
            description: "Evaluates at the indexed sample location.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/evaluateattributeatsample".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "attrib".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "attrib".into(),
                    label: "value".into(),
                    description: "The input value.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "uint".into(),
                    label: "sampleindex".into(),
                    description: "The sample location.".into(),
                }],
            }]},
            version: "sm5".into(),
            stages: vec![ShaderStage::Fragment],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "EvaluateAttributeSnapped".into(),
            description: "Evaluates at the pixel centroid with an offset.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/evaluateattributesnapped".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "attrib".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "attrib".into(),
                    label: "value".into(),
                    description: "The input value.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "int2".into(),
                    label: "offset".into(),
                    description: "A 2D offset from the pixel center using a 16x16 grid.".into(),
                }],
            }]},
            version: "sm5".into(),
            stages: vec![ShaderStage::Fragment],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "exp".into(),
            description: "Returns the base-e exponential, or ex, of the specified value.".into(),
            link: Some(
                "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-exp"
                    .into(),
            ),
            data: ShaderSymbolData::Functions {
                signatures: type_size_iter(&["float"], true, true, true)
                    .iter()
                    .map(|v| ShaderSignature {
                        returnType: v.format(),
                        description: "The base-e exponential of the x parameter.".into(),
                        parameters: vec![ShaderParameter {
                            count: None,
                            ty: v.format(),
                            label: "x".into(),
                            description: "The input value.".into(),
                        }],
                    })
                    .collect(),
            },
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "exp2".into(),
            description: "Returns the base 2 exponential, or 2^x, of the specified value.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-exp2".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The base 2 exponential of the x parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The input value.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "f16tof32".into(),
            description: "Converts the float16 stored in the low-half of the uint to a float."
                .into(),
            link: Some(
                "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/f16tof32".into(),
            ),
            data: ShaderSymbolData::Functions {
                signatures: type_size_iter(&["uint"], false, true, false)
                    .iter()
                    .map(|v| ShaderSignature {
                        returnType: v.format_with_type("float"),
                        description: "The converted value.".into(),
                        parameters: vec![ShaderParameter {
                            count: None,
                            ty: v.format(),
                            label: "x".into(),
                            description: "The input value.".into(),
                        }],
                    })
                    .collect(),
            },
            version: "sm4".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "f32tof16".into(),
            description: "Converts an input into a float16 type.".into(),
            link: Some(
                "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/f32tof16".into(),
            ),
            data: ShaderSymbolData::Functions {
                signatures: type_size_iter(&["float"], false, true, false)
                    .iter()
                    .map(|v| ShaderSignature {
                        returnType: v.format_with_type("uint"),
                        description: "The converted value.".into(),
                        parameters: vec![ShaderParameter {
                            count: None,
                            ty: v.format(),
                            label: "x".into(),
                            description: "The input value.".into(),
                        }],
                    })
                    .collect(),
            },
            version: "sm4".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "faceforward".into(),
            description: "Flips the surface-normal (if needed) to face in a direction opposite to i; returns the result in n.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-faceforward".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], false, true, false).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "A floating-point, surface normal vector that is facing the view direction.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "n".into(),
                    description: "The resulting floating-point surface-normal vector.".into(),
                },ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "i".into(),
                    description: "A floating-point, incident vector that points from the view position to the shading position.".into(),
                },ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "ng".into(),
                    description: "A floating-point surface-normal vector.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "firstbithigh".into(),
            description: "Gets the location of the first set bit starting from the highest order bit and working downward, per component.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/firstbithigh".into()),
            data: ShaderSymbolData::Functions { signatures:  type_size_iter(&["int", "uint"], true, true, false).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The location of the first set bit.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "value".into(),
                    description: "The input value.".into(),
                }],
            }).collect()},
            version: "sm5".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "firstbitlow".into(),
            description: "Returns the location of the first set bit starting from the lowest order bit and working upward, per component.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/firstbitlow".into()),
            data: ShaderSymbolData::Functions { signatures:  type_size_iter(&["int", "uint"], true, true, false).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The location of the first set bit.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "value".into(),
                    description: "The input value.".into(),
                }],
            }).collect()},
            version: "sm5".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "floor".into(),
            description: "Returns the largest integer that is less than or equal to the specified value.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-floor".into()),
            data: ShaderSymbolData::Functions { signatures:  type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The largest integer value (returned as a floating-point type) that is less than or equal to the x parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "value".into(),
                    description: "The input value.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "fma".into(),
            description: "Returns the double-precision fused multiply-addition of a * b + c.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-fma".into()),
            data: ShaderSymbolData::Functions { signatures:  type_size_iter(&["double"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The double-precision fused multiply-addition of parameters a * b + c. The returned value must be accurate to 0.5 units of least precision (ULP).".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "a".into(),
                    description: "The first value in the fused multiply-addition.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "b".into(),
                    description: "The second value in the fused multiply-addition.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "c".into(),
                    description: "The third value in the fused multiply-addition.".into(),
                }],
            }).collect()},
            version: "sm5".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "fmod".into(),
            description: "Returns the floating-point remainder of x/y.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-fmod".into()),
            data: ShaderSymbolData::Functions { signatures:  type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The floating-point remainder of the x parameter divided by the y parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: " The floating-point dividend.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "y".into(),
                    description: "The floating-point divisor.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "frac".into(),
            description: "Returns the fractional (or decimal) part of x; which is greater than or equal to 0 and less than 1.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-frac".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The fractional part of the x parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "frexp".into(),
            description: "Returns the mantissa and exponent of the specified floating-point value.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-frac".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The mantissa of the x parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified floating-point value. If the x parameter is 0, this function returns 0 for both the mantissa and the exponent.".into(),
                },
                ShaderParameter {
                    count: None,
                    // TODO: should add qualifier field.
                    ty: format!("out {}", v.format()),
                    label: "exp".into(),
                    description: "The returned exponent of the x parameter.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "fwidth".into(),
            description: "Returns the absolute value of the partial derivatives of the specified value.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-fwidth".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The absolute value of the partial derivatives of the x parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                }],
            }).collect()},
            version: "sm2".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "GetRenderTargetSampleCount".into(),
            description: "Gets the number of samples for a render target.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-getrendertargetsamplecount".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "uint".into(),
                description: "The number of samples.".into(),
                parameters: vec![],
            }]},
            version: "sm4".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "GetRenderTargetSamplePosition".into(),
            description: "Gets the sampling position (x,y) for a given sample index.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-getrendertargetsampleposition".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "float2".into(),
                description: "The (x,y) position of the given sample.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "int".into(),
                    label: "index".into(),
                    description: "".into() 
                }],
            }]},
            version: "sm4".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "GroupMemoryBarrier".into(),
            description: "Blocks execution of all threads in a group until all group shared accesses have been completed.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/groupmemorybarrier".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![],
            }]},
            version: "sm5".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "GroupMemoryBarrierWithGroupSync".into(),
            description: "Blocks execution of all threads in a group until all group shared accesses have been completed and all threads in the group have reached this call.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/groupmemorybarrierwithgroupsync".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![],
            }]},
            version: "sm5".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "InterlockedAdd".into(),
            description: "Performs a guaranteed atomic add of value to the dest resource variable."
                .into(),
            link: Some(
                "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/interlockedadd"
                    .into(),
            ),
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
                        },
                        ShaderParameter {
                            count: None,
                            ty: "T".into(),
                            label: "value".into(),
                            description: "The input value.".into(),
                        },
                        ShaderParameter {
                            count: None,
                            ty: "T".into(),
                            label: "original_value".into(),
                            description: "Optional. The original input value.".into(),
                        },
                    ],
                }],
            },
            version: "sm5".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "InterlockedAnd".into(),
            description: "Performs a guaranteed atomic and.".into(),
            link: Some(
                "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/interlockedand"
                    .into(),
            ),
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
                        },
                        ShaderParameter {
                            count: None,
                            ty: "T".into(),
                            label: "value".into(),
                            description: "The input value.".into(),
                        },
                        ShaderParameter {
                            count: None,
                            ty: "T".into(),
                            label: "original_value".into(),
                            description: "Optional. The original input value.".into(),
                        },
                    ],
                }],
            },
            version: "sm5".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "InterlockedCompareExchange".into(),
            description: "Atomically compares the destination with the comparison value. If they are identical, the destination is overwritten with the input value. The original value is set to the destination's original value.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/interlockedcompareexchange".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "R".into(),
                    label: "dest".into(),
                    description: "The destination address.".into() 
                },
                ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "compare_value".into(),
                    description: "The comparison value.".into() 
                },
                ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "value".into(),
                    description: "The input value.".into() 
                },
                ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "original_value".into(),
                    description: "Optional. The original input value.".into() 
                }],
            }]},
            version: "sm5".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "InterlockedCompareStore".into(),
            description: "Atomically compares the destination to the comparison value. If they are identical, the destination is overwritten with the input value.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/interlockedcomparestore".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "R".into(),
                    label: "dest".into(),
                    description: "The destination address.".into() 
                },
                ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "value".into(),
                    description: "The input value.".into() 
                },
                ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "original_value".into(),
                    description: "Optional. The original input value.".into() 
                }],
            }]},
            version: "sm5".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "InterlockedExchange".into(),
            description: "Assigns value to dest and returns the original value.".into(),
            link: Some(
                "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/interlockedexchange"
                    .into(),
            ),
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
                        },
                        ShaderParameter {
                            count: None,
                            ty: "T".into(),
                            label: "value".into(),
                            description: "The input value.".into(),
                        },
                        ShaderParameter {
                            count: None,
                            ty: "T".into(),
                            label: "original_value".into(),
                            description: "Optional. The original input value.".into(),
                        },
                    ],
                }],
            },
            version: "sm5".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "InterlockedMax".into(),
            description: "Performs a guaranteed atomic max.".into(),
            link: Some(
                "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/interlockedmax"
                    .into(),
            ),
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
                        },
                        ShaderParameter {
                            count: None,
                            ty: "T".into(),
                            label: "value".into(),
                            description: "The input value.".into(),
                        },
                        ShaderParameter {
                            count: None,
                            ty: "T".into(),
                            label: "original_value".into(),
                            description: "Optional. The original input value.".into(),
                        },
                    ],
                }],
            },
            version: "sm5".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "InterlockedMin".into(),
            description: "Performs a guaranteed atomic min.".into(),
            link: Some(
                "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/interlockedmin"
                    .into(),
            ),
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
                        },
                        ShaderParameter {
                            count: None,
                            ty: "T".into(),
                            label: "value".into(),
                            description: "The input value.".into(),
                        },
                        ShaderParameter {
                            count: None,
                            ty: "T".into(),
                            label: "original_value".into(),
                            description: "Optional. The original input value.".into(),
                        },
                    ],
                }],
            },
            version: "sm5".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "InterlockedOr".into(),
            description: "Performs a guaranteed atomic or.".into(),
            link: Some(
                "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/interlockedor".into(),
            ),
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
                        },
                        ShaderParameter {
                            count: None,
                            ty: "T".into(),
                            label: "value".into(),
                            description: "The input value.".into(),
                        },
                        ShaderParameter {
                            count: None,
                            ty: "T".into(),
                            label: "original_value".into(),
                            description: "Optional. The original input value.".into(),
                        },
                    ],
                }],
            },
            version: "sm5".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "InterlockedXor".into(),
            description: "Performs a guaranteed atomic xor.".into(),
            link: Some(
                "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/interlockedxor"
                    .into(),
            ),
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
                        },
                        ShaderParameter {
                            count: None,
                            ty: "T".into(),
                            label: "value".into(),
                            description: "The input value.".into(),
                        },
                        ShaderParameter {
                            count: None,
                            ty: "T".into(),
                            label: "original_value".into(),
                            description: "Optional. The original input value.".into(),
                        },
                    ],
                }],
            },
            version: "sm5".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "isfinite".into(),
            description: "Determines if the specified floating-point value is finite.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-isfinite".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format_with_type("bool"),
                description: "Returns a value of the same size as the input, with a value set to True if the x parameter is finite; otherwise False.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "isinf".into(),
            description: "Determines if the specified value is infinite.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-isinf".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format_with_type("bool"),
                description: "Returns a value of the same size as the input, with a value set to True if the x parameter is +INF or -INF. Otherwise, False.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "isnan".into(),
            description: "Determines if the specified value is NAN or QNAN.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-isnan".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format_with_type("bool"),
                description: "Returns a value of the same size as the input, with a value set to True if the x parameter is NAN or QNAN. Otherwise, False.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "ldexp".into(),
            description: "Returns the result of multiplying the specified value by two, raised to the power of the specified exponent.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-ldexp".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The result of multiplying the x parameter by two, raised to the power of the exp parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "exp".into(),
                    description: "The specified exponent.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "length".into(),
            description: "Returns the length of the specified floating-point vector.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-length".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], false, true, false).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "A floating-point scalar that represents the length of the x parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format_as_scalar(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "lerp".into(),
            description: "Performs a linear interpolation.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-lerp".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The result of the linear interpolation.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The first-floating point value.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The second-floating point value.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "s".into(),
                    description: "A value that linearly interpolates between the x parameter and the y parameter.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "lit".into(),
            description: "Returns a lighting coefficient vector.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-lit".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "float4".into(),
                description: "The lighting coefficient vector.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "n_dot_l".into(),
                    description: "The dot product of the normalized surface normal and the light vector.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "n_dot_h".into(),
                    description: "The dot product of the half-angle vector and the surface normal.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "m".into(),
                    description: "A specular exponent.".into(),
                }],
            }]},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "log".into(),
            description: "Returns the base-e logarithm of the specified value.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-log".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The base-e logarithm of the x parameter. If the x parameter is negative, this function returns indefinite. If the x parameter is 0, this function returns -INF.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "log10".into(),
            description: "Returns the base-10 logarithm of the specified value.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-log10".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The base-10 logarithm of the x parameter. If the x parameter is negative, this function returns indefinite. If the x is 0, this function returns -INF.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "log2".into(),
            description: "Returns the base-2 logarithm of the specified value.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-log2".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The base-2 logarithm of the x parameter. If the x parameter is negative, this function returns indefinite. If the x is 0, this function returns +INF.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "mad".into(),
            description: "Performs an arithmetic multiply/add operation on three values.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/mad".into()),
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
                            },
                            ShaderParameter {
                                count: None,
                                ty: v.format(),
                                label: "a".into(),
                                description: "The first addition value.".into(),
                            },
                            ShaderParameter {
                                count: None,
                                ty: v.format(),
                                label: "b".into(),
                                description: "The second addition value..".into(),
                            },
                        ],
                    })
                    .collect(),
            },
            version: "sm5".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "max".into(),
            description: "Selects the greater of x and y.".into(),
            link: Some(
                "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-max"
                    .into(),
            ),
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
                            },
                            ShaderParameter {
                                count: None,
                                ty: v.format(),
                                label: "y".into(),
                                description: "The y input value.".into(),
                            },
                        ],
                    })
                    .collect(),
            },
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "min".into(),
            description: "Selects the lesser  of x and y.".into(),
            link: Some(
                "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-min"
                    .into(),
            ),
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
                            },
                            ShaderParameter {
                                count: None,
                                ty: v.format(),
                                label: "y".into(),
                                description: "The y input value.".into(),
                            },
                        ],
                    })
                    .collect(),
            },
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "modf".into(),
            description: "Splits the value x into fractional and integer parts, each of which has the same sign as x.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-modf".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float", "int"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The signed-fractional portion of x.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The x input value.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "ip".into(),
                    description: "The integer portion of x.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "msad4".into(),
            description: "Compares a 4-byte reference value and an 8-byte source value and accumulates a vector of 4 sums. Each sum corresponds to the masked sum of absolute differences of a different byte alignment between the reference value and the source value.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-msad4".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "uint4".into(),
                description: "A vector of 4 sums. Each sum corresponds to the masked sum of absolute differences of different byte alignments between the reference value and the source value. msad4 doesn't include a difference in the sum if that difference is masked (that is, the reference byte is 0).".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "uint".into(),
                    label: "reference".into(),
                    description: "The reference array of 4 bytes in one uint value.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "uint2".into(),
                    label: "source".into(),
                    description: "The source array of 8 bytes in two uint2 values.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "uint4".into(),
                    label: "accum".into(),
                    description: "A vector of 4 values. msad4 adds this vector to the masked sum of absolute differences of the different byte alignments between the reference value and the source value.".into(),
                }],
            }]},
            version: "sm5".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "mul".into(),
            description: "Multiplies x and y using matrix math. The inner dimension x-columns and y-rows must be equal.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-mul".into()),
            // TODO: handle all overrides vec * scalar & co...
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float", "int"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The result of x times y. The result has the dimension x-rows x y-columns.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The x input value. If x is a vector, it treated as a row vector.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "y".into(),
                    description: " The y input value. If y is a vector, it treated as a column vector.".into(),
                }],
            }).collect()},
            version: "sm5".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "noise".into(),
            description: "Generates a random value using the Perlin-noise algorithm.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-noise".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], false, true, false).iter().map(|v| ShaderSignature {
                returnType: v.format_as_scalar(),
                description: "The Perlin noise value within a range between -1 and 1.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "A floating-point vector from which to generate Perlin noise.".into(),
                }],
            }).collect()},
            version: "sm5".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "normalize".into(),
            description: "Normalizes the specified floating-point vector according to x / length(x).".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-normalize".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], false, true, false).iter().map(|v| ShaderSignature {
                returnType: v.format_as_scalar(),
                description: "The normalized x parameter. If the length of the x parameter is 0, the result is indefinite.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified floating-point vector.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "pow".into(),
            description: "Returns the specified value raised to the specified power.".into(),
            link: Some(
                "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-pow"
                    .into(),
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
                            },
                            ShaderParameter {
                                count: None,
                                ty: v.format(),
                                label: "y".into(),
                                description: "The specified power.".into(),
                            },
                        ],
                    })
                    .collect(),
            },
            version: "sm5".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "printf".into(),
            description: "Submits an custom shader message to the information queue.".into(),
            link: Some(
                "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/printf".into(),
            ),
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
                        },
                        ShaderParameter {
                            count: None,
                            ty: "T".into(),
                            label: "...".into(),
                            description: "Optional arguments.".into(),
                        },
                    ],
                }],
            },
            version: "sm4".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "Process2DQuadTessFactorsAvg".into(),
            description: "Generates the corrected tessellation factors for a quad patch.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/process2dquadtessfactorsavg".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "float4".into(),
                    label: "RawEdgeFactors".into(),
                    description: "The edge tessellation factors, passed into the tessellator stage.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float2".into(),
                    label: "InsideScale".into(),
                    description: "The scale factor applied to the UV tessellation factors computed by the tessellation stage. The allowable range for InsideScale is 0.0 to 1.0.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float4".into(),
                    label: "RoundedEdgeTessFactors".into(),
                    description: "The rounded edge-tessellation factors calculated by the tessellator stage.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float2".into(),
                    label: "RoundedInsideTessFactors".into(),
                    description: "The rounded tessellation factors calculated by the tessellator stage for inside edges.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float2".into(),
                    label: "UnroundedInsideTessFactors".into(),
                    description: "The tessellation factors calculated by the tessellator stage for inside edges.".into(),
                }],
            }]},
            version: "sm5".into(),
            stages: vec![ShaderStage::TesselationControl],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "Process2DQuadTessFactorsMax".into(),
            description: "Generates the corrected tessellation factors for a quad patch.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/process2dquadtessfactorsmax".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "float4".into(),
                    label: "RawEdgeFactors".into(),
                    description: "The edge tessellation factors, passed into the tessellator stage.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float2".into(),
                    label: "InsideScale".into(),
                    description: "The scale factor applied to the UV tessellation factors computed by the tessellation stage. The allowable range for InsideScale is 0.0 to 1.0.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float4".into(),
                    label: "RoundedEdgeTessFactors".into(),
                    description: "The rounded edge-tessellation factors calculated by the tessellator stage.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float2".into(),
                    label: "RoundedInsideTessFactors".into(),
                    description: "The rounded tessellation factors calculated by the tessellator stage for inside edges.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float2".into(),
                    label: "UnroundedInsideTessFactors".into(),
                    description: "The tessellation factors calculated by the tessellator stage for inside edges.".into(),
                }],
            }]},
            version: "sm5".into(),
            stages: vec![ShaderStage::TesselationControl],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "Process2DQuadTessFactorsMin".into(),
            description: "Generates the corrected tessellation factors for a quad patch.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/process2dquadtessfactorsmin".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "float4".into(),
                    label: "RawEdgeFactors".into(),
                    description: "The edge tessellation factors, passed into the tessellator stage.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float2".into(),
                    label: "InsideScale".into(),
                    description: "The scale factor applied to the UV tessellation factors computed by the tessellation stage. The allowable range for InsideScale is 0.0 to 1.0.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float4".into(),
                    label: "RoundedEdgeTessFactors".into(),
                    description: "The rounded edge-tessellation factors calculated by the tessellator stage.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float2".into(),
                    label: "RoundedInsideTessFactors".into(),
                    description: "The rounded tessellation factors calculated by the tessellator stage for inside edges.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float2".into(),
                    label: "UnroundedInsideTessFactors".into(),
                    description: "The tessellation factors calculated by the tessellator stage for inside edges.".into(),
                }],
            }]},
            version: "sm5".into(),
            stages: vec![ShaderStage::TesselationControl],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "ProcessIsolineTessFactors".into(),
            description: "Generates the rounded tessellation factors for an isoline.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/processisolinetessfactors".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "RawDetailFactor".into(),
                    description: "The desired detail factor.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "RawDensityFactor".into(),
                    description: "The desired density factor.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "RoundedDetailFactor".into(),
                    description: "The rounded detail factor clamped to a range that can be used by the tessellator.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "RoundedDensityFactor".into(),
                    description: "The rounded density factor clamped to a rangethat can be used by the tessellator.".into(),
                }],
            }]},
            version: "sm5".into(),
            stages: vec![ShaderStage::TesselationControl],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "ProcessQuadTessFactorsAvg".into(),
            description: "Generates the corrected tessellation factors for a quad patch.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/processquadtessfactorsavg".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "float4".into(),
                    label: "RawEdgeFactors".into(),
                    description: "The edge tessellation factors, passed into the tessellator stage.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "InsideScale".into(),
                    description: "The scale factor applied to the UV tessellation factors computed by the tessellation stage. The allowable range for InsideScale is 0.0 to 1.0.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float4".into(),
                    label: "RoundedEdgeTessFactors".into(),
                    description: "The rounded edge-tessellation factors calculated by the tessellator stage.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float2".into(),
                    label: "RoundedInsideTessFactors".into(),
                    description: "The rounded tessellation factors calculated by the tessellator stage for inside edges.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float2".into(),
                    label: "UnroundedInsideTessFactors".into(),
                    description: "The tessellation factors calculated by the tessellator stage for inside edges.".into(),
                }],
            }]},
            version: "sm5".into(),
            stages: vec![ShaderStage::TesselationControl],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "ProcessQuadTessFactorsMax".into(),
            description: "Generates the corrected tessellation factors for a quad patch.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/processquadtessfactorsmax".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "float4".into(),
                    label: "RawEdgeFactors".into(),
                    description: "The edge tessellation factors, passed into the tessellator stage.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "InsideScale".into(),
                    description: "The scale factor applied to the UV tessellation factors computed by the tessellation stage. The allowable range for InsideScale is 0.0 to 1.0.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float4".into(),
                    label: "RoundedEdgeTessFactors".into(),
                    description: "The rounded edge-tessellation factors calculated by the tessellator stage.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float2".into(),
                    label: "RoundedInsideTessFactors".into(),
                    description: "The rounded tessellation factors calculated by the tessellator stage for inside edges.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float2".into(),
                    label: "UnroundedInsideTessFactors".into(),
                    description: "The tessellation factors calculated by the tessellator stage for inside edges.".into(),
                }],
            }]},
            version: "sm5".into(),
            stages: vec![ShaderStage::TesselationControl],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "ProcessQuadTessFactorsMin".into(),
            description: "Generates the corrected tessellation factors for a quad patch.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/processquadtessfactorsmin".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "float4".into(),
                    label: "RawEdgeFactors".into(),
                    description: "The edge tessellation factors, passed into the tessellator stage.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "InsideScale".into(),
                    description: "The scale factor applied to the UV tessellation factors computed by the tessellation stage. The allowable range for InsideScale is 0.0 to 1.0.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float4".into(),
                    label: "RoundedEdgeTessFactors".into(),
                    description: "The rounded edge-tessellation factors calculated by the tessellator stage.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float2".into(),
                    label: "RoundedInsideTessFactors".into(),
                    description: "The rounded tessellation factors calculated by the tessellator stage for inside edges.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float2".into(),
                    label: "UnroundedInsideTessFactors".into(),
                    description: "The tessellation factors calculated by the tessellator stage for inside edges.".into(),
                }],
            }]},
            version: "sm5".into(),
            stages: vec![ShaderStage::TesselationControl],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "ProcessTriTessFactorsAvg".into(),
            description: "Generates the corrected tessellation factors for a tri patch.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/processtritessfactorsavg".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "float4".into(),
                    label: "RawEdgeFactors".into(),
                    description: "The edge tessellation factors, passed into the tessellator stage.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "InsideScale".into(),
                    description: "The scale factor applied to the UV tessellation factors computed by the tessellation stage. The allowable range for InsideScale is 0.0 to 1.0.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float3".into(),
                    label: "RoundedEdgeTessFactors".into(),
                    description: "The rounded edge-tessellation factors calculated by the tessellator stage.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "RoundedInsideTessFactors".into(),
                    description: "The rounded tessellation factors calculated by the tessellator stage for inside edges.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "UnroundedInsideTessFactors".into(),
                    description: "The tessellation factors calculated by the tessellator stage for inside edges.".into(),
                }],
            }]},
            version: "sm5".into(),
            stages: vec![ShaderStage::TesselationControl],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "ProcessTriTessFactorsMax".into(),
            description: "Generates the corrected tessellation factors for a tri patch.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/processtritessfactorsmax".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "float4".into(),
                    label: "RawEdgeFactors".into(),
                    description: "The edge tessellation factors, passed into the tessellator stage.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "InsideScale".into(),
                    description: "The scale factor applied to the UV tessellation factors computed by the tessellation stage. The allowable range for InsideScale is 0.0 to 1.0.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float3".into(),
                    label: "RoundedEdgeTessFactors".into(),
                    description: "The rounded edge-tessellation factors calculated by the tessellator stage.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "RoundedInsideTessFactors".into(),
                    description: "The rounded tessellation factors calculated by the tessellator stage for inside edges.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "UnroundedInsideTessFactors".into(),
                    description: "The tessellation factors calculated by the tessellator stage for inside edges.".into(),
                }],
            }]},
            version: "sm5".into(),
            stages: vec![ShaderStage::TesselationControl],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "ProcessTriTessFactorsMin".into(),
            description: "Generates the corrected tessellation factors for a tri patch.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/processtritessfactorsmin".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "void".into(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "float4".into(),
                    label: "RawEdgeFactors".into(),
                    description: "The edge tessellation factors, passed into the tessellator stage.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "InsideScale".into(),
                    description: "The scale factor applied to the UV tessellation factors computed by the tessellation stage. The allowable range for InsideScale is 0.0 to 1.0.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float3".into(),
                    label: "RoundedEdgeTessFactors".into(),
                    description: "The rounded edge-tessellation factors calculated by the tessellator stage.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "RoundedInsideTessFactors".into(),
                    description: "The rounded tessellation factors calculated by the tessellator stage for inside edges.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "float".into(),
                    label: "UnroundedInsideTessFactors".into(),
                    description: "The tessellation factors calculated by the tessellator stage for inside edges.".into(),
                }],
            }]},
            version: "sm5".into(),
            stages: vec![ShaderStage::TesselationControl],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "radians".into(),
            description: "Converts the specified value from degrees to radians.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-radians".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The x parameter converted from degrees to radians.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "rcp".into(),
            description: "Calculates a fast, approximate, per-component reciprocal.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/rcp".into()),
            data: ShaderSymbolData::Functions {
                signatures: type_size_iter(&["float", "double"], true, true, true)
                    .iter()
                    .map(|v| ShaderSignature {
                        returnType: v.format(),
                        description: "The reciprocal of the x parameter.".into(),
                        parameters: vec![ShaderParameter {
                            count: None,
                            ty: v.format(),
                            label: "x".into(),
                            description: "The specified value.".into(),
                        }],
                    })
                    .collect(),
            },
            version: "sm5".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "reflect".into(),
            description: "Returns a reflection vector using an incident ray and a surface normal.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-reflect".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], false, true, false).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "A floating-point, reflection vector.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "i".into(),
                    description: "A floating-point, incident vector.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "n".into(),
                    description: "A floating-point, normal vector.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "refract".into(),
            description: "Returns a refraction vector using an entering ray, a surface normal, and a refraction index.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-refract".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], false, true, false).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "A floating-point, refraction vector. If the angle between the entering ray i and the surface normal n is too great for a given refraction index ?, the return value is (0,0,0).".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "i".into(),
                    description: "A floating-point, ray direction vector.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "n".into(),
                    description: "A floating-point, surface normal vector.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: v.format_as_scalar(),
                    label: "f".into(),
                    description: "A floating-point, refraction index scalar.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "reversebits".into(),
            description: "Reverses the order of the bits, per component.".into(),
            link: Some(
                "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/reversebits".into(),
            ),
            data: ShaderSymbolData::Functions {
                signatures: type_size_iter(&["uint"], true, true, false)
                    .iter()
                    .map(|v| ShaderSignature {
                        returnType: v.format(),
                        description: "The input value, with the bit order reversed.".into(),
                        parameters: vec![ShaderParameter {
                            count: None,
                            ty: v.format(),
                            label: "value".into(),
                            description: "The input value.".into(),
                        }],
                    })
                    .collect(),
            },
            version: "sm5".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "round".into(),
            description: "Rounds the specified value to the nearest integer. Halfway cases are rounded to the nearest even.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-round".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The x parameter, rounded to the nearest integer within a floating-point type.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "rsqrt".into(),
            description: "Returns the reciprocal of the square root of the specified value.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-round".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The reciprocal of the square root of the x parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "saturate".into(),
            description: "Clamps the specified value within the range of 0 to 1.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-saturate".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The x parameter, clamped within the range of 0 to 1.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "sign".into(),
            description: "Returns the sign of x.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-saturate".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float", "int"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "Returns -1 if x is less than zero; 0 if x equals zero; and 1 if x is greater than zero.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The input value.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "sin".into(),
            description: "Returns the sine of the specified value.".into(),
            link: Some(
                "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-sin"
                    .into(),
            ),
            data: ShaderSymbolData::Functions {
                signatures: type_size_iter(&["float"], true, true, true)
                    .iter()
                    .map(|v| ShaderSignature {
                        returnType: v.format(),
                        description: "The sine of the x parameter.".into(),
                        parameters: vec![ShaderParameter {
                            count: None,
                            ty: v.format(),
                            label: "x".into(),
                            description: "The input value.".into(),
                        }],
                    })
                    .collect(),
            },
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "sincos".into(),
            description: "Returns the sine and cosine of x.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-sincos".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value, in radians.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: format!("out {}", v.format()),
                    label: "s".into(),
                    description: "Returns the sine of x.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: format!("out {}", v.format()),
                    label: "c".into(),
                    description: "Returns the cosine of x.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "sinh".into(),
            description: "Returns the hyperbolic sine of the specified value.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-sinh".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The hyperbolic sine of the x parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value, in radians.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "smoothstep".into(),
            description: "Returns a smooth Hermite interpolation between 0 and 1, if x is in the range [min, max].".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-smoothstep".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "Returns 0 if x is less than min; 1 if x is greater than max; otherwise, a value between 0 and 1 if x is in the range [min, max].".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "min".into(),
                    description: "The minimum range of the x parameter.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "max".into(),
                    description: "The maximum range of the x parameter.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value to be interpolated.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "sqrt".into(),
            description: "Returns the square root of the specified floating-point value, per component.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-sqrt".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The square root of the x parameter, per component.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified floating-point value.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "step".into(),
            description: "Compares two values, returning 0 or 1 based on which value is greater.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-sqrt".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "1 if the x parameter is greater than or equal to the y parameter; otherwise, 0.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "y".into(),
                    description: "The first floating-point value to compare.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The second floating-point value to compare.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "tan".into(),
            description: "Returns the tangent of the specified value.".into(),
            link: Some(
                "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-tan"
                    .into(),
            ),
            data: ShaderSymbolData::Functions {
                signatures: type_size_iter(&["float"], true, true, true)
                    .iter()
                    .map(|v| ShaderSignature {
                        returnType: v.format(),
                        description: "The tangent of the x parameter.".into(),
                        parameters: vec![ShaderParameter {
                            count: None,
                            ty: v.format(),
                            label: "x".into(),
                            description: "The specified value, in radians.".into(),
                        }],
                    })
                    .collect(),
            },
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "tanh".into(),
            description: "Returns the hyperbolic tangent of the specified value.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-tanh".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The hyperbolic tangent of the x parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified value, in radians.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        for dim in 1..=4 {
            let dim_text = if dim == 4 {
                "CUBE".into()
            } else {
                format!("{}D", dim)
            };
            let dim_text_lower = dim_text.to_lowercase();
            symbols.push(ShaderSymbol {
                label: format!("tex{}", dim_text),
                description: format!("Samples a {} texture.", dim_text),
                link: Some(format!("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-tex{}", dim_text_lower)),
                data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                    returnType: "float4".into(),
                    description: "The value of the texture data.".into(),
                    parameters: vec![ShaderParameter {
                    count: None,
                        ty: format!("sampler{}", dim_text),
                        label: "s".into(),
                        description: "The sampler state.".into(),
                    },
                    ShaderParameter {
                    count: None,
                        ty: format!("float{}", dim),
                        label: "t".into(),
                        description: "The texture coordinate.".into(),
                    }],
                }]},
                version: "sm1".into(),
                stages: vec![ShaderStage::Fragment],
                content: None,
                range: None,
            });
            symbols.push(ShaderSymbol {
                label: format!("tex{}", dim_text),
                description: format!("Samples a {} texture using a gradient to select the mip level.", dim_text),
                link: Some(format!("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-tex{}-s-t-ddx-ddy", dim_text_lower)),
                data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                    returnType: "float4".into(),
                    description: "The value of the texture data.".into(),
                    parameters: vec![ShaderParameter {
                    count: None,
                        ty: format!("sampler{}", dim_text),
                        label: "s".into(),
                        description: "The sampler state.".into(),
                    },
                    ShaderParameter {
                    count: None,
                        ty: format!("float{}", dim),
                        label: "t".into(),
                        description: "The texture coordinate.".into(),
                    },
                    ShaderParameter {
                    count: None,
                        ty: format!("float{}", dim),
                        label: "ddx".into(),
                        description: "Rate of change of the surface geometry in the x direction.".into(),
                    },
                    ShaderParameter {
                    count: None,
                        ty: format!("float{}", dim),
                        label: "ddy".into(),
                        description: "Rate of change of the surface geometry in the y direction.".into(),
                    }],
                }]},
                version: "sm2".into(),
                stages: vec![ShaderStage::Fragment],
                content: None,
                range: None,
            });
            symbols.push(ShaderSymbol {
                label: format!("tex{}bias", dim_text),
                description: format!("Samples a {} texture after biasing the mip level by t.w.", dim_text),
                link: Some(format!("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-tex{}bias", dim_text_lower)),
                data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                    returnType: "float4".into(),
                    description: "The value of the texture data.".into(),
                    parameters: vec![ShaderParameter {
                    count: None,
                        ty: format!("sampler{}", dim_text),
                        label: "s".into(),
                        description: "The sampler state.".into(),
                    },
                    ShaderParameter {
                    count: None,
                        ty: format!("float{}", dim),
                        label: "t".into(),
                        description: "The texture coordinate.".into(),
                    }],
                }]},
                version: "sm2".into(),
                stages: vec![ShaderStage::Fragment],
                content: None,
                range: None,
            });
            symbols.push(ShaderSymbol {
                label: format!("tex{}grad", dim_text),
                description: format!("Samples a {} texture using a gradient to select the mip level.", dim_text),
                link: Some(format!("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-tex{}grad", dim_text_lower)),
                data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                    returnType: "float4".into(),
                    description: "The value of the texture data.".into(),
                    parameters: vec![ShaderParameter {
                    count: None,
                        ty: format!("sampler{}", dim_text),
                        label: "s".into(),
                        description: "The sampler state.".into(),
                    },
                    ShaderParameter {
                    count: None,
                        ty: format!("float{}", dim),
                        label: "t".into(),
                        description: "The texture coordinate.".into(),
                    },
                    ShaderParameter {
                    count: None,
                        ty: format!("float{}", dim),
                        label: "ddx".into(),
                        description: "Rate of change of the surface geometry in the x direction.".into(),
                    },
                    ShaderParameter {
                    count: None,
                        ty: format!("float{}", dim),
                        label: "ddy".into(),
                        description: "Rate of change of the surface geometry in the y direction.".into(),
                    }],
                }]},
                version: "sm2".into(),
                stages: vec![ShaderStage::Fragment],
                content: None,
                range: None,
            });
            symbols.push(ShaderSymbol {
                label: format!("tex{}lod", dim_text),
                description: format!("Samples a {} texture with mipmaps. The mipmap LOD is specified in t.w.", dim_text),
                link: Some(format!("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-tex{}lod", dim_text_lower)),
                data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                    returnType: "float4".into(),
                    description: "The value of the texture data.".into(),
                    parameters: vec![ShaderParameter {
                    count: None,
                        ty: format!("sampler{}", dim_text),
                        label: "s".into(),
                        description: "The sampler state.".into(),
                    },
                    ShaderParameter {
                    count: None,
                        ty: "float4".into(),
                        label: "t".into(),
                        description: "The texture coordinate.".into(),
                    }],
                }]},
                version: "sm3".into(),
                stages: vec![ShaderStage::Fragment],
                content: None,
                range: None,
            });
            symbols.push(ShaderSymbol {
                label: format!("tex{}proj", dim_text),
                description: format!("Samples a {} texture using a projective divide; the texture coordinate is divided by t.w before the lookup takes place.", dim_text),
                link: Some(format!("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-tex{}proj", dim_text_lower)),
                data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                    returnType: "float4".into(),
                    description: "The value of the texture data.".into(),
                    parameters: vec![ShaderParameter {
                    count: None,
                        ty: format!("sampler{}", dim_text),
                        label: "s".into(),
                        description: "The sampler state.".into(),
                    },
                    ShaderParameter {
                    count: None,
                        ty: "float4".into(),
                        label: "t".into(),
                        description: "The texture coordinate.".into(),
                    }],
                }]},
                version: "sm2".into(),
                stages: vec![ShaderStage::Fragment],
                content: None,
                range: None,
            });
        }
        symbols.push(ShaderSymbol {
            label: "transpose".into(),
            description: "Transposes the specified input matrix.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-transpose".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float", "int", "bool"], false, false, true).iter().map(|v| ShaderSignature {
                returnType: "float".into(),
                description: "The transposed value of the x parameter.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "s".into(),
                    description: "The specified matrix.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "trunc".into(),
            description: "Truncates a floating-point value to the integer component.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-trunc".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["float"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: "float".into(),
                description: "The input value truncated to an integer component.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "x".into(),
                    description: "The specified input.".into(),
                }],
            }).collect()},
            version: "sm1".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        // sm 6.0
        symbols.push(ShaderSymbol {
            label: "QuadReadAcrossDiagonal".into(),
            description: "Returns the specified local value which is read from the diagonally opposite lane in this quad.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/QuadReadAcrossDiagonal".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "T".into(),
                description: "The specified local value which is read from the diagonally opposite lane in this quad.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "localValue".into(),
                    description: "The requested type.".into(),
                }],
            }]},
            version: "sm6".into(),
            stages: vec![ShaderStage::Fragment, ShaderStage::Compute],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "QuadReadLaneAt".into(),
            description: "Returns the specified source value from the lane identified by the lane ID within the current quad.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/QuadReadLaneAt".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "T".into(),
                description: "The specified source value. The result of this function is uniform across the quad. If the source lane is inactive, the results are undefined.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "localValue".into(),
                    description: "The requested type.".into(),
                }, ShaderParameter {
                    count: None,
                    ty: "uint".into(),
                    label: "quadLaneID".into(),
                    description: "The lane ID; this will be a value from 0 to 3.".into(),
                }],
            }]},
            version: "sm6".into(),
            stages: vec![ShaderStage::Fragment, ShaderStage::Compute],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "QuadReadAcrossX".into(),
            description: "Returns the specified local value read from the other lane in this quad in the X direction.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/QuadReadAcrossX".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "T".into(),
                description: "The specified local value. If the source lane is inactive, the results are undefined.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "localValue".into(),
                    description: "The requested type.".into(),
                }],
            }]},
            version: "sm6".into(),
            stages: vec![ShaderStage::Fragment, ShaderStage::Compute],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "QuadReadAcrossY".into(),
            description: "Returns the specified local value read from the other lane in this quad in the Y direction.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/QuadReadAcrossY".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "T".into(),
                description: "The specified local value. If the source lane is inactive, the results are undefined.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "localValue".into(),
                    description: "The requested type.".into(),
                }],
            }]},
            version: "sm6".into(),
            stages: vec![ShaderStage::Fragment, ShaderStage::Compute],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "WaveActiveAllEqual".into(),
            description: "Returns true for each component of expr that is the same for every active lane in the current wave.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/WaveActiveAllEqual".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "booln".into(),
                description: "Returns true for each component of expr that is the same for every active lane in the current wave.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "expr".into(),
                    description: "The expression to evaluate. type can be a basic scalar, vector, or matrix type.".into(),
                }],
            }]},
            version: "sm6".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "WaveActiveBitAnd".into(),
            description: "Returns the bitwise AND of all the values of the expression across all active lanes in the current wave and replicates it back to all active lanes.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/WaveActiveBitAnd".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["int"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The bitwise AND value.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "expr".into(),
                    description: "The expression to evaluate.".into(),
                }],
            }).collect()},
            version: "sm6".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "WaveActiveBitOr".into(),
            description: "Returns the bitwise OR of all the values of <expr> across all active non-helper lanes in the current wave, and replicates it back to all active non-helper lanes.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/WaveActiveBitOr".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["int"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The bitwise OR value.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "expr".into(),
                    description: "The expression to evaluate.".into(),
                }],
            }).collect()},
            version: "sm6".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "WaveActiveBitXor".into(),
            description: "Returns the bitwise XOR of all the values of the expression across all active lanes in the current wave and replicates it back to all active lanes.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/WaveActiveBitXor".into()),
            data: ShaderSymbolData::Functions { signatures: type_size_iter(&["int"], true, true, true).iter().map(|v| ShaderSignature {
                returnType: v.format(),
                description: "The bitwise XOR value.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: v.format(),
                    label: "expr".into(),
                    description: "The expression to evaluate.".into(),
                }],
            }).collect()},
            version: "sm6".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "WaveActiveCountBits".into(),
            description: "Counts the number of boolean variables which evaluate to true across all active lanes in the current wave, and replicates the result to all lanes in the wave.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/WaveActiveCountBits".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "uint".into(),
                description: "The number of lanes for which the boolean variable evaluates to true, across all active lanes in the current wave.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "bool".into(),
                    label: "bBit".into(),
                    description: "The boolean variables to evaluate. Providing an explicit true Boolean value returns the number of active lanes.".into(),
                }],
            }]},
            version: "sm6".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "WaveActiveMax".into(),
            description: "Returns the maximum value of the expression across all active lanes in the current wave and replicates it back to all active lanes.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/WaveActiveMax".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "T".into(),
                description: "The maximum value.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "expr".into(),
                    description: "The expression to evaluate.".into(),
                }],
            }]},
            version: "sm6".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "WaveActiveMin".into(),
            description: "Returns the maximum value of the expression across all active lanes in the current wave and replicates it back to all active lanes.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/WaveActiveMin".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "T".into(),
                description: "The minimum value.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "expr".into(),
                    description: "The expression to evaluate.".into(),
                }],
            }]},
            version: "sm6".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "WaveActiveProduct".into(),
            description: "Multiplies the values of the expression together across all active lanes in the current wave and replicates it back to all active lanes.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/WaveActiveProduct".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "T".into(),
                description: "The product value.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "expr".into(),
                    description: "The expression to evaluate.".into(),
                }],
            }]},
            version: "sm6".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "WaveActiveSum".into(),
            description: "Sums up the value of the expression across all active lanes in the current wave and replicates it to all lanes in the current wave.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/waveallsum".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "T".into(),
                description: "The sum value.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "expr".into(),
                    description: "The expression to evaluate.".into(),
                }],
            }]},
            version: "sm6".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "WaveActiveAllTrue".into(),
            description:
                "Returns true if the expression is true in all active lanes in the current wave."
                    .into(),
            link: Some(
                "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/wavealltrue".into(),
            ),
            data: ShaderSymbolData::Functions {
                signatures: vec![ShaderSignature {
                    returnType: "bool".into(),
                    description: "True if the expression is true in all lanes.".into(),
                    parameters: vec![ShaderParameter {
                        count: None,
                        ty: "bool".into(),
                        label: "expr".into(),
                        description: "The expression to evaluate.".into(),
                    }],
                }],
            },
            version: "sm6".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "WaveActiveAnyTrue".into(),
            description: "Returns true if the expression is true in any of the active lanes in the current wave.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/waveanytrue".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "bool".into(),
                description: "True if the expression is true in any lane.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "bool".into(),
                    label: "expr".into(),
                    description: "The expression to evaluate.".into(),
                }],
            }]},
            version: "sm6".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "WaveActiveBallot".into(),
            description: "Returns a uint4 containing a bitmask of the evaluation of the Boolean expression for all active lanes in the current wave.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/waveballot".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "uint4".into(),
                description: "A uint4 containing a bitmask of the evaluation of the Boolean expression for all active lanes in the current wave. The least-significant bit corresponds to the lane with index zero. The bits corresponding to inactive lanes will be zero. The bits that are greater than or equal to WaveGetLaneCount will be zero.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "bool".into(),
                    label: "expr".into(),
                    description: "The boolean expression to evaluate.".into(),
                }],
            }]},
            version: "sm6".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "WaveGetLaneCount".into(),
            description: "Returns the number of lanes in a wave on this architecture.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/WaveGetLaneCount".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "uint".into(),
                description: "The result will be between 4 and 128, and includes all waves: active, inactive, and/or helper lanes. The result returned from this function may vary significantly depending on the driver implementation.".into(),
                parameters: vec![],
            }]},
            version: "sm6".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "WaveGetLaneIndex".into(),
            description: "Returns the index of the current lane within the current wave.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/WaveGetLaneIndex".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "uint".into(),
                description: "The current lane index. The result will be between 0 and the result returned from WaveGetLaneCount.".into(),
                parameters: vec![],
            }]},
            version: "sm6".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "WaveIsFirstLane".into(),
            description:
                "Returns true only for the active lane in the current wave with the smallest index."
                    .into(),
            link: Some(
                "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/WaveIsFirstLane"
                    .into(),
            ),
            data: ShaderSymbolData::Functions {
                signatures: vec![ShaderSignature {
                    returnType: "bool".into(),
                    description:
                        "True only for the active lane in the current wave with the smallest index."
                            .into(),
                    parameters: vec![],
                }],
            },
            version: "sm6".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "WavePrefixCountBits".into(),
            description: "Returns the sum of all the specified boolean variables set to true across all active lanes with indices smaller than the current lane..".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/WavePrefixCountBits".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "uint".into(),
                description: "The sum of all the specified Boolean variables set to true across all active lanes with indices smaller than the current lane.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "bool".into(),
                    label: "bBit".into(),
                    description: "The specified boolean variables.".into(),
                }],
            }]},
            version: "sm6".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "WavePrefixProduct".into(),
            description: "Returns the product of all of the values in the active lanes in this wave with indices less than this lane.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/WavePrefixProduct".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "T".into(),
                description: "The product of all the values.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "value".into(),
                    description: "The value to multiply.".into(),
                }],
            }]},
            version: "sm6".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "WavePrefixSum".into(),
            description: "Returns the sum of all of the values in the active lanes with smaller indices than this one.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/WavePrefixSum".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "T".into(),
                description: "The sum of the values.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "value".into(),
                    description: "The value to sum up.".into(),
                }],
            }]},
            version: "sm6".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "WaveReadLaneFirst".into(),
            description: "Returns the value of the expression for the active lane of the current wave with the smallest index.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/WaveReadLaneFirst".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "T".into(),
                description: "The resulting value is uniform across the wave.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "expr".into(),
                    description: "The expression to evaluate.".into(),
                }],
            }]},
            version: "sm6".into(),
            stages: vec![],
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "WaveReadLaneAt".into(),
            description: "Returns the value of the expression for the given lane index within the specified wave.".into(),
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/WaveReadLaneAt".into()),
            data: ShaderSymbolData::Functions { signatures: vec![ShaderSignature {
                returnType: "T".into(),
                description: "The resulting value is the result of expr. It will be uniform if laneIndex is uniform.".into(),
                parameters: vec![ShaderParameter {
                    count: None,
                    ty: "T".into(),
                    label: "expr".into(),
                    description: "The expression to evaluate.".into(),
                },
                ShaderParameter {
                    count: None,
                    ty: "uint".into(),
                    label: "laneIndex".into(),
                    description: "The index of the lane for which the expr result will be returned.".into(),
                }],
            }]},
            version: "sm6".into(),
            stages: vec![],
            content: None,
            range: None,
        });
    }
}
