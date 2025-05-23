use shader_sense::{
    shader::ShaderStage,
    symbols::symbols::{
        ShaderBuiltinSymbol, ShaderParameter, ShaderSignature, ShaderSymbol, ShaderSymbolData,
    },
};

use super::HlslIntrinsicParser;

pub fn new_hlsl_scalar(label: &str, description: &str, version: &str) -> ShaderSymbol {
    ShaderSymbol {
        label: label.into(),
        description: description.into(),
        version: version.to_string(),
        stages: vec![],
        link: Some(
            "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-scalar"
                .into(),
        ),
        data: ShaderSymbolData::Types {
            constructors: vec![ShaderSignature {
                returnType: "".into(),
                description: format!("Constructor for type {}", label),
                parameters: vec![ShaderParameter {
                    ty: label.into(),
                    label: "value".into(),
                    count: None,
                    description: "".into(),
                }],
            }],
        },
        range: None,
        content: None,
    }
}

impl HlslIntrinsicParser {
    pub fn add_types(&self, symbols: &mut ShaderBuiltinSymbol) {
        /*fn get_texture_object_methods() -> Vec<ShaderMethod> {
            vec![
                ShaderMethod {
                    label: "GetDimensions".into(),
                    signature: ShaderSignature {
                        returnType: "void".into(),
                        description: "".into(),
                        parameters: vec![
                            ShaderParameter {
                                ty: "uint".into(),
                                label: "dim".into(),
                                description: "The length, in bytes, of the buffer.".into(),
                            }
                        ]
                    }
                },
                ShaderMethod {
                    label: "Load".into(),
                    signature: ShaderSignature {
                        returnType: "void".into(),
                        description: "".into(),
                        parameters: vec![
                            ShaderParameter {
                                ty: "int".into(),
                                label: "Location".into(),
                                description: "The location of the buffer".into(),
                            },
                            ShaderParameter {
                                ty: "uint".into(),
                                label: "Status".into(),
                                description: "The status of the operation. You can't access the status directly; instead, pass the status to the CheckAccessFullyMapped intrinsic function. CheckAccessFullyMapped returns TRUE if all values from the corresponding Sample, Gather, or Load operation accessed mapped tiles in a tiled resource. If any values were taken from an unmapped tile, CheckAccessFullyMapped returns FALSE.".into(),
                            }
                        ]
                    }
                }
            ]
        }
        fn get_buffer_object_methods() -> Vec<ShaderMethod> {
            vec![] // Load
        }*/
        // sm 4.0 : Object<Type, Samples> name
        // https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-to-type
        symbols.push(ShaderSymbol {
            label: "Buffer".into(),
            description: "".into(),
            version: "sm4".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-buffer".into()),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "Texture1D".into(),
            description: "".into(),
            version: "sm4".into(),
            stages: vec![],
            link: Some(
                "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-texture1d"
                    .into(),
            ),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "Texture1DArray".into(),
            description: "".into(),
            version: "sm4".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-texture1darray".into()),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "Texture2D".into(),
            description: "".into(),
            version: "sm4".into(),
            stages: vec![],
            link: Some(
                "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-texture2d"
                    .into(),
            ),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "Texture2DArray".into(),
            description: "".into(),
            version: "sm4".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-texture2darray".into()),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "Texture3D".into(),
            description: "".into(),
            version: "sm4".into(),
            stages: vec![],
            link: Some(
                "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-texture3d"
                    .into(),
            ),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "TextureCube".into(),
            description: "".into(),
            version: "sm4".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-texturecube".into()),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "TextureCubeArray".into(),
            description: "".into(),
            version: "sm4.1".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-texturecubearray".into()),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "Texture2DMS".into(),
            description: "".into(),
            version: "sm4.1".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-texture2dms".into()),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "Texture2DMSArray".into(),
            description: "".into(),
            version: "sm4.1".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-Texture2DMSArray".into()),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });
        // sm 5.0 : Object<Type, Samples> name
        // https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/d3d11-graphics-reference-sm5-objects
        symbols.push(ShaderSymbol {
            label: "AppendStructuredBuffer".into(),
            description: "".into(),
            version: "sm5".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-AppendStructuredBuffer".into()),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![/*ShaderMethod {
                    // GetDimensions
                    // Load
                    // Operator[]
                }*/],
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "ByteAddressBuffer".into(),
            description: "".into(),
            version: "sm5".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-ByteAddressBuffer".into()),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "ByteAddressBuffer".into(),
            description: "".into(),
            version: "sm5".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-ByteAddressBuffer".into()),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "ConsumeStructuredBuffer".into(),
            description: "".into(),
            version: "sm5".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-ConsumeStructuredBuffer".into()),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "InputPatch".into(),
            description: "".into(),
            version: "sm5".into(),
            stages: vec![ShaderStage::TesselationControl],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-InputPatch".into()),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "OutputPatch".into(),
            description: "".into(),
            version: "sm5".into(),
            stages: vec![ShaderStage::TesselationControl],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-OutputPatch".into()),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "RWBuffer".into(),
            description: "".into(),
            version: "sm5".into(),
            stages: vec![],
            link: Some(
                "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-RWBuffer"
                    .into(),
            ),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "RWByteAddressBuffer".into(),
            description: "".into(),
            version: "sm5".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-RWByteAddressBuffer".into()),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "RWStructuredBuffer".into(),
            description: "".into(),
            version: "sm5".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-RWStructuredBuffer".into()),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "RWTexture1D".into(),
            description: "".into(),
            version: "sm5".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-RWTexture1D".into()),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "RWTexture1DArray".into(),
            description: "".into(),
            version: "sm5".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-RWTexture1DArray".into()),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "RWTexture2D".into(),
            description: "".into(),
            version: "sm5".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-RWTexture2D".into()),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "RWTexture2DArray".into(),
            description: "".into(),
            version: "sm5".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-RWTexture2DArray".into()),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "RWTexture3D".into(),
            description: "".into(),
            version: "sm5".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-RWTexture3D".into()),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "StructuredBuffer".into(),
            description: "".into(),
            version: "sm5".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-StructuredBuffer".into()),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });
        // sm 5.1
        symbols.push(ShaderSymbol {
            label: "StructuredBuffer".into(),
            description: "".into(),
            version: "sm5.1".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/sm5-object-StructuredBuffer".into()),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "RasterizerOrderedBuffer".into(),
            description: "".into(),
            version: "sm5.1".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/shader-model-5-1-objects".into()),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "RasterizerOrderedByteAddressBuffer".into(),
            description: "".into(),
            version: "sm5.1".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/shader-model-5-1-objects".into()),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "RasterizerOrderedStructuredBuffer".into(),
            description: "".into(),
            version: "sm5.1".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/shader-model-5-1-objects".into()),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "RasterizerOrderedTexture1D".into(),
            description: "".into(),
            version: "sm5.1".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/shader-model-5-1-objects".into()),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "RasterizerOrderedTexture1DArray".into(),
            description: "".into(),
            version: "sm5.1".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/shader-model-5-1-objects".into()),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "RasterizerOrderedTexture2D".into(),
            description: "".into(),
            version: "sm5.1".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/shader-model-5-1-objects".into()),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "RasterizerOrderedTexture2DArray".into(),
            description: "".into(),
            version: "sm5.1".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/shader-model-5-1-objects".into()),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });
        symbols.push(ShaderSymbol {
            label: "RasterizerOrderedTexture3D".into(),
            description: "".into(),
            version: "sm5.1".into(),
            stages: vec![],
            link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/shader-model-5-1-objects".into()),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![],
                methods: vec![],
            },
            content: None,
            range: None,
        });

        // Manually push types as they are not in documentation
        let mut scalar_types = Vec::new();
        scalar_types.push(new_hlsl_scalar(
            "bool",
            "conditional type, values may be either true or false",
            "",
        ));
        scalar_types.push(new_hlsl_scalar("int", "32-bit signed integer", ""));
        scalar_types.push(new_hlsl_scalar("uint", "32-bit unsigned integer", ""));
        scalar_types.push(new_hlsl_scalar("dword", "32-bit unsigned integer", ""));
        scalar_types.push(new_hlsl_scalar("half", "16-bit floating point value", ""));
        scalar_types.push(new_hlsl_scalar("float", "32-bit floating point value", ""));
        scalar_types.push(new_hlsl_scalar(
            "double",
            "64-bit floating point value.",
            "",
        ));
        // Minimum are only supported with windows 8+
        scalar_types.push(new_hlsl_scalar(
            "min16float",
            "minimum 16-bit floating point value. Only supported on Windows 8+ only.",
            "",
        ));
        scalar_types.push(new_hlsl_scalar(
            "min10float",
            "minimum 10-bit floating point value. Only supported on Windows 8+ only.",
            "",
        ));
        scalar_types.push(new_hlsl_scalar(
            "min16int",
            "minimum 16-bit signed integer. Only supported on Windows 8+ only.",
            "",
        ));
        scalar_types.push(new_hlsl_scalar(
            "min12int",
            "minimum 12-bit signed integer. Only supported on Windows 8+ only.",
            "",
        ));
        scalar_types.push(new_hlsl_scalar(
            "min16uint",
            "minimum 16-bit unsigned integer. Only supported on Windows 8+ only.",
            "",
        ));
        scalar_types.push(new_hlsl_scalar(
            "uint64_t",
            "A 64-bit unsigned integer.",
            "sm6",
        ));
        scalar_types.push(new_hlsl_scalar(
            "int64_t",
            "A 64-bit signed integer.",
            "sm6",
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
                symbols.push(ShaderSymbol {
                    label: fmt.clone(),
                    description: format!("Vector with {} components of {}", component_col, scalar.label),
                    link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-vector".into()),
                    data: ShaderSymbolData::Types { constructors: vec![
                        ShaderSignature {
                            returnType: fmt.clone(),
                            description: format!("Constructor for type {}", fmt),
                            parameters: vec![ShaderParameter {
                                ty: fmt.clone(),
                                label: "value".into(),
                                count: None,
                                description: "".into(),
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
                            }).collect(),
                        }
                    ]},
                    version: "".into(),
                    stages: vec![],
                    range: None,
                    content:None,
                });
                /*symbols.push(ShaderSymbol {
                    label: format!("vector<{},{}>", scalar.label, component_col),
                    description: format!("Vector with {} components of {}", component_col, scalar.label),
                    link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-vector".into()),
                    data: ShaderSymbolData::Types { ty:fmt },
                    version: "".into(),
                    stages: vec![]
                });*/
                for component_row in 1..=4 {
                    let fmt = format!("{}{}x{}", scalar.label, component_row, component_col);
                    symbols.push(ShaderSymbol{
                        label: fmt.clone(),
                        description: format!("Matrice with {} rows and {} columns of {}", component_row, component_col, scalar.label),
                        link: Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-matrix".into()),
                        data: ShaderSymbolData::Types { constructors: vec![
                            ShaderSignature {
                                returnType: fmt.clone(),
                                description: format!("Constructor for type {}", fmt),
                                parameters: vec![ShaderParameter {
                                    ty: fmt.clone(),
                                    label: "value".into(),
                                    count: None,
                                    description: "".into(),
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
                                    }).collect::<Vec<ShaderParameter>>()
                                ).collect::<Vec<Vec<ShaderParameter>>>().concat(),
                            }
                        ] },
                        version: "".into(),
                        stages: vec![],
                        range: None,
                        content:None,
                    });
                    /*symbols.push(ShaderSymbol{
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
        symbols.append(&mut scalar_types);
    }
}
