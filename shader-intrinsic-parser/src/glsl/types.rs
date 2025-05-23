use shader_sense::symbols::symbols::{
    ShaderBuiltinSymbol, ShaderParameter, ShaderSignature, ShaderSymbol, ShaderSymbolData,
};

use super::GlslIntrinsicParser;

impl GlslIntrinsicParser {
    pub fn add_types(&self, symbols: &mut ShaderBuiltinSymbol) {
        pub fn new_glsl_type(label: &str, description: &str, version: &str) -> ShaderSymbol {
            ShaderSymbol {
                label: label.into(),
                description: description.into(),
                version: version.to_string(),
                stages: vec![],
                link: None,
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
        pub fn new_glsl_vec_type(
            label: &str,
            description: &str,
            version: &str,
            ty: &str,
            component_count: u32,
        ) -> ShaderSymbol {
            ShaderSymbol {
                label: label.into(),
                description: description.into(),
                version: version.to_string(),
                stages: vec![],
                link: None,
                data: ShaderSymbolData::Types {
                    constructors: vec![
                        ShaderSignature {
                            returnType: "".into(),
                            description: format!("Constructor for type {}", label),
                            parameters: vec![ShaderParameter {
                                ty: label.into(),
                                label: "value".into(),
                                count: None,
                                description: "".into(),
                            }],
                        },
                        ShaderSignature {
                            returnType: label.into(),
                            description: format!("Constructor for type {}", label),
                            parameters: (0..component_count)
                                .map(|parameter_index| ShaderParameter {
                                    ty: ty.into(),
                                    label: get_vector_component_label(parameter_index),
                                    count: None,
                                    description: "".into(),
                                })
                                .collect(),
                        },
                    ],
                },
                range: None,
                content: None,
            }
        }
        pub fn new_glsl_mat_type(
            label: &str,
            description: &str,
            version: &str,
            ty: &str,
            col_count: u32,
            row_count: u32,
        ) -> ShaderSymbol {
            ShaderSymbol {
                label: label.into(),
                description: description.into(),
                version: version.to_string(),
                stages: vec![],
                link: None,
                data: ShaderSymbolData::Types {
                    constructors: vec![
                        ShaderSignature {
                            returnType: "".into(),
                            description: format!("Constructor for type {}", label),
                            parameters: vec![ShaderParameter {
                                ty: label.into(),
                                label: "value".into(),
                                count: None,
                                description: "".into(),
                            }],
                        },
                        ShaderSignature {
                            returnType: ty.into(),
                            description: format!("Constructor for type {}", label),
                            parameters: (0..col_count)
                                .map(|col_index| {
                                    (0..row_count)
                                        .map(|row_index| ShaderParameter {
                                            ty: ty.into(),
                                            label: get_matrix_component_label(col_index, row_index),
                                            count: None,
                                            description: "".into(),
                                        })
                                        .collect::<Vec<ShaderParameter>>()
                                })
                                .collect::<Vec<Vec<ShaderParameter>>>()
                                .concat(),
                        },
                    ],
                },
                range: None,
                content: None,
            }
        }
        // Manually push types as they are not in documentation
        symbols.push(new_glsl_type(
            "bool",
            "conditional type, values may be either true or false",
            "110",
        ));
        symbols.push(new_glsl_type(
            "int",
            " a signed, two's complement, 32-bit integer",
            "110",
        ));
        symbols.push(new_glsl_type("uint", " an unsigned 32-bit integer", "110"));
        symbols.push(new_glsl_type(
            "float",
            "an IEEE-754 single-precision floating point number",
            "110",
        ));
        symbols.push(new_glsl_type(
            "double",
            "an IEEE-754 double-precision floating-point number",
            "110",
        ));
        for component in 2..=4 {
            // Vectors
            symbols.push(new_glsl_vec_type(
                format!("bvec{}", component).as_str(),
                format!("Vector with {} components of booleans", component).as_str(),
                "bool",
                "110",
                component,
            ));
            symbols.push(new_glsl_vec_type(
                format!("ivec{}", component).as_str(),
                format!("Vector with {} components of signed integers", component).as_str(),
                "int",
                "110",
                component,
            ));
            symbols.push(new_glsl_vec_type(
                format!("uvec{}", component).as_str(),
                format!("Vector with {} components of unsigned integers", component).as_str(),
                "uint",
                "110",
                component,
            ));
            symbols.push(new_glsl_vec_type(
                format!("vec{}", component).as_str(),
                format!(
                    "Vector with {} components of single-precision floating-point numbers",
                    component
                )
                .as_str(),
                "float",
                "110",
                component,
            ));
            symbols.push(new_glsl_vec_type(
                format!("dvec{}", component).as_str(),
                format!(
                    "Vector with {} components of double-precision floating-point numbers",
                    component
                )
                .as_str(),
                "double",
                "110",
                component,
            ));
            // Matrices
            symbols.push(new_glsl_mat_type(
                format!("mat{}", component).as_str(),
                format!(
                    "Matrice with {} columns & rows of single-precision floating-point numbers",
                    component
                )
                .as_str(),
                "float",
                "110",
                component,
                component,
            ));
            symbols.push(new_glsl_mat_type(
                format!("dmat{}", component).as_str(),
                format!(
                    "Matrice with {} columns & rows of double-precision floating-point numbers",
                    component
                )
                .as_str(),
                "double",
                "110",
                component,
                component,
            ));
            for component_row in 2..=4 {
                symbols.push(new_glsl_mat_type(
                    format!("mat{}x{}", component, component_row).as_str(), 
                    format!("Matrice with {} columns and {} rows of single-precision floating-point numbers", component, component_row).as_str(),
                    "float",
                    "110",
                    component,
                    component_row
                ));
                symbols.push(new_glsl_mat_type(
                    format!("dmat{}x{}", component, component_row).as_str(), 
                    format!("Matrice with {} columns and {} rows of double-precision floating-point numbers", component, component_row).as_str(),
                    "double", 
                    "110", 
                    component,
                    component_row
                ));
            }
        }
        // Samplers
        let sampler_types = [
            "1D",
            "2D",
            "3D",
            "Cube",
            "2DRect",
            "1DArray",
            "2DArray",
            "CubeArray",
            "Buffer",
            "2DMS",
            "2DMSArray",
        ];
        for sampler_type in sampler_types {
            symbols.push(new_glsl_type(
                format!("sampler{}", sampler_type).as_str(),
                format!("Floating-point sampler for Texture{}", sampler_type).as_str(),
                "110",
            ));
            symbols.push(new_glsl_type(
                format!("isampler{}", sampler_type).as_str(),
                format!("Signed integer sampler for Texture{}", sampler_type).as_str(),
                "110",
            ));
            symbols.push(new_glsl_type(
                format!("usampler{}", sampler_type).as_str(),
                format!("Unsigned integer sampler for Texture{}", sampler_type).as_str(),
                "110",
            ));
        }
        // Shadow Samplers
        let shadow_sampler_types = [
            "1D",
            "2D",
            "Cube",
            "2DRect",
            "1DArray",
            "2DArray",
            "CubeArray",
        ];
        for shadow_sampler_type in shadow_sampler_types {
            symbols.push(new_glsl_type(
                format!("sampler{}Shadow", shadow_sampler_type).as_str(),
                format!("Shadow sampler for Texture{}", shadow_sampler_type).as_str(),
                "110",
            ));
        }
        // Atomic counters
        symbols.push(new_glsl_type("atomic_uint", "An Atomic Counter is a GLSL variable type whose storage comes from a Buffer Object. Atomic counters, as the name suggests, can have atomic memory operations performed on them. They can be thought of as a very limited form of buffer image variable.", "460"));
    }
}
