//! Intrinsics loaded from json files

use std::{collections::HashMap, sync::LazyLock};

use crate::{
    shader::{ShaderCompilationParams, ShadingLanguage},
    symbols::{
        symbol_list::{ShaderSymbolList, ShaderSymbolListRef},
        symbols::{ShaderSymbol, ShaderSymbolData, ShaderSymbolIntrinsic, ShaderSymbolMode},
    },
};

static INTRINSICS: LazyLock<HashMap<ShadingLanguage, ShaderIntrinsics>> = LazyLock::new(|| {
    HashMap::from([
        (
            ShadingLanguage::Hlsl,
            ShaderIntrinsics::new(ShadingLanguage::Hlsl),
        ),
        (
            ShadingLanguage::Glsl,
            ShaderIntrinsics::new(ShadingLanguage::Glsl),
        ),
        (
            ShadingLanguage::Wgsl,
            ShaderIntrinsics::new(ShadingLanguage::Wgsl),
        ),
    ])
});

pub struct ShaderIntrinsics {
    shader_intrinsics: ShaderSymbolList,
}

impl ShaderIntrinsics {
    fn get_symbol_intrinsic_path(shading_language: ShadingLanguage) -> &'static str {
        match shading_language {
            ShadingLanguage::Wgsl => include_str!("wgsl/wgsl-intrinsics.json"),
            ShadingLanguage::Hlsl => include_str!("hlsl/hlsl-intrinsics.json"),
            ShadingLanguage::Glsl => include_str!("glsl/glsl-intrinsics.json"),
        }
    }
    fn new(shading_language: ShadingLanguage) -> Self {
        let mut shader_intrinsics = ShaderSymbolList::parse_from_json(
            Self::get_symbol_intrinsic_path(shading_language).into(),
        );
        shader_intrinsics.macros.push(ShaderSymbol {
            label: "__SHADER_SENSE__".into(),
            requirement: None,
            data: ShaderSymbolData::Macro {
                value: "1".into(),
                parameters: vec![],
            },
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "This is available because the shader is currently validated by shader-sense."
                    .into(),
                Some("https://github.com/antaalt/shader-sense".into()),
            )),
        });
        Self { shader_intrinsics }
    }
    pub fn get(shading_language: ShadingLanguage) -> &'static ShaderIntrinsics {
        INTRINSICS.get(&shading_language).unwrap()
    }
    pub fn get_intrinsics_symbol<'a>(
        &'a self,
        shader_compilation_params: &ShaderCompilationParams,
    ) -> ShaderSymbolListRef<'a> {
        // Filter intrinsics with given params.
        self.shader_intrinsics
            .filter(|_ty, symbol| match &symbol.requirement {
                Some(requirement) => requirement.is_met(shader_compilation_params),
                None => true,
            })
    }
}
