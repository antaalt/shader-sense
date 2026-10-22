#[cfg(not(target_os = "wasi"))]
use shader_sense::validator::dxc::Dxc;
use shader_sense::{
    shader::{HlslShaderModel, HlslVersion, ShaderStage, ShaderStageMask},
    symbols::{
        symbol_list::ShaderSymbolList,
        symbols::{
            HlslRequirementParameter, RequirementParameter, ShaderSymbol, ShaderSymbolData,
            ShaderSymbolIntrinsic, ShaderSymbolMode,
        },
    },
};

use crate::hlsl::HlslIntrinsicParser;

impl HlslIntrinsicParser {
    // Small hack for wasi compilation as this crate will never run in wasi, do not care.
    #[cfg(target_os = "wasi")]
    pub fn add_macros(&self, symbols: &mut ShaderSymbolList) {}
    #[cfg(not(target_os = "wasi"))]
    pub fn add_macros(&self, symbols: &mut ShaderSymbolList) {
        // Get predefined macros
        // https://github.com/microsoft/DirectXShaderCompiler/wiki/Predefined-Version-Macros
        // https://github.com/microsoft/DirectXShaderCompiler/blob/a47537f0e2680441f547817a66c3b817200ac874/tools/clang/lib/Frontend/InitPreprocessor.cpp#L385
        fn get_stage_value(stage: Option<ShaderStage>) -> &'static str {
            match stage {
                Some(stage) => match stage {
                    // Matches https://github.com/microsoft/DirectXShaderCompiler/blob/a47537f0e2680441f547817a66c3b817200ac874/include/dxc/DXIL/DxilConstants.h#L223
                    ShaderStage::Fragment => "0",
                    ShaderStage::Vertex => "1",
                    ShaderStage::Geometry => "2",
                    ShaderStage::TesselationControl => "3",
                    ShaderStage::TesselationEvaluation => "4",
                    ShaderStage::Compute => "5",
                    // ShaderStage::Library => "6"
                    //ShaderStage::RayGeneration => "7",
                    //ShaderStage::Intersect => "8",
                    //ShaderStage::AnyHit => "9",
                    //ShaderStage::ClosestHit => "10",
                    //ShaderStage::Miss => "11",
                    //ShaderStage::Callable => "12",
                    ShaderStage::Mesh => "13",
                    ShaderStage::Task => "14",
                    // TODO: ShaderStage::Library => "15"
                    // ShaderStage::Invalid => "16"
                    _ => "6", // For now, RT stage uses lib profile.
                },
                None => "6", // Lib profile value.
            }
        }
        fn add_macro(symbols: &mut ShaderSymbolList, name: &str, value: &str) {
            symbols.macros.push(ShaderSymbol {
                label: name.into(),
                mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                    "".into(), 
                    Some("https://github.com/microsoft/DirectXShaderCompiler/wiki/Predefined-Version-Macros".into())
                )),
                requirement: None,
                data: ShaderSymbolData::Macro {
                    value: value.into(),
                    parameters: vec![],
                },
            });
        }
        fn add_macro_with_req(
            symbols: &mut ShaderSymbolList,
            name: &str,
            value: &str,
            req: HlslRequirementParameter,
        ) {
            symbols.macros.push(ShaderSymbol {
                label: name.into(),
                mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                    "".into(), 
                    Some("https://github.com/microsoft/DirectXShaderCompiler/wiki/Predefined-Version-Macros".into())
                )),
                requirement: Some(RequirementParameter::Hlsl(req)),
                data: ShaderSymbolData::Macro {
                    value: value.into(),
                    parameters: vec![],
                },
            });
        }

        // Set stage defines
        add_macro(
            symbols,
            "__SHADER_STAGE_VERTEX".into(),
            get_stage_value(Some(ShaderStage::Vertex)),
        );
        add_macro(
            symbols,
            "__SHADER_STAGE_PIXEL".into(),
            get_stage_value(Some(ShaderStage::Fragment)),
        );
        add_macro(
            symbols,
            "__SHADER_STAGE_GEOMETRY".into(),
            get_stage_value(Some(ShaderStage::Geometry)),
        );
        add_macro(
            symbols,
            "__SHADER_STAGE_HULL".into(),
            get_stage_value(Some(ShaderStage::TesselationControl)),
        );
        add_macro(
            symbols,
            "__SHADER_STAGE_DOMAIN".into(),
            get_stage_value(Some(ShaderStage::TesselationEvaluation)),
        );
        add_macro(
            symbols,
            "__SHADER_STAGE_COMPUTE".into(),
            get_stage_value(Some(ShaderStage::Compute)),
        );
        add_macro(
            symbols,
            "__SHADER_STAGE_AMPLIFICATION".into(),
            get_stage_value(Some(ShaderStage::Task)),
        );
        add_macro(
            symbols,
            "__SHADER_STAGE_MESH".into(),
            get_stage_value(Some(ShaderStage::Mesh)),
        );
        add_macro(
            symbols,
            "__SHADER_STAGE_LIBRARY".into(),
            get_stage_value(None),
        );
        // Handle current stage macro.
        add_macro_with_req(
            symbols,
            "__SHADER_TARGET_STAGE".into(),
            "__SHADER_STAGE_VERTEX",
            HlslRequirementParameter {
                stages: Some(ShaderStageMask::VERTEX),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "__SHADER_TARGET_STAGE".into(),
            "__SHADER_STAGE_PIXEL",
            HlslRequirementParameter {
                stages: Some(ShaderStageMask::FRAGMENT),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "__SHADER_TARGET_STAGE".into(),
            "__SHADER_STAGE_COMPUTE",
            HlslRequirementParameter {
                stages: Some(ShaderStageMask::COMPUTE),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "__SHADER_TARGET_STAGE".into(),
            "__SHADER_STAGE_HULL",
            HlslRequirementParameter {
                stages: Some(ShaderStageMask::TESSELATION_CONTROL),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "__SHADER_TARGET_STAGE".into(),
            "__SHADER_STAGE_DOMAIN",
            HlslRequirementParameter {
                stages: Some(ShaderStageMask::TESSELATION_EVALUATION),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "__SHADER_TARGET_STAGE".into(),
            "__SHADER_STAGE_MESH",
            HlslRequirementParameter {
                stages: Some(ShaderStageMask::MESH),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "__SHADER_TARGET_STAGE".into(),
            "__SHADER_STAGE_AMPLIFICATION",
            HlslRequirementParameter {
                stages: Some(ShaderStageMask::TASK),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "__SHADER_TARGET_STAGE".into(),
            "__SHADER_STAGE_GEOMETRY",
            HlslRequirementParameter {
                stages: Some(ShaderStageMask::GEOMETRY),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "__SHADER_TARGET_STAGE".into(),
            "__SHADER_STAGE_LIBRARY",
            HlslRequirementParameter {
                stages: Some(ShaderStage::raytracing()),
                ..Default::default()
            },
        );
        // Fallback when no stage set
        add_macro(
            symbols,
            "__SHADER_TARGET_STAGE".into(),
            "__SHADER_STAGE_LIBRARY",
        );

        // Set enable 16 bits.
        add_macro_with_req(
            symbols,
            "__HLSL_ENABLE_16_BIT".into(),
            "1",
            HlslRequirementParameter {
                enable_16bit_types: Some(true),
                ..Default::default()
            },
        );

        // Set shader model version
        // Dxc only support > 6.0, so ignore others.
        add_macro(symbols, "__SHADER_TARGET_MAJOR".into(), "6");
        add_macro_with_req(
            symbols,
            "__SHADER_TARGET_MINOR".into(),
            "0",
            HlslRequirementParameter {
                shader_model: Some(HlslShaderModel::ShaderModel6),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "__SHADER_TARGET_MINOR".into(),
            "1",
            HlslRequirementParameter {
                shader_model: Some(HlslShaderModel::ShaderModel6_1),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "__SHADER_TARGET_MINOR".into(),
            "2",
            HlslRequirementParameter {
                shader_model: Some(HlslShaderModel::ShaderModel6_2),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "__SHADER_TARGET_MINOR".into(),
            "3",
            HlslRequirementParameter {
                shader_model: Some(HlslShaderModel::ShaderModel6_3),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "__SHADER_TARGET_MINOR".into(),
            "4",
            HlslRequirementParameter {
                shader_model: Some(HlslShaderModel::ShaderModel6_4),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "__SHADER_TARGET_MINOR".into(),
            "5",
            HlslRequirementParameter {
                shader_model: Some(HlslShaderModel::ShaderModel6_5),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "__SHADER_TARGET_MINOR".into(),
            "6",
            HlslRequirementParameter {
                shader_model: Some(HlslShaderModel::ShaderModel6_6),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "__SHADER_TARGET_MINOR".into(),
            "7",
            HlslRequirementParameter {
                shader_model: Some(HlslShaderModel::ShaderModel6_7),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "__SHADER_TARGET_MINOR".into(),
            "8",
            HlslRequirementParameter {
                shader_model: Some(HlslShaderModel::ShaderModel6_8),
                ..Default::default()
            },
        );

        // Set HLSL version
        add_macro_with_req(
            symbols,
            "__HLSL_VERSION".into(),
            "2016",
            HlslRequirementParameter {
                version: Some(HlslVersion::V2016),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "__HLSL_VERSION".into(),
            "2017",
            HlslRequirementParameter {
                version: Some(HlslVersion::V2017),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "__HLSL_VERSION".into(),
            "2018",
            HlslRequirementParameter {
                version: Some(HlslVersion::V2018),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "__HLSL_VERSION".into(),
            "2021",
            HlslRequirementParameter {
                version: Some(HlslVersion::V2021),
                ..Default::default()
            },
        );

        // Macro to differentiate between FXC & DXC
        add_macro(symbols, "__hlsl_dx_compiler".into(), "1".into());

        add_macro(
            symbols,
            "__DXC_VERSION_MAJOR".into(),
            &Dxc::DXC_VERSION_MAJOR.to_string(),
        );
        add_macro(
            symbols,
            "__DXC_VERSION_MINOR".into(),
            &Dxc::DXC_VERSION_MINOR.to_string(),
        );
        add_macro(
            symbols,
            "__DXC_VERSION_RELEASE".into(),
            &Dxc::DXC_VERSION_RELEASE.to_string(),
        );
        add_macro(
            symbols,
            "__DXC_VERSION_COMMITS".into(),
            &Dxc::DXC_VERSION_COMMIT.to_string(),
        );

        // SPIRV
        add_macro_with_req(
            symbols,
            "__spirv__".into(),
            "1".into(),
            HlslRequirementParameter {
                spirv: Some(true),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "__SPIRV_MAJOR_VERSION__".into(),
            &Dxc::DXC_SPIRV_VERSION_MAJOR.to_string(),
            HlslRequirementParameter {
                spirv: Some(true),
                ..Default::default()
            },
        );
        add_macro_with_req(
            symbols,
            "__SPIRV_MINOR_VERSION__".into(),
            &Dxc::DXC_SPIRV_VERSION_MINOR.to_string(),
            HlslRequirementParameter {
                spirv: Some(true),
                ..Default::default()
            },
        );
    }
}
