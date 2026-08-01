//! Description of a symbol
use std::path::PathBuf;

use serde::{Deserialize, Serialize};

use crate::{
    position::ShaderRange,
    shader::{HlslShaderModel, HlslVersion, ShaderCompilationParams, ShaderStageMask},
};

#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub struct ShaderParameter {
    pub ty: String,
    pub label: String,
    pub count: Option<ShaderSymbolArray>,
    pub description: String,
    #[serde(skip)] // Runtime only
    pub range: Option<ShaderRange>,
}

#[allow(non_snake_case)] // for JSON
#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub struct ShaderSignature {
    pub returnType: String, // Should be an option for constructor
    pub description: String,
    pub parameters: Vec<ShaderParameter>,
}

#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub struct ShaderLabelSignature {
    pub label: String,
    pub description: String,
    pub signature: ShaderSignature,
}

impl ShaderSignature {
    pub fn format(&self, label: &str) -> String {
        let signature = self
            .parameters
            .iter()
            .map(|p| format!("{} {}", p.ty, p.label))
            .collect::<Vec<String>>();
        format!("{} {}({})", self.returnType, label, signature.join(", "))
    }
    pub fn format_with_context(&self, label: &str, context: &str) -> String {
        let signature = self
            .parameters
            .iter()
            .map(|p| format!("{} {}", p.ty, p.label))
            .collect::<Vec<String>>();
        format!(
            "{} {}::{}({})",
            self.returnType,
            context,
            label,
            signature.join(", ")
        )
    }
}

pub type ShaderScope = ShaderRange;

#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub struct ShaderMember {
    pub context: String,
    pub parameters: ShaderParameter,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ShaderMethod {
    pub label: String,
    pub context: String,
    pub signature: ShaderSignature,
    #[serde(skip)] // Runtime only
    pub range: Option<ShaderRange>,
}

impl ShaderMember {
    pub fn as_symbol(&self, file_path: Option<PathBuf>) -> ShaderSymbol {
        ShaderSymbol {
            label: self.parameters.label.clone(),
            requirement: None,
            data: ShaderSymbolData::Parameter {
                context: self.context.clone(),
                ty: self.parameters.ty.clone(),
                count: self.parameters.count.clone(),
            },
            mode: match file_path {
                // We assume it as range if it has path.
                // TODO: This should not be a global. Should use symbol directly in fact...
                Some(file_path) => ShaderSymbolMode::Runtime(ShaderSymbolRuntime::global(
                    file_path,
                    self.parameters.range.clone().unwrap(),
                )),
                None => ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                    self.parameters.description.clone(),
                    None,
                )),
            },
        }
    }
}

impl ShaderMethod {
    pub fn as_symbol(&self, file_path: Option<PathBuf>) -> ShaderSymbol {
        ShaderSymbol {
            label: self.label.clone(),
            requirement: None,
            data: ShaderSymbolData::Method {
                context: self.context.clone(),
                signatures: vec![self.signature.clone()],
            },
            mode: match file_path {
                // We assume it as range if it has path.
                // TODO: This should not be a global. Should use symbol directly in fact...
                Some(file_path) => ShaderSymbolMode::Runtime(ShaderSymbolRuntime::global(
                    file_path,
                    self.range.clone().unwrap(),
                )),
                None => ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                    self.signature.description.clone(),
                    None,
                )),
            },
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ShaderEnumValue {
    pub label: String,
    pub description: String,
    pub value: Option<String>,
    #[serde(skip)] // Runtime only
    pub range: Option<ShaderRange>,
}

impl ShaderEnumValue {
    pub fn as_symbol(&self, file_path: Option<PathBuf>, context: &str) -> ShaderSymbol {
        ShaderSymbol {
            label: self.label.clone(),
            requirement: None,
            data: ShaderSymbolData::Parameter {
                context: context.into(),
                ty: context.into(),
                count: None,
            },
            mode: match file_path {
                // We assume it as range if it has path.
                // TODO: This should not be a global. Should use symbol directly in fact...
                Some(file_path) => ShaderSymbolMode::Runtime(ShaderSymbolRuntime::global(
                    file_path,
                    self.range.clone().unwrap(),
                )),
                None => ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                    self.description.clone(),
                    None,
                )),
            },
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum ShaderSymbolArray {
    Fixed(u32),
    Unsized,
}

impl From<u32> for ShaderSymbolArray {
    fn from(value: u32) -> Self {
        Self::Fixed(value)
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum ShaderSymbolData {
    // A bit of duplicate from variables ? Should be struct (Which should be renamed something else)
    Types {
        constructors: Vec<ShaderSignature>,
    },
    Struct {
        constructors: Vec<ShaderSignature>, // Need a range aswell for hover.
        members: Vec<ShaderMember>,         // Need a range aswell for hover.
        methods: Vec<ShaderMethod>,         // Need a range aswell for hover.
    },
    Constants {
        ty: String,
        qualifier: String,
        value: String,
    },
    Functions {
        signatures: Vec<ShaderSignature>,
    },
    Parameter {
        context: String,
        ty: String,
        count: Option<ShaderSymbolArray>,
    },
    Method {
        context: String,
        signatures: Vec<ShaderSignature>,
    },
    Keyword {},
    // Mostly runtime, but GLSL has global variable in builtin that need serial.
    Variables {
        ty: String,
        count: Option<ShaderSymbolArray>,
    },
    Enum {
        values: Vec<ShaderEnumValue>,
    },
    #[serde(skip)] // This is runtime only. No serialization.
    CallExpression {
        label: String,
        range: ShaderRange, // label range.
        parameters: Vec<(String, ShaderRange)>,
    },
    #[serde(skip)] // This is runtime only. No serialization.
    Include {
        target: PathBuf,
    },
    Macro {
        value: String,
        parameters: Vec<String>,
    },
}

#[allow(non_snake_case)] // for JSON
#[derive(Default, Debug, Serialize, Deserialize, Clone)]
pub struct HlslRequirementParameter {
    pub stages: Option<ShaderStageMask>, // Stages required by this symbol.
    pub min_version: Option<HlslVersion>, // Minimum HLSL version for this symbol.
    pub version: Option<HlslVersion>,    // Exact HLSL version for this symbol.
    pub min_shader_model: Option<HlslShaderModel>, // Minimum shader model for this symbol.
    pub shader_model: Option<HlslShaderModel>, // Exact shader model for this symbol.
    pub spirv: Option<bool>,             // Requires SPIRV
    pub enable_16bit_types: Option<bool>, // Requires 16bit types.
}
#[allow(non_snake_case)] // for JSON
#[derive(Default, Debug, Serialize, Deserialize, Clone)]
pub struct GlslRequirementParameter {
    pub stages: Option<ShaderStageMask>,
    pub min_version: Option<u32>,  // min glsl version
    pub extension: Option<String>, // Extension required for this symbol.
}
#[allow(non_snake_case)] // for JSON
#[derive(Default, Debug, Serialize, Deserialize, Clone)]
pub struct WgslRequirementParameter {}

#[allow(non_snake_case)] // for JSON
#[derive(Default, Debug, Serialize, Deserialize, Clone)]
pub enum RequirementParameter {
    #[default]
    None, // no filter parameters
    Hlsl(HlslRequirementParameter),
    Glsl(GlslRequirementParameter),
    Wgsl(WgslRequirementParameter),
}

impl RequirementParameter {
    pub fn is_met(&self, shader_compilation_params: &ShaderCompilationParams) -> bool {
        match self {
            RequirementParameter::None => true, // No requirement. All good.
            RequirementParameter::Hlsl(requirement) => {
                // TODO: Should try to detect shader stage from file name at a higher level.
                let is_stage_ok = match &requirement.stages {
                    Some(required_stages) => match shader_compilation_params.shader_stage {
                        Some(param_stage) => required_stages.contains(&param_stage),
                        None => true, // requirement, but no stage set. Pass them all
                    },
                    None => true, // No requirements, dont care about stage set.
                };
                let is_version_ok = match &requirement.version {
                    Some(version) => *version == shader_compilation_params.hlsl.version,
                    None => true,
                };
                let is_min_version_ok = match &requirement.min_version {
                    Some(min_version) => *min_version <= shader_compilation_params.hlsl.version,
                    None => true,
                };
                let is_shader_model_ok = match &requirement.shader_model {
                    Some(shader_model) => {
                        *shader_model == shader_compilation_params.hlsl.shader_model
                    }
                    None => true,
                };
                let is_min_shader_model_ok = match &requirement.min_shader_model {
                    Some(min_shader_model) => {
                        *min_shader_model <= shader_compilation_params.hlsl.shader_model
                    }
                    None => true,
                };
                let is_spirv_ok = match &requirement.spirv {
                    Some(spirv) => *spirv == shader_compilation_params.hlsl.spirv,
                    None => true,
                };
                let is_16bit_ok = match &requirement.enable_16bit_types {
                    Some(enable_16bit_types) => {
                        *enable_16bit_types == shader_compilation_params.hlsl.enable16bit_types
                    }
                    None => true,
                };
                is_stage_ok
                    && is_min_version_ok
                    && is_version_ok
                    && is_min_shader_model_ok
                    && is_shader_model_ok
                    && is_spirv_ok
                    && is_16bit_ok
            }
            RequirementParameter::Glsl(requirement) => {
                let is_stage_ok = match &requirement.stages {
                    Some(required_stages) => match shader_compilation_params.shader_stage {
                        Some(param_stage) => required_stages.contains(&param_stage),
                        None => true, // requirement, but no stage set. Pass them all
                    },
                    None => true, // No requirements, dont care about stage set.
                };
                let is_version_ok = match &requirement.min_version {
                    Some(_min_version) => true, // TODO: make this work.
                    None => true,
                };
                let is_extension_ok = match &requirement.extension {
                    Some(_extension) => true, // TODO: make this work.
                    None => true,
                };
                is_stage_ok && is_version_ok && is_extension_ok
            }
            RequirementParameter::Wgsl(_) => {
                // Nothing yet here to filter.
                true
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct ShaderSymbolRuntime {
    pub file_path: PathBuf,            // file of the symbol.
    pub range: ShaderRange,            // Range of symbol in shader
    pub scope: Option<ShaderScope>,    // Owning scope
    pub scope_stack: Vec<ShaderScope>, // Stack of declaration
}

impl ShaderSymbolRuntime {
    pub fn new(
        file_path: PathBuf,
        range: ShaderRange,
        scope: Option<ShaderScope>,
        scope_stack: Vec<ShaderScope>,
    ) -> Self {
        Self {
            file_path,
            range,
            scope,
            scope_stack,
        }
    }
    pub fn global(file_path: PathBuf, range: ShaderRange) -> Self {
        Self::new(file_path, range, None, Vec::new())
    }
    pub fn owner(file_path: PathBuf, range: ShaderRange, scope: Option<ShaderScope>) -> Self {
        Self::new(file_path, range, scope, Vec::new())
    }
    pub fn variable(file_path: PathBuf, range: ShaderRange, scope_stack: Vec<ShaderScope>) -> Self {
        Self::new(file_path, range, None, scope_stack)
    }
}

#[derive(Debug, Clone)]
pub struct ShaderSymbolRuntimeContext {}

impl ShaderSymbolRuntimeContext {
    pub fn new() -> Self {
        Self {}
    }
}

#[allow(non_snake_case)] // for JSON
#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub struct ShaderSymbolIntrinsic {
    pub description: String,  // Description of the item
    pub link: Option<String>, // Link to some external documentation
}

impl ShaderSymbolIntrinsic {
    pub fn new(description: String, link: Option<String>) -> Self {
        Self { description, link }
    }
}

#[allow(non_snake_case)] // for JSON
#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum ShaderSymbolMode {
    Intrinsic(ShaderSymbolIntrinsic),
    // We do not want to serialize runtime info.
    #[serde(skip_serializing)]
    #[serde(skip_deserializing)]
    Runtime(ShaderSymbolRuntime),
    #[serde(skip_serializing)]
    #[serde(skip_deserializing)]
    RuntimeContext(ShaderSymbolRuntimeContext),
}
impl ShaderSymbolMode {
    pub fn map_intrinsic(&self) -> Option<&ShaderSymbolIntrinsic> {
        if let ShaderSymbolMode::Intrinsic(intrinsic) = &self {
            Some(intrinsic)
        } else {
            None
        }
    }
    pub fn map_runtime(&self) -> Option<&ShaderSymbolRuntime> {
        if let ShaderSymbolMode::Runtime(runtime) = &self {
            Some(runtime)
        } else {
            None
        }
    }
    pub fn map_runtime_context(&self) -> Option<&ShaderSymbolRuntimeContext> {
        if let ShaderSymbolMode::RuntimeContext(runtime) = &self {
            Some(runtime)
        } else {
            None
        }
    }
    pub fn unwrap_intrinsic(&self) -> &ShaderSymbolIntrinsic {
        if let ShaderSymbolMode::Intrinsic(intrinsic) = &self {
            intrinsic
        } else {
            panic!("Trying to unwrap as intrinsic but type is not.");
        }
    }
    pub fn unwrap_runtime_context(&self) -> &ShaderSymbolRuntimeContext {
        if let ShaderSymbolMode::RuntimeContext(runtime) = &self {
            runtime
        } else {
            panic!("Trying to unwrap as runtime context but type is not.");
        }
    }
    pub fn unwrap_runtime(&self) -> &ShaderSymbolRuntime {
        if let ShaderSymbolMode::Runtime(runtime) = &self {
            runtime
        } else {
            panic!("Trying to unwrap as runtime but type is not.");
        }
    }
}

#[allow(non_snake_case)] // for JSON
#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ShaderSymbol {
    pub label: String,                             // Label for the item
    pub requirement: Option<RequirementParameter>, // Used for filtering symbols.
    pub data: ShaderSymbolData,                    // Data for the variable
    pub mode: ShaderSymbolMode,                    // Data for runtime or intrinsic.
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq, Hash)]
pub enum ShaderSymbolType {
    Types,
    Constants,
    Variables,
    CallExpression,
    Functions,
    Keyword,
    Macros,
    Include,
}

impl ShaderSymbolType {
    // Transient symbol are not serialized nor used for hover & completion.
    pub fn is_transient(&self) -> bool {
        match &self {
            Self::CallExpression => true,
            _ => false,
        }
    }
}

impl ShaderSymbol {
    pub fn is_type(&self, ty: ShaderSymbolType) -> bool {
        match self.get_type() {
            Some(tty) => tty == ty,
            None => false,
        }
    }
    pub fn is_transient(&self) -> bool {
        match self.get_type() {
            Some(ty) => ty.is_transient(),
            None => false,
        }
    }
    pub fn get_type(&self) -> Option<ShaderSymbolType> {
        match &self.data {
            ShaderSymbolData::Types { constructors: _ } => Some(ShaderSymbolType::Types),
            ShaderSymbolData::Struct {
                constructors: _,
                members: _,
                methods: _,
            } => Some(ShaderSymbolType::Types),
            ShaderSymbolData::Constants {
                ty: _,
                qualifier: _,
                value: _,
            } => Some(ShaderSymbolType::Constants),
            ShaderSymbolData::Variables { ty: _, count: _ } => Some(ShaderSymbolType::Variables),
            ShaderSymbolData::Parameter {
                context: _,
                ty: _,
                count: _,
            } => Some(ShaderSymbolType::Variables),
            ShaderSymbolData::Method {
                context: _,
                signatures: _,
            } => Some(ShaderSymbolType::Functions),
            ShaderSymbolData::Enum { values: _ } => Some(ShaderSymbolType::Types),
            ShaderSymbolData::CallExpression {
                label: _,
                range: _,
                parameters: _,
            } => Some(ShaderSymbolType::CallExpression),
            ShaderSymbolData::Functions { signatures: _ } => Some(ShaderSymbolType::Functions),
            ShaderSymbolData::Keyword {} => Some(ShaderSymbolType::Keyword),
            ShaderSymbolData::Include { target: _ } => Some(ShaderSymbolType::Include),
            ShaderSymbolData::Macro {
                value: _,
                parameters: _,
            } => Some(ShaderSymbolType::Macros),
        }
    }
    pub fn format(&self) -> String {
        match &self.data {
            ShaderSymbolData::Types { constructors: _ } => format!("{}", self.label.clone()),
            ShaderSymbolData::Struct {
                constructors: _,
                members: _,
                methods: _,
            } => format!("struct {}", self.label.clone()),
            ShaderSymbolData::Constants {
                ty,
                qualifier,
                value,
            } => format!("{} {} {} = {};", qualifier, ty, self.label.clone(), value),
            ShaderSymbolData::Variables { ty, count } => match count {
                Some(count) => format!(
                    "{} {}[{}]",
                    ty,
                    self.label,
                    match count {
                        ShaderSymbolArray::Fixed(size) => size.to_string(),
                        ShaderSymbolArray::Unsized => "".into(),
                    }
                ),
                None => format!("{} {}", ty, self.label),
            },
            ShaderSymbolData::Enum { values: _ } => format!("enum {}", self.label),
            ShaderSymbolData::Parameter { context, ty, count } => match count {
                Some(count) => format!(
                    "{} {}::{}[{}]",
                    ty,
                    context,
                    self.label,
                    match count {
                        ShaderSymbolArray::Fixed(size) => size.to_string(),
                        ShaderSymbolArray::Unsized => "".into(),
                    }
                ),
                None => format!("{} {}::{}", ty, context, self.label),
            },
            ShaderSymbolData::Method {
                context,
                signatures,
            } => signatures[0].format_with_context(&self.label, context), // TODO: append +1 symbol
            ShaderSymbolData::CallExpression {
                label,
                range: _,
                parameters,
            } => format!(
                "{}({})",
                label,
                parameters
                    .iter()
                    .map(|(label, _)| label.clone())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            ShaderSymbolData::Functions { signatures } => signatures[0].format(&self.label), // TODO: append +1 symbol
            ShaderSymbolData::Keyword {} => format!("{}", self.label.clone()),
            ShaderSymbolData::Include { target: _ } => {
                format!("#include \"{}\"", self.label)
            }
            ShaderSymbolData::Macro { value, parameters } => {
                if parameters.len() == 0 {
                    format!("#define {} {}", self.label, value)
                } else {
                    format!("#define {}({}) {}", self.label, parameters.join(","), value)
                }
            }
        }
    }
}
