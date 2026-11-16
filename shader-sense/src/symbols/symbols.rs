//! Description of a symbol
use std::{collections::HashMap, path::PathBuf};

use serde::{Deserialize, Serialize};

use crate::{
    position::ShaderRange,
    shader::{
        HlslShaderModel, HlslVersion, ShaderCompilationParams, ShaderStageMask, ShadingLanguage,
    },
};

#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub struct ShaderParameter {
    pub ty: String,
    pub label: String,
    pub modifier: Option<String>,
    pub count: Option<ShaderSymbolArray>,
    pub description: String,
    #[serde(skip)] // Runtime only
    pub range: Option<ShaderRange>,
}

impl ShaderParameter {
    /// Format the parameter as declared, such as `out float2 value`.
    pub fn format(&self) -> String {
        match &self.modifier {
            Some(modifier) => format!("{} {} {}", modifier, self.ty, self.label),
            None => format!("{} {}", self.ty, self.label),
        }
    }
}

// Split a type such as `Foo<T, Bar<float> >` into its base name `Foo` and its template arguments `[T, Bar<float>]`.
pub fn split_template_type(ty: &str) -> (&str, Vec<&str>) {
    let (Some(start), Some(end)) = (ty.find('<'), ty.rfind('>')) else {
        return (ty.trim(), Vec::new());
    };
    if end < start {
        return (ty.trim(), Vec::new());
    }
    let mut arguments = Vec::new();
    let mut depth = 0;
    let mut argument_start = start + 1;
    for (i, c) in ty[..end].char_indices().skip_while(|(i, _)| *i <= start) {
        match c {
            '<' | '(' => depth += 1,
            '>' | ')' => depth -= 1,
            ',' if depth == 0 => {
                arguments.push(ty[argument_start..i].trim());
                argument_start = i + 1;
            }
            _ => {}
        }
    }
    let last = ty[argument_start..end].trim();
    if !last.is_empty() || !arguments.is_empty() {
        arguments.push(last);
    }
    (ty[..start].trim(), arguments)
}

// Check that type matches expected type, where template parameters of expected type are bound to the type they match.
fn match_type<'a>(
    expected: &'a str,
    ty: &'a str,
    template_parameters: &[ShaderTemplateParameter],
    bindings: &mut HashMap<&'a str, &'a str>,
) -> bool {
    let expected = expected.trim();
    let ty = ty.trim();
    if template_parameters.iter().any(|p| p.label == expected) {
        return *bindings.entry(expected).or_insert(ty) == ty;
    }
    let (expected_base, expected_arguments) = split_template_type(expected);
    let (base, arguments) = split_template_type(ty);
    // Arguments with default value might be omitted, so only compare the given ones.
    // TODO: should compare omitted ones against the default value of the struct.
    expected_base == base
        && expected_arguments
            .iter()
            .zip(arguments.iter())
            .all(|(expected, ty)| match_type(expected, ty, template_parameters, bindings))
}

// Bind the template parameters of a templated type to the arguments of its instantiation `ty`.
// Omitted arguments use their default value, which might depend on previous parameters.
pub fn get_template_bindings(
    template_parameters: &[ShaderTemplateParameter],
    ty: &str,
) -> HashMap<String, String> {
    let (_, arguments) = split_template_type(ty);
    let mut bindings = HashMap::new();
    for (i, parameter) in template_parameters.iter().enumerate() {
        let value = match arguments.get(i) {
            Some(argument) => argument.to_string(),
            None => match &parameter.default {
                Some(default) => substitute_template_type(default, &bindings),
                None => continue, // Unknown, keep the parameter as is.
            },
        };
        bindings.insert(parameter.label.clone(), value);
    }
    bindings
}

// Replace every template parameter in ty with the type it is bound to.
pub fn substitute_template_type(ty: &str, bindings: &HashMap<String, String>) -> String {
    if bindings.is_empty() {
        return ty.into();
    }
    let mut substituted = String::with_capacity(ty.len());
    let mut identifier_start = None;
    let is_identifier = |c: char| c.is_alphanumeric() || c == '_';
    for (i, c) in ty.char_indices().chain(std::iter::once((ty.len(), ' '))) {
        match (identifier_start, is_identifier(c)) {
            (None, true) => identifier_start = Some(i),
            (Some(start), false) => {
                let identifier = &ty[start..i];
                substituted.push_str(bindings.get(identifier).map_or(identifier, |s| s.as_str()));
                identifier_start = None;
            }
            _ => {}
        }
        if !is_identifier(c) && i < ty.len() {
            substituted.push(c);
        }
    }
    substituted
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct ShaderTemplateParameter {
    pub label: String,
    pub default: Option<String>, // Default type or value if any.
}

impl ShaderTemplateParameter {
    pub fn new(label: &str) -> Self {
        Self {
            label: label.into(),
            default: None,
        }
    }
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
    // Check parameter types match exactly. An unknown type (None) match any parameter.
    // Template parameters match any type, as long as they are bound to the same type everywhere.
    pub fn match_types(
        &self,
        types: &[Option<String>],
        template_parameters: &[ShaderTemplateParameter],
    ) -> bool {
        let mut bindings = HashMap::new();
        self.parameters.len() == types.len()
            && self
                .parameters
                .iter()
                .zip(types.iter())
                .all(|(parameter, ty)| {
                    ty.as_ref().map_or(true, |ty| {
                        match_type(&parameter.ty, ty, template_parameters, &mut bindings)
                    })
                })
    }
    pub fn format(&self, label: &str, shading_language: ShadingLanguage) -> String {
        match shading_language {
            ShadingLanguage::Glsl | ShadingLanguage::Hlsl => {
                let signature = self
                    .parameters
                    .iter()
                    .map(|p| p.format())
                    .collect::<Vec<String>>();
                format!("{} {}({})", self.returnType, label, signature.join(", "))
            }
            ShadingLanguage::Wgsl => {
                let signature = self
                    .parameters
                    .iter()
                    .map(|p| p.format())
                    .collect::<Vec<String>>();
                format!(
                    "fn {}({}) -> {}",
                    label,
                    signature.join(", "),
                    self.returnType
                )
            }
        }
    }
    pub fn format_with_context(
        &self,
        label: &str,
        context: &str,
        shading_language: ShadingLanguage,
    ) -> String {
        match shading_language {
            ShadingLanguage::Glsl | ShadingLanguage::Hlsl => {
                let signature = self
                    .parameters
                    .iter()
                    .map(|p| p.format())
                    .collect::<Vec<String>>();
                format!(
                    "{} {}::{}({})",
                    self.returnType,
                    context,
                    label,
                    signature.join(", ")
                )
            }
            ShadingLanguage::Wgsl => {
                let signature = self
                    .parameters
                    .iter()
                    .map(|p| p.format())
                    .collect::<Vec<String>>();
                format!(
                    "fn {}::{}({}) -> {}",
                    context,
                    label,
                    signature.join(", "),
                    self.returnType
                )
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ShaderScope {
    pub range: ShaderRange,
    pub namespace: Option<String>,
}

impl ShaderScope {
    pub fn new(range: ShaderRange) -> ShaderScope {
        ShaderScope {
            range,
            namespace: None,
        }
    }
    pub fn new_namespace(range: ShaderRange, namespace: String) -> ShaderScope {
        ShaderScope {
            range,
            namespace: Some(namespace),
        }
    }
}

// A namespace block. Namespaces are not symbols: they can be reopened, so they do not have a
// single definition, and their content is found through the scope stack of the symbols instead.
// They are only stored to know in which namespace a position is, and which namespaces exist.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ShaderNamespace {
    pub file_path: PathBuf,
    pub path: Vec<String>, // Full path of the block, from outer to inner namespace.
    pub range: ShaderRange,
}

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
    pub file_path: PathBuf,                                // file of the symbol.
    pub range: ShaderRange,                                // Range of symbol in shader
    pub scope: Option<ShaderScope>,                        // Owning scope
    pub scope_stack: Vec<ShaderScope>,                     // Stack of declaration
    pub template_parameters: Vec<ShaderTemplateParameter>, // Template parameters of the declaration.
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
            template_parameters: Vec::new(),
        }
    }
    pub fn with_template_parameters(
        mut self,
        template_parameters: Vec<ShaderTemplateParameter>,
    ) -> Self {
        self.template_parameters = template_parameters;
        self
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
    pub fn get_namespace(&self) -> Vec<String> {
        self.scope_stack
            .iter()
            .filter_map(|s| s.namespace.clone())
            .collect()
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
    pub fn get_namespace(&self) -> Vec<String> {
        // TODO: intrinsics should be able to have a namespace. Store it aswell
        match self {
            ShaderSymbolMode::Intrinsic(_) => vec![],
            ShaderSymbolMode::Runtime(r) => r.get_namespace(),
            ShaderSymbolMode::RuntimeContext(_) => vec![],
        }
    }
    pub fn get_template_parameters(&self) -> &[ShaderTemplateParameter] {
        match self {
            ShaderSymbolMode::Runtime(r) => &r.template_parameters,
            _ => &[],
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

#[derive(Debug, Serialize, Deserialize, Clone, Copy, PartialEq, Eq, Hash)]
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
        self.get_type() == ty
    }
    pub fn is_transient(&self) -> bool {
        self.get_type().is_transient()
    }
    pub fn get_type(&self) -> ShaderSymbolType {
        match &self.data {
            ShaderSymbolData::Types { constructors: _ } => ShaderSymbolType::Types,
            ShaderSymbolData::Struct {
                constructors: _,
                members: _,
                methods: _,
            } => ShaderSymbolType::Types,
            ShaderSymbolData::Constants {
                ty: _,
                qualifier: _,
                value: _,
            } => ShaderSymbolType::Constants,
            ShaderSymbolData::Variables { ty: _, count: _ } => ShaderSymbolType::Variables,
            ShaderSymbolData::Parameter {
                context: _,
                ty: _,
                count: _,
            } => ShaderSymbolType::Variables,
            ShaderSymbolData::Method {
                context: _,
                signatures: _,
            } => ShaderSymbolType::Functions,
            ShaderSymbolData::Enum { values: _ } => ShaderSymbolType::Types,
            ShaderSymbolData::CallExpression {
                label: _,
                range: _,
                parameters: _,
            } => ShaderSymbolType::CallExpression,
            ShaderSymbolData::Functions { signatures: _ } => ShaderSymbolType::Functions,
            ShaderSymbolData::Keyword {} => ShaderSymbolType::Keyword,
            ShaderSymbolData::Include { target: _ } => ShaderSymbolType::Include,
            ShaderSymbolData::Macro {
                value: _,
                parameters: _,
            } => ShaderSymbolType::Macros,
        }
    }
    pub fn format(&self, shading_language: ShadingLanguage) -> String {
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
            } => match shading_language {
                ShadingLanguage::Glsl | ShadingLanguage::Hlsl => {
                    format!("{} {} {} = {};", qualifier, ty, self.label.clone(), value)
                }
                ShadingLanguage::Wgsl => {
                    format!("{} {}: {} = {};", qualifier, self.label.clone(), ty, value)
                }
            },
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
            } => signatures[0].format_with_context(&self.label, context, shading_language), // TODO: append +1 symbol
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
            ShaderSymbolData::Functions { signatures } => {
                signatures[0].format(&self.label, shading_language)
            } // TODO: append +1 symbol
            ShaderSymbolData::Keyword {} => format!("{}", self.label.clone()),
            ShaderSymbolData::Include { target: _ } => match shading_language {
                ShadingLanguage::Glsl | ShadingLanguage::Hlsl => {
                    format!("#include \"{}\"", self.label)
                }
                ShadingLanguage::Wgsl => {
                    format!("import {}", self.label)
                }
            },
            ShaderSymbolData::Macro { value, parameters } => match shading_language {
                ShadingLanguage::Glsl | ShadingLanguage::Hlsl => {
                    if parameters.len() == 0 {
                        format!("#define {} {}", self.label, value)
                    } else {
                        format!("#define {}({}) {}", self.label, parameters.join(","), value)
                    }
                }
                ShadingLanguage::Wgsl => {
                    assert!(parameters.is_empty());
                    format!("const {} = {};", self.label, value)
                }
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn split_template_type_ok() {
        assert_eq!(split_template_type("float"), ("float", vec![]));
        assert_eq!(split_template_type("Foo<>"), ("Foo", vec![]));
        assert_eq!(
            split_template_type("Foo<T, Bar<float, 2> >"),
            ("Foo", vec!["T", "Bar<float, 2>"])
        );
    }
    #[test]
    fn template_bindings_ok() {
        let template_parameters = vec![
            ShaderTemplateParameter::new("T"),
            ShaderTemplateParameter {
                label: "A".into(),
                default: Some("vector<T, 2>".into()),
            },
        ];
        let instantiate = |ty: &str, member: &str| {
            substitute_template_type(member, &get_template_bindings(&template_parameters, ty))
        };
        assert_eq!(instantiate("Foo<float, int>", "A"), "int");
        // Default value depend on previous parameter.
        assert_eq!(instantiate("Foo<float>", "A"), "vector<float, 2>");
        // Only whole identifiers are substituted.
        assert_eq!(instantiate("Foo<int>", "Bar<T, TT>"), "Bar<int, TT>");
        // Unbound parameters are kept.
        assert_eq!(instantiate("Foo", "T"), "T");
    }
    #[test]
    fn match_template_types_ok() {
        let parameter = |ty: &str| ShaderParameter {
            ty: ty.into(),
            label: "p".into(),
            modifier: None,
            count: None,
            description: "".into(),
            range: None,
        };
        let signature = ShaderSignature {
            returnType: "void".into(),
            description: "".into(),
            parameters: vec![parameter("Foo<T, float>"), parameter("T")],
        };
        let template_parameters = vec![ShaderTemplateParameter::new("T")];
        let types = |types: &[&str]| -> Vec<Option<String>> {
            types.iter().map(|ty| Some(ty.to_string())).collect()
        };
        // T is bound to the same type for all parameters.
        assert!(signature.match_types(&types(&["Foo<int, float>", "int"]), &template_parameters));
        assert!(!signature.match_types(&types(&["Foo<int, float>", "float"]), &template_parameters));
        // Omitted default arguments are accepted.
        assert!(signature.match_types(&types(&["Foo<int>", "int"]), &template_parameters));
        // Base type must match.
        assert!(!signature.match_types(&types(&["Bar<int>", "int"]), &template_parameters));
        // Without template parameters, T is a regular type.
        assert!(!signature.match_types(&types(&["Foo<int, float>", "int"]), &[]));
    }
}
