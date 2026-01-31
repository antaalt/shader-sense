//! Shader stage and specific helpers
use std::{
    collections::HashMap,
    ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Not},
    path::PathBuf,
    str::FromStr,
};

use serde::{Deserialize, Serialize};

/// All shading language supported
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum ShadingLanguage {
    Wgsl,
    Hlsl,
    Glsl,
}

/// Mask for all shaders stages supported.
#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub struct ShaderStageMask(u32);

impl ShaderStageMask {
    pub const VERTEX: Self = Self(1 << 0);
    pub const FRAGMENT: Self = Self(1 << 1);
    pub const COMPUTE: Self = Self(1 << 2);
    pub const TESSELATION_CONTROL: Self = Self(1 << 3);
    pub const TESSELATION_EVALUATION: Self = Self(1 << 4);
    pub const MESH: Self = Self(1 << 5);
    pub const TASK: Self = Self(1 << 6);
    pub const GEOMETRY: Self = Self(1 << 7);
    pub const RAY_GENERATION: Self = Self(1 << 8);
    pub const CLOSEST_HIT: Self = Self(1 << 9);
    pub const ANY_HIT: Self = Self(1 << 10);
    pub const CALLABLE: Self = Self(1 << 11);
    pub const MISS: Self = Self(1 << 12);
    pub const INTERSECT: Self = Self(1 << 13);
}

impl Default for ShaderStageMask {
    fn default() -> Self {
        Self(0)
    }
}
impl ShaderStageMask {
    pub const fn from_u32(x: u32) -> Self {
        Self(x)
    }
    pub const fn as_u32(self) -> u32 {
        self.0
    }
    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }
    pub const fn contains(self, other: &ShaderStage) -> bool {
        let mask = other.as_mask();
        self.0 & mask.0 == mask.0
    }
}
impl BitOr for ShaderStageMask {
    type Output = Self;
    #[inline]
    fn bitor(self, rhs: Self) -> Self {
        Self(self.0 | rhs.0)
    }
}
impl BitOrAssign for ShaderStageMask {
    #[inline]
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs
    }
}
impl BitAnd for ShaderStageMask {
    type Output = Self;
    #[inline]
    fn bitand(self, rhs: Self) -> Self {
        Self(self.0 & rhs.0)
    }
}
impl BitAndAssign for ShaderStageMask {
    #[inline]
    fn bitand_assign(&mut self, rhs: Self) {
        *self = *self & rhs
    }
}
impl BitXor for ShaderStageMask {
    type Output = Self;
    #[inline]
    fn bitxor(self, rhs: Self) -> Self {
        Self(self.0 ^ rhs.0)
    }
}
impl BitXorAssign for ShaderStageMask {
    #[inline]
    fn bitxor_assign(&mut self, rhs: Self) {
        *self = *self ^ rhs
    }
}
impl Not for ShaderStageMask {
    type Output = Self;
    #[inline]
    fn not(self) -> Self {
        Self(!self.0)
    }
}

/// All shader stage supported
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
#[serde(rename_all = "camelCase")]
pub enum ShaderStage {
    Vertex,
    Fragment, // aka pixel shader
    Compute,
    TesselationControl,    // aka hull shader
    TesselationEvaluation, // aka domain shader
    Mesh,
    Task, // aka amplification shader
    Geometry,
    RayGeneration,
    ClosestHit,
    AnyHit,
    Callable,
    Miss,
    Intersect,
}

impl ShaderStage {
    /// Get a stage from its filename. Mostly follow glslang guideline
    pub fn from_file_name(file_name: &String) -> Option<ShaderStage> {
        // TODO: add control for these
        let paths = HashMap::from([
            ("vert", ShaderStage::Vertex),
            ("frag", ShaderStage::Fragment),
            ("comp", ShaderStage::Compute),
            ("task", ShaderStage::Task),
            ("mesh", ShaderStage::Mesh),
            ("tesc", ShaderStage::TesselationControl),
            ("tese", ShaderStage::TesselationEvaluation),
            ("geom", ShaderStage::Geometry),
            ("rgen", ShaderStage::RayGeneration),
            ("rchit", ShaderStage::ClosestHit),
            ("rahit", ShaderStage::AnyHit),
            ("rcall", ShaderStage::Callable),
            ("rmiss", ShaderStage::Miss),
            ("rint", ShaderStage::Intersect),
        ]);
        let extension_list = file_name.rsplit(".");
        for extension in extension_list {
            if let Some(stage) = paths.get(extension) {
                return Some(stage.clone());
            } else {
                continue;
            }
        }
        // For header files & undefined, will output issue with missing version...
        None
    }
    pub const fn as_mask(&self) -> ShaderStageMask {
        match self {
            ShaderStage::Vertex => ShaderStageMask::VERTEX,
            ShaderStage::Fragment => ShaderStageMask::FRAGMENT,
            ShaderStage::Compute => ShaderStageMask::COMPUTE,
            ShaderStage::TesselationControl => ShaderStageMask::TESSELATION_CONTROL,
            ShaderStage::TesselationEvaluation => ShaderStageMask::TESSELATION_EVALUATION,
            ShaderStage::Mesh => ShaderStageMask::MESH,
            ShaderStage::Task => ShaderStageMask::TASK,
            ShaderStage::Geometry => ShaderStageMask::GEOMETRY,
            ShaderStage::RayGeneration => ShaderStageMask::RAY_GENERATION,
            ShaderStage::ClosestHit => ShaderStageMask::CLOSEST_HIT,
            ShaderStage::AnyHit => ShaderStageMask::ANY_HIT,
            ShaderStage::Callable => ShaderStageMask::CALLABLE,
            ShaderStage::Miss => ShaderStageMask::MISS,
            ShaderStage::Intersect => ShaderStageMask::INTERSECT,
        }
    }
    /// All graphics pipeline stages.
    pub fn graphics() -> ShaderStageMask {
        ShaderStageMask::VERTEX
            | ShaderStageMask::FRAGMENT
            | ShaderStageMask::GEOMETRY
            | ShaderStageMask::TESSELATION_CONTROL
            | ShaderStageMask::TESSELATION_EVALUATION
            | ShaderStageMask::TASK
            | ShaderStageMask::MESH
    }
    /// All compute pipeline stages.
    pub fn compute() -> ShaderStageMask {
        ShaderStageMask::COMPUTE
    }
    /// All raytracing pipeline stages.
    pub fn raytracing() -> ShaderStageMask {
        ShaderStageMask::RAY_GENERATION
            | ShaderStageMask::INTERSECT
            | ShaderStageMask::CLOSEST_HIT
            | ShaderStageMask::ANY_HIT
            | ShaderStageMask::MISS
            | ShaderStageMask::INTERSECT
    }
}

impl FromStr for ShaderStage {
    type Err = ();

    fn from_str(input: &str) -> Result<ShaderStage, Self::Err> {
        // Be case insensitive for parsing.
        let lower_input = input.to_lowercase();
        match lower_input.as_str() {
            "vertex" => Ok(ShaderStage::Vertex),
            "fragment" | "pixel" => Ok(ShaderStage::Fragment),
            "compute" => Ok(ShaderStage::Compute),
            "tesselationcontrol" | "hull" => Ok(ShaderStage::TesselationControl),
            "tesselationevaluation" | "domain" => Ok(ShaderStage::TesselationEvaluation),
            "mesh" => Ok(ShaderStage::Mesh),
            "task" | "amplification" => Ok(ShaderStage::Task),
            "geometry" => Ok(ShaderStage::Geometry),
            "raygeneration" => Ok(ShaderStage::RayGeneration),
            "closesthit" => Ok(ShaderStage::ClosestHit),
            "anyhit" => Ok(ShaderStage::AnyHit),
            "callable" => Ok(ShaderStage::Callable),
            "miss" => Ok(ShaderStage::Miss),
            "intersect" => Ok(ShaderStage::Intersect),
            _ => Err(()),
        }
    }
}
impl ToString for ShaderStage {
    fn to_string(&self) -> String {
        match self {
            ShaderStage::Vertex => "vertex".to_string(),
            ShaderStage::Fragment => "fragment".to_string(),
            ShaderStage::Compute => "compute".to_string(),
            ShaderStage::TesselationControl => "tesselationcontrol".to_string(),
            ShaderStage::TesselationEvaluation => "tesselationevaluation".to_string(),
            ShaderStage::Mesh => "mesh".to_string(),
            ShaderStage::Task => "task".to_string(),
            ShaderStage::Geometry => "geometry".to_string(),
            ShaderStage::RayGeneration => "raygeneration".to_string(),
            ShaderStage::ClosestHit => "closesthit".to_string(),
            ShaderStage::AnyHit => "anyhit".to_string(),
            ShaderStage::Callable => "callable".to_string(),
            ShaderStage::Miss => "miss".to_string(),
            ShaderStage::Intersect => "intersect".to_string(),
        }
    }
}

impl FromStr for ShadingLanguage {
    type Err = ();

    fn from_str(input: &str) -> Result<ShadingLanguage, Self::Err> {
        match input {
            "wgsl" => Ok(ShadingLanguage::Wgsl),
            "hlsl" => Ok(ShadingLanguage::Hlsl),
            "glsl" => Ok(ShadingLanguage::Glsl),
            _ => Err(()),
        }
    }
}
impl ToString for ShadingLanguage {
    fn to_string(&self) -> String {
        String::from(match &self {
            ShadingLanguage::Wgsl => "wgsl",
            ShadingLanguage::Hlsl => "hlsl",
            ShadingLanguage::Glsl => "glsl",
        })
    }
}

/// Generic tag to define a language to be used in template situations
pub trait ShadingLanguageTag {
    /// Get the language of the tag.
    fn get_language() -> ShadingLanguage;
}

/// Hlsl tag
pub struct HlslShadingLanguageTag {}
impl ShadingLanguageTag for HlslShadingLanguageTag {
    fn get_language() -> ShadingLanguage {
        ShadingLanguage::Hlsl
    }
}
/// Glsl tag
pub struct GlslShadingLanguageTag {}
impl ShadingLanguageTag for GlslShadingLanguageTag {
    fn get_language() -> ShadingLanguage {
        ShadingLanguage::Glsl
    }
}
/// Wgsl tag
pub struct WgslShadingLanguageTag {}
impl ShadingLanguageTag for WgslShadingLanguageTag {
    fn get_language() -> ShadingLanguage {
        ShadingLanguage::Wgsl
    }
}

/// All HLSL shader model existing.
///
/// Note that DXC only support shader model up to 6.0, and FXC is not supported.
/// So shader model below 6 are only present for documentation purpose.
#[derive(Debug, Default, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum HlslShaderModel {
    ShaderModel1,
    ShaderModel1_1,
    ShaderModel1_2,
    ShaderModel1_3,
    ShaderModel1_4,
    ShaderModel2,
    ShaderModel3,
    ShaderModel4,
    ShaderModel4_1,
    ShaderModel5,
    ShaderModel5_1,
    ShaderModel6,
    ShaderModel6_1,
    ShaderModel6_2,
    ShaderModel6_3,
    ShaderModel6_4,
    ShaderModel6_5,
    ShaderModel6_6,
    ShaderModel6_7,
    #[default]
    ShaderModel6_8,
}

impl HlslShaderModel {
    /// Get first shader model version
    pub fn earliest() -> HlslShaderModel {
        HlslShaderModel::ShaderModel1
    }
    /// Get last shader model version
    pub fn latest() -> HlslShaderModel {
        HlslShaderModel::ShaderModel6_8
    }
}

/// All HLSL version supported
#[derive(Debug, Default, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum HlslVersion {
    V2016,
    V2017,
    V2018,
    #[default]
    V2021,
}

/// Hlsl compilation parameters for DXC.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct HlslCompilationParams {
    pub shader_model: HlslShaderModel,
    pub version: HlslVersion,
    pub enable16bit_types: bool,
    pub spirv: bool,
}

/// Glsl target client
#[derive(Debug, Default, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum GlslTargetClient {
    Vulkan1_0,
    Vulkan1_1,
    Vulkan1_2,
    #[default]
    Vulkan1_3,
    OpenGL450,
}

impl GlslTargetClient {
    /// Check if glsl is for OpenGL or Vulkan
    pub fn is_opengl(&self) -> bool {
        match *self {
            GlslTargetClient::OpenGL450 => true,
            _ => false,
        }
    }
}

/// All SPIRV version supported for glsl
#[derive(Debug, Default, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum GlslSpirvVersion {
    SPIRV1_0,
    SPIRV1_1,
    SPIRV1_2,
    SPIRV1_3,
    SPIRV1_4,
    SPIRV1_5,
    #[default]
    SPIRV1_6,
}
/// Glsl compilation parameters for glslang.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct GlslCompilationParams {
    pub client: GlslTargetClient,
    pub spirv: GlslSpirvVersion,
    pub preamble: String,
}

/// Wgsl compilation parameters for naga.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct WgslCompilationParams {}

/// Parameters for includes.
#[derive(Default, Debug, Clone)]
pub struct ShaderContextParams {
    pub defines: HashMap<String, String>,
    pub includes: Vec<PathBuf>,
    pub path_remapping: HashMap<PathBuf, PathBuf>,
}

/// Parameters for compilation
#[derive(Default, Debug, Clone)]
pub struct ShaderCompilationParams {
    pub entry_point: Option<String>,
    pub shader_stage: Option<ShaderStage>,
    pub experimental_macro_expansion: bool,
    pub hlsl: HlslCompilationParams,
    pub glsl: GlslCompilationParams,
    pub wgsl: WgslCompilationParams,
}

/// Generic parameters passed to validation and inspection.
#[derive(Default, Debug, Clone)]
pub struct ShaderParams {
    pub context: ShaderContextParams,
    pub compilation: ShaderCompilationParams,
}
