use std::{
    collections::HashMap,
    format,
    path::{Path, PathBuf},
};

use log::{info, warn};
use lsp_types::{request::WorkspaceConfiguration, ConfigurationParams, Url};
use serde::{Deserialize, Serialize};

use serde_json::Value;
use shader_sense::{
    include::canonicalize,
    shader::{
        GlslCompilationParams, GlslSpirvVersion, GlslTargetClient, HlslCompilationParams,
        HlslShaderModel, HlslVersion, ShaderCompilationParams, ShaderContextParams, ShaderParams,
        ShaderStage, ShadingLanguage, WgslCompilationParams,
    },
    shader_error::ShaderDiagnosticSeverity,
};

use crate::{
    profile_scope,
    server::{
        async_message::{AsyncCacheRequest, AsyncMessage},
        ServerLanguage,
    },
};

use super::shader_variant::ShaderVariant;

#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct ServerHlslConfig {
    pub shader_model: Option<HlslShaderModel>,
    pub version: Option<HlslVersion>,
    pub enable16bit_types: Option<bool>,
    pub spirv: Option<bool>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct ServerGlslConfig {
    pub target_client: Option<GlslTargetClient>,
    pub spirv_version: Option<GlslSpirvVersion>,
    pub preamble: Option<String>, // Path to a preamble file per language.
}

#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub enum ServerTraceLevel {
    #[default]
    Off,
    Messages,
    Verbose,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct ServerTrace {
    server: ServerTraceLevel,
}

impl ServerTrace {
    pub fn new(level: ServerTraceLevel) -> Self {
        Self { server: level }
    }
    pub fn is_verbose(&self) -> bool {
        self.server == ServerTraceLevel::Verbose
    }
}

/// Serialized configuration override that can be used for a specific engine for example (Unreal / Unity config).
// Only use option to allow non defined values.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct ServerSerializedConfigOverride {
    includes: Option<Vec<String>>,
    defines: Option<HashMap<String, String>>,
    path_remapping: Option<HashMap<String, String>>,
    stage_define: Option<HashMap<ShaderStage, HashMap<String, String>>>,
    hlsl: Option<ServerHlslConfig>,
    glsl: Option<ServerGlslConfig>,
}

/// Serialized configuration for the server to be sent through workspace/configuration lsp request or as input when starting the server.
// Only use option to allow non defined values.
#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct ServerSerializedConfig {
    includes: Option<Vec<String>>,            // Includes folder to check
    defines: Option<HashMap<String, String>>, // Defines to set
    path_remapping: Option<HashMap<String, String>>, // Virtual path remapping
    validate: Option<bool>,                   // Validation via standard API
    symbols: Option<bool>,                    // Query symbols
    symbol_diagnostics: Option<bool>,         // Debug option to visualise issues with tree-sitter
    validate_config: Option<bool>,            // Validate user config
    automatic_variant_discovery: Option<bool>, // Reuse a dependent main-file context for document diagnostics.
    experimental_macro_expansion: Option<bool>, // Experimental test for the new feature.
    stage_define: Option<HashMap<ShaderStage, HashMap<String, String>>>, // Specific macro defined per shader stage
    trace: Option<ServerTrace>,      // Level of error to display
    severity: Option<String>,        // Severity of diagnostic to display
    config_override: Option<String>, // Override configuration file
    hlsl: Option<ServerHlslConfig>,  // Hlsl specific configuration
    glsl: Option<ServerGlslConfig>,  // Glsl specific configuration
}

/// Configuration computed from both server configuration and engine configuration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ServerConfig {
    includes: Vec<PathBuf>,
    defines: HashMap<String, String>,
    path_remapping: HashMap<PathBuf, PathBuf>,
    stage_define: HashMap<ShaderStage, HashMap<String, String>>,
    validate: bool,
    symbols: bool,
    symbol_diagnostics: bool,
    automatic_variant_discovery: bool,
    experimental_macro_expansion: bool,
    trace: ServerTrace,
    severity: ShaderDiagnosticSeverity,
    hlsl: HlslCompilationParams,
    glsl: GlslCompilationParams,
    wgsl: WgslCompilationParams,
}

impl ServerSerializedConfig {
    fn verify_user_path(path: &str) -> PathBuf {
        // Try to canonicalize path.
        // If it fail, still return it to avoid crashing server with invalid config.
        canonicalize(Path::new(&path)).unwrap_or_else(|err| {
            warn!("Failed to canonicalize setting path {}", err);
            PathBuf::from(path)
        })
    }
    pub fn validate(&self) -> Result<(), Vec<String>> {
        // Validation is opt-out, so default is validate, unless specified otherwise.
        if let Some(validate_config) = self.validate_config {
            if !validate_config {
                return Ok(());
            }
        }
        let mut errors = Vec::new();
        if let Some(glsl) = &self.glsl {
            // Validate preamble path.
            if let Some(preamble) = &glsl.preamble {
                let preamble_path = Self::verify_user_path(preamble);
                if let Ok(exist) = std::fs::exists(preamble_path) {
                    if !exist {
                        errors.push(format!("Preamble file at {:#?} not found", preamble));
                    }
                } else {
                    errors.push(format!("Preamble file at {:#?} not found", preamble));
                }
            }
            if let Some(target_client) = &glsl.target_client {
                match target_client {
                    GlslTargetClient::Vulkan1_0
                    | GlslTargetClient::Vulkan1_1
                    | GlslTargetClient::Vulkan1_2
                    | GlslTargetClient::Vulkan1_3 => {
                        if let Some(spirv_version) = &glsl.spirv_version {
                            if *spirv_version == GlslSpirvVersion::None {
                                errors.push(format!(
                                    "No SPIRV version set, but required for Vulkan client."
                                ));
                            }
                        } else {
                            // Default version not None, so its fine.
                        }
                    }
                    _ => {}
                }
            }
        }
        if let Some(config_override) = &self.config_override {
            let config_override_path = Self::verify_user_path(config_override);
            if let Ok(exist) = std::fs::exists(config_override_path) {
                if !exist {
                    errors.push(format!(
                        "Config override file at {:#?} not found",
                        config_override
                    ));
                }
            } else {
                errors.push(format!(
                    "Config override file at {:#?} not found",
                    config_override
                ));
            }
        }
        if let Some(includes) = &self.includes {
            for include in includes {
                let include_path = Self::verify_user_path(include);
                if let Ok(exist) = std::fs::exists(include_path) {
                    if !exist {
                        errors.push(format!("Include folder at {:#?} not found", include));
                    }
                } else {
                    errors.push(format!("Include folder at {:#?} not found", include));
                }
            }
        }
        if let Some(path_remappings) = &self.path_remapping {
            for (_virtual_path, path) in path_remappings {
                let verified_path = Self::verify_user_path(path);
                if let Ok(exist) = std::fs::exists(verified_path) {
                    if !exist {
                        errors.push(format!("Virtual path folder at {:#?} not found", path));
                    }
                } else {
                    errors.push(format!("Virtual path folder at {:#?} not found", path));
                }
            }
        }
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
    pub fn compute_engine_config(self, previous_config: ServerConfig) -> ServerConfig {
        // Convert ServerConfig to ServerEngineConfig
        let mut config = ServerConfig {
            includes: self
                .includes
                .map(|i| i.into_iter().map(|i| Self::verify_user_path(&i)).collect())
                .unwrap_or(previous_config.includes),
            defines: self.defines.unwrap_or(previous_config.defines),
            path_remapping: self
                .path_remapping
                .map(|i| {
                    i.into_iter()
                        .map(|(v, i)| (PathBuf::from(v), Self::verify_user_path(&i)))
                        .collect()
                })
                .unwrap_or(previous_config.path_remapping),
            experimental_macro_expansion: self
                .experimental_macro_expansion
                .unwrap_or(previous_config.experimental_macro_expansion),
            validate: self.validate.unwrap_or(previous_config.validate),
            symbols: self.symbols.unwrap_or(previous_config.symbols),
            symbol_diagnostics: self
                .symbol_diagnostics
                .unwrap_or(previous_config.symbol_diagnostics),
            automatic_variant_discovery: self
                .automatic_variant_discovery
                .unwrap_or(previous_config.automatic_variant_discovery),
            trace: self.trace.unwrap_or(previous_config.trace),
            stage_define: self.stage_define.unwrap_or(previous_config.stage_define),
            severity: self
                .severity
                .map(|s| ShaderDiagnosticSeverity::from(s.as_str()))
                .unwrap_or(previous_config.severity),
            hlsl: self
                .hlsl
                .map(|hlsl| HlslCompilationParams {
                    shader_model: hlsl
                        .shader_model
                        .unwrap_or(previous_config.hlsl.shader_model),
                    version: hlsl.version.unwrap_or(previous_config.hlsl.version),
                    enable16bit_types: hlsl
                        .enable16bit_types
                        .unwrap_or(previous_config.hlsl.enable16bit_types),
                    spirv: hlsl.spirv.unwrap_or(previous_config.hlsl.spirv),
                })
                .unwrap_or(previous_config.hlsl),
            glsl: self
                .glsl
                .map(|glsl| GlslCompilationParams {
                    client: glsl.target_client.unwrap_or(previous_config.glsl.client),
                    spirv: glsl.spirv_version.unwrap_or(previous_config.glsl.spirv),
                    preamble_path: glsl
                        .preamble
                        .map(|p| Self::verify_user_path(&p))
                        .or(previous_config.glsl.preamble_path.clone()),
                    preamble_content: None, // Loaded later to be up to date
                })
                .unwrap_or(previous_config.glsl),
            wgsl: WgslCompilationParams {},
        };
        // Get engine config if set and override them.
        if let Some(config_override) = self.config_override {
            if config_override.is_empty() {
                return config;
            }
            let config_override_path = Self::verify_user_path(&config_override);
            let settings = match std::fs::read_to_string(&config_override_path) {
                Ok(setting) => setting,
                Err(err) => {
                    warn!(
                        "Failed to read engine settings at {:?}: {}",
                        config_override_path, err
                    );
                    return config;
                }
            };
            let override_config =
                match serde_json::from_str::<ServerSerializedConfigOverride>(&settings) {
                    Ok(setting) => setting,
                    Err(err) => {
                        warn!(
                            "Failed to parse engine settings at {:?}: {}",
                            config_override_path, err
                        );
                        return config;
                    }
                };
            // Merge config with settings.
            config
                .defines
                .extend(override_config.defines.unwrap_or_default());
            config.includes.extend(
                override_config
                    .includes
                    .map(|i| {
                        i.into_iter()
                            .map(|i| Self::verify_user_path(&i))
                            .collect::<Vec<PathBuf>>()
                    })
                    .unwrap_or_default(),
            );
            config.path_remapping.extend(
                override_config
                    .path_remapping
                    .map(|i| {
                        i.into_iter()
                            .map(|(v, i)| (PathBuf::from(v), Self::verify_user_path(&i)))
                            .collect::<HashMap<PathBuf, PathBuf>>()
                    })
                    .unwrap_or_default(),
            );
            config
                .stage_define
                .extend(override_config.stage_define.unwrap_or_default());
            if let Some(override_glsl) = override_config.glsl {
                if let Some(spirv_version) = override_glsl.spirv_version {
                    config.glsl.spirv = spirv_version;
                }
                if let Some(target_client) = override_glsl.target_client {
                    config.glsl.client = target_client;
                }
            }
            if let Some(override_hlsl) = override_config.hlsl {
                if let Some(version) = override_hlsl.version {
                    config.hlsl.version = version;
                }
                if let Some(shader_model) = override_hlsl.shader_model {
                    config.hlsl.shader_model = shader_model;
                }
                if let Some(enable16bit_types) = override_hlsl.enable16bit_types {
                    config.hlsl.enable16bit_types = enable16bit_types;
                }
                if let Some(spirv) = override_hlsl.spirv {
                    config.hlsl.spirv = spirv;
                }
            }
            config
        } else {
            config
        }
    }
}

impl ServerConfig {
    pub const DEFAULT_SYMBOLS: bool = true;
    pub const DEFAULT_VALIDATE: bool = true;
    pub const DEFAULT_SYMBOL_DIAGNOSTIC: bool = false; // Mostly for debug
    pub const DEFAULT_AUTOMATIC_VARIANT_DISCOVERY: bool = false;
    pub const DEFAULT_EXPERIMENTAL_MACRO_EXPANSION: bool = false;
    pub const DEFAULT_SEVERITY: ShaderDiagnosticSeverity = ShaderDiagnosticSeverity::Error;
    pub const DEFAULT_TRACE: ServerTrace = ServerTrace {
        server: ServerTraceLevel::Off,
    };

    pub fn into_shader_params(
        &self,
        workspace_folder: Option<&Url>,
        variant: Option<ShaderVariant>,
    ) -> ShaderParams {
        let (mut defines, mut includes, entry_point, shader_stage) = match variant {
            Some(variant) => (
                variant.defines.clone(),
                variant.includes.clone(),
                Some(variant.entry_point),
                variant.stage,
            ),
            None => (HashMap::new(), Vec::new(), None, None),
        };
        defines.extend(self.defines.clone());
        includes.extend(self.includes.clone());
        // Insert workspace folder at start for cwd.
        if let Some(workspace_folder) = workspace_folder {
            includes.insert(0, workspace_folder.to_file_path().unwrap());
        }
        let hlsl = self.hlsl.clone();
        let glsl = if let Some(preamble_path) = &self.glsl.preamble_path {
            let mut glsl = self.glsl.clone();
            // TODO: preamble content can be outdated if not saved.
            glsl.preamble_content = std::fs::read_to_string(preamble_path).ok();
            glsl
        } else {
            self.glsl.clone()
        };
        let wgsl = self.wgsl.clone();
        // Add stage defines
        let stage_defines = if let Some(shader_stage) = &shader_stage {
            match self.stage_define.get(shader_stage) {
                Some(stage_defines) => stage_defines.clone(),
                None => HashMap::new(),
            }
        } else {
            HashMap::new()
        };
        defines.extend(stage_defines);
        ShaderParams {
            context: ShaderContextParams {
                defines,
                includes,
                path_remapping: self.path_remapping.clone(),
            },
            compilation: ShaderCompilationParams {
                entry_point,
                shader_stage,
                experimental_macro_expansion: self.experimental_macro_expansion,
                hlsl: hlsl,
                glsl: glsl,
                wgsl: wgsl,
            },
        }
    }
    pub fn get_validate(&self) -> bool {
        self.validate
    }
    pub fn get_symbols(&self) -> bool {
        self.symbols
    }
    pub fn get_symbol_diagnostics(&self) -> bool {
        self.symbol_diagnostics
    }
    pub fn get_automatic_variant_discovery(&self) -> bool {
        self.automatic_variant_discovery
    }
    pub fn is_verbose(&self) -> bool {
        self.trace.is_verbose()
    }
    pub fn get_severity(&self) -> ShaderDiagnosticSeverity {
        self.severity.clone() // TODO: ref
    }
    pub fn get_glsl_preamble_path(&self) -> Option<&PathBuf> {
        self.glsl.preamble_path.as_ref()
    }
    pub fn is_generating_spirv(&self, shading_language: ShadingLanguage) -> bool {
        match shading_language {
            ShadingLanguage::Wgsl => false,
            ShadingLanguage::Hlsl => self.hlsl.spirv,
            ShadingLanguage::Glsl => self.glsl.spirv != GlslSpirvVersion::None,
        }
    }
    pub fn set_trace(&mut self, trace: ServerTrace) {
        self.trace = trace
    }
}

impl Default for ServerConfig {
    fn default() -> Self {
        Self {
            includes: Vec::new(),
            defines: HashMap::new(),
            path_remapping: HashMap::new(),
            validate: ServerConfig::DEFAULT_VALIDATE,
            symbols: ServerConfig::DEFAULT_SYMBOLS,
            automatic_variant_discovery: ServerConfig::DEFAULT_AUTOMATIC_VARIANT_DISCOVERY,
            experimental_macro_expansion: ServerConfig::DEFAULT_EXPERIMENTAL_MACRO_EXPANSION,
            stage_define: HashMap::new(),
            symbol_diagnostics: ServerConfig::DEFAULT_SYMBOL_DIAGNOSTIC,
            trace: ServerConfig::DEFAULT_TRACE,
            severity: ServerConfig::DEFAULT_SEVERITY,
            hlsl: HlslCompilationParams::default(),
            glsl: GlslCompilationParams::default(),
            wgsl: WgslCompilationParams::default(),
        }
    }
}

impl ServerLanguage {
    pub fn request_configuration(&mut self) {
        let config = ConfigurationParams {
            items: vec![lsp_types::ConfigurationItem {
                scope_uri: None,
                section: Some("shader-validator".to_owned()),
            }],
        };
        self.connection.send_request::<WorkspaceConfiguration>(
            config,
            |server: &mut ServerLanguage, value: Value| {
                // Sent 1 item, received 1 in an array
                let mut parsed_config: Vec<Option<ServerSerializedConfig>> =
                    serde_json::from_value(value)?;
                let serialized_config = parsed_config.remove(0).unwrap_or_default();
                if let Err(errors) = serialized_config.validate() {
                    server.connection.send_notification_error(format!(
                        "Config received is invalid:\n{}",
                        errors.join("\n")
                    ));
                }
                let config = serialized_config.compute_engine_config(server.config.clone());
                if server.config != config {
                    profile_scope!("Updating server config: {:#?}", config);
                    server.config = config.clone();
                    // Republish all diagnostics
                    let async_updates: Vec<AsyncCacheRequest> = server
                        .watched_files
                        .files
                        .iter()
                        .filter(|(_, file)| file.is_cachable_file())
                        .map(|(url, cached_file)| {
                            // Mark dirty to force revalidation on setting changes.
                            AsyncCacheRequest::new(url.clone(), cached_file.shading_language, true)
                        })
                        .collect();
                    Ok(AsyncMessage::UpdateCache(async_updates))
                } else {
                    info!("Requested configuration has not changed.");
                    Ok(AsyncMessage::None)
                }
            },
        );
    }
}

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, path::PathBuf};

    use lsp_types::Url;
    use shader_sense::shader::{GlslSpirvVersion, GlslTargetClient, ShaderStage, ShadingLanguage};

    use crate::server::{
        server_config::{ServerConfig, ServerGlslConfig, ServerSerializedConfig},
        shader_variant::ShaderVariant,
    };

    #[test]
    fn test_empty_config() {
        let cfg: ServerSerializedConfig = serde_json::from_str("{}").unwrap();
        let cfg = cfg.compute_engine_config(ServerConfig::default());
        assert!(cfg.get_validate() == ServerConfig::DEFAULT_VALIDATE);
        let cfg_inverse: ServerSerializedConfig = serde_json::from_str(
            format!(
                "{{\"validate\": {}}}",
                if ServerConfig::DEFAULT_VALIDATE {
                    "false"
                } else {
                    "true"
                }
            )
            .as_str(),
        )
        .unwrap();
        let cfg_inverse = cfg_inverse.compute_engine_config(ServerConfig::default());
        assert!(cfg_inverse.get_validate() == !ServerConfig::DEFAULT_VALIDATE);
    }

    #[test]
    fn test_default_config() {
        let cfg: ServerSerializedConfig = serde_json::from_str(
            r#"{
            "stageDefine": {
                "vertex": { "MY_MACRO_VERTEX":"1"}
            }
        }"#,
        )
        .unwrap();
        let cfg = cfg.compute_engine_config(ServerConfig::default());
        let vertex_shader_params = cfg.into_shader_params(
            None,
            Some(ShaderVariant {
                url: Url::parse("file://test").unwrap(),
                stage: Some(ShaderStage::Vertex),
                shading_language: ShadingLanguage::Hlsl,
                entry_point: "".into(),
                defines: HashMap::new(),
                includes: Vec::new(),
            }),
        );
        let compute_shader_params = cfg.into_shader_params(
            None,
            Some(ShaderVariant {
                url: Url::parse("file://test").unwrap(),
                stage: Some(ShaderStage::Compute),
                shading_language: ShadingLanguage::Hlsl,
                entry_point: "".into(),
                defines: HashMap::new(),
                includes: Vec::new(),
            }),
        );
        assert!(vertex_shader_params
            .context
            .defines
            .contains_key("MY_MACRO_VERTEX"));
        assert!(
            vertex_shader_params
                .context
                .defines
                .get("MY_MACRO_VERTEX")
                .unwrap()
                == "1"
        );
        assert!(!compute_shader_params
            .context
            .defines
            .contains_key("MY_MACRO_VERTEX"));
    }

    #[test]
    fn test_stage_define() {
        let cfg = ServerSerializedConfig::default();
        let cfg = cfg.compute_engine_config(ServerConfig::default());
        assert!(cfg.get_symbols() == ServerConfig::DEFAULT_SYMBOLS);
        assert!(cfg.get_validate() == ServerConfig::DEFAULT_VALIDATE);
        assert!(cfg.get_symbol_diagnostics() == ServerConfig::DEFAULT_SYMBOL_DIAGNOSTIC);
        assert!(
            cfg.get_automatic_variant_discovery()
                == ServerConfig::DEFAULT_AUTOMATIC_VARIANT_DISCOVERY
        );
        assert!(cfg.is_verbose() == ServerConfig::DEFAULT_TRACE.is_verbose());
        assert!(cfg.get_severity() == ServerConfig::DEFAULT_SEVERITY);
    }

    #[test]
    #[cfg(not(target_os = "wasi"))] // File not in right workspace.
    fn test_engine_config() {
        // compute_engine_config does not return error and try to recover if invalid content.
        // To check if it passed successfully, we need to check if no logs were outputed.
        struct TestLogger;
        impl log::Log for TestLogger {
            fn enabled(&self, metadata: &log::Metadata) -> bool {
                metadata.level() < log::Level::Info
            }
            fn log(&self, record: &log::Record) {
                if self.enabled(record.metadata()) {
                    assert!(
                        false,
                        "Did not expected any logs. but got : {} - {}",
                        record.level(),
                        record.args()
                    );
                }
            }
            fn flush(&self) {}
        }
        static LOGGER: TestLogger = TestLogger;
        log::set_logger(&LOGGER)
            .map(|_| log::set_max_level(log::LevelFilter::Warn))
            .unwrap();
        let cfg = ServerSerializedConfig {
            includes: Some(vec!["D:/other/path/to/my/include".into()]),
            config_override: Some("../shader-sense/test/config-override.json".into()),
            ..Default::default()
        };
        let cfg = cfg.compute_engine_config(ServerConfig::default());
        assert!(cfg.includes.len() == 2);
        assert!(cfg.includes[0] == PathBuf::from("D:/other/path/to/my/include"));
        assert!(cfg.includes[1] == PathBuf::from("D:/path/to/my/include"));
        assert!(*cfg.defines.get("MY_MACRO").unwrap() == String::from("1"));
    }

    #[test]
    fn test_config_validation() {
        let invalid_config = ServerSerializedConfig {
            glsl: Some(ServerGlslConfig {
                target_client: Some(GlslTargetClient::Vulkan1_3),
                spirv_version: Some(GlslSpirvVersion::None),
                ..Default::default()
            }),
            ..Default::default()
        };
        let result = invalid_config.validate();
        assert!(result.is_err());
        let valid_config = ServerSerializedConfig {
            glsl: Some(ServerGlslConfig {
                target_client: Some(GlslTargetClient::Vulkan1_3),
                spirv_version: Some(GlslSpirvVersion::SPIRV1_6),
                ..Default::default()
            }),
            ..Default::default()
        };
        let result = valid_config.validate();
        assert!(result.is_ok());
    }
}
