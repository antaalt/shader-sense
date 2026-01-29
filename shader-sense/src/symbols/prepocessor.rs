//! Preprocessor API

use std::{
    collections::HashSet,
    path::{Path, PathBuf},
};

use crate::{
    include::IncludeHandler,
    position::{ShaderFileRange, ShaderRange},
    shader::ShaderContextParams,
    shader_error::ShaderDiagnostic,
    symbols::{
        shader_module::ShaderSymbols,
        symbol_list::{ShaderSymbolList, ShaderSymbolListRef},
        symbols::{
            ShaderSymbol, ShaderSymbolData, ShaderSymbolMode, ShaderSymbolRuntime,
            ShaderSymbolRuntimeContext,
        },
    },
};

#[derive(Debug, Default, Clone)]
pub struct ShaderRegion {
    pub range: ShaderRange,
    // Could add some ShaderRegionType::Condition / ShaderRegionType::User...
    pub is_active: bool, // Is this region passing preprocess
}

impl ShaderRegion {
    pub fn new(range: ShaderRange, is_active: bool) -> Self {
        Self { range, is_active }
    }
}

#[derive(Debug, Default, Clone)]
pub struct ShaderPreprocessorContext {
    defines: Vec<ShaderSymbol>,
    include_handler: IncludeHandler,
    dirty_files: HashSet<PathBuf>, // Dirty files that need to be recomputed no matter what.
    depth: usize,
}

impl ShaderPreprocessorContext {
    pub fn main(file_path: &Path, shader_params: ShaderContextParams) -> Self {
        Self {
            defines: shader_params
                .defines
                .iter()
                .map(|(key, value)| ShaderSymbol {
                    label: key.clone(),
                    requirement: None,
                    data: ShaderSymbolData::Macro {
                        value: value.clone(),
                        parameters: vec![],
                    },
                    mode: ShaderSymbolMode::RuntimeContext(ShaderSymbolRuntimeContext::new()),
                })
                .collect(),
            include_handler: IncludeHandler::main(
                &file_path,
                shader_params.includes,
                shader_params.path_remapping,
            ),
            dirty_files: HashSet::new(),
            depth: 0,
        }
    }
    pub fn mark_dirty(&mut self, file_path: PathBuf) {
        self.dirty_files.insert(file_path);
    }
    pub fn search_path_in_includes(&mut self, path: &Path) -> Option<PathBuf> {
        self.include_handler.search_path_in_includes(path)
    }
    pub fn push_directory_stack(&mut self, canonical_path: &Path) {
        self.include_handler.push_directory_stack(canonical_path);
    }
    pub fn append_defines(&mut self, defines: Vec<ShaderPreprocessorDefine>) {
        self.defines
            .extend(defines.iter().map(|define| define.symbol.clone()));
    }
    pub fn increase_depth(&mut self) -> bool {
        if self.depth < IncludeHandler::DEPTH_LIMIT {
            self.depth += 1;
            true
        } else {
            false
        }
    }
    pub fn decrease_depth(&mut self) {
        assert!(self.depth > 0, "Decreasing depth but zero.");
        self.depth -= 1;
    }
    pub fn get_visited_count(&mut self, path: &Path) -> usize {
        self.include_handler.get_visited_count(path)
    }
    pub fn is_dirty(&self, file_path: &Path, context: &ShaderPreprocessorContext) -> bool {
        // Compare defines to determine if context is different.
        // Check if we need to force an update aswell.
        fn are_defines_equal(lhs: &Vec<ShaderSymbol>, rhs: &Vec<ShaderSymbol>) -> bool {
            if lhs.len() != rhs.len() {
                return false;
            }
            for lhs_symbol in lhs.iter() {
                if rhs
                    .iter()
                    .find(|rhs_symbol| {
                        lhs_symbol.label == rhs_symbol.label
                            && match (&lhs_symbol.data, &rhs_symbol.data) {
                                (
                                    ShaderSymbolData::Macro {
                                        value: l_value,
                                        parameters: l_parameter,
                                    },
                                    ShaderSymbolData::Macro {
                                        value: r_value,
                                        parameters: r_parameter,
                                    },
                                ) => l_value == r_value || l_parameter == r_parameter,
                                _ => false,
                            }
                    })
                    .is_none()
                {
                    return false;
                }
            }
            true
        }
        fn are_includes_equal(lhs: &HashSet<PathBuf>, rhs: &HashSet<PathBuf>) -> bool {
            if lhs.len() != rhs.len() {
                return false;
            }
            for lhs_symbol in lhs.iter() {
                if rhs
                    .iter()
                    .find(|rhs_symbol| lhs_symbol.as_os_str() == rhs_symbol.as_os_str())
                    .is_none()
                {
                    return false;
                }
            }
            true
        }
        !are_defines_equal(&context.defines, &self.defines)
            || !are_includes_equal(
                context.include_handler.get_includes(),
                self.include_handler.get_includes(),
            )
            || context.dirty_files.contains(file_path)
    }
    pub fn get_define_value(&self, name: &str) -> Option<String> {
        self.defines
            .iter()
            .find(|symbol| *symbol.label == *name)
            .map(|symbol| match &symbol.data {
                ShaderSymbolData::Macro {
                    value,
                    parameters: _,
                } => value.clone(),
                _ => panic!("Expected ShaderSymbolData::Macro"),
            })
    }
    pub fn get_defines(&self) -> &Vec<ShaderSymbol> {
        &self.defines
    }
}

#[derive(Debug, Clone)]
pub struct ShaderPreprocessorInclude {
    // TODO: move cache to symbol data
    pub cache: Option<ShaderSymbols>,
    symbol: ShaderSymbol,
}

#[derive(Debug, Clone)]
pub struct ShaderPreprocessorDefine {
    symbol: ShaderSymbol,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub enum ShaderPreprocessorMode {
    #[default]
    Default,
    Once,
    OnceVisited,
}

#[derive(Debug, Default, Clone)]
pub struct ShaderPreprocessor {
    pub context: ShaderPreprocessorContext, // Defines from includer files when included, or config.

    pub includes: Vec<ShaderPreprocessorInclude>,
    pub defines: Vec<ShaderPreprocessorDefine>,
    pub regions: Vec<ShaderRegion>,
    pub diagnostics: Vec<ShaderDiagnostic>, // preprocessor errors
    pub mode: ShaderPreprocessorMode,
}
impl ShaderPreprocessorDefine {
    pub fn new(
        name: String,
        range: ShaderFileRange,
        value: Option<String>,
        arguments: Option<Vec<String>>,
    ) -> Self {
        Self {
            symbol: ShaderSymbol {
                label: name.clone(),
                requirement: None,
                data: ShaderSymbolData::Macro {
                    value: match &value {
                        Some(value) => value.clone(),
                        None => "".into(),
                    },
                    parameters: arguments.unwrap_or_default(),
                },
                mode: ShaderSymbolMode::Runtime(ShaderSymbolRuntime::global(
                    range.file_path,
                    range.range,
                )),
            },
        }
    }
    pub fn get_file_path(&self) -> &Path {
        &self.symbol.mode.unwrap_runtime().file_path
    }
    pub fn get_range(&self) -> &ShaderRange {
        &self.symbol.mode.unwrap_runtime().range
    }
    pub fn get_name(&self) -> &String {
        &self.symbol.label
    }
    pub fn get_value(&self) -> Option<&String> {
        match &self.symbol.data {
            ShaderSymbolData::Macro {
                value,
                parameters: _,
            } => Some(value),
            _ => None,
        }
    }
}
impl ShaderPreprocessorInclude {
    pub fn new(relative_path: String, absolute_path: PathBuf, range: ShaderFileRange) -> Self {
        Self {
            cache: None,
            symbol: ShaderSymbol {
                label: relative_path,
                requirement: None,
                data: ShaderSymbolData::Include {
                    target: absolute_path,
                },
                mode: ShaderSymbolMode::Runtime(ShaderSymbolRuntime::global(
                    range.file_path,
                    range.range,
                )),
            },
        }
    }
    pub fn get_range(&self) -> &ShaderRange {
        &self.symbol.mode.unwrap_runtime().range
    }
    pub fn get_file_range(&self) -> ShaderFileRange {
        let runtime = self.symbol.mode.unwrap_runtime();
        runtime.range.clone_into_file(runtime.file_path.clone())
    }
    pub fn get_relative_path(&self) -> &String {
        &self.symbol.label
    }
    pub fn get_absolute_path(&self) -> &Path {
        match &self.symbol.data {
            ShaderSymbolData::Include { target } => &target,
            _ => panic!("Expected ShaderSymbolData::Link"),
        }
    }
    pub fn get_cache(&self) -> &ShaderSymbols {
        self.cache.as_ref().unwrap()
    }
    pub fn get_cache_mut(&mut self) -> &mut ShaderSymbols {
        self.cache.as_mut().unwrap()
    }
}

impl ShaderPreprocessor {
    pub fn new(context: ShaderPreprocessorContext) -> Self {
        Self {
            context: context,
            includes: Vec::new(),
            defines: Vec::new(),
            regions: Vec::new(),
            diagnostics: Vec::new(),
            mode: ShaderPreprocessorMode::default(),
        }
    }
    pub fn preprocess_symbols<'a>(
        &'a self,
        shader_symbols: &'a ShaderSymbolList,
    ) -> ShaderSymbolListRef<'a> {
        // Filter inactive regions symbols
        let inactive_regions: Vec<&ShaderRegion> =
            self.regions.iter().filter(|r| !r.is_active).collect();
        let mut preprocessed_symbols =
            shader_symbols.filter(move |_symbol_type, symbol| match &symbol.mode {
                ShaderSymbolMode::Runtime(runtime) => inactive_regions
                    .iter()
                    .find(|r| r.range.contain_bounds(&runtime.range))
                    .is_none(),
                ShaderSymbolMode::RuntimeContext(_) => true, // Global range
                ShaderSymbolMode::Intrinsic(_) => true,      // Global range
            });
        // Add defines
        let mut define_symbols: Vec<&ShaderSymbol> =
            self.defines.iter().map(|define| &define.symbol).collect();
        // Add includes as symbol
        let mut include_symbols: Vec<&ShaderSymbol> = self
            .includes
            .iter()
            .map(|include| &include.symbol)
            .collect();
        preprocessed_symbols.macros.append(&mut define_symbols);
        preprocessed_symbols.includes.append(&mut include_symbols);
        preprocessed_symbols
    }
}
