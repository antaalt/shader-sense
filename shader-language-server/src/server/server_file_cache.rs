use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
    rc::Rc,
};

use crate::{
    profile_scope,
    server::{
        async_message::AsyncCacheRequest,
        clean_url,
        common::{lsp_range_to_shader_range, read_string_lossy},
        server_language_data::ServerLanguageData,
    },
};
use log::{debug, info, warn};
use lsp_types::Url;
use shader_sense::{
    position::ShaderFileRange,
    shader::ShadingLanguage,
    shader_error::{ShaderDiagnostic, ShaderDiagnosticList, ShaderDiagnosticSeverity, ShaderError},
    symbols::{
        intrinsics::ShaderIntrinsics,
        prepocessor::ShaderPreprocessorContext,
        shader_module::{ShaderModuleHandle, ShaderSymbols},
        shader_module_parser::ShaderModuleParser,
        symbol_list::ShaderSymbolListRef,
        symbol_provider::SymbolProvider,
    },
    validator::validator::ValidatorImpl,
};

use super::{server_config::ServerConfig, shader_variant::ShaderVariant};

#[derive(Debug, Clone, Default)]
pub struct ServerFileCacheData {
    pub symbol_cache: ShaderSymbols, // Store symbols to avoid computing them at every change.
    pub intrinsics: ShaderSymbolListRef<'static>, // Cached intrinsics to not recompute them everytime
    pub diagnostic_cache: ShaderDiagnosticList,   // Cached diagnostic
}

#[derive(Debug, Clone)]
pub struct ServerFileCache {
    pub shading_language: ShadingLanguage,
    pub shader_module: ShaderModuleHandle, // Store content on change as its not on disk.
    pub data: Option<ServerFileCacheData>, // Data for file opened and edited.
    // A file can be dependency, main, dependent variant or main variant.
    is_main_file: bool,    // main file are opened file in editor.
    is_variant_file: bool, // variant are set through variant window.
}

impl ServerFileCache {
    pub fn is_main_file(&self) -> bool {
        self.is_main_file
    }
    pub fn is_cachable_file(&self) -> bool {
        self.is_main_file || self.is_variant_file
    }
    pub fn has_data(&self) -> bool {
        self.data.is_some()
    }
    pub fn get_data(&self) -> &ServerFileCacheData {
        assert!(
            self.data.is_some(),
            "Trying to get data from file {} which does not have cache.",
            RefCell::borrow(&self.shader_module).file_path.display()
        );
        self.data.as_ref().unwrap()
    }
}

pub struct ServerLanguageFileCache {
    pub files: HashMap<Url, ServerFileCache>,
    pub variant: Option<ShaderVariant>,
    pub workspace_folder: Vec<Url>,
}

impl ServerLanguageFileCache {
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
            variant: None,
            workspace_folder: Vec::new(),
        }
    }
    fn get_workspace_folder(&self, uri: &Url) -> Option<&Url> {
        let file_path = uri.to_file_path().unwrap();
        self.workspace_folder
            .iter()
            .find(|w| file_path.starts_with(&w.to_file_path().unwrap()))
    }
    // Get all main dependent files using the given file.
    pub fn get_dependent_main_files(&self, dependent_url: &Url) -> HashSet<Url> {
        let dependant_file_path = dependent_url.to_file_path().unwrap();
        self.files
            .iter()
            .filter(|(file_url, file)| {
                *file_url != dependent_url
                    && file.is_cachable_file()
                    && file.has_data()
                    && file
                        .get_data()
                        .symbol_cache
                        .has_dependency(&dependant_file_path)
            })
            .map(|(url, _file)| url.clone())
            .collect()
    }
    pub fn get_primary_dependent_main_file(&self, dependent_url: &Url) -> Option<Url> {
        let mut dependent_files: Vec<_> = self
            .get_dependent_main_files(dependent_url)
            .into_iter()
            .collect();
        dependent_files.sort_by(|left, right| left.as_str().cmp(right.as_str()));
        dependent_files.into_iter().next()
    }
    fn supports_automatic_dependency_diagnostic_context(&self, uri: &Url) -> bool {
        matches!(
            self.files.get(uri),
            Some(file) if file.shading_language == ShadingLanguage::Glsl
        )
    }
    pub fn get_document_diagnostic_context(&self, uri: &Url) -> Url {
        if self.get_relying_variant(uri).is_some()
            || !self.supports_automatic_dependency_diagnostic_context(uri)
        {
            return uri.clone();
        }
        self.get_primary_dependent_main_file(uri)
            .unwrap_or_else(|| uri.clone())
    }
    // Get all main files relying on the given file.
    #[allow(dead_code)]
    pub fn get_relying_main_files(&self, url: &Url) -> HashSet<Url> {
        match self.files.get(url) {
            Some(file) => {
                let mut relying_on_files = HashSet::new();
                file.get_data().symbol_cache.visit_includes(&mut |include| {
                    let include_uri = Url::from_file_path(&include.get_absolute_path()).unwrap();
                    let is_relying_on = match self.files.get(&include_uri) {
                        Some(deps) => deps.is_cachable_file(),
                        None => false,
                    };
                    if is_relying_on {
                        relying_on_files.insert(include_uri);
                    }
                });
                relying_on_files
            }
            None => {
                debug_assert!(
                    false,
                    "Trying to get relying main files for file {} that is not watched",
                    url
                );
                HashSet::new()
            }
        }
    }
    // Get all files relying on the given file.
    pub fn get_all_relying_files(&self, url: &Url) -> HashSet<Url> {
        match self.files.get(url) {
            Some(file) => {
                let mut relying_on_files = HashSet::new();
                file.get_data().symbol_cache.visit_includes(&mut |include| {
                    let include_uri = Url::from_file_path(&include.get_absolute_path()).unwrap();
                    match self.files.get(&include_uri) {
                        Some(_) => {
                            relying_on_files.insert(include_uri);
                        }
                        None => {}
                    };
                });
                relying_on_files
            }
            None => {
                debug_assert!(
                    false,
                    "Trying to get all relying files for file {} that is not watched",
                    url
                );
                HashSet::new()
            }
        }
    }
    #[allow(unused)]
    pub fn get_relying_variant(&self, url: &Url) -> Option<Url> {
        let file_path = url.to_file_path().unwrap();
        match &self.variant {
            Some(variant) => {
                let is_relying_on = match self.files.get(&variant.url) {
                    Some(variant_cached_file) => variant_cached_file
                        .get_data()
                        .symbol_cache
                        .has_dependency(&file_path),
                    None => false,
                };
                is_relying_on.then_some(variant.url.clone())
            }
            None => None,
        }
    }
    fn __cache_file_data(
        &mut self,
        uri: &Url,
        validator: &dyn ValidatorImpl,
        shader_module_parser: &mut ShaderModuleParser,
        symbol_provider: &SymbolProvider,
        config: &ServerConfig,
        dirty_deps: HashSet<PathBuf>,
    ) -> Result<(), ShaderError> {
        let file_path = uri.to_file_path().unwrap();

        // Get variant if its our URL.
        let variant = self.variant.clone().filter(|v| v.url == *uri);

        // Compute params
        let shader_params =
            config.into_shader_params(self.get_workspace_folder(uri), variant.clone());
        let mut context =
            ShaderPreprocessorContext::main(&file_path, shader_params.context.clone());

        // Do not recache & revalidate if not dirty.
        match &self.files.get(&uri).unwrap().data {
            Some(data) => {
                let is_dirty = data
                    .symbol_cache
                    .get_preprocessor()
                    .context
                    .is_dirty(&file_path, &context);
                let has_cache = self.files.get_mut(&uri).unwrap().data.is_some();
                let has_dirty = !dirty_deps.is_empty();
                if !is_dirty && !has_dirty && has_cache {
                    return Ok(());
                }
            }
            None => {}
        };
        for dirty_dep in dirty_deps {
            context.mark_dirty(dirty_dep);
        }

        // Get old data and replace it by dummy to avoid empty data on early exit.
        let old_data = self.files.get_mut(&uri).unwrap().data.take();
        self.files.get_mut(&uri).unwrap().data = Some(ServerFileCacheData::default());

        // Get symbols for main file.
        let (mut symbols, symbol_diagnostics) = if config.get_symbols() {
            profile_scope!("Querying symbols for file {}", uri);
            let shading_language = self.files.get(uri).unwrap().shading_language;
            let shader_module = Rc::clone(&self.files.get(uri).unwrap().shader_module);
            let shader_module = RefCell::borrow(&shader_module);
            match symbol_provider.query_symbols_with_context(
                &shader_module,
                &mut context,
                &shader_params.compilation,
                &mut |include| {
                    let include_uri = Url::from_file_path(&include.get_absolute_path()).unwrap();
                    let included_file = self.watch_dependency(
                        &include_uri,
                        shading_language,
                        shader_module_parser,
                    )?;
                    Ok(Some(Rc::clone(&included_file.shader_module)))
                },
                old_data.map(|e| e.symbol_cache),
            ) {
                Ok(symbols) => (symbols, ShaderDiagnosticList::default()),
                Err(error) => {
                    // Return this error & store it to display it as a diagnostic & dont prevent linting.
                    match error.into_diagnostic(ShaderDiagnosticSeverity::Warning) {
                        Some(diagnostic) => (
                            ShaderSymbols::new(&file_path, shader_params.context.clone()),
                            ShaderDiagnosticList {
                                diagnostics: vec![diagnostic],
                            },
                        ),
                        None => return Err(error),
                    }
                }
            }
        } else {
            (ShaderSymbols::default(), ShaderDiagnosticList::default())
        };
        // Get diagnostics
        let diagnostics = if config.get_validate() {
            profile_scope!("Validating file {}", uri);
            let shading_language = self.files.get(uri).unwrap().shading_language;
            let shader_module = Rc::clone(&self.files.get(uri).unwrap().shader_module);

            let mut diagnostic_list = {
                // TODO: should print warning if validation is too long.
                profile_scope!("Raw validation");
                let variant_shader_module = match &variant {
                    Some(variant) => {
                        Rc::clone(&self.files.get(&variant.url).unwrap().shader_module)
                    }
                    None => shader_module,
                };
                let diagnostics = match validator.validate_shader(
                    &RefCell::borrow(&variant_shader_module).content,
                    RefCell::borrow(&variant_shader_module).file_path.as_path(),
                    &shader_params,
                    &mut |deps_path: &Path| -> Option<String> {
                        let deps_uri = Url::from_file_path(deps_path).unwrap();
                        let deps_file = match self.get_file(&deps_uri) {
                            Some(deps_file) => deps_file,
                            None => {
                                if config.get_symbols() {
                                    warn!(
                                        "Should only get there if symbols did not add deps: {} from includer {}.",
                                        deps_uri,
                                        uri, // This is includer as we dont recurse here.
                                    );
                                }
                                // If include does not exist, add it to watched files.
                                match self.watch_dependency(
                                    &deps_uri,
                                    shading_language,
                                    shader_module_parser,
                                ) {
                                    Ok(deps_file) => deps_file,
                                    Err(err) => {
                                        warn!("Failed to watch deps {}", err);
                                        return None;
                                    }
                                }
                            }
                        };
                        let content = RefCell::borrow(&deps_file.shader_module).content.clone();
                        Some(content)
                    },
                ) {
                    Ok(diagnostics) => diagnostics,
                    Err(err) => ShaderDiagnosticList { diagnostics: vec![
                        ShaderDiagnostic {
                            severity: ShaderDiagnosticSeverity::Error,
                            error: format!("Failed to validate shader: {:?}", err),
                            range: ShaderFileRange::zero(file_path.clone())
                        }
                    ]},
                };
                diagnostics
            };
            {
                // Filter by severity.
                let required_severity = config.get_severity();
                diagnostic_list
                    .diagnostics
                    .retain(|e| e.severity.is_required(required_severity.clone()));
            }
            {
                // If includes have issues, diagnose them.
                let mut ascended_diagnostics: Vec<ShaderDiagnostic> =
                    symbols
                        .get_preprocessor()
                        .includes
                        .iter()
                        .filter_map(|include| {
                            for diagnostic in &diagnostic_list.diagnostics {
                                if diagnostic.severity != ShaderDiagnosticSeverity::Error {
                                    continue;
                                }
                                let diagnostic_path = &diagnostic.range.file_path;
                                if *diagnostic_path == file_path {
                                    continue; // Main file diagnostics
                                }
                                if *diagnostic_path == include.get_absolute_path() {
                                    return Some(ShaderDiagnostic {
                                        severity: ShaderDiagnosticSeverity::Error,
                                        error: format!(
                                            "File {} has issues:\n{}", // TODO: add command to file
                                            include.get_relative_path(),
                                            diagnostic.error
                                        ),
                                        range: include.get_file_range(),
                                    });
                                }
                                match include.cache.as_ref().unwrap().find_include(&mut |i| {
                                    i.get_absolute_path() == *diagnostic_path
                                }) {
                                    Some(includer) => {
                                        return Some(ShaderDiagnostic {
                                            severity: ShaderDiagnosticSeverity::Error,
                                            error: format!(
                                                "File {} has issues:\n{}",
                                                includer.get_relative_path(),
                                                diagnostic.error
                                            ),
                                            range: include.get_file_range(),
                                        })
                                    }
                                    None => {}
                                }
                            }
                            None
                        })
                        .collect();
                diagnostic_list
                    .diagnostics
                    .append(&mut ascended_diagnostics);
            }
            diagnostic_list
        } else {
            ShaderDiagnosticList::default()
        };

        symbols
            .get_preprocessor_mut()
            .diagnostics
            .extend(symbol_diagnostics.diagnostics);
        let shading_language = self.files.get(uri).unwrap().shading_language;
        let intrinsics = ShaderIntrinsics::get(shading_language)
            .get_intrinsics_symbol(&shader_params.compilation);
        self.files.get_mut(uri).unwrap().data = Some(ServerFileCacheData {
            symbol_cache: symbols,
            intrinsics,
            diagnostic_cache: diagnostics,
        });
        Ok(())
    }

    pub fn cache_file_data(
        &mut self,
        uri: &Url,
        validator: &dyn ValidatorImpl,
        shader_module_parser: &mut ShaderModuleParser,
        symbol_provider: &SymbolProvider,
        config: &ServerConfig,
        dirty_deps: HashSet<PathBuf>,
    ) -> Result<HashSet<Url>, ShaderError> {
        profile_scope!("Caching file data for file {}", uri);
        assert!(
            self.files.get(&uri).unwrap().is_cachable_file(),
            "Trying to cache data of dependency {}...",
            uri
        );
        // Check if we cache this file for the first time.
        // Fill it default to avoid early return and empty cache.
        let file_path = uri.to_file_path().unwrap();

        // Get variant if its our URL.
        let variant = self.variant.clone().filter(|v| v.url == *uri);

        info!("Caching file {} as variant: {:#?}", uri, variant);
        let mut old_relying_files = if self.files.get(&uri).unwrap().data.is_some() {
            self.get_all_relying_files(uri)
        } else {
            HashSet::new() // No old relying files as no cache
        };
        self.__cache_file_data(
            uri,
            validator,
            shader_module_parser,
            symbol_provider,
            config,
            dirty_deps,
        )?;

        // Copy variant deps data to all its relying data.
        if let Some(variant) = &variant {
            let variant_file = self.files.get(&variant.url).unwrap();
            let mut file_to_cache = HashMap::new();
            variant_file
                .get_data()
                .symbol_cache
                .visit_includes(&mut |include| {
                    // Here, we could visit the same include twice, which will overwrite final cache.
                    let include_url = Url::from_file_path(include.get_absolute_path()).unwrap();
                    match self.files.get(&include_url) {
                        Some(cached_file) => {
                            // Ensure we did not already got cache for this file,
                            // second include might have way less symbols (because of include guard mostly)
                            if !file_to_cache.contains_key(&include_url) {
                                if cached_file.is_main_file() {
                                    let symbol_cache = include.cache.clone().unwrap();
                                    let diagnostic_cache = ShaderDiagnosticList {
                                        diagnostics: variant_file
                                            .get_data()
                                            .diagnostic_cache
                                            .diagnostics
                                            .iter()
                                            .filter(|d| {
                                                let deps_file_path = &d.range.file_path;
                                                *deps_file_path == include.get_absolute_path()
                                                    || symbol_cache.has_dependency(deps_file_path)
                                            })
                                            .cloned()
                                            .collect(),
                                    };
                                    let shading_language =
                                        self.files.get(uri).unwrap().shading_language;
                                    let intrinsics = ShaderIntrinsics::get(shading_language)
                                        .get_intrinsics_symbol(
                                            &config
                                                .into_shader_params(
                                                    self.get_workspace_folder(uri),
                                                    Some(variant.clone()),
                                                )
                                                .compilation,
                                        );
                                    file_to_cache.insert(
                                        include_url,
                                        ServerFileCacheData {
                                            symbol_cache,
                                            intrinsics,
                                            diagnostic_cache,
                                        },
                                    );
                                }
                            }
                        }
                        None => {}
                    }
                });
            for (include_url, mut include_data) in file_to_cache {
                // When copying variant cache, some file in tree might be at their second include,
                // which remove most of their symbols due to include guard.
                // To workaround this, try to find their first occurence in variant and copy it.
                let mut first_include: HashSet<PathBuf> = HashSet::new();
                let mut reached_include = false;
                let variant_file = self.files.get(&variant.url).unwrap();
                include_data
                    .symbol_cache
                    .visit_includes_mut(&mut |include| {
                        // We only need previously declared element. Stop once we reach it.
                        if !reached_include {
                            reached_include = include.get_absolute_path() == file_path;
                            match variant_file.get_data().symbol_cache.find_include(
                                &mut |variant_include| {
                                    include.get_absolute_path()
                                        == variant_include.get_absolute_path()
                                },
                            ) {
                                Some(variant_include) => {
                                    if first_include.insert(include.get_absolute_path().into()) {
                                        include.cache = variant_include.cache.clone();
                                    }
                                }
                                None => {} // Not found
                            }
                        }
                    });
                self.files.get_mut(&include_url).unwrap().data = Some(include_data);
                // Mark them for publishing diagnostics.
            }
        }
        // Get dangling dependencies that need to be removed.
        let new_relying_files = self.get_all_relying_files(uri);
        old_relying_files.retain(|f| {
            if f != uri {
                // Check if file was removed from include tree.
                if new_relying_files.iter().find(|n| *n == f).is_none() {
                    self.is_dangling_file(f)
                } else {
                    false // Keep it by removing it from update.
                }
            } else {
                false // Avoid removing main file twice.
            }
        });
        // Remove these deps from cache.
        for old_relying_file in &old_relying_files {
            info!("Removing dangling deps {}", old_relying_file);
            self.files.remove(old_relying_file);
        }

        debug_assert!(
            self.get_file(uri).unwrap().data.is_some(),
            "Failed to cache data for file {}",
            uri
        );
        Ok(old_relying_files)
    }
    pub fn cache_batched_file_data<F: Fn(&str, u32, u32)>(
        &mut self,
        mut async_cache_requests: Vec<AsyncCacheRequest>,
        language_data: &mut HashMap<ShadingLanguage, ServerLanguageData>,
        config: &ServerConfig,
        progress_callback: F,
    ) -> Result<(HashSet<Url>, HashSet<Url>), ShaderError> {
        fn get_file_name(uri: &Url) -> String {
            uri.to_file_path()
                .unwrap()
                .file_name()
                .unwrap()
                .to_string_lossy()
                .into_owned()
        }

        // Check if preamble file changed and update it.
        // We only track it if its open in editor.
        let need_to_recompute_all_glsl =
            if let Some(preamble_path) = config.get_glsl_preamble_path() {
                let preamble_uri = Url::from_file_path(preamble_path).unwrap();
                let has_glsl_preamble_in_request = async_cache_requests
                    .iter()
                    .find(|r| r.url == preamble_uri)
                    .is_some();
                has_glsl_preamble_in_request
            } else {
                false
            };
        if need_to_recompute_all_glsl {
            for (url, file) in &self.files {
                if file.shading_language == ShadingLanguage::Glsl {
                    // Check if file already in request. If not, update it.
                    if async_cache_requests
                        .iter()
                        .find(|r| r.url == *url)
                        .is_none()
                    {
                        async_cache_requests.push(AsyncCacheRequest::new(
                            url.clone(),
                            ShadingLanguage::Glsl,
                            true,
                        ));
                    }
                }
            }
        }
        // Get unique files to update in batch aswell as dirty ones.
        let dirty_files: HashSet<Url> = async_cache_requests
            .iter()
            .filter(|r| r.dirty)
            .map(|r| r.url.clone())
            .collect();
        let dirty_dependencies: HashSet<PathBuf> = dirty_files
            .iter()
            .map(|url| url.to_file_path().unwrap())
            .collect();

        // Check variant need to be updated first
        let variant_url = self.variant.clone().map(|v| v.url);
        let need_to_recompute_variant = if let Some(variant_url) = &variant_url {
            let has_variant_in_request = async_cache_requests
                .iter()
                .find(|r| r.url == *variant_url)
                .is_some();
            let has_variant_relying_files_in_request =
                if let Some(variant_data) = &self.files.get(&variant_url).unwrap().data {
                    async_cache_requests
                        .iter()
                        .find(|r| {
                            let file_path = r.url.to_file_path().unwrap();
                            variant_data
                                .symbol_cache
                                .find_include(&mut |include| {
                                    include.get_absolute_path().as_os_str() == file_path.as_os_str()
                                })
                                .is_some()
                        })
                        .is_some()
                } else {
                    false
                };
            has_variant_in_request || has_variant_relying_files_in_request
        } else {
            false // no variant.
        };
        let mut files_to_clear = HashSet::new();
        let mut dependent_files_to_update = HashSet::new();
        let mut files_updating: HashSet<Url> =
            async_cache_requests.iter().map(|r| r.url.clone()).collect();
        let mut unique_remaining_files = files_updating.clone();
        let mut files_to_publish = HashSet::new();
        let mut file_progress_index = 0;
        if need_to_recompute_variant {
            // Recompute variant.
            let variant = self.variant.as_ref().unwrap();
            let variant_url = variant.url.clone();
            let variant_shading_language = variant.shading_language;
            let language_data = language_data.get_mut(&variant_shading_language).unwrap();
            unique_remaining_files.remove(&variant_url);
            files_to_publish.insert(variant_url.clone());
            files_updating.insert(variant_url.clone());
            let file_name = get_file_name(&variant_url);
            file_progress_index += 1;
            progress_callback(
                &file_name,
                file_progress_index,
                unique_remaining_files.len() as u32 + 1,
            );
            let removed_files = self.cache_file_data(
                &variant_url,
                language_data.validator.as_mut(),
                &mut language_data.shader_module_parser,
                &mut language_data.symbol_provider,
                &config,
                dirty_dependencies.clone(),
            )?;
            files_to_clear.extend(removed_files);
            // Remove request for relying files as they are already updated by variant.
            let relying_files = self.get_all_relying_files(&variant_url);
            unique_remaining_files.retain(|f| {
                if relying_files.contains(f) {
                    let dependent_files = self.get_dependent_main_files(f);
                    dependent_files_to_update.extend(dependent_files);
                    false
                } else {
                    true
                }
            });
            files_updating.extend(relying_files);
            // If file is dirty, request update for dependent files.
            if dirty_files.contains(&variant_url) {
                let dependent_files = self.get_dependent_main_files(&variant_url);
                for dependent_file in dependent_files {
                    if !files_updating.contains(&dependent_file) {
                        info!(
                            "File {} is being updated as its relying on {}",
                            dependent_file, variant_url
                        );
                        files_updating.insert(dependent_file.clone());
                        dependent_files_to_update.insert(dependent_file);
                    }
                }
            }
        }
        for remaining_file in &unique_remaining_files {
            let file_name = get_file_name(&remaining_file);
            file_progress_index += 1;
            progress_callback(
                &file_name,
                file_progress_index,
                (unique_remaining_files.len() + dependent_files_to_update.len()) as u32
                    + need_to_recompute_variant as u32,
            );
            // Check file is still watched and a main file
            let shading_language = match self.files.get(&remaining_file) {
                Some(file) => {
                    if !file.is_main_file() {
                        files_to_clear.insert(remaining_file.clone());
                        continue;
                    } else {
                        files_to_publish.insert(remaining_file.clone());
                        file.shading_language
                    }
                }
                None => {
                    files_to_clear.insert(remaining_file.clone());
                    continue;
                }
            };
            let language_data = language_data.get_mut(&shading_language).unwrap();
            let removed_files = self.cache_file_data(
                &remaining_file,
                language_data.validator.as_mut(),
                &mut language_data.shader_module_parser,
                &mut language_data.symbol_provider,
                &config,
                dirty_dependencies.clone(),
            )?;
            files_to_clear.extend(removed_files);
            // If file is dirty, request update for dependent files.
            if dirty_files.contains(&remaining_file) {
                let dependent_files = self.get_dependent_main_files(&remaining_file);
                for dependent_file in dependent_files {
                    if !files_updating.contains(&dependent_file) {
                        info!(
                            "File {} is being updated as its relying on {}",
                            dependent_file, remaining_file
                        );
                        files_updating.insert(dependent_file.clone());
                        dependent_files_to_update.insert(dependent_file);
                    }
                }
            }
        }
        // Update dependent files now that we did everything else.
        for dependent_file in &dependent_files_to_update {
            let shading_language = self.files.get(&dependent_file).unwrap().shading_language;
            let language_data = language_data.get_mut(&shading_language).unwrap();
            let file_name = get_file_name(&dependent_file);
            file_progress_index += 1;
            progress_callback(
                &file_name,
                file_progress_index,
                (unique_remaining_files.len() + dependent_files_to_update.len()) as u32
                    + need_to_recompute_variant as u32,
            );
            let removed_files = self.cache_file_data(
                &dependent_file,
                language_data.validator.as_mut(),
                &mut language_data.shader_module_parser,
                &mut language_data.symbol_provider,
                &config,
                dirty_dependencies.clone(),
            )?;
            files_to_clear.extend(removed_files);
        }
        debug_assert!(
            (unique_remaining_files.len()
                + dependent_files_to_update.len()
                + need_to_recompute_variant as usize)
                == file_progress_index as usize,
            "Invalid count for progress report ({} unique files, {} dependent, {} variant, expecting a total of {})",
            unique_remaining_files.len(),
            dependent_files_to_update.len(),
            need_to_recompute_variant as u32,
            file_progress_index
        );
        // TODO: Diagnostics return here are unique but might be in incorrect order...
        files_to_publish.extend(dependent_files_to_update);
        Ok((files_to_clear, files_to_publish))
    }
    pub fn watch_variant_file(
        &mut self,
        uri: &Url,
        lang: ShadingLanguage,
        shader_module_parser: &mut ShaderModuleParser,
    ) -> Result<(), ShaderError> {
        assert!(*uri == clean_url(&uri));
        let file_path = uri.to_file_path().unwrap();
        // Check if watched file already watched as deps or variant.
        match self.files.get_mut(&uri) {
            Some(cached_file) => {
                if !cached_file.is_variant_file {
                    cached_file.is_variant_file = true;
                    info!(
                        "Starting watching {:#?} file as variant file at {}. {} files in cache.",
                        lang,
                        file_path.display(),
                        self.files.len(),
                    );
                }
            }
            None => {
                let text = read_string_lossy(&file_path).unwrap();
                let shader_module = Rc::new(RefCell::new(
                    shader_module_parser.create_module(&file_path, &text)?,
                ));
                let cached_file = ServerFileCache {
                    shading_language: lang,
                    shader_module: shader_module,
                    data: None,
                    is_main_file: false,
                    is_variant_file: true,
                };
                let none = self.files.insert(uri.clone(), cached_file);
                assert!(none.is_none());
                info!(
                    "Starting watching {:#?} variant file at {}. {} files in cache.",
                    lang,
                    file_path.display(),
                    self.files.len(),
                );
            }
        };
        Ok(())
    }
    pub fn watch_main_file(
        &mut self,
        uri: &Url,
        lang: ShadingLanguage,
        text: &str,
        shader_module_parser: &mut ShaderModuleParser,
    ) -> Result<(), ShaderError> {
        assert!(*uri == clean_url(&uri));
        let file_path = uri.to_file_path().unwrap();

        // Check if watched file already watched as deps
        match self.files.get_mut(&uri) {
            Some(cached_file) => {
                assert!(
                    !cached_file.is_main_file,
                    "File {} already watched as main.",
                    uri
                );
                cached_file.is_main_file = true;
                // When the cached module was loaded as an include dependency,
                // its content was read from disk via `read_string_lossy` /
                // `std::fs::read_to_string`, which preserves the on-disk bytes
                // verbatim (e.g. CRLF on Windows). The client-provided didOpen
                // text may have been normalized by the editor (e.g. CRLF -> LF),
                // so the two buffers can differ in length. We cannot simply
                // replace `.content` and keep the existing tree-sitter tree:
                // the stored tree's byte offsets would still refer to the
                // longer pre-normalization buffer, and subsequent symbol
                // queries (e.g. `symbol_parser::get_name`) would slice the
                // new, shorter content out of bounds and panic with
                // "byte index N is out of bounds of ...".
                //
                // When the content actually differs, re-parse from scratch so
                // the tree and content stay in sync. When they match byte-for-
                // byte, skip the re-parse as an optimization.
                {
                    let mut module = RefCell::borrow_mut(&cached_file.shader_module);
                    if module.content != *text {
                        *module = shader_module_parser.create_module(&file_path, text)?;
                    }
                }
                info!(
                    "Starting watching {:#?} dependency file as main file at {}. {} files in cache.",
                    lang,
                    file_path.display(),
                    self.files.len(),
                );
            }
            None => {
                let shader_module = Rc::new(RefCell::new(
                    shader_module_parser.create_module(&file_path, &text)?,
                ));
                debug_assert!(self.variant.as_ref().map(|v| v.url != *uri).unwrap_or(true));
                let cached_file = ServerFileCache {
                    shading_language: lang,
                    shader_module: shader_module,
                    data: None,
                    is_main_file: true,
                    is_variant_file: false, // Cannot be a variant if its not watched.
                };
                let none = self.files.insert(uri.clone(), cached_file);
                debug_assert!(none.is_none());
                info!(
                    "Starting watching {:#?} main file at {}. {} files in cache.",
                    lang,
                    file_path.display(),
                    self.files.len(),
                );
            }
        };
        Ok(())
    }
    pub fn watch_dependency(
        &mut self,
        uri: &Url,
        lang: ShadingLanguage,
        shader_module_parser: &mut ShaderModuleParser,
    ) -> Result<&ServerFileCache, ShaderError> {
        assert!(*uri == clean_url(&uri));
        let file_path = uri.to_file_path().unwrap();
        // If file is not watched, add it as deps.
        match self.files.get(&uri) {
            Some(file) => {
                if file.is_main_file() {
                    debug!(
                        "Starting watching {:#?} main file as deps at {}. {} files in cache.",
                        lang,
                        file_path.display(),
                        self.files.len(),
                    );
                } else if file.is_variant_file {
                    debug!(
                        "Already watched {:#?} deps file as variant at {}. {} files in cache.",
                        lang,
                        file_path.display(),
                        self.files.len(),
                    );
                } else {
                    debug!(
                        "Already watched {:#?} deps file at {}. {} files in cache.",
                        lang,
                        file_path.display(),
                        self.files.len(),
                    );
                }
            }
            None => {
                let text = read_string_lossy(&file_path).unwrap();
                let shader_module = Rc::new(RefCell::new(
                    shader_module_parser.create_module(&file_path, &text)?,
                ));
                let cached_file = ServerFileCache {
                    shading_language: lang,
                    shader_module: shader_module,
                    data: None,
                    is_main_file: false,
                    is_variant_file: self
                        .variant
                        .as_ref()
                        .map(|v| v.url == *uri)
                        .unwrap_or(false),
                };
                let none = self.files.insert(uri.clone(), cached_file);
                assert!(none.is_none());
                info!(
                    "Starting watching {:#?} dependency file at {}. {} files in cache.",
                    lang,
                    file_path.display(),
                    self.files.len(),
                );
            }
        }
        Ok(self.files.get(&uri).unwrap())
    }
    pub fn update_file(
        &mut self,
        uri: &Url,
        shader_module_parser: &mut ShaderModuleParser,
        range: Option<lsp_types::Range>,
        partial_content: Option<&String>,
    ) -> Result<(), ShaderError> {
        let cached_file = self.get_file(uri).unwrap();
        profile_scope!(
            "Updating file {} (Content {:?} at {:?})",
            uri,
            partial_content,
            range
        );
        // Update abstract syntax tree
        if let (Some(range), Some(partial_content)) = (range, partial_content) {
            let shader_range = lsp_range_to_shader_range(&range);
            shader_module_parser.update_module_partial(
                &mut RefCell::borrow_mut(&cached_file.shader_module),
                &shader_range,
                &partial_content,
            )?;
        } else if let Some(whole_content) = partial_content {
            shader_module_parser.update_module(
                &mut RefCell::borrow_mut(&cached_file.shader_module),
                &whole_content,
            )?;
        } else {
            // No update on content to perform.
            assert!(false, "Calling update_file unnecessarily");
        }
        Ok(())
    }
    pub fn get_file(&self, uri: &Url) -> Option<&ServerFileCache> {
        assert!(*uri == clean_url(&uri));
        match self.files.get(uri) {
            Some(cached_file) => Some(&cached_file),
            None => None,
        }
    }
    fn is_used_as_dependency(&self, uri: &Url) -> Option<(&Url, &ServerFileCache)> {
        let file_path = uri.to_file_path().unwrap();
        self.files.iter().find(|(file_url, file_cache)| {
            if *file_url != uri {
                file_cache.has_data()
                    && file_cache
                        .get_data()
                        .symbol_cache
                        .has_dependency(&file_path)
            } else {
                false
            }
        })
    }
    fn is_dangling_file(&self, uri: &Url) -> bool {
        match self.files.get(uri) {
            Some(cached_file) => {
                !cached_file.is_cachable_file() && self.is_used_as_dependency(uri).is_none()
            }
            None => {
                debug_assert!(
                    false,
                    "Checking if file {} is dangling but its not watched.",
                    uri
                );
                false
            }
        }
    }
    // Dependency removal are handled by remove_variant & remove_file.
    pub fn remove_variant_file(&mut self, uri: &Url) -> Result<Vec<Url>, ShaderError> {
        let used_as_deps = self.is_used_as_dependency(uri).is_some();
        let mut dangling_files = self.get_all_relying_files(uri);
        match self.files.get_mut(&uri) {
            Some(cached_file) => {
                if used_as_deps || cached_file.is_main_file() {
                    let shading_language = cached_file.shading_language;
                    // Used as deps. Reset cache only if not main.
                    if !cached_file.is_main_file {
                        cached_file.data = None;
                    }
                    debug_assert!(cached_file.is_variant_file);
                    cached_file.is_variant_file = false;
                    info!(
                        "Converted {:#?} variant file to {} at {}. {} files in cache.",
                        shading_language,
                        if cached_file.is_main_file {
                            "main file"
                        } else {
                            "deps file"
                        },
                        uri,
                        self.files.len()
                    );
                    Ok(vec![])
                } else {
                    match self.files.remove(uri) {
                        Some(mut cached_file) => {
                            let shading_language = cached_file.shading_language;
                            assert!(
                                cached_file.data.is_some(),
                                "Removing variant file without data"
                            );
                            // Get dangling dependencies that need to be removed.
                            dangling_files.retain(|f| {
                                if uri != f {
                                    self.is_dangling_file(f)
                                } else {
                                    false // Avoid removing main file twice.
                                }
                            });
                            // Remove main file before deps & drop cache for ref.
                            let data = cached_file.data.unwrap();
                            drop(data);
                            cached_file.is_variant_file = false; // Just to be sure.
                            info!(
                                "Removed {:#?} main file at {}. {} files in cache.",
                                cached_file.shading_language,
                                uri,
                                self.files.len()
                            );
                            // Remove these deps from cache.
                            for dangling_file in &dangling_files {
                                self.files.remove(dangling_file);
                                info!(
                                    "Removed {:#?} dangling deps {}. {} files in cache.",
                                    shading_language,
                                    dangling_file,
                                    self.files.len()
                                );
                            }
                            Ok(
                                vec![vec![uri.clone()], dangling_files.into_iter().collect()]
                                    .concat(),
                            )
                        }
                        None => Err(ShaderError::InternalErr(format!(
                            "Trying to remove variant file {} that is not watched",
                            uri.path()
                        ))),
                    }
                }
            }
            None => Err(ShaderError::InternalErr(format!(
                "Trying to remove variant file {} that is not watched",
                uri.path()
            ))),
        }
    }
    pub fn remove_main_file(&mut self, uri: &Url) -> Result<Vec<Url>, ShaderError> {
        let used_as_deps = self.is_used_as_dependency(uri).is_some();
        let mut dangling_files = if self.files.get(&uri).unwrap().data.is_some() {
            self.get_all_relying_files(uri)
        } else {
            HashSet::new()
        };
        match self.files.get_mut(&uri) {
            Some(cached_file) => {
                if used_as_deps || cached_file.is_variant_file {
                    let shading_language = cached_file.shading_language;
                    // Used as deps. Reset cache only if not main.
                    if !cached_file.is_variant_file {
                        cached_file.data = None;
                    }
                    debug_assert!(cached_file.is_main_file);
                    cached_file.is_main_file = false;
                    info!(
                        "Converted {:#?} main file to {} at {}. {} files in cache.",
                        shading_language,
                        if cached_file.is_main_file {
                            "variant file"
                        } else {
                            "deps file"
                        },
                        uri,
                        self.files.len()
                    );
                    Ok(vec![])
                } else {
                    match self.files.remove(uri) {
                        Some(mut cached_file) => {
                            let shading_language = cached_file.shading_language;
                            // Get dangling dependencies that need to be removed.
                            dangling_files.retain(|f| {
                                if uri != f {
                                    self.is_dangling_file(f)
                                } else {
                                    false // Avoid removing main file twice.
                                }
                            });
                            // Remove main file before deps & drop cache for ref.
                            if let Some(data) = cached_file.data {
                                drop(data);
                            }
                            cached_file.is_main_file = false; // Just to be sure.
                            info!(
                                "Removed {:#?} main file at {}. {} files in cache.",
                                cached_file.shading_language,
                                uri,
                                self.files.len()
                            );
                            // Remove these deps from cache.
                            for dangling_file in &dangling_files {
                                self.files.remove(dangling_file);
                                info!(
                                    "Removed {:#?} dangling deps {}. {} files in cache.",
                                    shading_language,
                                    dangling_file,
                                    self.files.len()
                                );
                            }
                            Ok(
                                vec![vec![uri.clone()], dangling_files.into_iter().collect()]
                                    .concat(),
                            )
                        }
                        None => Err(ShaderError::InternalErr(format!(
                            "Trying to remove main file {} that is not watched",
                            uri.path()
                        ))),
                    }
                }
            }
            None => Err(ShaderError::InternalErr(format!(
                "Trying to remove main file {} that is not watched",
                uri.path()
            ))),
        }
    }
    pub fn get_all_symbols<'a>(&'a self, uri: &Url) -> ShaderSymbolListRef<'a> {
        let cached_file = self.files.get(uri).unwrap();
        assert!(cached_file.data.is_some(), "File {} do not have cache", uri);
        let data = &cached_file.get_data();
        // Add main file symbols
        let mut symbol_cache = data.symbol_cache.get_all_symbols();
        // Add config symbols
        for symbol in data.symbol_cache.get_context().get_defines().iter() {
            symbol_cache.macros.push(&symbol);
        }
        // Add intrinsics symbols
        symbol_cache.append(data.intrinsics.clone());
        symbol_cache
    }
}
