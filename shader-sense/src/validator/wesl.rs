//! Validation for wesl with [`wesl`]
//!
//! Wesl does not perform any semantic validation by itself, it only resolves imports,
//! conditional compilation & mangling. So the generated wgsl is validated with [`naga`],
//! and naga errors are mapped back to the original wesl source by using the spans that
//! are kept in the generated syntax tree.

use std::{
    borrow::Cow,
    cell::RefCell,
    collections::HashMap,
    fmt::Write,
    path::{Path, PathBuf},
};

use naga::{
    front::wgsl,
    valid::{Capabilities, ValidationFlags},
};
use wesl::{
    error::ResolveError,
    sourcemap::{BasicSourceMap, SourceMap},
    syntax::{
        CompoundStatement, GlobalDeclaration, ModulePath, PathOrigin, Span, Statement,
        StatementNode, TranslationUnit,
    },
    CompileResult, Compiler, Resolver,
};

use crate::{
    include::{canonicalize, find_wesl_module_file},
    position::{ShaderFileRange, ShaderPosition},
    shader::{ShaderParams, ShaderStage},
    shader_error::{ShaderDiagnostic, ShaderDiagnosticList, ShaderDiagnosticSeverity, ShaderError},
    validator::validator::CompilationResult,
};

use super::validator::ValidatorImpl;

pub struct Wesl {}

/// Convert a SPIR-V binary module to its WGSL representation.
pub fn spirv_to_wgsl(spirv: &[u8]) -> Result<String, ShaderError> {
    // TODO: Option should change depending on target spirv version (adjust_coordinate_space which is > SPV1.0).
    let module = naga::front::spv::parse_u8_slice(spirv, &naga::front::spv::Options::default())
        .map_err(|err| {
            ShaderError::ValidationError(format!("Failed to parse SPIR-V module: {}", err))
        })?;
    let mut validator = naga::valid::Validator::new(ValidationFlags::all(), Capabilities::all());
    let module_info = validator.validate(&module).map_err(|err| {
        ShaderError::ValidationError(format!(
            "Failed to validate SPIR-V module: {}",
            err.emit_to_string("")
        ))
    })?;
    naga::back::wgsl::write_string(
        &module,
        &module_info,
        naga::back::wgsl::WriterFlags::empty(),
    )
    .map_err(|err| ShaderError::InternalErr(format!("Failed to write WGSL: {}", err)))
}
/// Convert a WGSL shader to its SPIR-V binary representation.
pub fn wgsl_to_spirv(shader_content: &str) -> Result<Vec<u8>, ShaderError> {
    let module = wgsl::parse_str(shader_content).map_err(|err| {
        ShaderError::ValidationError(format!(
            "Failed to parse WGSL module: {}",
            err.emit_to_string(shader_content)
        ))
    })?;
    let mut validator = naga::valid::Validator::new(ValidationFlags::all(), Capabilities::all());
    let module_info = validator.validate(&module).map_err(|err| {
        ShaderError::ValidationError(format!(
            "Failed to validate WGSL module: {}",
            err.emit_to_string(shader_content)
        ))
    })?;
    let words = naga::back::spv::write_vec(
        &module,
        &module_info,
        &naga::back::spv::Options::default(),
        None, // Emit every entry point.
    )
    .map_err(|err| ShaderError::InternalErr(format!("Failed to write SPIR-V module: {}", err)))?;
    // Little endian, to match what naga::front::spv expects.
    Ok(words.iter().flat_map(|word| word.to_le_bytes()).collect())
}

impl Wesl {
    pub fn new() -> Self {
        Self {}
    }
}

/// A statement of the generated wgsl, with its original span.
struct StatementMapping {
    /// Line range in generated wgsl (inclusive).
    first_line: usize,
    last_line: usize,
    span: Span,
}

/// A global declaration of the generated wgsl, with its original span & module.
struct DeclarationMapping {
    /// Line range in generated wgsl (inclusive).
    first_line: usize,
    last_line: usize,
    span: Span,
    /// Module the declaration comes from. None if it is the main module.
    module_path: Option<ModulePath>,
    /// Statements ordered in pre-order, so that the last match is the innermost one.
    statements: Vec<StatementMapping>,
}

/// Generated wgsl along with what is required to map it back to original wesl sources.
struct WgslMapping {
    wgsl: String,
    line_starts: Vec<usize>,
    declarations: Vec<DeclarationMapping>,
    /// Mangled name to original name.
    mangled_names: Vec<(String, String)>,
}

impl WgslMapping {
    fn new(syntax: &TranslationUnit, sourcemap: Option<&BasicSourceMap>) -> Self {
        // Write wgsl ourselves instead of relying on TranslationUnit Display to know where each declaration is.
        let mut wgsl = String::new();
        if !syntax.global_directives.is_empty() {
            for directive in &syntax.global_directives {
                writeln!(wgsl, "{directive}").unwrap();
            }
            wgsl.push('\n');
        }
        let mut declarations = Vec::new();
        let mut mangled_names = Vec::new();
        for decl in &syntax.global_declarations {
            if matches!(decl.node(), GlobalDeclaration::Void) {
                continue;
            }
            let decl_wgsl = decl.to_string();
            let first_line = wgsl.matches('\n').count();
            let last_line = first_line + decl_wgsl.lines().count().max(1) - 1;
            let module_path = Self::get_declaration_name(decl.node())
                .and_then(|name| {
                    sourcemap
                        .and_then(|sourcemap| sourcemap.item(&name))
                        .map(|entry| (name, entry))
                })
                .map(|(name, entry)| {
                    if name != entry.name {
                        mangled_names.push((name, entry.name.clone()));
                    }
                    entry.path.clone()
                });
            let mut statements = Vec::new();
            if let GlobalDeclaration::Function(function) = decl.node() {
                let lines: Vec<&str> = decl_wgsl.lines().collect();
                let mut cursor = 0;
                Self::map_compound(&function.body, &lines, &mut cursor, &mut statements);
                for statement in &mut statements {
                    statement.first_line += first_line;
                    statement.last_line += first_line;
                }
            }
            declarations.push(DeclarationMapping {
                first_line,
                last_line,
                span: decl.span(),
                module_path,
                statements,
            });
            wgsl.push_str(&decl_wgsl);
            wgsl.push_str("\n\n");
        }
        // Replace longest first to avoid replacing a name that is part of another one.
        mangled_names.sort_by(|lhs, rhs| rhs.0.len().cmp(&lhs.0.len()));
        let line_starts = std::iter::once(0)
            .chain(wgsl.match_indices('\n').map(|(index, _)| index + 1))
            .collect();
        Self {
            wgsl,
            line_starts,
            declarations,
            mangled_names,
        }
    }
    fn get_declaration_name(decl: &GlobalDeclaration) -> Option<String> {
        match decl {
            GlobalDeclaration::Declaration(decl) => Some(decl.ident.name().clone()),
            GlobalDeclaration::TypeAlias(decl) => Some(decl.ident.name().clone()),
            GlobalDeclaration::Struct(decl) => Some(decl.ident.name().clone()),
            GlobalDeclaration::Function(decl) => Some(decl.ident.name().clone()),
            _ => None,
        }
    }
    fn map_compound(
        compound: &CompoundStatement,
        lines: &[&str],
        cursor: &mut usize,
        statements: &mut Vec<StatementMapping>,
    ) {
        for statement in &compound.statements {
            Self::map_statement(statement, lines, cursor, statements);
        }
    }
    // Statements are printed in order, and nested statements are only indented.
    // So we can find each of them by looking for their first line after the previous one.
    fn map_statement(
        statement: &StatementNode,
        lines: &[&str],
        cursor: &mut usize,
        statements: &mut Vec<StatementMapping>,
    ) {
        if matches!(statement.node(), Statement::Void) {
            return;
        }
        let statement_wgsl = statement.to_string();
        let line_count = statement_wgsl.lines().count().max(1);
        let header = statement_wgsl.lines().next().unwrap_or("").trim();
        let Some(first_line) = lines[*cursor..]
            .iter()
            .position(|line| line.trim() == header)
            .map(|index| index + *cursor)
        else {
            return; // Could not find it, skip it and its children.
        };
        let last_line = first_line + line_count - 1;
        // Nodes generated by wesl have a default span. Their children might still be mappable.
        let span = statement.span();
        if span.end > span.start {
            statements.push(StatementMapping {
                first_line,
                last_line,
                span,
            });
        }
        *cursor = first_line + 1;
        match statement.node() {
            Statement::Compound(compound) => {
                Self::map_compound(compound, lines, cursor, statements)
            }
            Statement::If(if_statement) => {
                Self::map_compound(&if_statement.if_clause.body, lines, cursor, statements);
                for clause in &if_statement.else_if_clauses {
                    Self::map_compound(&clause.body, lines, cursor, statements);
                }
                if let Some(clause) = &if_statement.else_clause {
                    Self::map_compound(&clause.body, lines, cursor, statements);
                }
            }
            Statement::Switch(switch_statement) => {
                for clause in &switch_statement.clauses {
                    Self::map_compound(&clause.body, lines, cursor, statements);
                }
            }
            Statement::Loop(loop_statement) => {
                Self::map_compound(&loop_statement.body, lines, cursor, statements);
                if let Some(continuing) = &loop_statement.continuing {
                    Self::map_compound(&continuing.body, lines, cursor, statements);
                }
            }
            Statement::For(for_statement) => {
                Self::map_compound(&for_statement.body, lines, cursor, statements)
            }
            Statement::While(while_statement) => {
                Self::map_compound(&while_statement.body, lines, cursor, statements)
            }
            _ => {}
        }
        *cursor = (last_line + 1).min(lines.len());
    }
    fn get_line(&self, offset: usize) -> usize {
        match self.line_starts.binary_search(&offset) {
            Ok(line) => line,
            Err(line) => line - 1,
        }
    }
    /// Find the original span & module of a byte offset in generated wgsl.
    fn find_origin(&self, offset: usize) -> Option<(Span, Option<&ModulePath>)> {
        let line = self.get_line(offset);
        let declaration = self
            .declarations
            .iter()
            .find(|decl| decl.first_line <= line && line <= decl.last_line)?;
        let span = declaration
            .statements
            .iter()
            .rev()
            .find(|statement| statement.first_line <= line && line <= statement.last_line)
            .map(|statement| statement.span)
            .unwrap_or(declaration.span);
        Some((span, declaration.module_path.as_ref()))
    }
    fn unmangle(&self, message: &str) -> String {
        let mut message = message.to_string();
        for (mangled, name) in &self.mangled_names {
            message = message.replace(mangled, name);
        }
        message
    }
}

/// Resolve module paths to files, relative to the package root set in [`crate::shader::WgslCompilationParams`].
/// Files are read through the include callback so that unsaved content can be used.
struct WeslResolver<'a> {
    package_root: PathBuf,
    packages: &'a HashMap<String, PathBuf>,
    main_module_path: ModulePath,
    main_file_path: &'a Path,
    main_content: &'a str,
    include_callback: RefCell<&'a mut dyn FnMut(&Path) -> Option<String>>,
    /// Loaded modules with their file & content, used to map diagnostics.
    modules: RefCell<HashMap<ModulePath, (PathBuf, String)>>,
    /// Last module that failed to resolve, as resolve errors have no location.
    unresolved_module: RefCell<Option<ModulePath>>,
}

impl<'a> WeslResolver<'a> {
    fn new(
        main_file_path: &'a Path,
        main_content: &'a str,
        params: &'a ShaderParams,
        include_callback: &'a mut dyn FnMut(&Path) -> Option<String>,
    ) -> Self {
        let package_root = params
            .compilation
            .wgsl
            .package_root
            .clone()
            .or_else(|| main_file_path.parent().map(|parent| parent.into()))
            .unwrap_or_default();
        let package_root = canonicalize(&package_root).unwrap_or(package_root);
        // Main module path depends on its location in the package, required for super:: imports.
        let main_module_path = match main_file_path.strip_prefix(&package_root) {
            Ok(relative_path) => ModulePath::new(
                PathOrigin::Absolute,
                relative_path
                    .with_extension("")
                    .components()
                    .map(|component| component.as_os_str().to_string_lossy().to_string())
                    .collect(),
            ),
            // File outside of the package, it can still import package modules.
            Err(_) => ModulePath::new(
                PathOrigin::Absolute,
                vec![main_file_path
                    .file_stem()
                    .map(|stem| stem.to_string_lossy().to_string())
                    .unwrap_or("main".into())],
            ),
        };
        Self {
            package_root,
            packages: &params.compilation.wgsl.packages,
            main_module_path,
            main_file_path,
            main_content,
            include_callback: RefCell::new(include_callback),
            modules: RefCell::new(HashMap::new()),
            unresolved_module: RefCell::new(None),
        }
    }
    /// Find the file of a module, with .wesl extension, or .wgsl as fallback.
    fn find_file(&self, path: &ModulePath) -> Result<PathBuf, ResolveError> {
        let root = match &path.origin {
            PathOrigin::Absolute => &self.package_root,
            PathOrigin::Package(name) => self.packages.get(name).ok_or_else(|| {
                ResolveError::ModuleNotFound(
                    path.clone(),
                    format!(
                        "package `{}` is not declared in wgsl packages setting",
                        name
                    ),
                )
            })?,
            // Compiler only pass absolute path to resolver.
            PathOrigin::Relative(_) => {
                return Err(ResolveError::ModuleNotFound(
                    path.clone(),
                    "relative module path".into(),
                ))
            }
        };
        find_wesl_module_file(root, &path.components).ok_or_else(|| {
            let mut file_path = root.clone();
            file_path.extend(&path.components);
            ResolveError::FileNotFound(file_path.with_extension("wesl"), "module file".into())
        })
    }
    /// Get the file & content of a loaded module.
    fn get_module(&self, module_path: Option<&ModulePath>) -> Option<(PathBuf, String)> {
        match module_path {
            Some(module_path) if *module_path != self.main_module_path => {
                self.modules.borrow().get(module_path).cloned()
            }
            _ => Some((self.main_file_path.into(), self.main_content.into())),
        }
    }
    /// Find the import statement of a module in loaded modules, returning the importer & statement span.
    fn find_import(&self, module_path: &ModulePath) -> Option<(ModulePath, Span)> {
        let name = match (&module_path.origin, module_path.components.last()) {
            (_, Some(name)) => name.as_str(),
            (PathOrigin::Package(name), None) => name.as_str(),
            _ => return None,
        };
        let find_in = |content: &str| -> Option<Span> {
            // Imports might span multiple lines, so look for the name until the end of the statement.
            let mut import_start = None;
            let mut offset = 0;
            for line in content.split_inclusive('\n') {
                let trimmed = line.trim();
                if trimmed.starts_with("import") {
                    import_start = Some(offset + line.find("import").unwrap());
                }
                if let Some(start) = import_start {
                    let has_name = trimmed
                        .split(|c: char| !(c.is_alphanumeric() || c == '_'))
                        .any(|word| word == name);
                    if has_name {
                        let end = content[start..]
                            .find(';')
                            .map(|end| start + end + 1)
                            .unwrap_or(offset + line.len());
                        return Some(Span::new(start..end));
                    }
                    if trimmed.ends_with(';') {
                        import_start = None;
                    }
                }
                offset += line.len();
            }
            None
        };
        if let Some(span) = find_in(self.main_content) {
            return Some((self.main_module_path.clone(), span));
        }
        self.modules
            .borrow()
            .iter()
            .find_map(|(path, (_, content))| find_in(content).map(|span| (path.clone(), span)))
    }
}

impl Resolver for WeslResolver<'_> {
    fn resolve_source<'b>(&'b self, path: &ModulePath) -> Result<Cow<'b, str>, ResolveError> {
        if *path == self.main_module_path {
            return Ok(Cow::Borrowed(self.main_content));
        }
        let content = self.find_file(path).and_then(|file_path| {
            let content = (self.include_callback.borrow_mut())(&file_path).ok_or_else(|| {
                ResolveError::FileNotFound(file_path.clone(), "module file".into())
            })?;
            Ok((file_path, content))
        });
        let (file_path, content) = content.inspect_err(|_| {
            *self.unresolved_module.borrow_mut() = Some(path.clone());
        })?;
        self.modules
            .borrow_mut()
            .insert(path.clone(), (file_path, content.clone()));
        Ok(Cow::Owned(content))
    }
    fn display_name(&self, path: &ModulePath) -> Option<String> {
        self.fs_path(path)
            .ok()
            .map(|file_path| file_path.display().to_string())
    }
    fn fs_path(&self, path: &ModulePath) -> Result<PathBuf, ResolveError> {
        if *path == self.main_module_path {
            Ok(self.main_file_path.into())
        } else {
            self.find_file(path)
        }
    }
}

/// Origin of the code being validated, used to resolve modules into files.
struct ModuleContext<'a> {
    resolver: &'a WeslResolver<'a>,
}

impl<'a> ModuleContext<'a> {
    /// Get the file & content of a module.
    fn resolve(&self, module_path: Option<&ModulePath>) -> Option<(PathBuf, String)> {
        self.resolver.get_module(module_path)
    }
    /// Create a diagnostic from a span in a module. Fallback to the start of main file if not resolvable.
    fn create_diagnostic(
        &self,
        error: String,
        span: Option<Span>,
        module_path: Option<&ModulePath>,
    ) -> ShaderDiagnostic {
        let range = span
            .filter(|span| span.end > span.start)
            .and_then(|span| {
                let (file_path, content) = self.resolve(module_path)?;
                let content = content.as_str();
                // Only highlight the first line of the span, it might be a whole function.
                let end = content[span.start..span.end.min(content.len())]
                    .find(['\r', '\n'])
                    .map(|end| span.start + end)
                    .unwrap_or(span.end);
                let start = ShaderPosition::from_byte_offset(content, span.start).ok()?;
                let end = ShaderPosition::from_byte_offset(content, end).ok()?;
                Some(ShaderFileRange::new(file_path, start, end))
            })
            .unwrap_or(ShaderFileRange::zero(self.resolver.main_file_path.into()));
        ShaderDiagnostic {
            severity: ShaderDiagnosticSeverity::Error,
            error,
            range,
        }
    }
    fn from_wesl_error(&self, error: wesl::Error) -> ShaderDiagnostic {
        match error {
            wesl::Error::Error(diagnostic) => {
                let mut span = diagnostic.detail.span;
                let mut module_path = diagnostic.detail.module_path.clone();
                // Resolve errors have no location, point to the import statement instead.
                if span.is_none() {
                    let unresolved_module = self.resolver.unresolved_module.borrow().clone();
                    if let Some((importer, import_span)) = unresolved_module
                        .and_then(|unresolved| self.resolver.find_import(&unresolved))
                    {
                        span = Some(import_span);
                        module_path = Some(importer);
                    }
                }
                let message = match &diagnostic.detail.declaration {
                    Some(declaration) => format!("{} (in `{}`)", diagnostic.error, declaration),
                    None => diagnostic.error.to_string(),
                };
                self.create_diagnostic(message, span, module_path.as_ref())
            }
            error => self.create_diagnostic(error.to_string(), None, None),
        }
    }
    fn from_naga_error(
        &self,
        mapping: &WgslMapping,
        message: String,
        spans: impl Iterator<Item = (naga::Span, String)>,
    ) -> ShaderDiagnosticList {
        let message = mapping.unmangle(&message);
        let mut list = ShaderDiagnosticList::empty();
        for (span, label) in spans {
            let origin = span
                .to_range()
                .and_then(|range| mapping.find_origin(range.start));
            let error = if label.is_empty() {
                message.clone()
            } else {
                format!("{}\n{}", message, mapping.unmangle(&label))
            };
            list.push(match origin {
                Some((span, module_path)) => self.create_diagnostic(error, Some(span), module_path),
                None => self.create_diagnostic(error, None, None),
            });
        }
        if list.is_empty() {
            list.push(self.create_diagnostic(message, None, None));
        }
        list
    }
}

/// Concatenate an error and all its sources, as naga nests the real cause.
fn error_chain(error: &dyn std::error::Error) -> String {
    let mut message = error.to_string();
    let mut source = error.source();
    while let Some(error) = source {
        write!(message, ": {}", error).unwrap();
        source = error.source();
    }
    message
}

impl ValidatorImpl for Wesl {
    fn validate_shader(
        &self,
        shader_content: &str,
        file_path: &Path,
        params: &ShaderParams,
        include_callback: &mut dyn FnMut(&Path) -> Option<String>,
    ) -> Result<(CompilationResult, ShaderDiagnosticList), ShaderError> {
        let resolver = WeslResolver::new(file_path, shader_content, params, include_callback);
        let main_module_path = resolver.main_module_path.clone();
        let mut compiler = Compiler::default().with_resolver(&resolver);
        compiler.options.keep_main = true;
        compiler.options.sourcemap = true;
        // TODO: pass defines as compiler.options.features

        let context = ModuleContext {
            resolver: &resolver,
        };
        let compile_result: CompileResult = match compiler.compile_module(&main_module_path) {
            Ok(compile_result) => compile_result,
            Err(error) => {
                return Ok((
                    CompilationResult::None,
                    ShaderDiagnosticList::from(context.from_wesl_error(error)),
                ));
            }
        };
        let mapping = WgslMapping::new(&compile_result.syntax, compile_result.sourcemap.as_ref());

        // Wesl does not validate semantic, so rely on naga for this.
        let module = match wgsl::parse_str(&mapping.wgsl) {
            Ok(module) => module,
            Err(error) => {
                let labels = error
                    .labels()
                    .map(|(span, label)| (span, label.to_string()))
                    .collect::<Vec<_>>();
                let list =
                    context.from_naga_error(&mapping, error.message().into(), labels.into_iter());
                return Ok((CompilationResult::None, list));
            }
        };
        let mut validator =
            naga::valid::Validator::new(ValidationFlags::all(), Capabilities::all());
        match validator.validate(&module) {
            Ok(_) => Ok((
                CompilationResult::Wgsl(mapping.wgsl),
                ShaderDiagnosticList::empty(),
            )),
            Err(error) => {
                let list = context.from_naga_error(
                    &mapping,
                    error_chain(error.as_inner()),
                    error.spans().cloned(),
                );
                Ok((CompilationResult::None, list))
            }
        }
    }
    fn disassemble(&self, compilation_result: &CompilationResult) -> Result<String, ShaderError> {
        match compilation_result {
            CompilationResult::Wgsl(str) => Ok(str.clone()), // No disassembly, already human readable.
            CompilationResult::None | CompilationResult::Dxil(_) | CompilationResult::Spirv(_) => {
                Err(ShaderError::InternalErr(format!(
                    "Naga cannot disassemble {compilation_result:?}."
                )))
            }
        }
    }
    fn support(&self, shader_stage: ShaderStage) -> bool {
        match shader_stage {
            ShaderStage::Vertex | ShaderStage::Fragment | ShaderStage::Compute => true,
            _ => false,
        }
    }
}
