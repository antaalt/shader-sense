use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
};

use serde::{Deserialize, Serialize};

use crate::{include::IncludeHandler, shader::ShaderStage, shader_error::ShaderDiagnostic};

use super::{symbol_provider::ShaderSymbolParams, symbol_tree::ShaderSymbols};

#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub struct ShaderParameter {
    pub ty: String,
    pub label: String,
    pub count: Option<u32>,
    pub description: String,
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
}

#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub struct ShaderPosition {
    pub file_path: PathBuf, // TODO:TREE: move filepath out of there for clarity. One tree = one file.
    pub line: u32,
    pub pos: u32,
}
impl Ord for ShaderPosition {
    fn cmp(&self, other: &Self) -> Ordering {
        (&self.file_path, &self.line, &self.pos).cmp(&(&other.file_path, &other.line, &other.pos))
    }
}

impl PartialOrd for ShaderPosition {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for ShaderPosition {
    fn eq(&self, other: &Self) -> bool {
        (&self.file_path, &self.line, &self.pos) == (&other.file_path, &other.line, &other.pos)
    }
}

impl Eq for ShaderPosition {}

impl ShaderPosition {
    pub fn new(file_path: PathBuf, line: u32, pos: u32) -> Self {
        Self {
            file_path,
            line,
            pos,
        }
    }

    pub fn from_byte_offset(
        content: &str,
        byte_offset: usize,
        file_path: &Path,
    ) -> std::io::Result<ShaderPosition> {
        // https://en.wikipedia.org/wiki/UTF-8
        if byte_offset == 0 {
            Ok(ShaderPosition {
                line: 0,
                pos: 0,
                file_path: PathBuf::from(file_path),
            })
        } else if content.len() == 0 {
            Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "Content is empty.",
            ))
        } else if byte_offset >= content.len() {
            Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "byte_offset is out of bounds.",
            ))
        } else {
            let line = content[..byte_offset].lines().count() - 1;
            let line_start = content[..byte_offset]
                .lines()
                .last()
                .expect("No last line available.");
            let pos = content[byte_offset..].as_ptr() as usize - line_start.as_ptr() as usize;
            if line_start.is_char_boundary(pos) {
                Ok(ShaderPosition {
                    line: line as u32,
                    pos: pos as u32,
                    file_path: PathBuf::from(file_path),
                })
            } else {
                Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "Pos in line is not at UTF8 char boundary.",
                ))
            }
        }
    }
    pub fn to_byte_offset(&self, content: &str) -> std::io::Result<usize> {
        // https://en.wikipedia.org/wiki/UTF-8
        match content.lines().nth(self.line as usize) {
            Some(line) => {
                // This pointer operation is safe to operate because lines iterator should start at char boundary.
                let line_byte_offset = line.as_ptr() as usize - content.as_ptr() as usize;
                assert!(
                    content.is_char_boundary(line_byte_offset),
                    "Start of line is not char boundary."
                );
                // We have line offset, find pos offset.
                match content[line_byte_offset..]
                    .char_indices()
                    .nth(self.pos as usize)
                {
                    Some((byte_offset, _)) => {
                        let global_offset = line_byte_offset + byte_offset;
                        if content.len() <= global_offset {
                            Err(std::io::Error::new(
                                std::io::ErrorKind::InvalidData,
                                "Byte offset is not in content range.",
                            ))
                        } else if !content.is_char_boundary(global_offset) {
                            Err(std::io::Error::new(
                                std::io::ErrorKind::InvalidData,
                                "Position is not at UTF8 char boundary.",
                            ))
                        } else {
                            Ok(global_offset)
                        }
                    }
                    None => {
                        if self.pos as usize == line.chars().count() {
                            assert!(content.is_char_boundary(line_byte_offset + line.len()));
                            Ok(line_byte_offset + line.len())
                        } else {
                            Err(std::io::Error::new(
                                std::io::ErrorKind::InvalidInput,
                                format!("Position is not in range of line"),
                            ))
                        }
                    }
                }
            }
            // Last line in line iterator is skipped if its empty.
            None => Ok(content.len()), // Line is out of bounds, assume its at the end.
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct ShaderRange {
    pub start: ShaderPosition,
    pub end: ShaderPosition,
}

pub type ShaderScope = ShaderRange;

impl ShaderRange {
    pub fn new(start: ShaderPosition, end: ShaderPosition) -> Self {
        Self { start, end }
    }
    pub fn whole(_content: &str) -> Self {
        todo!()
    }
    pub fn contain_bounds(&self, position: &ShaderRange) -> bool {
        self.contain(&position.start) && self.contain(&position.end)
    }
    pub fn contain(&self, position: &ShaderPosition) -> bool {
        assert!(
            self.start.file_path == self.end.file_path,
            "Position start & end should have same value."
        );
        // Check same file
        if position.file_path == self.start.file_path {
            // Check line & position bounds.
            if position.line > self.start.line && position.line < self.end.line {
                true
            } else if position.line == self.start.line && position.line == self.end.line {
                position.pos >= self.start.pos && position.pos <= self.end.pos
            } else if position.line == self.start.line && position.line < self.end.line {
                position.pos >= self.start.pos
            } else if position.line == self.end.line && position.line > self.start.line {
                position.pos <= self.end.pos
            } else {
                false
            }
        } else {
            false
        }
    }
    pub fn join(mut lhs: ShaderRange, rhs: ShaderRange) -> ShaderRange {
        lhs.start = std::cmp::min(lhs.start, rhs.start);
        lhs.end = std::cmp::min(lhs.end, rhs.end);
        lhs
    }
}

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
    defines: HashMap<String, String>, // TODO: Should store position aswell... At least target file path.
    include_handler: IncludeHandler,
    dirty_files: HashSet<PathBuf>, // Dirty files that need to be recomputed no matter what.
    depth: u32,
}

impl ShaderPreprocessorContext {
    pub fn main(file_path: &Path, symbol_params: ShaderSymbolParams) -> Self {
        Self {
            defines: symbol_params.defines,
            include_handler: IncludeHandler::main(
                &file_path,
                symbol_params.includes,
                symbol_params.path_remapping,
            ),
            dirty_files: HashSet::new(),
            depth: 0,
        }
    }
    pub fn mark_dirty(&mut self, file_path: &Path) {
        self.dirty_files.insert(file_path.into());
    }
    pub fn search_path_in_includes(&mut self, path: &Path) -> Option<PathBuf> {
        self.include_handler.search_path_in_includes(path)
    }
    pub fn append_defines(&mut self, defines: Vec<ShaderPreprocessorDefine>) {
        for define in defines {
            self.defines
                .insert(define.name, define.value.unwrap_or("".into()));
        }
    }
    pub fn increase_depth(&mut self) -> bool {
        const DEPTH_LIMIT: u32 = 30;
        self.depth += 1;
        self.depth < DEPTH_LIMIT
    }
    pub fn decrease_depth(&mut self) {
        assert!(self.depth > 0, "Decreasing depth but zero.");
        self.depth -= 1;
    }
    pub fn is_visited(&mut self, path: &Path) -> bool {
        self.include_handler.is_visited(path)
    }
    pub fn visit_file(&mut self, path: &Path) {
        self.include_handler.visit_file(path);
    }
    pub fn is_dirty(&self, file_path: &Path, context: &ShaderPreprocessorContext) -> bool {
        // Compare defines to determine if context is different.
        // Check if we need to force an update aswell.
        context.defines != self.defines || context.dirty_files.contains(file_path)
    }
    pub fn get_define_value(&self, name: &str) -> Option<String> {
        self.defines
            .iter()
            .find(|(key, _)| *key == name)
            .map(|(_, value)| value.clone())
    }
    pub fn get_defines(&self) -> &HashMap<String, String> {
        &self.defines
    }
}

#[derive(Debug, Default, Clone)]
pub struct ShaderPreprocessorInclude {
    pub relative_path: String,
    pub absolute_path: PathBuf,
    pub range: ShaderRange,
    pub cache: Option<ShaderSymbols>,
}

#[derive(Debug, Default, Clone)]
pub struct ShaderPreprocessorDefine {
    pub name: String,
    pub range: Option<ShaderRange>,
    pub value: Option<String>,
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
    pub fn new(name: String, range: ShaderRange, value: Option<String>) -> Self {
        Self {
            name,
            range: Some(range),
            value,
        }
    }
}
impl ShaderPreprocessorInclude {
    pub fn new(relative_path: String, absolute_path: PathBuf, range: ShaderRange) -> Self {
        Self {
            relative_path,
            absolute_path,
            range,
            cache: None,
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
    /*pub fn preprocess_symbols(&self, shader_symbols: &mut ShaderSymbolTree) {
        // Filter inactive regions symbols
        shader_symbols.retain(|symbol| {
            let is_in_inactive_region = match &symbol.range {
                Some(range) => {
                    for region in &self.regions {
                        if !region.is_active && region.range.contain_bounds(&range) {
                            return false; // Symbol is in inactive region. Remove it.
                        }
                    }
                    true
                }
                None => true, // keep
            };
            is_in_inactive_region
        });
        // Add defines
        let mut define_symbols: Vec<ShaderSymbol> = self
            .defines
            .iter()
            .map(|define| {
                ShaderSymbol {
                    label: define.name.clone(),
                    description: match &define.value {
                        Some(value) => {
                            format!("Preprocessor macro. Expanding to \n```\n{}\n```", value)
                        }
                        None => format!("Preprocessor macro."),
                    },
                    version: "".into(),
                    stages: vec![],
                    link: None,
                    data: ShaderSymbolData::Macro {
                        value: match &define.value {
                            Some(value) => value.clone(),
                            None => "".into(),
                        },
                    },
                    range: define.range.clone(),
                    content: None, // No content for define
                }
            })
            .collect();
        // Add includes as symbol
        let mut include_symbols: Vec<ShaderSymbol> = self
            .includes
            .iter()
            .map(|include| {
                ShaderSymbol {
                    label: include.relative_path.clone(),
                    description: format!("Including file {}", include.absolute_path.display()),
                    version: "".into(),
                    stages: vec![],
                    link: None,
                    data: ShaderSymbolData::Link {
                        target: ShaderPosition::new(include.absolute_path.clone(), 0, 0),
                    },
                    range: Some(include.range.clone()),
                    content: None, // No content for include
                }
            })
            .collect();
        shader_symbols.macros.append(&mut define_symbols);
        shader_symbols.includes.append(&mut include_symbols);
    }*/
}

pub type ShaderMember = ShaderParameter;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ShaderMethod {
    pub label: String,
    pub signature: ShaderSignature,
}

impl ShaderMember {
    pub fn as_symbol(&self) -> ShaderSymbol {
        ShaderSymbol {
            label: self.label.clone(),
            description: self.description.clone(),
            version: "".into(),
            stages: vec![],
            link: None,
            data: ShaderSymbolData::Variables {
                ty: self.ty.clone(),
                count: self.count,
            },
            range: None,   // Should have a position ?
            content: None, // No content for member
        }
    }
}

impl ShaderMethod {
    pub fn as_symbol(&self) -> ShaderSymbol {
        ShaderSymbol {
            label: self.label.clone(),
            description: self.signature.description.clone(),
            version: "".into(),
            stages: vec![],
            link: None,
            data: ShaderSymbolData::Functions {
                signatures: vec![self.signature.clone()],
            },
            range: None,   // Should have a position ?
            content: None, // TODO:TREE: content for method
        }
    }
}

#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub enum ShaderSymbolData {
    #[default]
    None,
    // A bit of duplicate from variables ? Should be struct (Which should be renamed something else)
    Types {
        // Constructor in child ? How to handle it ? If same name as parent, assume constructor ?
        constructors: Vec<ShaderSignature>,
    },
    Struct {
        constructors: Vec<ShaderSignature>, // Need a range aswell for hover.
        // TODO: Can be moved to child.
        members: Vec<ShaderMember>, // Need a range aswell for hover.
        methods: Vec<ShaderMethod>, // Need a range aswell for hover.
    },
    Constants {
        ty: String,
        qualifier: String,
        value: String,
    },
    Functions {
        signatures: Vec<ShaderSignature>,
    },
    Keyword {},
    // Mostly runtime, but GLSL has global variable in builtin that need serial.
    Variables {
        ty: String,
        count: Option<u32>,
    },
    #[serde(skip)] // This is runtime only. No serialization.
    Scope {},
    #[serde(skip)] // This is runtime only. No serialization.
    CallExpression {
        label: String,
        range: ShaderRange, // label range.
        parameters: Vec<(String, ShaderRange)>,
    },
    #[serde(skip)] // This is runtime only. No serialization.
    Link {
        // TODO:TREE: Rename include
        target: ShaderPosition,
    },
    #[serde(skip)] // This is runtime only. No serialization.
    Macro {
        // TODO:TREE: harmonise define & macro
        value: String,
    },
}

#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub struct ShaderSymbolContent {
    #[serde(skip)] // Runtime info. No serialization.
    pub range: Option<ShaderRange>, // Range of symbol scope. TODO:TREE: Remove option.
    pub childrens: Vec<ShaderSymbol>, // Symbols found in symbol scope
}

#[allow(non_snake_case)] // for JSON
#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub struct ShaderSymbol {
    pub label: String,                        // Label for the item
    pub description: String,                  // Description of the item
    pub version: String,                      // Minimum version required for the item.
    pub stages: Vec<ShaderStage>,             // Shader stages of the item
    pub link: Option<String>,                 // Link to some external documentation
    pub data: ShaderSymbolData,               // Data for the variable
    pub content: Option<ShaderSymbolContent>, // Content of shader (function & structure)
    #[serde(skip)] // Runtime info. No serialization.
    pub range: Option<ShaderRange>, // Range of symbol label in shader
}

#[derive(Debug, Default, Serialize, Deserialize, Clone, PartialEq, Eq)]
pub enum ShaderSymbolType {
    #[default]
    Types,
    Constants,
    Variables,
    CallExpression,
    Functions,
    Keyword,
    Macros,
    Include,
    Scope,
}

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct ShaderSymbolTree {
    root: ShaderSymbolContent, // TODO:TREE: Could have a vec of shader symbol directly here as range is useless
    contain_builtins: bool,
}

// TODO:TREE: could be a dedicated shadersymboltree instead, for handling struct as a scope with childrens.
#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct ShaderBuiltinSymbol {
    symbols: Vec<ShaderSymbol>,
}

impl ShaderBuiltinSymbol {
    pub fn push(&mut self, symbol: ShaderSymbol) {
        self.symbols.push(symbol);
    }
    pub fn append(&mut self, symbols: &mut Vec<ShaderSymbol>) {
        self.symbols.append(symbols);
    }
}

impl ShaderSymbolTree {
    // TODO:TREE: move
    pub fn parse_from_json(file_content: String) -> ShaderBuiltinSymbol {
        serde_json::from_str::<ShaderBuiltinSymbol>(&file_content)
            .expect("Failed to parse ShaderBuiltinSymbol")
    }
    // Find all symbols which are defined at a given position, ignoring those who are not.
    pub fn find_symbols_defined_at(&self, position: &ShaderPosition) -> Vec<&ShaderSymbol> {
        self.iter_all()
            .filter(|symbol| {
                // Skip transient symbols
                let is_transient = match symbol.get_type() {
                    Some(ty) => ty.is_transient(),
                    None => false,
                };
                let is_in_range = match &symbol.range {
                    Some(range) => *position > range.start,
                    None => true, // Keep global symbols
                };
                is_in_range && !is_transient
            })
            .collect()
    }
    // Find symbol at given position and return its stack in the tree
    pub fn find_symbol_at(&self, position: &ShaderPosition) -> Vec<&ShaderSymbol> {
        fn find_symbol_at_level<'a>(
            content: &'a ShaderSymbolContent,
            position: &ShaderPosition,
        ) -> Vec<&'a ShaderSymbol> {
            match content
                .childrens
                .iter()
                .find(|symbol| match &symbol.content {
                    Some(content) => match &content.range {
                        Some(range) => range.contain(position),
                        None => false,
                    },
                    None => false,
                }) {
                Some(children) => match &children.content {
                    Some(content) => {
                        let mut stack = vec![children];
                        stack.extend(find_symbol_at_level(content, position));
                        stack
                    }
                    None => vec![children],
                },
                None => Vec::new(),
            }
        }
        find_symbol_at_level(&self.root, position)
    }
    // Find symbol at given position and return its stack in the tree
    pub fn find_parent_symbol(&mut self, position: &ShaderPosition) -> Option<&mut ShaderSymbol> {
        fn find_symbol_at_level<'a>(
            content: &'a mut ShaderSymbolContent,
            position: &ShaderPosition,
        ) -> Option<*mut ShaderSymbol> {
            match content
                .childrens
                .iter_mut()
                .find(|symbol| match &symbol.content {
                    Some(content) => match &content.range {
                        Some(range) => range.contain(position),
                        None => false,
                    },
                    None => false,
                }) {
                Some(children) => match &mut children.content {
                    Some(content) => match find_symbol_at_level(content, position) {
                        Some(symbol) => Some(symbol),
                        None => Some(children),
                    },
                    None => None,
                },
                None => None,
            }
        }
        // SAFETY: we just iterate on it and never edit it.
        find_symbol_at_level(&mut self.root, position).map(|s| unsafe { s.as_mut().unwrap() })
    }
    pub fn add_global_symbol(&mut self, symbol: ShaderSymbol) {
        self.root.childrens.push(symbol);
    }
    pub fn append(&mut self, tree: ShaderSymbolTree) {
        for children in tree.root.childrens {
            self.root.childrens.push(children);
        }
    }
    pub fn append_builtins(&mut self, builtins: ShaderBuiltinSymbol) {
        assert!(!self.contain_builtins);
        self.contain_builtins = true;
        for children in builtins.symbols {
            self.root.childrens.push(children);
        }
    }
    // Get content from symbols.
    pub fn get_content(&self) -> &ShaderSymbolContent {
        &self.root
    }
    // Only iterate on variables accessible at root scope.
    pub fn iter_root(&self) -> ShaderSymbolTreeIterator {
        ShaderSymbolTreeIterator::new(self, false)
    }
    // Iterate on all variables by recursing children
    pub fn iter_all(&self) -> ShaderSymbolTreeIterator {
        ShaderSymbolTreeIterator::new(self, true)
    }
    // Only iterate mutably on variables accessible at root scope.
    pub fn iter_root_mut(&mut self) -> ShaderSymbolTreeIteratorMut {
        ShaderSymbolTreeIteratorMut::new(self, false)
    }
    // Iterate mutably on all variables by recursing children
    pub fn iter_all_mut(&mut self) -> ShaderSymbolTreeIteratorMut {
        ShaderSymbolTreeIteratorMut::new(self, true)
    }
}

pub struct ShaderSymbolTreeIterator<'a> {
    stack: Vec<std::slice::Iter<'a, ShaderSymbol>>,
    global: bool,
}
impl<'a> ShaderSymbolTreeIterator<'a> {
    fn new(tree: &'a ShaderSymbolTree, global: bool) -> Self {
        Self {
            stack: vec![tree.root.childrens.iter()],
            global,
        }
    }
}
impl<'a> Iterator for ShaderSymbolTreeIterator<'a> {
    type Item = &'a ShaderSymbol;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(top) = self.stack.last_mut() {
            if let Some(symbol) = top.next() {
                // If this symbol has children, and we want to recurse them, push their iterator to the stack for depth first search
                if self.global {
                    if let Some(content) = &symbol.content {
                        self.stack.push(content.childrens.iter());
                    }
                }
                return Some(symbol);
            } else {
                self.stack.pop();
            }
        }
        None
    }
}

pub struct ShaderSymbolTreeIteratorMut<'a> {
    stack: Vec<std::slice::IterMut<'a, ShaderSymbol>>,
    current: *mut ShaderSymbol,
    global: bool,
}
impl<'a> ShaderSymbolTreeIteratorMut<'a> {
    fn new(tree: &'a mut ShaderSymbolTree, global: bool) -> Self {
        Self {
            stack: vec![tree.root.childrens.iter_mut()],
            current: std::ptr::null_mut(), // unsafe pointer to avoid storing a mutable ref that prevent returning it.
            global,
        }
    }
}
impl<'a> Iterator for ShaderSymbolTreeIteratorMut<'a> {
    type Item = &'a mut ShaderSymbol;

    fn next(&mut self) -> Option<Self::Item> {
        // Add child stack if current has child.
        if self.global {
            if let Some(current) = unsafe { self.current.as_mut() } {
                if let Some(content) = &mut current.content {
                    self.stack.push(content.childrens.iter_mut());
                }
            }
        }
        // Reset pointer for safety.
        self.current = std::ptr::null_mut();
        while let Some(top) = self.stack.last_mut() {
            if let Some(symbol) = top.next() {
                self.current = symbol;
                break;
            } else {
                self.stack.pop();
            }
        }
        unsafe { self.current.as_mut() }
    }
}

// TODO:TREE: into iter convert to flat.

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
    pub fn get_type(&self) -> Option<ShaderSymbolType> {
        match &self.data {
            ShaderSymbolData::None => None,
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
            ShaderSymbolData::CallExpression {
                label: _,
                range: _,
                parameters: _,
            } => Some(ShaderSymbolType::CallExpression),
            ShaderSymbolData::Functions { signatures: _ } => Some(ShaderSymbolType::Functions),
            ShaderSymbolData::Keyword {} => Some(ShaderSymbolType::Keyword),
            ShaderSymbolData::Link { target: _ } => Some(ShaderSymbolType::Include),
            ShaderSymbolData::Macro { value: _ } => Some(ShaderSymbolType::Macros),
            ShaderSymbolData::Scope {} => Some(ShaderSymbolType::Scope),
        }
    }
    pub fn is_type(&self, ty: ShaderSymbolType) -> bool {
        match self.get_type() {
            Some(internal_ty) => internal_ty == ty,
            None => false,
        }
    }
    pub fn format(&self) -> String {
        match &self.data {
            ShaderSymbolData::None => format!("Unknown {}", self.label.clone()),
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
                Some(count) => format!("{} {}[{}]", ty, self.label, count),
                None => format!("{} {}", ty, self.label),
            },
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
            ShaderSymbolData::Link { target } => {
                format!("\"{}\":{}:{}", self.label, target.line, target.pos)
            }
            ShaderSymbolData::Macro { value } => {
                format!("#define {} {}", self.label, value)
            }
            ShaderSymbolData::Scope {} => "{}".into(),
        }
    }
}
