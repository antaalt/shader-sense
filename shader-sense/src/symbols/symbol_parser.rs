//! Trait and helper to parse tree-sitter AST
use std::path::{Path, PathBuf};

use tree_sitter::{Node, QueryMatch};

use crate::{
    position::{ShaderPosition, ShaderRange},
    shader::ShaderCompilationParams,
    shader_error::ShaderError,
    symbols::{
        symbol_list::{ShaderSymbolList, ShaderSymbolListRef},
        symbols::{ShaderSymbolData, ShaderSymbolMode},
    },
};

use super::{
    prepocessor::{ShaderPreprocessor, ShaderPreprocessorContext, ShaderRegion},
    shader_module::{ShaderModule, ShaderSymbols},
    symbol_provider::{SymbolIncludeCallback, SymbolProvider},
    symbols::{ShaderScope, ShaderSymbol},
};

pub(super) fn get_name<'a>(shader_content: &'a str, node: Node) -> &'a str {
    let range = node.range();
    &shader_content[range.start_byte..range.end_byte]
}

impl From<tree_sitter::Range> for ShaderRange {
    fn from(value: tree_sitter::Range) -> Self {
        ShaderRange::new(
            ShaderPosition::new(
                value.start_point.row as u32,
                value.start_point.column as u32,
            ),
            ShaderPosition::new(value.end_point.row as u32, value.end_point.column as u32),
        )
    }
}

impl From<tree_sitter::Point> for ShaderPosition {
    fn from(point: tree_sitter::Point) -> Self {
        ShaderPosition::new(point.row as u32, point.column as u32)
    }
}

pub struct ShaderSymbolListBuilder<'a> {
    shader_symbol_list: ShaderSymbolList,
    filter_callback: Box<&'a dyn Fn(&ShaderSymbol) -> bool>,
}
impl<'a> ShaderSymbolListBuilder<'a> {
    pub fn new(filter_callback: &'a dyn Fn(&ShaderSymbol) -> bool) -> Self {
        Self {
            shader_symbol_list: ShaderSymbolList::default(),
            filter_callback: Box::new(filter_callback),
        }
    }
    pub fn add_call_expression(&mut self, shader_symbol: ShaderSymbol) {
        if (self.filter_callback)(&shader_symbol) {
            self.shader_symbol_list.call_expression.push(shader_symbol);
        }
    }
    pub fn add_variable(&mut self, shader_symbol: ShaderSymbol) {
        if (self.filter_callback)(&shader_symbol) {
            self.shader_symbol_list.variables.push(shader_symbol);
        }
    }
    pub fn add_type(&mut self, shader_symbol: ShaderSymbol) {
        if (self.filter_callback)(&shader_symbol) {
            self.shader_symbol_list.types.push(shader_symbol);
        }
    }
    pub fn add_function(&mut self, shader_symbol: ShaderSymbol) {
        if (self.filter_callback)(&shader_symbol) {
            self.shader_symbol_list.functions.push(shader_symbol);
        }
    }
    pub fn get_shader_symbol_list(self) -> ShaderSymbolList {
        self.shader_symbol_list
    }
}

#[derive(Clone, Debug)]
pub struct ShaderWordRange {
    parent: Option<Box<ShaderWordRange>>, // Box to avoid recursive struct
    word: String,
    range: ShaderRange,
}

impl ShaderWordRange {
    pub fn new(word: String, range: ShaderRange, parent: Option<ShaderWordRange>) -> Self {
        Self {
            parent: match parent {
                Some(parent) => Some(Box::new(parent)),
                None => None,
            },
            word,
            range,
        }
    }
    pub fn get_word(&self) -> &str {
        &self.word
    }
    pub fn get_range(&self) -> &ShaderRange {
        &self.range
    }
    pub fn get_parent(&self) -> Option<&ShaderWordRange> {
        self.parent.as_ref().map(|p| p.as_ref())
    }
    fn get_parent_mut(&mut self) -> Option<&mut ShaderWordRange> {
        self.parent.as_mut().map(|p| p.as_mut())
    }
    pub fn set_root_parent(&mut self, root_parent: ShaderWordRange) {
        // Use a raw pointer to traverse without holding a mutable borrow
        let mut parent: *mut ShaderWordRange = self;
        unsafe {
            while let Some(p) = (*parent).get_parent_mut() {
                parent = p;
            }
            // Now parent is the deepest node, safe to assign
            (*parent).parent = Some(Box::new(root_parent));
        }
    }
    pub fn get_word_stack(&self) -> Vec<&ShaderWordRange> {
        let mut current_word = self;
        let mut stack = Vec::new();
        stack.push(self);
        while let Some(parent) = &current_word.parent {
            stack.push(parent.as_ref());
            current_word = parent.as_ref();
        }
        stack
    }
    pub fn set_parent(&mut self, parent: ShaderWordRange) {
        self.parent = Some(Box::new(parent));
    }
    pub fn is_field(&self) -> bool {
        self.parent.is_some()
    }
    // Look for matching symbol in symbol_list
    pub fn find_symbol_from_parent(
        &self,
        file_path: PathBuf,
        symbol_list: &ShaderSymbolListRef,
    ) -> Vec<ShaderSymbol> {
        if self.parent.is_none() {
            // Could be either a variable, a link, or a type.
            symbol_list
                .find_symbols_at(&self.word, &self.range.end.clone_into_file(file_path))
                .iter()
                .map(|s| (*s).clone())
                .collect()
        } else {
            // Will be a variable or function (root only), method, or member if chained.
            let stack = self.get_word_stack();
            let mut rev_stack = stack.iter().rev();
            let cursor_position = self.range.end.clone_into_file(file_path.clone());
            // Look for root symbol (either a function or variable)
            let root_symbol = match rev_stack.next() {
                Some(current_word) => {
                    match symbol_list.find_scoped_symbol(&current_word.word, &cursor_position) {
                        Some(symbol) => {
                            match &symbol.data {
                                ShaderSymbolData::CallExpression {
                                    label,
                                    range: _,
                                    parameters: _,
                                } => {
                                    match symbol_list
                                        .find_scoped_function_symbol(label, &cursor_position)
                                    {
                                        Some(function) => {
                                            if let ShaderSymbolData::Functions { signatures: _ } =
                                                &function.data
                                            {
                                                symbol
                                            } else {
                                                return vec![]; // Not a valid function
                                            }
                                        }
                                        None => return vec![], // No matching function found
                                    }
                                }
                                ShaderSymbolData::Functions { signatures: _ } => symbol,
                                ShaderSymbolData::Variables { ty: _, count: _ } => symbol,
                                ShaderSymbolData::Enum { values: _ } => symbol,
                                _ => return vec![], // Symbol found is not a variable nor a function.
                            }
                        }
                        None => {
                            return vec![]; // No variable found for main parent.
                        }
                    }
                }
                None => unreachable!("Should always have at least one symbol on this path."),
            };
            // Now loop over child for matching member elements
            let mut current_symbols = vec![root_symbol.clone()];
            while let Some(next_item) = &rev_stack.next() {
                // TODO: for now, we naively pick the first signature.
                // But we should pick instead the one closest by analyzing parameters.
                let ty = match &current_symbols[0].data {
                    // CallExpression & variable will only be called on first iteration
                    ShaderSymbolData::CallExpression {
                        label,
                        range: _,
                        parameters: _,
                    } => {
                        match symbol_list.find_scoped_function_symbol(label, &cursor_position) {
                            Some(function) => {
                                if let ShaderSymbolData::Functions { signatures } = &function.data {
                                    &signatures[0].returnType
                                } else {
                                    return vec![]; // Not a valid function
                                }
                            }
                            None => return vec![], // No matching function found
                        }
                    }
                    ShaderSymbolData::Enum { values } => match values
                        .iter()
                        .find(|v| v.label == next_item.get_word())
                        .map(|v| v.as_symbol(Some(file_path.clone()), &current_symbols[0].label))
                    {
                        Some(symbol) => return vec![symbol],
                        None => return vec![],
                    },
                    ShaderSymbolData::Functions { signatures } => &signatures[0].returnType,
                    ShaderSymbolData::Variables { ty, count: _ } => &ty,
                    // Method & parameter will only be called after first iteration
                    ShaderSymbolData::Method {
                        context: _,
                        signatures,
                    } => &signatures[0].returnType,
                    ShaderSymbolData::Parameter {
                        context: _,
                        ty,
                        count: _,
                    } => &ty,
                    _ => return vec![], // Invalid type
                };
                // Find the type symbol of the variable / method.
                let symbol_ty = match symbol_list.find_scoped_type_symbol(&ty, &cursor_position) {
                    Some(ty_symbol) => ty_symbol,
                    None => return vec![], // No matching type found
                };
                // Find the variable chained from the type.
                let symbols: Vec<ShaderSymbol> = match &symbol_ty.data {
                    ShaderSymbolData::Struct {
                        constructors: _,
                        members,
                        methods,
                    } => {
                        let file_path = if let ShaderSymbolMode::Runtime(runtime) = &symbol_ty.mode
                        {
                            Some(runtime.file_path.clone())
                        } else {
                            None
                        };
                        let member_symbols: Vec<ShaderSymbol> = members
                            .iter()
                            .filter(|m| m.parameters.label == next_item.word)
                            .map(|m| m.as_symbol(file_path.clone()))
                            .collect();
                        let method_symbols: Vec<ShaderSymbol> = methods
                            .iter()
                            .filter(|m| m.label == next_item.word)
                            .map(|m| m.as_symbol(file_path.clone()))
                            .collect();
                        [member_symbols, method_symbols].concat()
                    }
                    ShaderSymbolData::Types { constructors: _ } => {
                        return vec![]; // Cannot chain a default type.
                    }
                    _ => return vec![], // Data useless.
                };
                if symbols.is_empty() {
                    return vec![]; // No matching member / methods found.
                } else {
                    current_symbols = symbols;
                }
            }
            current_symbols
        }
    }
}

pub trait SymbolTreeParser {
    // The query to match tree node
    fn get_query(&self) -> String;
    // Process the match & convert it to symbol
    fn process_match(
        &self,
        symbol_match: &QueryMatch,
        file_path: &Path,
        shader_content: &str,
        scopes: &Vec<ShaderScope>,
        symbols: &mut ShaderSymbolListBuilder,
    );
    fn compute_scope_stack(
        &self,
        scopes: &Vec<ShaderScope>,
        range: &ShaderRange,
    ) -> Vec<ShaderScope> {
        scopes
            .iter()
            .filter_map(|e| {
                if e.contain_bounds(&range) {
                    Some(e.clone())
                } else {
                    None
                }
            })
            .collect::<Vec<ShaderScope>>()
    }
}

pub trait SymbolRegionFinder {
    fn query_regions_in_node<'a>(
        &self,
        shader_module: &ShaderModule,
        symbol_provider: &SymbolProvider,
        shader_params: &ShaderCompilationParams,
        node: tree_sitter::Node,
        preprocessor: &mut ShaderPreprocessor,
        context: &'a mut ShaderPreprocessorContext,
        include_callback: &'a mut SymbolIncludeCallback<'a>,
        old_symbols: Option<ShaderSymbols>,
    ) -> Result<Vec<ShaderRegion>, ShaderError>;
}

pub trait SymbolTreePreprocessorParser {
    // The query to match tree node
    fn get_query(&self) -> String;
    // Process the match & convert it to preprocessor
    fn process_match(
        &self,
        symbol_match: &QueryMatch,
        file_path: &Path,
        shader_content: &str,
        preprocessor: &mut ShaderPreprocessor,
        context: &mut ShaderPreprocessorContext,
    );
}

pub trait SymbolWordProvider {
    fn find_word_at_position_in_node(
        &self,
        shader_module: &ShaderModule,
        node: Node,
        position: &ShaderPosition,
    ) -> Result<ShaderWordRange, ShaderError>;
}
