use std::path::Path;

use tree_sitter::{Node, QueryMatch};

use crate::{
    shader_error::ShaderError,
    symbols::symbols::{ShaderPosition, ShaderRange, ShaderSymbolTree},
};

use super::{
    symbol_provider::{SymbolIncludeCallback, SymbolProvider},
    symbol_tree::{ShaderSymbols, SymbolTree},
    symbols::{
        ShaderPreprocessor, ShaderPreprocessorContext, ShaderRegion, ShaderScope, ShaderSymbol,
    },
};

pub(super) fn get_name<'a>(shader_content: &'a str, node: Node) -> &'a str {
    let range = node.range();
    &shader_content[range.start_byte..range.end_byte]
}

impl ShaderRange {
    pub(super) fn from_range(value: tree_sitter::Range, file_path: &Path) -> Self {
        ShaderRange {
            start: ShaderPosition {
                file_path: file_path.into(),
                line: value.start_point.row as u32,
                pos: value.start_point.column as u32,
            },
            end: ShaderPosition {
                file_path: file_path.into(),
                line: value.end_point.row as u32,
                pos: value.end_point.column as u32,
            },
        }
    }
}

impl ShaderPosition {
    pub(super) fn from_tree_sitter_point(point: tree_sitter::Point, file_path: &Path) -> Self {
        ShaderPosition {
            file_path: file_path.into(),
            line: point.row as u32,
            pos: point.column as u32,
        }
    }
}

pub struct ShaderSymbolTreeBuilder<'a> {
    shader_symbol_tree: ShaderSymbolTree,
    filter_callback: Box<&'a dyn Fn(&ShaderSymbol) -> bool>,
}
impl<'a> ShaderSymbolTreeBuilder<'a> {
    pub fn new(filter_callback: &'a dyn Fn(&ShaderSymbol) -> bool) -> Self {
        Self {
            shader_symbol_tree: ShaderSymbolTree::default(),
            filter_callback: Box::new(filter_callback),
        }
    }
    pub fn add_children(&mut self, shader_symbol: ShaderSymbol, global_scope: bool) {
        if (self.filter_callback)(&shader_symbol) {
            if !global_scope {
                match self.shader_symbol_tree.iter_all_mut().find(|symbol| {
                    match &symbol.content {
                        Some(content) => match &content.range {
                            Some(content_range) => match &shader_symbol.range {
                                Some(symbol_range) => content_range.contain_bounds(&symbol_range),
                                None => false, // Not in range
                            },
                            None => false,
                        },
                        None => false, // No content
                    }
                }) {
                    Some(symbol) => {
                        // We validated that their is content in find, so unwrap is safe.
                        symbol
                            .content
                            .as_mut()
                            .unwrap()
                            .childrens
                            .push(shader_symbol);
                    }
                    None => {
                        // No content found, adding to global scope.
                        self.shader_symbol_tree.add_global_symbol(shader_symbol)
                    }
                }
            } else {
                self.shader_symbol_tree.add_global_symbol(shader_symbol);
            }
        } // else filtered out.
    }
    pub fn get_shader_symbol_tree(&mut self) -> ShaderSymbolTree {
        std::mem::take(&mut self.shader_symbol_tree)
    }
}

pub trait SymbolTreeParser {
    // The query to match tree node
    fn get_query(&self) -> String;
    // Process the match & convert it to symbol
    fn process_match(
        &self,
        matches: QueryMatch,
        file_path: &Path,
        shader_content: &str,
        symbols: &mut ShaderSymbolTreeBuilder,
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
pub trait SymbolTreeFilter {
    // Filter symbol, keep them on true, remove them on false
    fn filter_symbol(&self, shader_symbol: &ShaderSymbol, file_name: &String) -> bool;
}

pub trait SymbolRegionFinder {
    fn query_regions_in_node<'a>(
        &self,
        symbol_tree: &SymbolTree,
        symbol_provider: &SymbolProvider,
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
        matches: QueryMatch,
        file_path: &Path,
        shader_content: &str,
        preprocessor: &mut ShaderPreprocessor,
        context: &mut ShaderPreprocessorContext,
    );
}

pub trait SymbolLabelChainProvider {
    fn find_label_chain_at_position_in_node(
        &self,
        symbol_tree: &SymbolTree,
        node: Node,
        position: &ShaderPosition,
    ) -> Result<Vec<(String, ShaderRange)>, ShaderError>;
}

pub trait SymbolLabelProvider {
    fn find_label_at_position_in_node(
        &self,
        symbol_tree: &SymbolTree,
        node: Node,
        position: &ShaderPosition,
    ) -> Result<(String, ShaderRange), ShaderError>;
}
