//! Shader module storing the [`tree_sitter`] AST
use std::{
    cell::RefCell,
    path::{Path, PathBuf},
    rc::Rc,
};

use tree_sitter::{Tree, TreeCursor};

use crate::{
    shader::ShaderContextParams,
    symbols::symbol_list::{ShaderSymbolList, ShaderSymbolListRef},
};

use super::prepocessor::{
    ShaderPreprocessor, ShaderPreprocessorContext, ShaderPreprocessorInclude,
};

/// Shader module holding the [`tree_sitter`]  AST.
/// Need to be created with [`ShaderModuleParser`]
#[derive(Debug, Clone)]
pub struct ShaderModule {
    pub file_path: PathBuf,
    pub content: String,
    pub tree: Tree,
}

pub type ShaderModuleHandle = Rc<RefCell<ShaderModule>>;

#[derive(Debug, Default, Clone)]
pub struct ShaderSymbols {
    pub(super) preprocessor: ShaderPreprocessor,
    pub(super) symbol_list: ShaderSymbolList,
}
impl ShaderSymbols {
    pub fn new(file_path: &Path, shader_params: ShaderContextParams) -> Self {
        Self {
            preprocessor: ShaderPreprocessor::new(ShaderPreprocessorContext::main(
                file_path,
                shader_params,
            )),
            symbol_list: ShaderSymbolList::default(),
        }
    }
    pub fn get_all_symbols<'a>(&'a self) -> ShaderSymbolListRef<'a> {
        let mut symbols = self.get_local_symbols();
        for include in &self.preprocessor.includes {
            assert!(
                include.cache.is_some(),
                "Include {} do not have cache, but is being queried.\n{}",
                include.get_relative_path(),
                self.dump_dependency_tree(&PathBuf::from("oui"))
            );
            symbols.append(include.get_cache().get_all_symbols());
        }
        symbols
    }
    pub fn get_local_symbols<'a>(&'a self) -> ShaderSymbolListRef<'a> {
        self.preprocessor.preprocess_symbols(&self.symbol_list)
    }
    pub fn get_context(&self) -> &ShaderPreprocessorContext {
        &self.preprocessor.context
    }
    // TODO: should abstract this.
    pub fn get_preprocessor(&self) -> &ShaderPreprocessor {
        &self.preprocessor
    }
    pub fn get_preprocessor_mut(&mut self) -> &mut ShaderPreprocessor {
        &mut self.preprocessor
    }
    pub fn visit_includes<F: FnMut(&ShaderPreprocessorInclude)>(&self, callback: &mut F) {
        for include in &self.preprocessor.includes {
            callback(&include);
            include.get_cache().visit_includes(callback);
        }
    }
    pub fn visit_includes_mut<F: FnMut(&mut ShaderPreprocessorInclude)>(
        &mut self,
        callback: &mut F,
    ) {
        for include in &mut self.preprocessor.includes {
            callback(include);
            include.cache.as_mut().unwrap().visit_includes_mut(callback);
        }
    }
    pub fn find_include_stack<F: FnMut(&ShaderPreprocessorInclude) -> bool>(
        &self,
        callback: &mut F,
    ) -> Option<Vec<&ShaderPreprocessorInclude>> {
        for include in &self.preprocessor.includes {
            if callback(&include) {
                return Some(vec![&include]);
            } else {
                match include.get_cache().find_include_stack(callback) {
                    Some(mut stack) => {
                        stack.insert(0, include);
                        return Some(stack);
                    }
                    None => {}
                }
            }
        }
        None
    }
    pub fn find_include<F: FnMut(&ShaderPreprocessorInclude) -> bool>(
        &self,
        callback: &mut F,
    ) -> Option<&ShaderPreprocessorInclude> {
        for include in &self.preprocessor.includes {
            if callback(&include) {
                return Some(&include);
            } else {
                match include.get_cache().find_include(callback) {
                    Some(include) => {
                        return Some(&include);
                    }
                    None => {}
                }
            }
        }
        None
    }
    pub fn find_direct_includer(&self, include_path: &Path) -> Option<&ShaderPreprocessorInclude> {
        match self.find_include_stack(&mut |include| *include.get_absolute_path() == *include_path)
        {
            Some(stack) => {
                assert!(!stack.is_empty());
                Some(stack[0])
            }
            None => None,
        }
    }
    pub fn has_dependency(&self, dependency_to_find_path: &Path) -> bool {
        self.find_include(&mut |e| *e.get_absolute_path() == *dependency_to_find_path)
            .is_some()
    }
    fn dump_dependency_node(
        &self,
        include: &ShaderPreprocessorInclude,
        header: String,
        is_last: bool,
    ) -> String {
        let mut dependency_tree = format!(
            "{}{} {} ({})\n",
            header,
            if is_last { "└─" } else { "├─" },
            include.get_absolute_path().display(),
            match &include.cache {
                Some(cache) => format!("Mode: {:?}", cache.preprocessor.mode),
                None => "Missing cache".into(),
            }
        );
        let childs_header = format!("{}{}", header, if is_last { "  " } else { "|  " });
        let mut deps_iter = match &include.cache {
            Some(data) => data.preprocessor.includes.iter().peekable(),
            None => {
                return dependency_tree;
            }
        };
        while let Some(included_include) = deps_iter.next() {
            dependency_tree.push_str(
                self.dump_dependency_node(
                    included_include,
                    childs_header.clone(),
                    deps_iter.peek().is_none(),
                )
                .as_str(),
            );
        }
        dependency_tree
    }
    pub fn dump_dependency_tree(&self, absolute_path: &PathBuf) -> String {
        let mut dependency_tree = format!("{}\n", absolute_path.display());
        let mut deps_iter = self.preprocessor.includes.iter().peekable();
        while let Some(include) = deps_iter.next() {
            dependency_tree.push_str(
                self.dump_dependency_node(include, "   ".into(), deps_iter.peek().is_none())
                    .as_str(),
            );
        }
        dependency_tree
    }
}

impl ShaderModule {
    // Dump AST from tree
    pub fn dump_ast(&self) -> String {
        Self::dump_ast_node(self.tree.root_node())
    }
    pub fn dump_ast_node(node: tree_sitter::Node) -> String {
        fn format_debug_cursor(cursor: &mut TreeCursor, depth: usize) -> String {
            let mut debug_tree = String::new();
            loop {
                debug_tree.push_str(&match cursor.field_name() {
                    Some(field_name) => format!(
                        "{}{}: {} [{}, {}] - [{}, {}]\n",
                        " ".repeat(depth * 2),
                        field_name,
                        if cursor.node().is_named() {
                            cursor.node().kind().into()
                        } else {
                            format!("\"{}\"", cursor.node().kind())
                        },
                        cursor.node().range().start_point.row,
                        cursor.node().range().start_point.column,
                        cursor.node().range().end_point.row,
                        cursor.node().range().end_point.column,
                    ),
                    None => format!(
                        "{}{} [{}, {}] - [{}, {}]\n",
                        " ".repeat(depth * 2),
                        if cursor.node().is_named() {
                            cursor.node().kind().into()
                        } else {
                            format!("\"{}\"", cursor.node().kind())
                        },
                        cursor.node().range().start_point.row,
                        cursor.node().range().start_point.column,
                        cursor.node().range().end_point.row,
                        cursor.node().range().end_point.column,
                    ),
                });
                if cursor.goto_first_child() {
                    debug_tree.push_str(format_debug_cursor(cursor, depth + 1).as_str());
                    cursor.goto_parent();
                }
                if !cursor.goto_next_sibling() {
                    break;
                }
            }
            debug_tree
        }
        format_debug_cursor(&mut node.walk(), 0)
    }
}
