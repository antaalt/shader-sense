use std::{
    cell::RefCell,
    path::{Path, PathBuf},
    rc::Rc,
};

use tree_sitter::{Tree, TreeCursor};

use super::{
    shader_language::ShaderLanguage,
    symbol_provider::ShaderSymbolParams,
    symbols::{
        ShaderPreprocessor, ShaderPreprocessorContext, ShaderPreprocessorInclude, ShaderSymbolTree,
    },
};

#[derive(Debug, Clone)]
pub struct ShaderModule {
    pub file_path: PathBuf,
    pub content: String,
    pub tree: Tree,
}
pub type SymbolTree = ShaderModule;

pub type ShaderModuleHandle = Rc<RefCell<ShaderModule>>;

#[derive(Debug, Default, Clone)]
pub struct ShaderSymbols {
    pub(super) preprocessor: ShaderPreprocessor,
    pub(super) symbol_tree: ShaderSymbolTree,
}
impl ShaderSymbols {
    pub fn new(file_path: &Path, symbol_params: ShaderSymbolParams) -> Self {
        Self {
            preprocessor: ShaderPreprocessor::new(ShaderPreprocessorContext::main(
                file_path,
                symbol_params,
            )),
            symbol_tree: ShaderSymbolTree::default(),
        }
    }
    pub fn get_symbol_tree(&self) -> &ShaderSymbolTree {
        // TODO:TREE: preprocess regions (or filter them at runtime in iterator ?)
        &self.symbol_tree
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
        match self.find_include_stack(&mut |include| include.absolute_path == *include_path) {
            Some(stack) => {
                assert!(!stack.is_empty());
                Some(stack[0])
            }
            None => None,
        }
    }
    pub fn has_dependency(&self, dependency_to_find_path: &Path) -> bool {
        self.find_include(&mut |e| e.absolute_path == *dependency_to_find_path)
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
            include.absolute_path.display(),
            if include.cache.is_some() {
                "Cache"
            } else {
                "Missing cache"
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
    pub fn new(language: &mut ShaderLanguage, path: &Path, content: &str) -> Self {
        language.create_module(path, &content).unwrap()
    }
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
                        cursor.node().kind(),
                        cursor.node().range().start_point.row,
                        cursor.node().range().start_point.column,
                        cursor.node().range().end_point.row,
                        cursor.node().range().end_point.column,
                    ),
                    None => {
                        if cursor.node().is_named() {
                            format!(
                                "{}{} [{}, {}] - [{}, {}]\n",
                                " ".repeat(depth * 2),
                                cursor.node().kind(),
                                cursor.node().range().start_point.row,
                                cursor.node().range().start_point.column,
                                cursor.node().range().end_point.row,
                                cursor.node().range().end_point.column,
                            )
                        } else {
                            format!(
                                "{}{:?} [{}, {}] - [{}, {}]\n",
                                " ".repeat(depth * 2),
                                cursor.node().kind(),
                                cursor.node().range().start_point.row,
                                cursor.node().range().start_point.column,
                                cursor.node().range().end_point.row,
                                cursor.node().range().end_point.column,
                            )
                        }
                    }
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
