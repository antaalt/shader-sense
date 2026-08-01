//! Symbol list containing all symbol and helper to navigate into them
use serde::{Deserialize, Serialize};

use crate::{
    position::ShaderFilePosition,
    symbols::symbols::{ShaderSymbol, ShaderSymbolMode, ShaderSymbolType},
};

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct ShaderSymbolList {
    pub types: Vec<ShaderSymbol>,
    pub constants: Vec<ShaderSymbol>,
    pub variables: Vec<ShaderSymbol>,
    #[serde(skip)] // Only used at runtime.
    pub call_expression: Vec<ShaderSymbol>,
    pub functions: Vec<ShaderSymbol>,
    pub keywords: Vec<ShaderSymbol>,
    pub macros: Vec<ShaderSymbol>,
    pub includes: Vec<ShaderSymbol>,
}
#[derive(Debug, Default, Clone)]
pub struct ShaderSymbolListRef<'a> {
    pub types: Vec<&'a ShaderSymbol>,
    pub constants: Vec<&'a ShaderSymbol>,
    pub variables: Vec<&'a ShaderSymbol>,
    pub call_expression: Vec<&'a ShaderSymbol>,
    pub functions: Vec<&'a ShaderSymbol>,
    pub keywords: Vec<&'a ShaderSymbol>,
    pub macros: Vec<&'a ShaderSymbol>,
    pub includes: Vec<&'a ShaderSymbol>,
}

impl ShaderSymbolList {
    // Parse intrinsic database
    pub fn parse_from_json(file_content: String) -> ShaderSymbolList {
        serde_json::from_str::<ShaderSymbolList>(&file_content)
            .expect("Failed to parse ShaderSymbolList. It probably needs to be regenerated using shader-intrinsic-parser to support new elements.")
    }
    // Append another symbol list to this one.
    pub fn append(&mut self, shader_symbol_list: ShaderSymbolList) {
        let mut shader_symbol_list_mut = shader_symbol_list;
        self.functions.append(&mut shader_symbol_list_mut.functions);
        self.variables.append(&mut shader_symbol_list_mut.variables);
        self.call_expression
            .append(&mut shader_symbol_list_mut.call_expression);
        self.constants.append(&mut shader_symbol_list_mut.constants);
        self.types.append(&mut shader_symbol_list_mut.types);
        self.keywords.append(&mut shader_symbol_list_mut.keywords);
        self.macros.append(&mut shader_symbol_list_mut.macros);
        self.includes.append(&mut shader_symbol_list_mut.includes);
    }
    pub fn as_ref<'a>(&'a self) -> ShaderSymbolListRef<'a> {
        ShaderSymbolListRef {
            types: self.types.iter().collect(),
            constants: self.constants.iter().collect(),
            variables: self.variables.iter().collect(),
            call_expression: self.call_expression.iter().collect(),
            functions: self.functions.iter().collect(),
            keywords: self.keywords.iter().collect(),
            macros: self.macros.iter().collect(),
            includes: self.includes.iter().collect(),
        }
    }
    pub fn filter<'a, P: Fn(ShaderSymbolType, &ShaderSymbol) -> bool>(
        &'a self,
        predicate: P,
    ) -> ShaderSymbolListRef<'a> {
        ShaderSymbolListRef {
            types: self
                .types
                .iter()
                .filter(|e| predicate(ShaderSymbolType::Types, *e))
                .collect(),
            constants: self
                .constants
                .iter()
                .filter(|e| predicate(ShaderSymbolType::Constants, *e))
                .collect(),
            variables: self
                .variables
                .iter()
                .filter(|e| predicate(ShaderSymbolType::Variables, *e))
                .collect(),
            call_expression: self
                .call_expression
                .iter()
                .filter(|e| predicate(ShaderSymbolType::CallExpression, *e))
                .collect(),
            functions: self
                .functions
                .iter()
                .filter(|e| predicate(ShaderSymbolType::Functions, *e))
                .collect(),
            keywords: self
                .keywords
                .iter()
                .filter(|e| predicate(ShaderSymbolType::Keyword, *e))
                .collect(),
            macros: self
                .macros
                .iter()
                .filter(|e| predicate(ShaderSymbolType::Macros, *e))
                .collect(),
            includes: self
                .includes
                .iter()
                .filter(|e| predicate(ShaderSymbolType::Include, *e))
                .collect(),
        }
    }
}
impl<'a> ShaderSymbolListRef<'a> {
    pub fn to_owned(&self) -> ShaderSymbolList {
        ShaderSymbolList {
            types: self.types.iter().map(|s| (*s).clone()).collect(),
            constants: self.constants.iter().map(|s| (*s).clone()).collect(),
            variables: self.variables.iter().map(|s| (*s).clone()).collect(),
            call_expression: self.call_expression.iter().map(|s| (*s).clone()).collect(),
            functions: self.functions.iter().map(|s| (*s).clone()).collect(),
            keywords: self.keywords.iter().map(|s| (*s).clone()).collect(),
            macros: self.macros.iter().map(|s| (*s).clone()).collect(),
            includes: self.includes.iter().map(|s| (*s).clone()).collect(),
        }
    }
    fn is_symbol_defined_at(
        shader_symbol: &ShaderSymbol,
        cursor_position: &ShaderFilePosition,
    ) -> bool {
        match &shader_symbol.mode {
            ShaderSymbolMode::Runtime(runtime) => {
                if runtime.file_path.as_os_str() == cursor_position.file_path.as_os_str() {
                    // Ensure symbols are already defined at pos
                    let is_already_defined =
                        if runtime.range.start.line == cursor_position.position.line {
                            cursor_position.position.pos > runtime.range.start.pos
                        } else {
                            cursor_position.position.line > runtime.range.start.line
                        };
                    if is_already_defined {
                        // If we are in main file, check if scope in range.
                        for symbol_scope in &runtime.scope_stack {
                            if !symbol_scope.contain(&cursor_position.position) {
                                return false; // scope not in range
                            }
                        }
                        true // scope in range
                    } else {
                        false
                    }
                } else {
                    // If we are not in main file, only show whats in global scope.
                    // TODO: should handle include position in file aswell.
                    runtime.scope_stack.is_empty() // Global scope or inaccessible
                }
            }
            ShaderSymbolMode::RuntimeContext(_) => true, // available in context.
            ShaderSymbolMode::Intrinsic(_) => true,      // intrinsics
        }
    }
    pub fn find_symbols_at(
        &'a self,
        label: &str,
        position: &ShaderFilePosition,
    ) -> Vec<&'a ShaderSymbol> {
        self.iter()
            .filter(|s| {
                !s.is_transient() && s.label == *label && Self::is_symbol_defined_at(s, position)
            })
            .collect()
    }
    pub fn filter_scoped_symbol(
        &'a self,
        cursor_position: &ShaderFilePosition,
    ) -> ShaderSymbolListRef<'a> {
        self.filter(|symbol_type, symbol| {
            !symbol_type.is_transient() && Self::is_symbol_defined_at(symbol, cursor_position)
        })
    }
    pub fn find_symbols(&'a self, label: &str) -> Vec<&'a ShaderSymbol> {
        self.iter()
            .filter(|s| s.label == *label && !s.is_transient())
            .collect::<Vec<&ShaderSymbol>>()
    }
    pub fn find_symbol(&'a self, label: &str) -> Option<&'a ShaderSymbol> {
        match self.iter().find(|e| e.label == *label) {
            Some(symbol) => return Some(symbol),
            None => None,
        }
    }
    pub fn find_function_symbol(&'a self, label: &str) -> Option<&'a ShaderSymbol> {
        self.functions
            .iter()
            .find(|s| s.label == *label)
            .map(|s| *s)
    }
    pub fn find_type_symbol(&'a self, label: &str) -> Option<&'a ShaderSymbol> {
        self.types.iter().find(|s| s.label == *label).map(|s| *s)
    }
    pub fn filter<P: Fn(ShaderSymbolType, &ShaderSymbol) -> bool>(
        &'a self,
        predicate: P,
    ) -> ShaderSymbolListRef<'a> {
        ShaderSymbolListRef {
            types: self
                .types
                .iter()
                .filter(|e| predicate(ShaderSymbolType::Types, *e))
                .map(|s| *s)
                .collect(),
            constants: self
                .constants
                .iter()
                .filter(|e| predicate(ShaderSymbolType::Constants, *e))
                .map(|s| *s)
                .collect(),
            variables: self
                .variables
                .iter()
                .filter(|e| predicate(ShaderSymbolType::Variables, *e))
                .map(|s| *s)
                .collect(),
            call_expression: self
                .call_expression
                .iter()
                .filter(|e| predicate(ShaderSymbolType::CallExpression, *e))
                .map(|s| *s)
                .collect(),
            functions: self
                .functions
                .iter()
                .filter(|e| predicate(ShaderSymbolType::Functions, *e))
                .map(|s| *s)
                .collect(),
            keywords: self
                .keywords
                .iter()
                .filter(|e| predicate(ShaderSymbolType::Keyword, *e))
                .map(|s| *s)
                .collect(),
            macros: self
                .macros
                .iter()
                .filter(|e| predicate(ShaderSymbolType::Macros, *e))
                .map(|s| *s)
                .collect(),
            includes: self
                .includes
                .iter()
                .filter(|e| predicate(ShaderSymbolType::Include, *e))
                .map(|s| *s)
                .collect(),
        }
    }
    pub fn iter(&'a self) -> ShaderSymbolListIterator<'a> {
        ShaderSymbolListIterator::new(&self)
    }
    pub fn append_as_reference(&mut self, shader_symbol_list: &'a ShaderSymbolList) {
        self.functions
            .append(&mut shader_symbol_list.functions.iter().collect());
        self.variables
            .append(&mut shader_symbol_list.variables.iter().collect());
        self.call_expression
            .append(&mut shader_symbol_list.call_expression.iter().collect());
        self.constants
            .append(&mut shader_symbol_list.constants.iter().collect());
        self.types
            .append(&mut shader_symbol_list.types.iter().collect());
        self.keywords
            .append(&mut shader_symbol_list.keywords.iter().collect());
        self.macros
            .append(&mut shader_symbol_list.macros.iter().collect());
        self.includes
            .append(&mut shader_symbol_list.includes.iter().collect());
    }
    pub fn append(&mut self, shader_symbol_list: ShaderSymbolListRef<'a>) {
        let mut shader_symbol_list_mut = shader_symbol_list;
        self.functions.append(&mut shader_symbol_list_mut.functions);
        self.variables.append(&mut shader_symbol_list_mut.variables);
        self.call_expression
            .append(&mut shader_symbol_list_mut.call_expression);
        self.constants.append(&mut shader_symbol_list_mut.constants);
        self.types.append(&mut shader_symbol_list_mut.types);
        self.keywords.append(&mut shader_symbol_list_mut.keywords);
        self.macros.append(&mut shader_symbol_list_mut.macros);
        self.includes.append(&mut shader_symbol_list_mut.includes);
    }
}

impl<'a> From<&'a ShaderSymbolList> for ShaderSymbolListRef<'a> {
    fn from(symbol_list: &'a ShaderSymbolList) -> Self {
        Self {
            types: symbol_list.types.iter().collect(),
            constants: symbol_list.constants.iter().collect(),
            variables: symbol_list.variables.iter().collect(),
            call_expression: symbol_list.call_expression.iter().collect(),
            functions: symbol_list.functions.iter().collect(),
            keywords: symbol_list.keywords.iter().collect(),
            macros: symbol_list.macros.iter().collect(),
            includes: symbol_list.includes.iter().collect(),
        }
    }
}

impl<'a> Into<ShaderSymbolList> for ShaderSymbolListRef<'a> {
    fn into(self) -> ShaderSymbolList {
        ShaderSymbolList {
            types: self.types.into_iter().cloned().collect(),
            constants: self.constants.into_iter().cloned().collect(),
            variables: self.variables.into_iter().cloned().collect(),
            call_expression: self.call_expression.into_iter().cloned().collect(),
            functions: self.functions.into_iter().cloned().collect(),
            keywords: self.keywords.into_iter().cloned().collect(),
            macros: self.macros.into_iter().cloned().collect(),
            includes: self.includes.into_iter().cloned().collect(),
        }
    }
}

pub struct ShaderSymbolListIterator<'a> {
    list: &'a ShaderSymbolListRef<'a>,
    current: Option<ShaderSymbolType>,
    iterator: std::slice::Iter<'a, &'a ShaderSymbol>,
}

impl<'a> ShaderSymbolListIterator<'a> {
    pub fn new(symbol_list: &'a ShaderSymbolListRef<'a>) -> Self {
        Self {
            list: symbol_list,
            current: Some(ShaderSymbolType::Types), // First one
            iterator: symbol_list.types.iter(),
        }
    }
}

impl<'a> Iterator for ShaderSymbolListIterator<'a> {
    type Item = &'a ShaderSymbol;

    fn next(&mut self) -> Option<Self::Item> {
        match self.iterator.next() {
            Some(symbol) => Some(symbol),
            None => match &self.current {
                Some(ty) => match ty {
                    ShaderSymbolType::Types => {
                        self.current = Some(ShaderSymbolType::Constants);
                        self.iterator = self.list.constants.iter();
                        self.next()
                    }
                    ShaderSymbolType::Constants => {
                        self.current = Some(ShaderSymbolType::Variables);
                        self.iterator = self.list.variables.iter();
                        self.next()
                    }
                    ShaderSymbolType::Variables => {
                        self.current = Some(ShaderSymbolType::CallExpression);
                        self.iterator = self.list.call_expression.iter();
                        self.next()
                    }
                    ShaderSymbolType::CallExpression => {
                        self.current = Some(ShaderSymbolType::Functions);
                        self.iterator = self.list.functions.iter();
                        self.next()
                    }
                    ShaderSymbolType::Functions => {
                        self.current = Some(ShaderSymbolType::Keyword);
                        self.iterator = self.list.keywords.iter();
                        self.next()
                    }
                    ShaderSymbolType::Keyword => {
                        self.current = Some(ShaderSymbolType::Macros);
                        self.iterator = self.list.macros.iter();
                        self.next()
                    }
                    ShaderSymbolType::Macros => {
                        self.current = Some(ShaderSymbolType::Include);
                        self.iterator = self.list.includes.iter();
                        self.next()
                    }
                    ShaderSymbolType::Include => {
                        self.current = None;
                        self.next()
                    }
                },
                None => None,
            },
        }
    }
}
