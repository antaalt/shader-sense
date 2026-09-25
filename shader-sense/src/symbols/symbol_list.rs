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

// Collect references to the symbols matching the predicate.
// `Filter` only reports a lower size bound of 0, and `Vec`'s `FromIterator` uses
// that lower bound as initial capacity, so a plain `.filter().collect()` regrows
// and memcpy the vector log2(n) times for each of the eight categories. Sizing it
// upfront trades a transient over-allocation for a single allocation.
fn filter_symbols<'a, P: Fn(ShaderSymbolType, &ShaderSymbol) -> bool>(
    symbols: &'a [ShaderSymbol],
    symbol_type: ShaderSymbolType,
    predicate: &P,
) -> Vec<&'a ShaderSymbol> {
    let mut filtered = Vec::with_capacity(symbols.len());
    filtered.extend(symbols.iter().filter(|e| predicate(symbol_type, *e)));
    filtered
}
// Same as `filter_symbols`, but filtering an already borrowed list.
fn filter_symbols_ref<'a, P: Fn(ShaderSymbolType, &ShaderSymbol) -> bool>(
    symbols: &[&'a ShaderSymbol],
    symbol_type: ShaderSymbolType,
    predicate: &P,
) -> Vec<&'a ShaderSymbol> {
    let mut filtered = Vec::with_capacity(symbols.len());
    filtered.extend(
        symbols
            .iter()
            .filter(|e| predicate(symbol_type, **e))
            .map(|e| *e),
    );
    filtered
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
            types: filter_symbols(&self.types, ShaderSymbolType::Types, &predicate),
            constants: filter_symbols(&self.constants, ShaderSymbolType::Constants, &predicate),
            variables: filter_symbols(&self.variables, ShaderSymbolType::Variables, &predicate),
            call_expression: filter_symbols(
                &self.call_expression,
                ShaderSymbolType::CallExpression,
                &predicate,
            ),
            functions: filter_symbols(&self.functions, ShaderSymbolType::Functions, &predicate),
            keywords: filter_symbols(&self.keywords, ShaderSymbolType::Keyword, &predicate),
            macros: filter_symbols(&self.macros, ShaderSymbolType::Macros, &predicate),
            includes: filter_symbols(&self.includes, ShaderSymbolType::Include, &predicate),
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
    // Single label lookups honouring the cursor scope.
    // These exist so that callers looking for one symbol do not have to materialize
    // a whole filtered list through `filter_scoped_symbol` first.
    pub fn find_scoped_symbol(
        &'a self,
        label: &str,
        cursor_position: &ShaderFilePosition,
    ) -> Option<&'a ShaderSymbol> {
        self.iter().find(|s| {
            s.label == *label && !s.is_transient() && Self::is_symbol_defined_at(s, cursor_position)
        })
    }
    pub fn find_scoped_function_symbol(
        &'a self,
        label: &str,
        cursor_position: &ShaderFilePosition,
    ) -> Option<&'a ShaderSymbol> {
        self.functions
            .iter()
            .find(|s| s.label == *label && Self::is_symbol_defined_at(s, cursor_position))
            .map(|s| *s)
    }
    pub fn find_scoped_type_symbol(
        &'a self,
        label: &str,
        cursor_position: &ShaderFilePosition,
    ) -> Option<&'a ShaderSymbol> {
        self.types
            .iter()
            .find(|s| s.label == *label && Self::is_symbol_defined_at(s, cursor_position))
            .map(|s| *s)
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
            types: filter_symbols_ref(&self.types, ShaderSymbolType::Types, &predicate),
            constants: filter_symbols_ref(&self.constants, ShaderSymbolType::Constants, &predicate),
            variables: filter_symbols_ref(&self.variables, ShaderSymbolType::Variables, &predicate),
            call_expression: filter_symbols_ref(
                &self.call_expression,
                ShaderSymbolType::CallExpression,
                &predicate,
            ),
            functions: filter_symbols_ref(&self.functions, ShaderSymbolType::Functions, &predicate),
            keywords: filter_symbols_ref(&self.keywords, ShaderSymbolType::Keyword, &predicate),
            macros: filter_symbols_ref(&self.macros, ShaderSymbolType::Macros, &predicate),
            includes: filter_symbols_ref(&self.includes, ShaderSymbolType::Include, &predicate),
        }
    }
    // Drop every symbol not matching the predicate, in place.
    // Prefer this over `filter` when the list is owned by the caller: it does not
    // allocate at all, and it does not keep the unfiltered list borrowed.
    pub fn retain<P: Fn(ShaderSymbolType, &ShaderSymbol) -> bool>(&mut self, predicate: P) {
        self.types.retain(|e| predicate(ShaderSymbolType::Types, e));
        self.constants
            .retain(|e| predicate(ShaderSymbolType::Constants, e));
        self.variables
            .retain(|e| predicate(ShaderSymbolType::Variables, e));
        self.call_expression
            .retain(|e| predicate(ShaderSymbolType::CallExpression, e));
        self.functions
            .retain(|e| predicate(ShaderSymbolType::Functions, e));
        self.keywords
            .retain(|e| predicate(ShaderSymbolType::Keyword, e));
        self.macros
            .retain(|e| predicate(ShaderSymbolType::Macros, e));
        self.includes
            .retain(|e| predicate(ShaderSymbolType::Include, e));
    }
    // In place version of `filter_scoped_symbol`.
    pub fn retain_scoped_symbol(&mut self, cursor_position: &ShaderFilePosition) {
        // Transient symbols are a whole category, no need to test them one by one.
        debug_assert!(ShaderSymbolType::CallExpression.is_transient());
        self.call_expression.clear();
        self.retain(|_symbol_type, symbol| Self::is_symbol_defined_at(symbol, cursor_position));
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
