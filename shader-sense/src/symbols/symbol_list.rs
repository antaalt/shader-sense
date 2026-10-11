//! Symbol list containing all symbol and helper to navigate into them
use serde::{Deserialize, Serialize};

use crate::{
    position::ShaderFilePosition,
    symbols::symbols::{
        ShaderNamespace, ShaderSymbol, ShaderSymbolMode, ShaderSymbolRuntime, ShaderSymbolType,
    },
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
    #[serde(skip)] // Only used at runtime. Not symbols, stored to resolve them.
    pub namespaces: Vec<ShaderNamespace>,
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
    pub namespaces: Vec<&'a ShaderNamespace>,
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
        self.namespaces
            .append(&mut shader_symbol_list_mut.namespaces);
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
            namespaces: self.namespaces.iter().collect(),
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
            namespaces: self.namespaces.iter().collect(), // Not symbols, never filtered.
        }
    }
    pub fn for_each_mut<P: FnMut(ShaderSymbolType, &mut ShaderSymbol)>(
        &mut self,
        mut predicate: P,
    ) {
        let ShaderSymbolList {
            types,
            constants,
            variables,
            call_expression,
            functions,
            keywords,
            macros,
            includes,
            namespaces: _, // Not symbols.
        } = self;
        for (symbol_type, symbols) in [
            (ShaderSymbolType::Types, types),
            (ShaderSymbolType::Constants, constants),
            (ShaderSymbolType::Variables, variables),
            (ShaderSymbolType::CallExpression, call_expression),
            (ShaderSymbolType::Functions, functions),
            (ShaderSymbolType::Keyword, keywords),
            (ShaderSymbolType::Macros, macros),
            (ShaderSymbolType::Include, includes),
        ] {
            symbols
                .iter_mut()
                .for_each(|symbol| predicate(symbol_type, symbol));
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
            namespaces: self.namespaces.iter().map(|n| (*n).clone()).collect(),
        }
    }
    fn is_defined_before(
        runtime: &ShaderSymbolRuntime,
        cursor_position: &ShaderFilePosition,
    ) -> bool {
        if runtime.range.start.line == cursor_position.position.line {
            cursor_position.position.pos > runtime.range.start.pos
        } else {
            cursor_position.position.line > runtime.range.start.line
        }
    }
    // Check if a symbol is accessible without qualification from cursor, which is in cursor_namespace.
    fn is_symbol_defined_at(
        shader_symbol: &ShaderSymbol,
        cursor_position: &ShaderFilePosition,
        cursor_namespace: &[String],
    ) -> bool {
        match &shader_symbol.mode {
            ShaderSymbolMode::Runtime(runtime) => {
                // Namespaced symbols are only visible from within their namespace, or a nested one.
                let mut cursor_namespace = cursor_namespace.iter();
                let is_in_namespace = runtime
                    .scope_stack
                    .iter()
                    .filter_map(|scope| scope.namespace.as_ref())
                    .all(|namespace| cursor_namespace.next() == Some(namespace));
                if !is_in_namespace {
                    return false;
                }
                if runtime.file_path.as_os_str() == cursor_position.file_path.as_os_str() {
                    // Ensure symbols are already defined at pos and that their scope is in range.
                    // Namespace scopes are skipped as they can be reopened, they are checked above.
                    Self::is_defined_before(runtime, cursor_position)
                        && runtime
                            .scope_stack
                            .iter()
                            .filter(|scope| scope.namespace.is_none())
                            .all(|scope| scope.range.contain(&cursor_position.position))
                } else {
                    // If we are not in main file, only show whats in global scope or namespaces.
                    // TODO: should handle include position in file aswell.
                    runtime
                        .scope_stack
                        .iter()
                        .all(|scope| scope.namespace.is_some())
                }
            }
            ShaderSymbolMode::RuntimeContext(_) => true, // available in context.
            ShaderSymbolMode::Intrinsic(_) => true,      // intrinsics
        }
    }
    // Check if a symbol is declared directly in namespace, and accessible from cursor when qualified with it.
    fn is_symbol_declared_in_namespace(
        shader_symbol: &ShaderSymbol,
        namespace: &[String],
        cursor_position: &ShaderFilePosition,
    ) -> bool {
        match &shader_symbol.mode {
            ShaderSymbolMode::Runtime(runtime) => {
                // Every scope must be a namespace, else the symbol is local to a body.
                let mut namespace = namespace.iter();
                let is_declared_in_namespace = runtime.scope_stack.iter().all(|scope| {
                    scope
                        .namespace
                        .as_ref()
                        .is_some_and(|scope_namespace| namespace.next() == Some(scope_namespace))
                }) && namespace.next().is_none();
                is_declared_in_namespace
                    && (runtime.file_path.as_os_str() != cursor_position.file_path.as_os_str()
                        || Self::is_defined_before(runtime, cursor_position))
            }
            // TODO: intrinsics should be able to have a namespace.
            ShaderSymbolMode::RuntimeContext(_) | ShaderSymbolMode::Intrinsic(_) => {
                namespace.is_empty()
            }
        }
    }
    // Get the path of the innermost namespace containing the cursor.
    pub fn find_namespace_at(&self, cursor_position: &ShaderFilePosition) -> Vec<String> {
        self.namespaces
            .iter()
            .filter(|namespace| {
                namespace.file_path.as_os_str() == cursor_position.file_path.as_os_str()
                    && namespace.range.contain(&cursor_position.position)
            })
            .max_by_key(|namespace| namespace.path.len())
            .map(|namespace| namespace.path.clone())
            .unwrap_or_default()
    }
    pub fn has_namespace(&self, namespace: &[String]) -> bool {
        self.namespaces.iter().any(|n| n.path == namespace)
    }
    // Resolve a namespace qualifier written at cursor, relative to the namespaces enclosing it, innermost first.
    pub fn resolve_namespace(
        &self,
        qualifier: &[&str],
        cursor_position: &ShaderFilePosition,
    ) -> Option<Vec<String>> {
        if self.namespaces.is_empty() {
            return None; // Most shaders do not use namespaces.
        }
        let cursor_namespace = self.find_namespace_at(cursor_position);
        (0..=cursor_namespace.len()).rev().find_map(|depth| {
            let mut namespace = cursor_namespace[..depth].to_vec();
            namespace.extend(qualifier.iter().map(|q| q.to_string()));
            self.has_namespace(&namespace).then_some(namespace)
        })
    }
    // Get the name of the namespaces declared directly in namespace.
    pub fn find_child_namespaces(&self, namespace: &[String]) -> Vec<&'a str> {
        let mut children: Vec<&'a str> = self
            .namespaces
            .iter()
            .filter(|n| n.path.len() == namespace.len() + 1 && n.path.starts_with(namespace))
            .map(|n| n.path[namespace.len()].as_str())
            .collect();
        children.sort_unstable();
        children.dedup();
        children
    }
    // Get the name of the namespaces accessible without qualification from cursor.
    pub fn find_namespaces_at(&self, cursor_position: &ShaderFilePosition) -> Vec<&'a str> {
        let cursor_namespace = self.find_namespace_at(cursor_position);
        let mut namespaces: Vec<&'a str> = (0..=cursor_namespace.len())
            .flat_map(|depth| self.find_child_namespaces(&cursor_namespace[..depth]))
            .collect();
        namespaces.sort_unstable();
        namespaces.dedup();
        namespaces
    }
    // Get the symbols accessible from cursor when qualified with namespace.
    pub fn find_namespace_symbols(
        &'a self,
        namespace: &[String],
        cursor_position: &ShaderFilePosition,
    ) -> Vec<&'a ShaderSymbol> {
        self.iter()
            .filter(|s| {
                !s.is_transient()
                    && Self::is_symbol_declared_in_namespace(s, namespace, cursor_position)
            })
            .collect()
    }
    pub fn find_namespace_symbol(
        &'a self,
        label: &str,
        namespace: &[String],
        cursor_position: &ShaderFilePosition,
    ) -> Option<&'a ShaderSymbol> {
        self.iter().find(|s| {
            s.label == *label
                && !s.is_transient()
                && Self::is_symbol_declared_in_namespace(s, namespace, cursor_position)
        })
    }
    pub fn find_symbols_at(
        &'a self,
        label: &str,
        position: &ShaderFilePosition,
    ) -> Vec<&'a ShaderSymbol> {
        let namespace = self.find_namespace_at(position);
        self.iter()
            .filter(|s| {
                !s.is_transient()
                    && s.label == *label
                    && Self::is_symbol_defined_at(s, position, &namespace)
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
        let namespace = self.find_namespace_at(cursor_position);
        self.iter().find(|s| {
            s.label == *label
                && !s.is_transient()
                && Self::is_symbol_defined_at(s, cursor_position, &namespace)
        })
    }
    pub fn find_scoped_function_symbol(
        &'a self,
        label: &str,
        cursor_position: &ShaderFilePosition,
    ) -> Option<&'a ShaderSymbol> {
        let namespace = self.find_namespace_at(cursor_position);
        self.functions
            .iter()
            .find(|s| {
                s.label == *label && Self::is_symbol_defined_at(s, cursor_position, &namespace)
            })
            .map(|s| *s)
    }
    pub fn find_scoped_type_symbol(
        &'a self,
        label: &str,
        cursor_position: &ShaderFilePosition,
    ) -> Option<&'a ShaderSymbol> {
        let namespace = self.find_namespace_at(cursor_position);
        self.types
            .iter()
            .find(|s| {
                s.label == *label && Self::is_symbol_defined_at(s, cursor_position, &namespace)
            })
            .map(|s| *s)
    }
    pub fn filter_scoped_symbol(
        &'a self,
        cursor_position: &ShaderFilePosition,
    ) -> ShaderSymbolListRef<'a> {
        let namespace = self.find_namespace_at(cursor_position);
        self.filter(|symbol_type, symbol| {
            !symbol_type.is_transient()
                && Self::is_symbol_defined_at(symbol, cursor_position, &namespace)
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
            namespaces: self.namespaces.clone(), // Not symbols, never filtered.
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
        let namespace = self.find_namespace_at(cursor_position);
        self.retain(|_symbol_type, symbol| {
            Self::is_symbol_defined_at(symbol, cursor_position, &namespace)
        });
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
        self.namespaces
            .append(&mut shader_symbol_list.namespaces.iter().collect());
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
        self.namespaces
            .append(&mut shader_symbol_list_mut.namespaces);
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
            namespaces: symbol_list.namespaces.iter().collect(),
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
            namespaces: self.namespaces.into_iter().cloned().collect(),
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
