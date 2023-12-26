use crate::{
    ast::IdentId,
    source_info::Span,
    types::{MonoType, PolyType},
};

use std::{collections::HashMap, fmt::Display};

pub type SymbolId = usize;

#[derive(PartialEq, Clone, Debug)]
pub struct Symbol {
    pub id: SymbolId,
    pub name: String,
    pub ty: Option<PolyType>,
    pub decl_source: Span,
}

impl Symbol {
    pub fn new(id: SymbolId, name: String) -> Self {
        Self {
            id,
            name,
            ty: None,
            decl_source: Span::new(0.into(), None),
        }
    }

    pub fn with_type(&self, ty: PolyType) -> Self {
        Self {
            ty: Some(ty),
            ..self.clone()
        }
    }

    pub fn with_source(&self, span: Span) -> Self {
        Self {
            decl_source: span,
            ..self.clone()
        }
    }

    pub fn unique_name(&self) -> String {
        format!("{}#{}", self.name, self.id)
    }
}

#[derive(Clone, Debug)]
pub struct SymbolTable {
    /// Maps symbol IDs to their types
    pub symbols: HashMap<SymbolId, Symbol>,

    /// Maps variables names in a scope to their symbol IDs
    scopes: HashMap<(usize, String), SymbolId>,

    /// Maps identifiers to their symbol IDs
    ident_lookup: HashMap<IdentId, SymbolId>,

    /// The scope depth of the symbol table
    current_depth: usize,

    /// A counter used to generate unique symbol IDs
    counter: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
            scopes: HashMap::new(),
            ident_lookup: HashMap::new(),
            current_depth: 0,
            counter: 0,
        }
    }

    /// Get the symbol for the given identifier
    pub fn symbol_for_ident(&self, ident: &IdentId) -> Option<Symbol> {
        self.ident_lookup
            .get(ident)
            .and_then(|id| self.symbols.get(id).cloned())
    }

    /// Associate an identifier with a symbol
    pub fn associate_ident(&mut self, ident: IdentId, symbol: SymbolId) {
        self.ident_lookup.insert(ident, symbol);
    }

    pub fn insert_mono(&mut self, name: String, ty: MonoType, source: Option<Span>) -> Symbol {
        self.insert(name, PolyType::Mono(ty), source)
    }

    pub fn insert(&mut self, name: String, ty: PolyType, source: Option<Span>) -> Symbol {
        let id = self.gen_id();
        let mut sym = Symbol::new(id, name.clone()).with_type(ty);
        if let Some(source) = source {
            sym = sym.with_source(source);
        }

        self.symbols.insert(id, sym.clone());
        self.scopes.insert(self.key(&name), id);

        sym
    }

    pub fn get(&self, name: &str) -> Option<Symbol> {
        for depth in (0..=self.current_depth).rev() {
            if let Some(id) = self.scopes.get(&(depth, name.to_owned())) {
                return self.symbols.get(id).cloned();
            }
        }

        None
    }

    pub fn get_in_current_scope(&self, name: &str) -> Option<Symbol> {
        self.scopes
            .get(&(self.current_depth, name.to_owned()))
            .and_then(|id| self.symbols.get(id).cloned())
    }

    pub fn enter_scope(&mut self) {
        self.current_depth += 1;
    }

    pub fn leave_scope(&mut self) {
        self.current_depth -= 1;
    }

    fn key(&self, name: &str) -> (usize, String) {
        (self.current_depth, name.to_owned())
    }

    fn gen_id(&mut self) -> SymbolId {
        let id = self.counter;
        self.counter += 1;
        id
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.ty {
            Some(ty) => write!(f, "{}@{}: {}", self.name, self.id, ty),
            None => write!(f, "{}@{}", self.name, self.id),
        }
    }
}

impl Display for SymbolTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for sym in self.symbols.values() {
            writeln!(f, "{}", sym)?;
        }

        Ok(())
    }
}
