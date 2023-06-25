use crate::petal::ast::Program;

use super::{
    symbol_table::SymbolTable,
    typechecker::{Constraint, TypeVarGen},
};

pub struct SemanticContext<'a> {
    pub program: &'a mut Program,
    pub symbol_table: SymbolTable,
    pub type_symbols: SymbolTable,

    pub ty_gen: TypeVarGen,
    pub type_constraints: Vec<Constraint>,
}
