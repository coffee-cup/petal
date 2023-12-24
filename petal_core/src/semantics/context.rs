use std::collections::HashMap;

use crate::{
    ast::{ExprId, Program},
    types::MonoType,
};

use super::{
    symbol_table::SymbolTable,
    typechecker::{Constraint, TypeVarGen},
};

pub struct SemanticContext<'a> {
    pub program: &'a mut Program,
    pub symbol_table: SymbolTable,
    pub expr_types: HashMap<ExprId, MonoType>,
    pub type_symbols: SymbolTable,

    pub ty_gen: TypeVarGen,
    pub type_constraints: Vec<Constraint>,
}
