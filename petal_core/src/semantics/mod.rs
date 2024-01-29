use std::collections::HashMap;



use self::{
    context::SemanticContext, errors::SemanticResult, symbol_table::SymbolTable,
    typechecker::TypeVarGen,
};

use super::{
    ast::{ExprId, Program},
    types::MonoType,
};

mod analysis;
mod constraint_generation;
pub mod context;
pub mod errors;
mod symbol_generation;
pub mod symbol_table;
mod typechecker;

impl<'a> SemanticContext<'a> {
    pub fn new(program: &'a mut Program) -> Self {
        let symbol_table = SymbolTable::new();

        let mut type_symbols = SymbolTable::new();

        type_symbols.insert_mono("Int".into(), MonoType::int(), None);
        type_symbols.insert_mono("Float".into(), MonoType::float(), None);
        type_symbols.insert_mono("Bool".into(), MonoType::bool(), None);
        type_symbols.insert_mono("String".into(), MonoType::string(), None);

        Self {
            symbol_table,
            type_symbols,
            ty_gen: TypeVarGen::new(),
            type_constraints: Vec::new(),
            expr_types: HashMap::new(),
            program,
            loop_ids: HashMap::new(),
            loop_counter: 0,
        }
    }

    pub fn analysis_program(&mut self) -> SemanticResult<()> {
        self.generate_symbols_for_program()?;
        self.check_program()?;
        self.analysis()?;

        // println!("Symbol table:\n{}", self.symbol_table);

        Ok(())
    }

    pub fn check_program(&mut self) -> SemanticResult<()> {
        for stmt in self.program.main_stmts.clone().iter() {
            self.stmt_constraints(*stmt, &None)?;
        }

        for func in self.program.functions.clone().iter() {
            self.stmt_constraints(func.body, &Some(func.clone()))?;
        }

        // debug!(
        //     "--- Constraints\n{}",
        //     self.type_constraints
        //         .iter()
        //         .map(|c| c.to_string())
        //         .collect::<Vec<String>>()
        //         .join("\n")
        // );

        // println!("--- (before) Expression types");
        // for (expr, ty) in self.expr_types.iter() {
        //     let expr = self.program.ast.expressions[*expr].clone();
        //     println!("{:?}: {}", expr.expr, ty);
        // }

        self.solve_constraints()?;

        // println!("\n--- (after) Expression types");
        // for (expr, ty) in self.expr_types.iter() {
        //     let expr = self.program.ast.expressions[*expr].clone();
        //     println!("{:?}: {}", expr.expr, ty);
        // }

        Ok(())
    }

    pub fn type_for_expr(&self, expr: &ExprId) -> Option<MonoType> {
        self.expr_types.get(expr).cloned()
    }

    fn gen_loop_id(&mut self) -> usize {
        let id = self.loop_counter;
        self.loop_counter += 1;
        id
    }
}
