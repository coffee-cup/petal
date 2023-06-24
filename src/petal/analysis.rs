use miette::{Diagnostic, SourceSpan};
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use thiserror::Error;

use crate::petal::{
    ast::{ExprNode, Stmt},
    typechecker::occurs_check,
};

use super::{
    ast::{
        BinaryOpType, Block, Expr, ExprId, FuncArg, FuncDecl, IdentId, Identifier, LetDecl,
        PrefixOpType, Program, StmtId, StmtNode, TypeAnnotation,
    },
    source_info::Span,
    typechecker::{
        Constraint, MonoTypeData, Substitution, TypeContext, TypeVarGen, Typechecker, Types,
    },
    types::{FunctionAppType, MonoType, PolyType, StructType},
};

#[derive(Diagnostic, Error, Clone, Debug)]
pub enum AnalysisError {
    #[error("Unknown analysis error")]
    UnknownError,

    #[error("Undeclared variable")]
    #[diagnostic(help("Declare a variable before using it (e.g. `let {name} = 1`)"))]
    UndeclaredVariable {
        name: String,

        #[label = "Undeclared variable `{name}`"]
        span: Span,
    },

    #[error("Unknown type `{name}`")]
    UndefinedType {
        name: String,

        #[label = "the type `{name}` is nowhere to be found"]
        span: Span,
    },

    #[error("Function `{name}` already exists")]
    #[diagnostic(help("All functions names must be unique across the program"))]
    FunctionAlreadyDeclared {
        name: String,

        #[label("the function `{name}` was first declared here")]
        first_declaration: Span,

        #[label = "and then declared again here"]
        span: Span,
    },

    #[error("Invalid type annotation `{name}`")]
    InvalidTypeAnnotation {
        name: String,

        #[label = "invalid type annotation `{name}`"]
        span: Span,
    },

    #[error("Variable `{name}` already defined")]
    VariableAlreadyDeclared {
        name: String,

        #[label("the variable `{name}` was first declared here")]
        first_declaration: Span,

        #[label = "and then declared again here"]
        span: Span,
    },

    #[error("Argument `{name}` already declared")]
    #[diagnostic(help("Function arguments must have unique names"))]
    ArgumentAlreadyDefined {
        name: String,

        #[label("the argument `{name}` was first declared here")]
        first_declaration: Span,

        #[label = "and then declared again here"]
        span: Span,
    },

    #[error("Mismatched types")]
    MismatchedTypes {
        lhs: MonoType,
        rhs: MonoType,

        #[label("the type `{lhs}`")]
        lhs_span: Span,

        #[label("cannot be equated with type `{rhs}`")]
        rhs_span: Span,
    },

    #[error("Mismatched types")]
    ExpectedType {
        expected: MonoType,
        found: MonoType,

        #[label("expected type `{expected}`, found `{found}`")]
        span: Span,
    },

    #[error("If condition invalid type")]
    #[diagnostic(help("The condition of an `if` statement must be a `Bool`"))]
    IfConditionTypeMismatch {
        ty: MonoType,

        #[label("in this `if` statement")]
        if_span: Span,

        #[label("the condition must be a `Bool`. Found `{ty}`")]
        span: Span,
    },

    #[error("Left hand side `{lhs_type}` does not match right hand side `{rhs_type}`")]
    #[diagnostic(help(
        "The left and right hand sides of a binary operation must have the same type"
    ))]
    InvalidBinaryOperation {
        op: BinaryOpType,
        lhs_type: MonoType,
        rhs_type: MonoType,

        #[label("`{op}` expression")]
        bin_span: Span,

        #[label("this is of type `{lhs_type}`")]
        lhs_span: Span,

        #[label("this is of type `{rhs_type}`")]
        rhs_span: Span,
    },

    #[error("Invalid function call")]
    #[diagnostic(help(
        "The left hand side of a function call must be an identifier to a function"
    ))]
    InvalidFunctionCall {
        #[label("this is not an identifier to a function")]
        span: Span,
    },

    #[error("Functions have different number of arguments")]
    IncorrectNumberOfArguments {
        expected: usize,
        found: usize,

        #[label("this function has {expected} parameters")]
        func_decl: Span,

        #[label("found {found} arguments")]
        found_span: Span,
    },
    // #[error("Variable {0} does not have an symbol associated with it")]
    // IdentifierDoesNotHaveSymbol(String),

    // #[error("Symbol {0} not found in symbol table")]
    // SymbolNotFound(String),

    // #[error("{0}")]
    // TypecheckError(#[from] TypecheckingErrorKind),
}

type AnalysisResult<T> = Result<T, AnalysisError>;

type SymbolId = usize;

#[derive(PartialEq, Clone, Debug)]
struct Symbol {
    id: SymbolId,
    name: String,
    ty: Option<PolyType>,
    decl_source: Span,
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
}

#[derive(Clone, Debug)]
struct SymbolTable {
    /// Maps symbol IDs to their types
    symbols: HashMap<SymbolId, Symbol>,

    /// Maps variables names in a scope to their symbol IDs
    scopes: HashMap<(usize, String), SymbolId>,

    ident_lookup: HashMap<IdentId, SymbolId>,

    current_depth: usize,

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

    pub fn symbol_for_ident(&self, ident: &IdentId) -> Option<Symbol> {
        self.ident_lookup
            .get(ident)
            .and_then(|id| self.symbols.get(id).cloned())
    }

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

    pub fn get(&self, name: &String) -> Option<Symbol> {
        for depth in (0..=self.current_depth).rev() {
            if let Some(id) = self.scopes.get(&(depth, name.clone())) {
                return self.symbols.get(id).cloned();
            }
        }

        None
    }

    pub fn get_in_current_scope(&self, name: &String) -> Option<Symbol> {
        self.scopes
            .get(&(self.current_depth, name.clone()))
            .and_then(|id| self.symbols.get(id).cloned())
    }

    pub fn is_defined_in_current_scope(&self, name: &String) -> bool {
        self.scopes.get(&self.key(name)).is_some()
    }

    pub fn enter_scope(&mut self) {
        self.current_depth += 1;
    }

    pub fn leave_scope(&mut self) {
        self.current_depth -= 1;
    }

    fn key(&self, name: &String) -> (usize, String) {
        (self.current_depth, name.clone())
    }

    fn gen_id(&mut self) -> SymbolId {
        let id = self.counter;
        self.counter += 1;
        id
    }
}

pub struct TypeTable(HashMap<String, MonoType>);

pub struct AnalysisContext<'a> {
    program: &'a mut Program,
    symbol_table: SymbolTable,
    type_symbols: SymbolTable,

    ty_gen: TypeVarGen,
    type_constraints: Vec<Constraint>,
}

impl<'a> AnalysisContext<'a> {
    pub fn new(program: &'a mut Program) -> Self {
        let symbol_table = SymbolTable::new();

        let mut type_symbols = SymbolTable::new();

        let type_ctx = TypeContext::new();
        type_symbols.insert_mono("Int".into(), MonoType::int(), None);
        type_symbols.insert_mono("Float".into(), MonoType::float(), None);
        type_symbols.insert_mono("Bool".into(), MonoType::bool(), None);
        type_symbols.insert_mono("String".into(), MonoType::string(), None);

        Self {
            symbol_table,
            type_symbols,
            ty_gen: TypeVarGen::new(),
            type_constraints: Vec::new(),
            program,
        }
    }

    pub fn analysis_program(&mut self) -> AnalysisResult<()> {
        println!("Analyzing program!");

        self.generate_symbols_for_program()?;

        // println!("1: Symbol table:\n{}", self.symbol_table);

        self.check_program()?;

        println!("Symbol table:\n{}", self.symbol_table);

        Ok(())
    }

    pub fn check_program(&mut self) -> AnalysisResult<()> {
        for stmt in self.program.main_stmts.clone().iter() {
            self.stmt_constraints(*stmt)?;
        }

        // TODO: Generate constraints for literally all the functions
        for func in self.program.functions.clone().iter() {
            self.stmt_constraints(func.body)?;
        }

        println!("\n--- Constraints:");
        for constraint in self.type_constraints.clone() {
            println!("{}", constraint);
        }
        println!("---\n");

        self.solve_constraints()?;

        Ok(())
    }

    pub fn generate_symbols_for_program(&mut self) -> AnalysisResult<()> {
        // Add all functions to symbol table
        for func in self.program.functions.iter() {
            let fun_name = self.program.get_ident_name(func.ident);

            if let Some(sym) = self.symbol_table.get(&fun_name) {
                return Err(AnalysisError::FunctionAlreadyDeclared {
                    name: sym.name,
                    first_declaration: sym.decl_source,
                    span: self.program.span_for_ident(func.ident),
                });
            }

            let func_ty = self.get_type_of_function_decl(func)?;

            let sym = self.symbol_table.insert(
                fun_name,
                func_ty,
                Some(self.program.span_for_ident(func.ident)),
            );
            self.symbol_table.associate_ident(func.ident, sym.id);
        }

        // Analysis the top-level statements
        for stmt in self.program.main_stmts.clone().iter() {
            self.generate_symbols_for_statement(*stmt)?;
        }

        for func in self.program.functions.clone().iter() {
            self.symbol_table.enter_scope();

            // Add function arguments to symbol table
            for arg in func.args.iter() {
                let ident = &self.program.ast.identifiers[arg.ident];

                if let Some(sym) = self.symbol_table.get_in_current_scope(&ident.name) {
                    return Err(AnalysisError::ArgumentAlreadyDefined {
                        name: sym.name.clone(),
                        first_declaration: sym.decl_source,
                        span: ident.span.clone(),
                    });
                }

                let ty = self.type_for_annotation(&arg.ty)?;

                let sym =
                    self.symbol_table
                        .insert_mono(ident.name.clone(), ty, Some(ident.span.clone()));
                self.symbol_table.associate_ident(arg.ident, sym.id);
            }

            self.generate_symbols_for_statement(func.body)?;

            self.symbol_table.leave_scope();
        }

        Ok(())
    }

    fn solve_constraints(&mut self) -> AnalysisResult<()> {
        let mut sub = Substitution::new();

        for constraint in self.type_constraints.clone() {
            match constraint {
                Constraint::Equal { lhs, rhs } => {
                    let sub2 = self.unify_constraint(lhs.apply(&sub), rhs.apply(&sub))?;
                    sub = sub.combine(sub2);
                }
            }
        }

        // println!("Substitution:\n\n{:#?}\n", sub);

        self.apply_substition_to_symbol_table(&sub);

        Ok(())
    }

    fn apply_substition_to_symbol_table(&mut self, sub: &Substitution) {
        for (_, sym) in self.symbol_table.symbols.iter_mut() {
            if let Some(ty) = &mut sym.ty {
                *ty = ty.apply(sub);
            }
        }
    }

    fn unify_constraint(
        &mut self,
        lhs_data: MonoTypeData,
        rhs_data: MonoTypeData,
    ) -> AnalysisResult<Substitution> {
        use MonoType::*;

        let sub = match (&lhs_data.ty, &rhs_data.ty) {
            (Variable(v1), Variable(v2)) => {
                if v1 == v2 {
                    Substitution::new()
                } else {
                    let mut sub = Substitution::new();
                    sub.insert(v1.clone(), Variable(v2.clone()));
                    sub
                }
            }
            (Variable(v), ty) | (ty, Variable(v)) => {
                if occurs_check(ty, &Variable(v.clone())) {
                    panic!("Infinite type")
                } else {
                    let mut sub = Substitution::new();
                    sub.insert(v.clone(), ty.clone());
                    sub
                }
            }
            (FunApp(f1), FunApp(f2)) => {
                if f1.params.len() != f2.params.len() {
                    unreachable!(
                        "Function param length should be checked during constraint gathering"
                    );
                }

                let mut sub = Substitution::new();

                // Unify the argument types
                for (a, b) in f1.params.iter().zip(f2.params.iter()) {
                    sub = sub.combine(self.unify_constraint(
                        MonoTypeData::new(a.apply(&sub)),
                        MonoTypeData::new(b.apply(&sub)),
                    )?);
                }

                // Unify the return types
                sub = sub.combine(self.unify_constraint(
                    MonoTypeData::new(f1.return_ty.apply(&sub)),
                    MonoTypeData::new(f2.return_ty.apply(&sub)),
                )?);

                sub
            }

            (t1 @ Struct(s1), t2 @ Struct(s2)) => {
                if s1.name != s2.name {
                    println!("s1 = {}, s2 = {}", s1.name, s2.name);

                    return Err(self.mismatch_type_error(t1, t2, &lhs_data, &rhs_data));
                }

                let mut sub = Substitution::new();
                for (a, b) in s1.params.iter().zip(s2.params.iter()) {
                    // TODO: Create a MonoTypeData for each param
                    // For this, I need to associate the param with an identifier node

                    sub = sub.combine(self.unify_constraint(
                        MonoTypeData::new(a.apply(&sub)),
                        MonoTypeData::new(b.apply(&sub)),
                    )?);
                }

                sub
            }

            (t1, t2) => {
                if t1 != t2 {
                    return Err(self.mismatch_type_error(t1, t2, &lhs_data, &rhs_data));
                }

                Substitution::new()
            }
        };

        Ok(sub)
    }

    // Return an error for mismatched types
    // This will use the most specific error possible for the given lhs and rhs types
    fn mismatch_type_error(
        &self,
        t1: &MonoType,
        t2: &MonoType,
        lhs_data: &MonoTypeData,
        rhs_data: &MonoTypeData,
    ) -> AnalysisError {
        // If condition type mismatch
        if let Some(
            n @ StmtNode {
                stmt: Stmt::IfStmt { .. },
                ..
            },
        ) = lhs_data
            .parent_stmt_id
            .map(|stmt_id| &self.program.ast.statements[stmt_id])
        {
            return AnalysisError::IfConditionTypeMismatch {
                ty: t1.clone(),
                span: self.span_for_monotype_data(&lhs_data).unwrap_or_default(),
                if_span: n.span.start().span_from_length(1),
            };
        }

        // Binary operand type mismatch
        if let Some(
            n @ ExprNode {
                expr: Expr::BinaryOp { op, .. },
                ..
            },
        ) = lhs_data
            .parent_expr_id
            .map(|expr_id| &self.program.ast.expressions[expr_id])
        {
            return AnalysisError::InvalidBinaryOperation {
                bin_span: op.span.clone(),
                op: op.binary_type.clone(),
                lhs_type: t1.clone(),
                rhs_type: t2.clone(),
                lhs_span: self.span_for_monotype_data(&lhs_data).unwrap_or_default(),
                rhs_span: self.span_for_monotype_data(&rhs_data).unwrap_or_default(),
            };
        }

        match (
            self.span_for_monotype_data(&lhs_data),
            self.span_for_monotype_data(&rhs_data),
        ) {
            (Some(lhs_span), Some(rhs_span)) => {
                // If we can, show the span of both types
                return AnalysisError::MismatchedTypes {
                    lhs: t1.clone(),
                    rhs: t2.clone(),
                    lhs_span,
                    rhs_span,
                };
            }
            (lhs_span, rhs_span) => {
                return AnalysisError::ExpectedType {
                    expected: t2.clone(),
                    found: t1.clone(),
                    span: lhs_span.or(rhs_span).unwrap_or_default(),
                }
            }
        }
    }

    fn span_for_monotype_data(&self, ty_data: &MonoTypeData) -> Option<Span> {
        if let Some(expr_id) = ty_data.expr_id {
            return Some(self.program.ast.expressions[expr_id].span.clone());
        } else if let Some(ident_id) = ty_data.ident_id {
            return Some(self.program.ast.identifiers[ident_id].span.clone());
        }

        None
    }

    fn associate_types(&mut self, lhs: MonoTypeData, rhs: MonoTypeData) {
        self.type_constraints.push(Constraint::Equal { lhs, rhs });
    }

    fn stmt_constraints(&mut self, stmt_id: StmtId) -> AnalysisResult<()> {
        let stmt_node = &self.program.ast.statements[stmt_id];

        match stmt_node.stmt.clone() {
            Stmt::Let(let_decl) => {
                let sym = self.symbol_table.symbol_for_ident(&let_decl.ident).unwrap();
                let var_ty = sym.ty.unwrap().instantiate(&mut self.ty_gen);

                let expr_ty = self.expr_constraints(let_decl.init)?;

                self.associate_types(
                    MonoTypeData::new(var_ty).with_ident(let_decl.ident),
                    MonoTypeData::new(expr_ty).with_expr(let_decl.init),
                );
            }

            Stmt::BlockStmt(block) => {
                self.block_constraints(&block)?;
            }

            Stmt::IfStmt {
                condition,
                then_block,
                else_block,
            } => {
                let condition_ty = self.expr_constraints(condition)?;
                self.associate_types(
                    MonoTypeData::new(condition_ty)
                        .with_expr(condition)
                        .with_parent_stmt(stmt_id),
                    MonoType::bool().into(),
                );

                self.stmt_constraints(then_block)?;
                if let Some(else_block) = else_block {
                    self.stmt_constraints(else_block)?;
                }
            }

            Stmt::ExprStmt(expr) => {
                self.expr_constraints(expr)?;
            }

            Stmt::Comment(_) => {}
        };

        Ok(())
    }

    fn block_constraints(&mut self, block: &Block) -> AnalysisResult<()> {
        for stmt in &block.statements {
            self.stmt_constraints(*stmt)?;
        }

        Ok(())
    }

    fn expr_constraints(&mut self, expr_id: ExprId) -> AnalysisResult<MonoType> {
        let expr_node = &self.program.ast.expressions[expr_id];

        match &expr_node.expr.clone() {
            Expr::Integer(_) => Ok(MonoType::int()),
            Expr::Float(_) => Ok(MonoType::float()),
            Expr::String(_) => Ok(MonoType::string()),
            Expr::Bool(_) => Ok(MonoType::bool()),
            Expr::Ident(ident) => {
                let i = &self.program.ast.identifiers[*ident];
                println!("Generating constraints for ident: {:?}", i);
                let sym = self.symbol_table.symbol_for_ident(ident).unwrap();
                let ty = sym.ty.unwrap();

                let instantiated = ty.instantiate(&mut self.ty_gen);

                Ok(instantiated)
            }

            Expr::Call { callee, args } => {
                let callee_expr = &self.program.ast.expressions[*callee];
                let callee_ident = match callee_expr.expr {
                    Expr::Ident(ident) => ident,
                    _ => {
                        return Err(AnalysisError::InvalidFunctionCall {
                            span: callee_expr.span.clone(),
                        })
                    }
                };

                let callee_sym = self.symbol_table.symbol_for_ident(&callee_ident).unwrap();
                let callee_ty = callee_sym.ty.unwrap().instantiate(&mut self.ty_gen);

                let fun_ty = match callee_ty {
                    MonoType::FunApp(fun) => fun,
                    _ => {
                        return Err(AnalysisError::InvalidFunctionCall {
                            span: callee_expr.span.clone(),
                        })
                    }
                };

                let arg_span = if args.len() > 0 {
                    Span::new(
                        self.program.ast.expressions[args[0]].span.start(),
                        self.program.ast.expressions[args[args.len() - 1]]
                            .span
                            .end(),
                    )
                } else {
                    Span::new(
                        self.program.ast.expressions[*callee].span.start(),
                        expr_node.span.end(),
                    )
                };

                if fun_ty.params.len() != args.len() {
                    return Err(AnalysisError::IncorrectNumberOfArguments {
                        expected: fun_ty.params.len(),
                        found: args.len(),
                        func_decl: callee_sym.decl_source.clone(),
                        found_span: arg_span,
                    });
                }

                let callee_ty = self.expr_constraints(*callee)?;

                let mut arg_tys = Vec::new();
                for (arg, param) in args.iter().zip(fun_ty.params.iter()) {
                    let arg_ty = self.expr_constraints(*arg)?;
                    self.associate_types(
                        MonoTypeData::new(arg_ty.clone()).with_expr(*arg),
                        MonoTypeData::new(param.clone()),
                    );

                    arg_tys.push(arg_ty);
                }

                let return_ty = self.ty_gen.gen_var();

                let expected_left_ty = MonoType::FunApp(FunctionAppType {
                    params: arg_tys.clone(),
                    return_ty: Box::new(return_ty.clone()),
                });

                self.associate_types(
                    MonoTypeData::new(callee_ty).with_expr(*callee),
                    MonoTypeData::new(expected_left_ty),
                );

                Ok(return_ty)
            }

            Expr::PrefixOp { op, right } => match op.prefix_type {
                PrefixOpType::Not => {
                    let right_ty = self.expr_constraints(*right)?;
                    self.associate_types(
                        MonoTypeData::new(right_ty.clone()).with_expr(*right),
                        MonoType::bool().into(),
                    );

                    Ok(right_ty)
                }
                PrefixOpType::Neg => {
                    let right_ty = self.expr_constraints(*right)?;
                    let return_ty = self.ty_gen.gen_var();

                    self.associate_types(
                        MonoTypeData::new(right_ty).with_expr(*right),
                        MonoTypeData::new(return_ty.clone()),
                    );

                    Ok(return_ty)
                }
            },

            Expr::BinaryOp { left, right, .. } => {
                let left_ty = self.expr_constraints(*left)?;
                let right_ty = self.expr_constraints(*right)?;

                self.associate_types(
                    MonoTypeData::new(left_ty)
                        .with_expr(*left)
                        .with_parent_expr(expr_id),
                    MonoTypeData::new(right_ty.clone()).with_expr(*right),
                );

                let return_ty = self.ty_gen.gen_var();
                self.associate_types(
                    MonoTypeData::new(return_ty.clone()).with_parent_expr(expr_id),
                    MonoTypeData::new(right_ty).with_expr(*right),
                );

                Ok(return_ty)
            }

            // Expr::BinaryOp {
            //     left,
            //     op,
            //     right,
            //     span,
            // } => todo!(),
            // Expr::PostfixOp { op, left, span } => todo!(),
            _ => todo!("expr_constraints not implemented for {:?}", expr_node.expr),
        }
    }

    fn generate_symbols_for_statement(&mut self, stmt_id: StmtId) -> AnalysisResult<()> {
        let stmt_node = &self.program.ast.statements[stmt_id];

        match &stmt_node.stmt.clone() {
            Stmt::Let(let_decl) => {
                let ident = &self.program.ast.identifiers[let_decl.ident];

                if let Some(sym) = self.symbol_table.get_in_current_scope(&ident.name) {
                    return Err(AnalysisError::VariableAlreadyDeclared {
                        name: sym.name.clone(),
                        first_declaration: sym.decl_source,
                        span: ident.span.clone(),
                    });
                }

                let ty = if let Some(ty) = let_decl.ty.clone() {
                    self.type_for_annotation(&ty)?
                } else {
                    self.ty_gen.gen_var()
                };

                let sym =
                    self.symbol_table
                        .insert_mono(ident.name.clone(), ty, Some(ident.span.clone()));
                self.symbol_table.associate_ident(let_decl.ident, sym.id);

                let init_expr = let_decl.init;
                self.generate_symbols_for_expression(init_expr)?;
            }

            Stmt::ExprStmt(expr) => {
                self.generate_symbols_for_expression(*expr)?;
            }

            Stmt::BlockStmt(block) => {
                self.symbol_table.enter_scope();

                for stmt in block.statements.clone() {
                    self.generate_symbols_for_statement(stmt)?;
                }

                self.symbol_table.leave_scope();
            }

            Stmt::IfStmt {
                condition,
                then_block,
                else_block,
            } => {
                self.generate_symbols_for_expression(*condition)?;
                self.generate_symbols_for_statement(*then_block)?;

                if let Some(else_block) = else_block {
                    self.generate_symbols_for_statement(*else_block)?;
                }
            }

            Stmt::Comment(_) => {}
        };

        Ok(())
    }

    fn generate_symbols_for_expression(&mut self, expr_id: ExprId) -> AnalysisResult<()> {
        let expr_node = self.program.ast.expressions[expr_id].clone();

        match &expr_node.expr {
            Expr::Integer(_) => {}
            Expr::Float(_) => {}
            Expr::String(_) => {}
            Expr::Bool(_) => {}

            Expr::Ident(ident_id) => {
                let ident = &self.program.ast.identifiers[*ident_id];

                if let Some(sym) = self.symbol_table.get(&ident.name) {
                    self.symbol_table.associate_ident(*ident_id, sym.id);
                } else {
                    return Err(AnalysisError::UndeclaredVariable {
                        name: ident.name.clone(),
                        span: ident.span.clone(),
                    });
                }
            }

            Expr::PrefixOp { right, .. } => {
                self.generate_symbols_for_expression(*right)?;
            }

            Expr::BinaryOp { left, right, .. } => {
                self.generate_symbols_for_expression(left.clone())?;
                self.generate_symbols_for_expression(*right)?;
            }

            Expr::PostfixOp { left, .. } => {
                self.generate_symbols_for_expression(*left)?;
            }

            Expr::Call { callee, args } => {
                self.generate_symbols_for_expression(*callee)?;

                for arg in args {
                    self.generate_symbols_for_expression(*arg)?;
                }
            }
        }

        Ok(())
    }

    // Get the polytype of a function declaration based on its signature
    fn get_type_of_function_decl(&self, func: &FuncDecl) -> AnalysisResult<PolyType> {
        // Get the type of the arguments
        let mut param_tys = Vec::new();
        for param in &func.args {
            let ty = self.type_for_annotation(&param.ty)?;
            param_tys.push(ty);
        }

        // Get the return type
        let return_ty = match &func.return_ty {
            Some(annotation) => {
                let ty = self.type_for_annotation(annotation)?;
                ty
            }
            None => MonoType::unit(),
        };

        let func_ty = FunctionAppType {
            params: param_tys,
            return_ty: Box::new(return_ty),
        };

        Ok(PolyType::Mono(MonoType::FunApp(func_ty)))
    }

    // Get the type of a type annotation by looking up the identifier in the type_symbols table
    fn type_for_annotation(&self, annotation: &TypeAnnotation) -> AnalysisResult<MonoType> {
        self.type_symbols
            .get(&annotation.name)
            .and_then(|sym| sym.ty)
            .map_or_else(
                || {
                    Err(AnalysisError::UndefinedType {
                        name: annotation.name.clone(),
                        span: annotation.span.clone(),
                    })
                },
                |ty| match ty {
                    PolyType::Mono(ty) => Ok(ty.clone()),
                    PolyType::Quantifier(_) => Err(AnalysisError::InvalidTypeAnnotation {
                        name: annotation.name.clone(),
                        span: annotation.span.clone(),
                    }),
                },
            )
    }

    // fn symbol_for_ident(&mut self, name: &Identifier) -> AnalysisResult<Symbol> {
    // if let Some(sym_id) = name.symbol_id {
    //     self.symbol_table.get_by_id(sym_id).ok_or_else(|| {
    //         AnalysisError::new(AnalysisErrorKind::SymbolNotFound(name.name.clone()))
    //     })
    // } else {
    //     err!(AnalysisErrorKind::IdentifierDoesNotHaveSymbol(
    //         name.name.clone()
    //     ))
    // }
    // }

    // fn typecheck_program(&mut self, program: &Program) -> AnalysisResult<()> {
    //     for stmt in &program.statements {
    //         self.stmt_constraints(&stmt)?;
    //     }

    //     println!("\n=== Constraints:");
    //     self.typechecker.print_constraints();
    //     println!("");

    //     let sub = self.typechecker.solve_constraints().map_err(|e| {
    //         let mut err = AnalysisError::new(AnalysisErrorKind::TypecheckError(e.kind));
    //         if let Some(span) = &e.span {
    //             err = err.with_span(span.clone());
    //         }
    //         err
    //     })?;
    //     println!("\n=== Substitutions:\n{:#?}\n", sub);

    //     self.apply_substition_to_symbol_table(&sub);

    //     // TODO: Check that we have no more type variables left after solving constraints

    //     Ok(())
    // }

    // fn apply_substition_to_symbol_table(&mut self, sub: &Substitution) {
    //     for (_, sym) in self.symbol_table.symbols.iter_mut() {
    //         if let Some(ty) = &mut sym.ty {
    //             *ty = ty.apply(sub);
    //         }
    //     }
    // }

    // fn get_type_of_function_decl(&mut self, func: &FuncDecl) -> AnalysisResult<PolyType> {
    //     // Get the type of the arguments
    //     let mut param_tys = Vec::new();
    //     for param in &func.args {
    //         let ty = self.type_for_annotation(&param.ty)?;
    //         param_tys.push(ty);
    //     }

    //     // Get the return type
    //     let return_ty = match &func.return_ty {
    //         Some(annotation) => {
    //             let ty = self.type_for_annotation(annotation)?;
    //             ty
    //         }
    //         None => MonoType::unit(),
    //     };

    //     let func_ty = FunctionAppType {
    //         params: param_tys,
    //         return_ty: Box::new(return_ty),
    //     };

    //     Ok(PolyType::Mono(MonoType::FunApp(func_ty)))
    // }

    // fn symbol_for_ident(&mut self, name: &Identifier) -> AnalysisResult<Symbol> {
    //     if let Some(sym_id) = name.symbol_id {
    //         self.symbol_table.get_by_id(sym_id).ok_or_else(|| {
    //             AnalysisError::new(AnalysisErrorKind::SymbolNotFound(name.name.clone()))
    //         })
    //     } else {
    //         err!(AnalysisErrorKind::IdentifierDoesNotHaveSymbol(
    //             name.name.clone()
    //         ))
    //     }
    // }

    // fn stmt_constraints(&mut self, stmt: &Stmt) -> AnalysisResult<()> {
    //     match stmt {
    //         Stmt::Struct(_) => {}
    //         Stmt::Func(_) => {}
    //         Stmt::Let(let_decl) => {
    //             let sym = self.symbol_for_ident(&let_decl.ident)?;
    //             let var_ty = match &sym.ty {
    //                 Some(PolyType::Mono(ty @ MonoType::Variable(_))) => ty.clone(),
    //                 _ => return err!(AnalysisErrorKind::UnknownError),
    //             };

    //             let expr_ty = self.expr_constraints(&let_decl.init)?;
    //             self.typechecker
    //                 .associate_types(var_ty, expr_ty, let_decl.span.clone());
    //         }
    //         Stmt::IfStmt {
    //             condition,
    //             then_block,
    //             else_block,
    //             ..
    //         } => {
    //             let condition_ty = self.expr_constraints(condition)?;
    //             self.typechecker
    //                 .associate_types(condition_ty, MonoType::bool(), condition.span());
    //             self.block_constraints(then_block)?;

    //             if let Some(else_block) = else_block {
    //                 self.block_constraints(else_block)?;
    //             }
    //         }
    //         Stmt::ExprStmt { expr, .. } => {
    //             let _ty = self.expr_constraints(expr)?;
    //         }
    //     };

    //     Ok(())
    // }

    // fn block_constraints(&mut self, block: &Block) -> AnalysisResult<()> {
    //     for stmt in &block.statements {
    //         self.stmt_constraints(stmt)?;
    //     }

    //     Ok(())
    // }

    // fn expr_constraints(&mut self, expr: &Expr) -> AnalysisResult<MonoType> {
    //     match expr {
    //         Expr::Integer { .. } => {
    //             let ty = self.typechecker.gen_type_var(expr.span());
    //             self.typechecker
    //                 .associate_types(ty.clone(), MonoType::int(), expr.span());
    //             Ok(ty)
    //         }
    //         Expr::Float { .. } => Ok(MonoType::float()),
    //         Expr::String { .. } => Ok(MonoType::string()),
    //         Expr::Ident(ident) => {
    //             let sym = self.symbol_for_ident(ident)?;
    //             let ty = match &sym.ty {
    //                 Some(ty) => ty.clone(),
    //                 None => return err!(AnalysisErrorKind::UnknownError, ident.span()),
    //             };

    //             let instantiated = self.typechecker.instantiate(ty);

    //             Ok(instantiated)
    //         }
    //         Expr::PrefixOp { op, right, span } => todo!(),
    //         Expr::BinaryOp {
    //             left,
    //             op,
    //             right,
    //             span,
    //         } => todo!(),
    //         Expr::PostfixOp { op, left, span } => todo!(),
    //         Expr::Conditional {
    //             condition,
    //             then_branch,
    //             else_branch,
    //             span,
    //         } => todo!(),
    //         Expr::Call { callee, args, span } => {
    //             let callee_ty = self.expr_constraints(callee)?;

    //             let arg_tys = args
    //                 .iter()
    //                 .map(|arg| self.expr_constraints(arg))
    //                 .collect::<AnalysisResult<Vec<_>>>()?;

    //             let return_ty = self.typechecker.gen_type_var(expr.span());

    //             // Based on the argument types and return type, this is what the callee type should be
    //             let expected_left_ty = MonoType::FunApp(FunctionAppType {
    //                 params: arg_tys.clone(),
    //                 return_ty: Box::new(return_ty.clone()),
    //             });

    //             println!("callee_ty: {}", callee_ty);
    //             println!("expected_left_ty: {}", expected_left_ty);

    //             self.typechecker
    //                 .associate_types(callee_ty, expected_left_ty, expr.span());

    //             Ok(return_ty)
    //         }

    //         // Comments should be statements since they don't return a value
    //         Expr::Comment { span, .. } => err!(AnalysisErrorKind::UnknownError, span.clone()),
    //     }
    // }

    // /// Rewrite the program to include symbols
    // fn rewrite_program_with_symbols(&mut self, program: &Program) -> AnalysisResult<Program> {
    //     let mut statements = Vec::new();

    //     for stmt in &program.statements {
    //         let new_stmt = self.rewrite_statement_with_symbols(stmt)?;
    //         statements.push(new_stmt);
    //     }

    //     Ok(Program { statements })
    // }

    // fn rewrite_statement_with_symbols(&mut self, stmt: &Stmt) -> AnalysisResult<Stmt> {
    //     let s = match stmt {
    //         Stmt::Struct(_) => todo!(),
    //         Stmt::Func(func) => {
    //             // TODO: This won't handle calling functions that haven't been declared yet
    //             // TODO:: We first need to load all the function declarations into the symbol table
    //             if self.symbol_table.get(&func.ident.name).is_some() {
    //                 return err!(
    //                     AnalysisErrorKind::FunctionAlreadyDeclared(func.ident.name.clone()),
    //                     func.ident.span()
    //                 );
    //             }

    //             let func_ty = self.get_type_of_function_decl(func)?;
    //             let func_sym = self.symbol_table.insert(func.ident.name.clone(), func_ty);

    //             self.symbol_table.enter_scope();
    //             let args = func
    //                 .args
    //                 .iter()
    //                 .map(|arg| {
    //                     let ty = self.type_for_annotation(&arg.ty)?;
    //                     let arg_sym = self.symbol_table.insert_mono(arg.ident.name.clone(), ty);

    //                     Ok(FuncArg {
    //                         ident: arg.ident.with_symbol_id(arg_sym.id),
    //                         ..arg.clone()
    //                     })
    //                 })
    //                 .collect::<AnalysisResult<Vec<_>>>()?;

    //             let body = self.rewrite_block_with_symbols(&func.body)?;

    //             self.symbol_table.leave_scope();

    //             Stmt::Func(FuncDecl {
    //                 ident: func.ident.with_symbol_id(func_sym.id),
    //                 args,
    //                 body,
    //                 ..func.clone()
    //             })
    //         }
    //         Stmt::Let(let_decl) => {
    //             if self
    //                 .symbol_table
    //                 .defined_in_current_scope(&let_decl.ident.name)
    //             {
    //                 return err!(
    //                     AnalysisErrorKind::VariableAlreadyDeclared(let_decl.ident.name.clone()),
    //                     let_decl.ident.span()
    //                 );
    //             }

    //             let ty = if let Some(ty) = &let_decl.ty {
    //                 self.type_for_annotation(&ty)?
    //             } else {
    //                 self.typechecker.gen_type_var(let_decl.ident.span())
    //             };

    //             let init = self.rewrite_expr_with_symbols(&let_decl.init)?;

    //             let sym = self
    //                 .symbol_table
    //                 .insert_mono(let_decl.ident.name.clone(), ty);

    //             Stmt::Let(LetDecl {
    //                 ident: let_decl.ident.with_symbol_id(sym.id),
    //                 init,
    //                 ..let_decl.clone()
    //             })
    //         }
    //         Stmt::IfStmt {
    //             condition,
    //             then_block,
    //             else_block,
    //             span,
    //         } => {
    //             let condition = self.rewrite_expr_with_symbols(condition)?;
    //             let then_block = self.rewrite_block_with_symbols(then_block)?;

    //             let else_block = if let Some(else_block) = else_block {
    //                 Some(self.rewrite_block_with_symbols(else_block)?)
    //             } else {
    //                 None
    //             };

    //             Stmt::IfStmt {
    //                 condition,
    //                 then_block,
    //                 else_block,
    //                 span: span.clone(),
    //             }
    //         }
    //         Stmt::ExprStmt { expr, span } => {
    //             let expr = self.rewrite_expr_with_symbols(expr)?;
    //             Stmt::ExprStmt {
    //                 expr: Box::new(expr),
    //                 span: span.clone(),
    //             }
    //         }
    //     };

    //     Ok(s)
    // }

    // fn rewrite_block_with_symbols(&mut self, block: &Block) -> AnalysisResult<Block> {
    //     let statements = block
    //         .statements
    //         .iter()
    //         .map(|stmt| self.rewrite_statement_with_symbols(stmt))
    //         .collect::<AnalysisResult<Vec<_>>>()?;

    //     Ok(Block {
    //         statements,
    //         ..block.clone()
    //     })
    // }

    // fn rewrite_expr_with_symbols(&mut self, expr: &Expr) -> AnalysisResult<Expr> {
    //     let e = match expr {
    //         Expr::Integer { .. } => expr.clone(),
    //         Expr::Float { .. } => expr.clone(),
    //         Expr::String { .. } => expr.clone(),
    //         Expr::Ident(ident) => {
    //             let sym = self.symbol_table.get(&ident.name).ok_or_else(|| {
    //                 AnalysisError::new(AnalysisErrorKind::UndeclaredVariable(ident.name.clone()))
    //                     .with_span(ident.span())
    //             })?;
    //             Expr::Ident(ident.with_symbol_id(sym.id))
    //         }
    //         Expr::PrefixOp { op, right, span } => Expr::PrefixOp {
    //             op: op.clone(),
    //             right: Box::new(self.rewrite_expr_with_symbols(&*right)?),
    //             span: span.clone(),
    //         },
    //         Expr::BinaryOp {
    //             left,
    //             op,
    //             right,
    //             span,
    //         } => Expr::BinaryOp {
    //             left: Box::new(self.rewrite_expr_with_symbols(&*left)?),
    //             op: op.clone(),
    //             right: Box::new(self.rewrite_expr_with_symbols(&*right)?),
    //             span: span.clone(),
    //         },
    //         Expr::PostfixOp { op, left, span } => Expr::PostfixOp {
    //             op: op.clone(),
    //             left: Box::new(self.rewrite_expr_with_symbols(&*left)?),
    //             span: span.clone(),
    //         },
    //         Expr::Conditional {
    //             condition,
    //             then_branch,
    //             else_branch,
    //             span,
    //         } => Expr::Conditional {
    //             condition: Box::new(self.rewrite_expr_with_symbols(&*condition)?),
    //             then_branch: Box::new(self.rewrite_expr_with_symbols(&*then_branch)?),
    //             else_branch: Box::new(self.rewrite_expr_with_symbols(&*else_branch)?),
    //             span: span.clone(),
    //         },
    //         Expr::Call { callee, args, span } => Expr::Call {
    //             callee: Box::new(self.rewrite_expr_with_symbols(&*callee)?),
    //             args: args
    //                 .iter()
    //                 .map(|arg| self.rewrite_expr_with_symbols(arg))
    //                 .collect::<AnalysisResult<Vec<_>>>()?,
    //             span: span.clone(),
    //         },
    //         Expr::Comment { .. } => expr.clone(),
    //     };

    //     Ok(e)
    // }

    // fn analyze_function(&mut self, func: &FuncDecl) -> AnalysisResult<()> {
    //     self.symbol_table.enter_scope();

    //     let mut type_ctx = TypeContext::new();
    //     for sym in self.symbol_table.symbols.values() {
    //         if let Some(ty) = &sym.ty {
    //             type_ctx.insert(sym.name.clone(), ty.clone());
    //         }
    //     }

    //     // Add the function arguments to the type context
    //     for arg in &func.args {
    //         let arg_ty = self.type_for_annotation(&arg.ty)?;
    //         type_ctx.insert_mono(arg.name.clone(), arg_ty.clone());

    //         self.symbol_table.insert_mono(arg.name.clone(), arg_ty);
    //     }

    //     println!("=== Types after decl:\n{}", type_ctx);

    //     for stmt in &func.body.statements {
    //         match stmt {
    //             Stmt::Let(let_decl) => {
    //                 if self
    //                     .symbol_table
    //                     .defined_in_current_scope(&let_decl.name.name)
    //                 {
    //                     return err!(
    //                         AnalysisErrorKind::VariableAlreadyDeclared(let_decl.name.name.clone()),
    //                         let_decl.name.span()
    //                     );
    //                 }

    //                 let ty = self.typechecker.gen_type_var();
    //                 let sym = self
    //                     .symbol_table
    //                     .insert_mono(let_decl.name.name.clone(), ty.clone());

    //                 let init_ty = self.typechecker.gen_type_var();
    //                 self.typechecker.associate_types(ty, init_ty);

    //                 println!("Symbol: {:?}", sym);
    //             }
    //             _ => panic!("Unknown statement {:?}", stmt),
    //         }
    //     }

    //     self.symbol_table.leave_scope();

    //     Ok(())
    // }

    // fn type_for_annotation(&mut self, annotation: &TypeAnnotation) -> AnalysisResult<MonoType> {
    //     self.type_symbols
    //         .get(&annotation.name)
    //         .and_then(|sym| sym.ty)
    //         .map_or_else(
    //             || {
    //                 err!(
    //                     AnalysisErrorKind::UndefinedType(annotation.name.clone()),
    //                     annotation.span()
    //                 )
    //             },
    //             |ty| match ty {
    //                 PolyType::Mono(ty) => Ok(ty.clone()),
    //                 PolyType::Quantifier(_) => {
    //                     err!(
    //                         AnalysisErrorKind::InvalidTypeAnnotation(annotation.name.clone()),
    //                         annotation.span()
    //                     )
    //                 }
    //             },
    //         )
    // }
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
        for (_, sym) in &self.symbols {
            writeln!(f, "{}", sym)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
}
