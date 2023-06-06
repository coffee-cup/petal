use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use thiserror::Error;

use crate::petal::ast::Stmt;

use super::{
    ast::{Block, Expr, FuncArg, FuncDecl, Identifier, LetDecl, Program, TypeAnnotation},
    positions::{HasSpan, Span},
    typechecker::{Constraint, TypeContext, Typechecker},
    types::{FunctionAppType, MonoType, PolyType, StructType},
};

#[derive(Error, Clone, Debug)]
pub enum AnalysisErrorKind {
    #[error("Unknown analysis error")]
    UnknownError,

    #[error("Undeclared variable {0}")]
    UndeclaredVariable(String),

    #[error("Undefined type {0}")]
    UndefinedType(String),

    #[error("Function {0} already exists")]
    FunctionAlreadyDeclared(String),

    #[error("Invalid type annotation {0}")]
    InvalidTypeAnnotation(String),

    #[error("Variable {0} already defined")]
    VariableAlreadyDeclared(String),
}

#[derive(Clone, Debug)]
pub struct AnalysisError {
    pub kind: AnalysisErrorKind,
    pub span: Option<Span>,
}

impl AnalysisError {
    pub fn new(kind: AnalysisErrorKind) -> Self {
        Self { kind, span: None }
    }

    pub fn with_span(&self, span: Span) -> Self {
        Self {
            span: Some(span),
            ..self.clone()
        }
    }
}

type AnalysisResult<T> = Result<T, AnalysisError>;

macro_rules! err {
    ($kind:expr) => {
        Err(AnalysisError::new($kind))
    };
    ($kind:expr, $span:expr) => {
        Err(AnalysisError::new($kind).with_span($span))
    };
}

type SymbolId = usize;

#[derive(PartialEq, Clone, Debug)]
struct Symbol {
    id: SymbolId,
    name: String,
    ty: Option<PolyType>,
}

impl Symbol {
    pub fn new(id: SymbolId, name: String) -> Self {
        Self { id, name, ty: None }
    }

    pub fn with_type(&self, ty: PolyType) -> Self {
        Self {
            id: self.id,
            name: self.name.clone(),
            ty: Some(ty),
        }
    }
}

#[derive(Clone, Debug)]
struct SymbolTable {
    /// Maps symbol IDs to their types
    symbols: HashMap<SymbolId, Symbol>,

    /// Maps variables names in a scope to their symbol IDs
    scopes: Vec<HashMap<String, SymbolId>>,

    counter: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
            scopes: vec![HashMap::new()],
            counter: 0,
        }
    }

    pub fn insert_none(&mut self, name: String) -> Symbol {
        let id = self.gen_id();
        let sym = Symbol::new(id, name.clone());
        self.symbols.insert(id, sym.clone());
        self.scopes.last_mut().unwrap().insert(name, id);

        sym
    }

    pub fn insert_mono(&mut self, name: String, ty: MonoType) -> Symbol {
        self.insert(name, PolyType::Mono(ty))
    }

    pub fn insert(&mut self, name: String, ty: PolyType) -> Symbol {
        let id = self.gen_id();
        let sym = Symbol::new(id, name.clone()).with_type(ty);
        self.symbols.insert(id, sym.clone());
        self.scopes.last_mut().unwrap().insert(name, id);

        sym
    }

    pub fn get(&mut self, name: &String) -> Option<Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(id) = scope.get(name) {
                let sym = self
                    .symbols
                    .get(id)
                    .expect(format!("Symbol ID {} not found in table", id).as_str());

                return Some(sym.clone());
            }
        }

        None
    }

    pub fn defined_in_current_scope(&self, name: &String) -> bool {
        self.scopes
            .last()
            .unwrap()
            .get(name)
            .map(|_| true)
            .unwrap_or(false)
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn leave_scope(&mut self) {
        self.scopes.pop();
    }

    fn gen_id(&mut self) -> SymbolId {
        let id = self.counter;
        self.counter += 1;
        id
    }
}

pub struct TypeTable(HashMap<String, MonoType>);

pub struct Analysis {
    symbol_table: SymbolTable,
    type_symbols: SymbolTable,

    typechecker: Typechecker,
}

impl Analysis {
    pub fn new() -> Self {
        let symbol_table = SymbolTable::new();

        // let mut type_ctx = TypeContext::new();

        // Load global types
        // type_ctx.insert_mono("Int".into(), MonoType::int());
        // type_ctx.insert_mono("Float".into(), MonoType::float());
        // type_ctx.insert_mono("Bool".into(), MonoType::bool());
        // type_ctx.insert_mono("String".into(), MonoType::string());

        let mut type_symbols = SymbolTable::new();
        type_symbols.insert_mono("Int".into(), MonoType::int());
        type_symbols.insert_mono("Float".into(), MonoType::float());
        type_symbols.insert_mono("Bool".into(), MonoType::bool());
        type_symbols.insert_mono("String".into(), MonoType::string());

        let typechecker = Typechecker::new();

        Self {
            symbol_table,
            type_symbols,
            typechecker,
        }
    }

    pub fn analysis_program(&mut self, program: &Program) -> AnalysisResult<()> {
        // self.load_function_declarations(program)?;

        // println!("\n=== Types:\n{}", self.type_ctx);

        let program = self.rewrite_program(program)?;

        println!("=== Symbols:\n{}", self.symbol_table);

        Ok(())
    }

    /// Generate types for all the top-level function declarations and load into the symbol table
    // fn load_function_declarations(&mut self, program: &Program) -> AnalysisResult<()> {
    //     for func in program.functions() {
    //         // Check if the function has already been declared
    //         if self.symbol_table.get(&func.name.name).is_some() {
    //             return err!(
    //                 AnalysisErrorKind::FunctionAlreadyDeclared(func.name.name.clone()),
    //                 func.name.span()
    //             );
    //         }

    //         // Get the type of the arguments
    //         let mut param_tys = Vec::new();
    //         for param in &func.args {
    //             let ty = self.type_for_annotation(&param.ty)?;
    //             param_tys.push(ty);
    //         }

    //         // Get the return type
    //         let return_ty = match &func.return_ty {
    //             Some(annotation) => {
    //                 let ty = self.type_for_annotation(annotation)?;
    //                 ty
    //             }
    //             None => MonoType::unit(),
    //         };

    //         let func_ty = FunctionAppType {
    //             params: param_tys,
    //             return_ty: Box::new(return_ty),
    //         };

    //         self.symbol_table.insert(
    //             func.name.name.clone(),
    //             PolyType::Mono(MonoType::FunApp(func_ty)),
    //         );
    //     }

    //     Ok(())
    // }

    fn get_type_of_function_decl(&mut self, func: &FuncDecl) -> AnalysisResult<PolyType> {
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

    fn generate_constraints(
        &mut self,
        mut constraints: &Vec<Constraint>,
    ) -> AnalysisResult<MonoType> {
        todo!()
    }

    /// Rewrite the program to include symbols
    fn rewrite_program(&mut self, program: &Program) -> AnalysisResult<Program> {
        let mut statements = Vec::new();

        for stmt in &program.statements {
            let new_stmt = self.rewrite_statement_with_symbols(stmt)?;
            statements.push(new_stmt);
        }

        Ok(Program { statements })
    }

    fn rewrite_statement_with_symbols(&mut self, stmt: &Stmt) -> AnalysisResult<Stmt> {
        let s = match stmt {
            Stmt::Struct(_) => todo!(),
            Stmt::Func(func) => {
                // TODO: This won't handle calling functions that haven't been declared yet
                // TODO:: We first need to load all the function declarations into the symbol table
                if self.symbol_table.get(&func.name.name).is_some() {
                    return err!(
                        AnalysisErrorKind::FunctionAlreadyDeclared(func.name.name.clone()),
                        func.name.span()
                    );
                }

                let func_ty = self.get_type_of_function_decl(func)?;
                let func_sym = self.symbol_table.insert(func.name.name.clone(), func_ty);

                self.symbol_table.enter_scope();
                let args = func
                    .args
                    .iter()
                    .map(|arg| {
                        // TODO: Should we assign a type variable here?
                        let ty = self.typechecker.gen_type_var();
                        let arg_sym = self.symbol_table.insert_mono(arg.name.name.clone(), ty);

                        FuncArg {
                            name: arg.name.with_symbol_id(arg_sym.id),
                            ..arg.clone()
                        }
                    })
                    .collect::<Vec<_>>();

                let body = self.rewrite_block_with_symbols(&func.body)?;

                self.symbol_table.leave_scope();

                Stmt::Func(FuncDecl {
                    name: func.name.with_symbol_id(func_sym.id),
                    args,
                    body,
                    ..func.clone()
                })
            }
            Stmt::Let(let_decl) => {
                if self
                    .symbol_table
                    .defined_in_current_scope(&let_decl.name.name)
                {
                    return err!(
                        AnalysisErrorKind::VariableAlreadyDeclared(let_decl.name.name.clone()),
                        let_decl.name.span()
                    );
                }

                let ty = if let Some(ty) = &let_decl.ty {
                    self.type_for_annotation(&ty)?
                } else {
                    self.typechecker.gen_type_var()
                };

                let sym = self
                    .symbol_table
                    .insert_mono(let_decl.name.name.clone(), ty);

                let init = self.rewrite_expr_with_symbols(&let_decl.init)?;

                Stmt::Let(LetDecl {
                    name: let_decl.name.with_symbol_id(sym.id),
                    init,
                    ..let_decl.clone()
                })
            }
            Stmt::IfStmt {
                condition,
                then_block,
                else_block,
                span,
            } => {
                let condition = self.rewrite_expr_with_symbols(condition)?;
                let then_block = self.rewrite_block_with_symbols(then_block)?;

                let else_block = if let Some(else_block) = else_block {
                    Some(self.rewrite_block_with_symbols(else_block)?)
                } else {
                    None
                };

                Stmt::IfStmt {
                    condition,
                    then_block,
                    else_block,
                    span: span.clone(),
                }
            }
            Stmt::ExprStmt { expr, span } => {
                let expr = self.rewrite_expr_with_symbols(expr)?;
                Stmt::ExprStmt {
                    expr: Box::new(expr),
                    span: span.clone(),
                }
            }
        };

        Ok(s)
    }

    fn rewrite_block_with_symbols(&mut self, block: &Block) -> AnalysisResult<Block> {
        let statements = block
            .statements
            .iter()
            .map(|stmt| self.rewrite_statement_with_symbols(stmt))
            .collect::<AnalysisResult<Vec<_>>>()?;

        Ok(Block {
            statements,
            ..block.clone()
        })
    }

    fn rewrite_expr_with_symbols(&mut self, expr: &Expr) -> AnalysisResult<Expr> {
        let e = match expr {
            Expr::Integer { .. } => expr.clone(),
            Expr::Float { .. } => expr.clone(),
            Expr::String { .. } => expr.clone(),
            Expr::Ident(ident) => {
                let sym = self.symbol_table.get(&ident.name).ok_or_else(|| {
                    AnalysisError::new(AnalysisErrorKind::UndeclaredVariable(ident.name.clone()))
                })?;
                Expr::Ident(ident.with_symbol_id(sym.id))
            }
            Expr::PrefixOp { op, right, span } => Expr::PrefixOp {
                op: op.clone(),
                right: Box::new(self.rewrite_expr_with_symbols(&*right)?),
                span: span.clone(),
            },
            Expr::BinaryOp {
                left,
                op,
                right,
                span,
            } => Expr::BinaryOp {
                left: Box::new(self.rewrite_expr_with_symbols(&*left)?),
                op: op.clone(),
                right: Box::new(self.rewrite_expr_with_symbols(&*right)?),
                span: span.clone(),
            },
            Expr::PostfixOp { op, left, span } => Expr::PostfixOp {
                op: op.clone(),
                left: Box::new(self.rewrite_expr_with_symbols(&*left)?),
                span: span.clone(),
            },
            Expr::Conditional {
                condition,
                then_branch,
                else_branch,
                span,
            } => Expr::Conditional {
                condition: Box::new(self.rewrite_expr_with_symbols(&*condition)?),
                then_branch: Box::new(self.rewrite_expr_with_symbols(&*then_branch)?),
                else_branch: Box::new(self.rewrite_expr_with_symbols(&*else_branch)?),
                span: span.clone(),
            },
            Expr::Call { callee, args, span } => Expr::Call {
                callee: Box::new(self.rewrite_expr_with_symbols(&*callee)?),
                args: args
                    .iter()
                    .map(|arg| self.rewrite_expr_with_symbols(arg))
                    .collect::<AnalysisResult<Vec<_>>>()?,
                span: span.clone(),
            },
            Expr::Comment { .. } => expr.clone(),
        };

        Ok(e)
    }

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

    fn type_for_annotation(&mut self, annotation: &TypeAnnotation) -> AnalysisResult<MonoType> {
        self.type_symbols
            .get(&annotation.name)
            .and_then(|sym| sym.ty)
            .map_or_else(
                || {
                    err!(
                        AnalysisErrorKind::UndefinedType(annotation.name.clone()),
                        annotation.span()
                    )
                },
                |ty| match ty {
                    PolyType::Mono(ty) => Ok(ty.clone()),
                    PolyType::Quantifier(_) => {
                        err!(
                            AnalysisErrorKind::InvalidTypeAnnotation(annotation.name.clone()),
                            annotation.span()
                        )
                    }
                },
            )
    }
}

impl Display for SymbolTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (name, sym) in &self.symbols {
            writeln!(f, "{}@{}: {:?}", sym.name, sym.id, sym)?;
        }

        Ok(())
    }
}
