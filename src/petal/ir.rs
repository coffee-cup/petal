use std::fmt::Display;

use super::{
    ast::{Expr, ExprId, FuncArg, FuncDecl, Stmt, StmtId},
    semantics::{context::SemanticContext, symbol_table::Symbol},
    types::{FunctionAppType, MonoType},
};

#[derive(Clone, Debug)]
pub struct IRProgram {
    pub functions: Vec<IRFunction>,
}

#[derive(Clone, Debug)]
pub struct IRParam {
    pub name: String,
    pub ty: MonoType,
}

#[derive(Clone, Debug)]
pub struct IRFunctionSignature {
    pub name: String,
    pub params: Vec<IRParam>,
    pub return_type: MonoType,
}

#[derive(Clone, Debug)]
pub struct IRFunction {
    pub signature: IRFunctionSignature,
    pub body: IRStatement,
}

#[derive(Clone, Debug)]
pub enum IRStatement {
    Let {
        name: String,
        ty: MonoType,
        init: IRExpression,
    },

    If {
        condition: IRExpression,
        then: Vec<IRStatement>,
    },

    Block {
        statements: Vec<IRStatement>,
    },

    Expr(IRExpression),
}

#[derive(Clone, Debug)]
pub enum IRExpression {
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    BoolLiteral(bool),
    Ident {
        name: String,
        ty: MonoType,
    },
    Call {
        name: String,
        args: Vec<IRExpression>,
        ty: MonoType,
    },
}

pub struct IRGeneration<'a> {
    semantics: &'a SemanticContext<'a>,
}

impl<'a> IRGeneration<'a> {
    pub fn new(semantics: &'a SemanticContext) -> Self {
        Self { semantics }
    }

    pub fn generate_ir(&self) -> IRProgram {
        let mut ir = IRProgram { functions: vec![] };
        for func in self.semantics.program.functions.iter() {
            let func_ir = self.ir_for_function(func);
            ir.functions.push(func_ir);
        }

        let main_func: IRFunction = {
            let main_stmts = self
                .semantics
                .program
                .main_stmts
                .iter()
                .map(|stmt| self.ir_for_statement(*stmt))
                .collect::<Vec<_>>();
            let main_ir_sig = IRFunctionSignature {
                name: "_start".into(),
                params: vec![],
                return_type: MonoType::unit(),
            };
            let main_func_ir = IRFunction {
                signature: main_ir_sig,
                body: IRStatement::Block {
                    statements: main_stmts,
                },
            };

            main_func_ir
        };
        ir.functions.push(main_func);

        ir
    }

    fn ir_for_function(&self, func: &FuncDecl) -> IRFunction {
        let sym = self
            .semantics
            .symbol_table
            .symbol_for_ident(&func.ident)
            .unwrap();

        let signature = self.signature_for_func(&sym, &func.args);
        let body = self.ir_for_statement(func.body);
        IRFunction { signature, body }
    }

    fn signature_for_func(&self, sym: &Symbol, args: &Vec<FuncArg>) -> IRFunctionSignature {
        let ty = sym.ty.clone().unwrap().extract_monotype().unwrap();

        let return_ty = match ty {
            MonoType::FunApp(FunctionAppType { return_ty, .. }) => return_ty,
            _ => unreachable!(),
        };

        let params = args
            .iter()
            .map(|arg| {
                let arg_sym = self
                    .semantics
                    .symbol_table
                    .symbol_for_ident(&arg.ident)
                    .unwrap();
                let arg_ty = arg_sym.ty.clone().unwrap().extract_monotype().unwrap();

                IRParam {
                    name: arg_sym.unique_name(),
                    ty: arg_ty,
                }
            })
            .collect::<Vec<_>>();

        IRFunctionSignature {
            name: sym.unique_name(),
            params,
            return_type: *return_ty,
        }
    }

    fn ir_for_statement(&self, stmt: StmtId) -> IRStatement {
        match self.get_stmt(stmt) {
            Stmt::Let(let_decl) => {
                let sym = self
                    .semantics
                    .symbol_table
                    .symbol_for_ident(&let_decl.ident)
                    .unwrap();
                let ty = sym.ty.clone().unwrap().extract_monotype().unwrap();
                IRStatement::Let {
                    name: sym.unique_name(),
                    ty,
                    init: self.ir_for_expr(let_decl.init),
                }
            }

            Stmt::IfStmt {
                condition,
                then_block,
                else_block,
            } => todo!(),

            Stmt::BlockStmt(block) => {
                let stmts = block
                    .statements
                    .iter()
                    .map(|stmt| self.ir_for_statement(*stmt))
                    .collect();
                IRStatement::Block { statements: stmts }
            }

            Stmt::ExprStmt(_) => todo!(),
            Stmt::Comment(_) => todo!(),
        }
    }

    fn ir_for_expr(&self, expr: ExprId) -> IRExpression {
        match self.get_expr(expr) {
            Expr::Integer(n) => IRExpression::IntLiteral(n),
            Expr::Float(n) => IRExpression::FloatLiteral(n),
            Expr::String(s) => IRExpression::StringLiteral(s),
            Expr::Bool(b) => IRExpression::BoolLiteral(b),
            Expr::Ident(ident) => {
                let sym = self
                    .semantics
                    .symbol_table
                    .symbol_for_ident(&ident)
                    .unwrap();
                let ty = sym.ty.clone().unwrap().extract_monotype().unwrap();
                IRExpression::Ident {
                    name: sym.unique_name(),
                    ty,
                }
            }
            Expr::PrefixOp { op, right } => todo!(),
            Expr::BinaryOp { op, left, right } => todo!(),
            Expr::PostfixOp { op, left } => todo!(),
            Expr::Call { callee, args } => todo!(),
        }
    }

    fn get_stmt(&self, stmt: StmtId) -> Stmt {
        self.semantics.program.ast.statements[stmt].stmt.clone()
    }

    fn get_expr(&self, expr: ExprId) -> Expr {
        self.semantics.program.ast.expressions[expr].expr.clone()
    }
}

impl Display for IRProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for func in self.functions.iter() {
            write!(f, "{}\n", func)?;
        }
        Ok(())
    }
}

impl Display for IRFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn {}", self.signature)?;
        write!(f, " {{\n")?;
        write!(f, "{}", self.body)?;
        write!(f, "}}\n")
    }
}

impl Display for IRParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.ty)
    }
}

impl Display for IRFunctionSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params_str = self
            .params
            .iter()
            .map(|param| format!("{}", param))
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "{}({}): {}", self.name, params_str, self.return_type)
    }
}

impl Display for IRStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IRStatement::Let { name, ty, init } => write!(f, "let {}: {} = {};", name, ty, init),
            IRStatement::Block { statements } => {
                for stmt in statements.iter() {
                    write!(f, "    {}\n", stmt)?;
                }

                Ok(())
            }
            IRStatement::If { condition, then } => {
                write!(f, "if {} {{\n", condition)?;
                for stmt in then.iter() {
                    write!(f, "    {}\n", stmt)?;
                }
                write!(f, "}}\n")
            }
            IRStatement::Expr(e) => {
                write!(f, "{}", e)
            }
        }
    }
}

impl Display for IRExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IRExpression::IntLiteral(n) => write!(f, "{}", n),
            IRExpression::FloatLiteral(n) => write!(f, "{}", n),
            IRExpression::StringLiteral(s) => write!(f, "{}", s),
            IRExpression::BoolLiteral(b) => write!(f, "{}", b),
            IRExpression::Ident { name, ty } => write!(f, "{}: {}", name, ty),
            IRExpression::Call { name, args, ty } => {
                let args_str = args
                    .iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{}({}): {}", name, args_str, ty)
            }
        }
    }
}
