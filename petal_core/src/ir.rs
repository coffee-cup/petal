use std::fmt::Display;

use crate::types::HasType;

use super::{
    ast::{BinaryOpType, Expr, ExprId, FuncArg, FuncDecl, PrefixOpType, Stmt, StmtId},
    semantics::{context::SemanticContext, symbol_table::Symbol},
    types::{FunctionAppType, MonoType},
};

#[derive(Clone, Debug)]
pub struct IRProgram {
    pub functions: Vec<IRFunction>,
    pub main_func: String,
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
    pub is_exported: bool,
}

#[derive(Clone, Debug)]
pub struct IRFunction {
    pub signature: IRFunctionSignature,
    pub locals: Vec<IRLocal>,
    pub body: IRStatement,
}

#[derive(Clone, Debug)]
pub struct IRLocal {
    pub name: String,
    pub ty: MonoType,
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
        then_block: Box<IRStatement>,
        else_block: Option<Box<IRStatement>>,
    },

    Return {
        expr: Option<IRExpression>,
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
    PrefixOp {
        op: IRUnOpType,
        right: Box<IRExpression>,
        ty: MonoType,
    },
    BinOp {
        op: IRBinOpType,
        left: Box<IRExpression>,
        right: Box<IRExpression>,
        ty: MonoType,
    },
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

#[derive(PartialEq, Clone, Debug)]
pub enum IRBinOpType {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,
}

#[derive(PartialEq, Clone, Debug)]
pub enum IRUnOpType {
    Neg,
    Not,
}

const MAIN_FUNC_NAME: &str = "_start";

impl From<BinaryOpType> for IRBinOpType {
    fn from(value: BinaryOpType) -> Self {
        match value {
            BinaryOpType::Add => IRBinOpType::Add,
            BinaryOpType::Subtract => IRBinOpType::Sub,
            BinaryOpType::Multiply => IRBinOpType::Mul,
            BinaryOpType::Divide => IRBinOpType::Div,
            BinaryOpType::Equality => IRBinOpType::Eq,
            BinaryOpType::Inequality => IRBinOpType::Neq,
            BinaryOpType::LessThan => IRBinOpType::Lt,
            BinaryOpType::GreaterThan => IRBinOpType::Gt,
            BinaryOpType::LessThanOrEqual => IRBinOpType::Leq,
            BinaryOpType::GreaterThanOrEqual => IRBinOpType::Geq,
        }
    }
}

impl From<PrefixOpType> for IRUnOpType {
    fn from(value: PrefixOpType) -> Self {
        match value {
            PrefixOpType::Neg => IRUnOpType::Neg,
            PrefixOpType::Not => IRUnOpType::Not,
        }
    }
}

pub struct IRGeneration<'a> {
    semantics: &'a SemanticContext<'a>,
}

impl<'a> IRGeneration<'a> {
    pub fn new(semantics: &'a SemanticContext) -> Self {
        Self { semantics }
    }

    pub fn generate_ir(&self) -> IRProgram {
        let mut ir = IRProgram {
            functions: vec![],
            main_func: MAIN_FUNC_NAME.into(),
        };
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
                name: MAIN_FUNC_NAME.into(),
                params: vec![],
                return_type: MonoType::unit(),
                is_exported: true,
            };

            let mut main_locals = Vec::new();
            for stmt in self.semantics.program.main_stmts.iter() {
                self.get_locals(*stmt, &mut main_locals);
            }

            IRFunction {
                signature: main_ir_sig,
                body: IRStatement::Block {
                    statements: main_stmts,
                },
                locals: main_locals,
            }
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

        let signature = self.signature_for_func(&sym, &func.args, func.is_exported);
        let body = self.ir_for_statement(func.body);

        let mut locals = Vec::new();
        self.get_locals(func.body, &mut locals);

        IRFunction {
            signature,
            body,
            locals,
        }
    }

    fn signature_for_func(
        &self,
        sym: &Symbol,
        args: &Vec<FuncArg>,
        is_exported: bool,
    ) -> IRFunctionSignature {
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
            name: sym.name.clone(),
            params,
            return_type: *return_ty,
            is_exported,
        }
    }

    fn get_locals(&self, stmt: StmtId, locals: &mut Vec<IRLocal>) {
        match self.get_stmt(stmt) {
            Stmt::Let(let_decl) => {
                let sym = self
                    .semantics
                    .symbol_table
                    .symbol_for_ident(&let_decl.ident)
                    .unwrap();

                locals.push(IRLocal {
                    name: sym.unique_name(),
                    ty: sym.ty.clone().unwrap().extract_monotype().unwrap(),
                });
            }
            Stmt::BlockStmt(block) => {
                for stmt in block.statements.iter() {
                    self.get_locals(*stmt, locals);
                }
            }
            _ => {}
        };
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
            } => {
                let condition = self.ir_for_expr(condition);
                let then_block = self.ir_for_statement(then_block);
                let else_block = else_block.map(|b| Box::new(self.ir_for_statement(b)));

                IRStatement::If {
                    condition,
                    then_block: Box::new(then_block),
                    else_block,
                }
            }

            Stmt::Return(expr) => IRStatement::Return {
                expr: expr.map(|expr| self.ir_for_expr(expr)),
            },

            Stmt::BlockStmt(block) => {
                let stmts = block
                    .statements
                    .iter()
                    .map(|stmt| self.ir_for_statement(*stmt))
                    .collect();
                IRStatement::Block { statements: stmts }
            }

            Stmt::ExprStmt(stmt) => {
                let expr = self.ir_for_expr(stmt);
                IRStatement::Expr(expr)
            }

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
            Expr::PrefixOp { op, right } => {
                let op = op.prefix_type.into();
                let right = self.ir_for_expr(right);
                let ty = self.semantics.type_for_expr(&expr).unwrap();
                IRExpression::PrefixOp {
                    op,
                    right: Box::new(right),
                    ty,
                }
            }
            Expr::BinaryOp { op, left, right } => {
                let op = op.binary_type.into();
                let left = self.ir_for_expr(left);
                let right = self.ir_for_expr(right);
                let ty = self.semantics.type_for_expr(&expr).unwrap();

                IRExpression::BinOp {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                    ty,
                }
            }
            Expr::PostfixOp { op: _, left: _ } => todo!(),
            Expr::Call { callee: _, args: _ } => todo!(),
        }
    }

    fn get_stmt(&self, stmt: StmtId) -> Stmt {
        self.semantics.program.ast.statements[stmt].stmt.clone()
    }

    fn get_expr(&self, expr: ExprId) -> Expr {
        self.semantics.program.ast.expressions[expr].expr.clone()
    }
}

impl HasType for IRExpression {
    fn ty(&self) -> MonoType {
        match self {
            IRExpression::IntLiteral(_) => MonoType::int(),
            IRExpression::FloatLiteral(_) => MonoType::float(),
            IRExpression::StringLiteral(_) => MonoType::string(),
            IRExpression::BoolLiteral(_) => MonoType::bool(),
            IRExpression::PrefixOp { ty, .. } => ty.clone(),
            IRExpression::BinOp { ty, .. } => ty.clone(),
            IRExpression::Ident { ty, .. } => ty.clone(),
            IRExpression::Call { ty, .. } => ty.clone(),
        }
    }
}

impl Display for IRProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for func in self.functions.iter() {
            writeln!(f, "{}", func)?;
        }
        Ok(())
    }
}

impl Display for IRFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.signature.is_exported {
            write!(f, "export ")?;
        }
        write!(f, "fn {}", self.signature)?;
        writeln!(f, " {{")?;

        for local in self.locals.iter() {
            writeln!(f, "    local {}: {};", local.name, local.ty)?;
        }
        writeln!(f)?;

        write!(f, "{}", self.body)?;
        writeln!(f, "}}")
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
                    writeln!(f, "    {}", stmt)?;
                }

                Ok(())
            }
            IRStatement::If {
                condition,
                then_block,
                else_block,
            } => {
                writeln!(f, "if {} {{", condition)?;
                writeln!(f, "    {}", then_block)?;
                write!(f, "}}")?;

                if let Some(else_block) = else_block {
                    writeln!(f, " else {{")?;
                    writeln!(f, "    {}", else_block)?;
                    write!(f, "}}")?;
                }
                writeln!(f, "")
            }
            IRStatement::Return { expr } => {
                if let Some(expr) = expr {
                    write!(f, "return {}", expr)
                } else {
                    write!(f, "return")
                }
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
            IRExpression::StringLiteral(s) => write!(f, "\"{}\"", s),
            IRExpression::BoolLiteral(b) => write!(f, "{}", b),
            IRExpression::PrefixOp {
                op,
                right: expr,
                ty,
            } => write!(f, "({}{}):{}", op, expr, ty),
            IRExpression::BinOp {
                op,
                left: lhs,
                right: rhs,
                ty,
            } => write!(f, "({} {} {}):{}", lhs, op, rhs, ty),
            IRExpression::Ident { name, ty } => write!(f, "{}:{}", name, ty),
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

impl Display for IRUnOpType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IRUnOpType::Neg => write!(f, "-"),
            IRUnOpType::Not => write!(f, "!"),
        }
    }
}

impl Display for IRBinOpType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IRBinOpType::Add => write!(f, "+"),
            IRBinOpType::Sub => write!(f, "-"),
            IRBinOpType::Mul => write!(f, "*"),
            IRBinOpType::Div => write!(f, "/"),
            IRBinOpType::Mod => write!(f, "%"),
            IRBinOpType::Eq => write!(f, "=="),
            IRBinOpType::Neq => write!(f, "!="),
            IRBinOpType::Lt => write!(f, "<"),
            IRBinOpType::Gt => write!(f, ">"),
            IRBinOpType::Leq => write!(f, "<="),
            IRBinOpType::Geq => write!(f, ">="),
            IRBinOpType::And => write!(f, "&&"),
            IRBinOpType::Or => write!(f, "||"),
        }
    }
}
