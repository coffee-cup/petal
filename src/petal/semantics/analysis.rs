use crate::petal::{
    ast::{ExprId, FuncDecl, StmtId},
    semantics::errors::SemanticError,
};

use super::{context::SemanticContext, errors::SemanticResult};

impl<'a> SemanticContext<'a> {
    pub fn analysis(&self) -> SemanticResult<()> {
        println!("\n--- Analysing program\n");

        for func in self.program.functions.iter() {
            self.analysis_function(func)?;
        }

        for stmt in self.program.main_stmts.iter() {
            self.analysis_statement(*stmt)?;
        }

        Ok(())
    }

    fn analysis_function(&self, func: &FuncDecl) -> SemanticResult<()> {
        self.analysis_statement(func.body)?;

        Ok(())
    }

    fn analysis_statement(&self, stmt: StmtId) -> SemanticResult<()> {
        use crate::petal::ast::Stmt;

        let stmt_node = self.program.ast.statements[stmt].clone();
        match stmt_node.stmt {
            Stmt::Let(let_decl) => {
                self.analysis_expression(let_decl.init)?;
            }
            Stmt::IfStmt {
                condition,
                then_block,
                else_block,
            } => {
                self.analysis_expression(condition)?;
                self.analysis_statement(then_block)?;
                if let Some(else_block) = else_block {
                    self.analysis_statement(else_block)?;
                }
            }

            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    self.analysis_expression(expr)?;
                }
            }

            Stmt::BlockStmt(block) => {
                for stmt in block.statements.iter() {
                    self.analysis_statement(*stmt)?;
                }
            }
            Stmt::ExprStmt(e) => self.analysis_expression(e)?,
            Stmt::Comment(_) => {}
        }

        Ok(())
    }

    fn analysis_expression(&self, expr: ExprId) -> SemanticResult<()> {
        use crate::petal::ast::Expr;
        use crate::petal::types::MonoType;

        let expr_node = self.program.ast.expressions[expr].clone();

        match expr_node.expr {
            Expr::Integer(_) => {}
            Expr::Float(_) => {}
            Expr::String(_) => {}
            Expr::Bool(_) => {}
            Expr::Ident(_) => {}
            Expr::PrefixOp { op, right } => todo!(),
            Expr::BinaryOp { op, left, right } => {
                let left_ty = self.type_for_expr(&left).unwrap();
                let right_ty = self.type_for_expr(&right).unwrap();

                if left_ty != MonoType::int() && left_ty != MonoType::float() {
                    let left_expr = self.program.ast.expressions[left].clone();
                    return Err(SemanticError::InvalidBinaryExpressionTypes {
                        ty: left_ty,
                        op: op.binary_type,
                        span: left_expr.span,
                    });
                }

                self.analysis_expression(left)?;
                self.analysis_expression(right)?;
            }
            Expr::PostfixOp { op, left } => todo!(),
            Expr::Call { callee, args } => todo!(),
        }

        Ok(())
    }
}
