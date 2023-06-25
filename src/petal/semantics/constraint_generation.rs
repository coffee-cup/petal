use crate::petal::{
    ast::{Block, Expr, ExprId, PrefixOpType, Stmt, StmtId},
    source_info::Span,
    types::{FunctionAppType, MonoType},
};

use super::{
    context::SemanticContext,
    errors::{SemanticError, SemanticResult},
    typechecker::{Constraint, MonoTypeData},
};

impl<'a> SemanticContext<'a> {
    fn associate_types(&mut self, lhs: MonoTypeData, rhs: MonoTypeData) {
        self.type_constraints.push(Constraint::Equal { lhs, rhs });
    }

    pub fn stmt_constraints(&mut self, stmt_id: StmtId) -> SemanticResult<()> {
        let stmt_node = &self.program.ast.statements[stmt_id];

        match stmt_node.stmt.clone() {
            Stmt::Let(let_decl) => {
                let sym = self.symbol_table.symbol_for_ident(&let_decl.ident).unwrap();
                let var_ty = sym.ty.unwrap().instantiate(&mut self.ty_gen);

                let expr_ty = self.expr_constraints(let_decl.init)?;

                // The type of the variable must be equal to the type of the expression
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

                // The condition must be a boolean
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

    fn block_constraints(&mut self, block: &Block) -> SemanticResult<()> {
        for stmt in &block.statements {
            self.stmt_constraints(*stmt)?;
        }

        Ok(())
    }

    fn expr_constraints(&mut self, expr_id: ExprId) -> SemanticResult<MonoType> {
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
                        return Err(SemanticError::InvalidFunctionCall {
                            span: callee_expr.span.clone(),
                        })
                    }
                };

                let callee_sym = self.symbol_table.symbol_for_ident(&callee_ident).unwrap();
                let callee_ty = callee_sym.ty.unwrap().instantiate(&mut self.ty_gen);

                let fun_ty = match callee_ty {
                    MonoType::FunApp(fun) => fun,
                    _ => {
                        return Err(SemanticError::InvalidFunctionCall {
                            span: callee_expr.span.clone(),
                        })
                    }
                };

                let arg_span = if !args.is_empty() {
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
                    return Err(SemanticError::IncorrectNumberOfArguments {
                        expected: fun_ty.params.len(),
                        found: args.len(),
                        func_decl: callee_sym.decl_source,
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

            Expr::PostfixOp { op: _, left: _ } => todo!(),
        }
    }
}
