use crate::{
    ast::{BinaryOpType, Block, Expr, ExprId, FuncDecl, PrefixOpType, Stmt, StmtId},
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

    pub fn stmt_constraints(
        &mut self,
        stmt_id: StmtId,
        curr_func: &Option<FuncDecl>,
    ) -> SemanticResult<()> {
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
                for stmt in &block.statements {
                    self.stmt_constraints(*stmt, curr_func)?;
                }
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

                self.stmt_constraints(then_block, curr_func)?;
                if let Some(else_block) = else_block {
                    self.stmt_constraints(else_block, curr_func)?;
                }
            }

            Stmt::Return(expr) => {
                let stmt_span = stmt_node.span.clone();

                match &expr {
                    Some(expr) => {
                        let expr_ty = self.expr_constraints(*expr)?;

                        match &curr_func.clone() {
                            Some(func) => {
                                let return_ty_data = if let Some(return_ty) = &func.return_ty {
                                    MonoTypeData::new(self.type_for_annotation(&return_ty).unwrap())
                                        .with_span(return_ty.span.clone())
                                } else {
                                    MonoTypeData::new(MonoType::unit()).with_ident(func.ident)
                                };

                                self.associate_types(
                                    MonoTypeData::new(expr_ty).with_parent_stmt(stmt_id),
                                    return_ty_data,
                                );
                            }
                            None => {
                                return Err(SemanticError::ReturnOutsideOfFunction {
                                    span: stmt_span,
                                });
                            }
                        }
                    }
                    None => {}
                }
            }

            Stmt::ExprStmt(expr) => {
                self.expr_constraints(expr)?;
            }

            Stmt::Comment(_) => {}
        };

        Ok(())
    }

    fn expr_constraints(&mut self, expr_id: ExprId) -> SemanticResult<MonoType> {
        let expr_node = &self.program.ast.expressions[expr_id];

        let ty = match &expr_node.expr.clone() {
            Expr::Integer(_) => MonoType::int(),
            Expr::Float(_) => MonoType::float(),
            Expr::String(_) => MonoType::string(),
            Expr::Bool(_) => MonoType::bool(),
            Expr::Ident(ident) => {
                let i = &self.program.ast.identifiers[*ident];
                let sym = self.symbol_table.symbol_for_ident(ident).unwrap();
                let ty = sym.ty.unwrap();

                let instantiated = ty.instantiate(&mut self.ty_gen);

                instantiated
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

                return_ty
            }

            Expr::PrefixOp { op, right } => match op.prefix_type {
                PrefixOpType::Not => {
                    let right_ty = self.expr_constraints(*right)?;
                    self.associate_types(
                        MonoTypeData::new(right_ty.clone()).with_expr(*right),
                        MonoType::bool().into(),
                    );

                    right_ty
                }
                PrefixOpType::Neg => {
                    let right_ty = self.expr_constraints(*right)?;
                    let return_ty = self.ty_gen.gen_var();

                    self.associate_types(
                        MonoTypeData::new(right_ty).with_expr(*right),
                        MonoTypeData::new(return_ty.clone()),
                    );

                    return_ty
                }
            },

            Expr::BinaryOp { left, right, op } => {
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

                return_ty
            }

            Expr::PostfixOp { op: _, left: _ } => todo!(),
        };

        self.expr_types.insert(expr_id, ty.clone());

        Ok(ty)
    }
}
