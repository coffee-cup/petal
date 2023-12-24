use crate::petal::{
    ast::{Expr, ExprId, FuncDecl, Stmt, StmtId, TypeAnnotation},
    types::{FunctionAppType, MonoType, PolyType},
};

use super::{
    context::SemanticContext,
    errors::{SemanticError, SemanticResult},
};

impl<'a> SemanticContext<'a> {
    pub fn generate_symbols_for_program(&mut self) -> SemanticResult<()> {
        // Add all functions to symbol table
        for func in self.program.functions.iter() {
            let fun_name = self.program.get_ident_name(func.ident);

            if let Some(sym) = self.symbol_table.get(&fun_name) {
                return Err(SemanticError::FunctionAlreadyDeclared {
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
                    return Err(SemanticError::ArgumentAlreadyDefined {
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

    fn generate_symbols_for_statement(&mut self, stmt_id: StmtId) -> SemanticResult<()> {
        let stmt_node = &self.program.ast.statements[stmt_id];

        match &stmt_node.stmt.clone() {
            Stmt::Let(let_decl) => {
                let ident = &self.program.ast.identifiers[let_decl.ident];

                if let Some(sym) = self.symbol_table.get_in_current_scope(&ident.name) {
                    return Err(SemanticError::VariableAlreadyDeclared {
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

            Stmt::Return(return_expr) => {
                if let Some(return_expr) = return_expr {
                    self.generate_symbols_for_expression(*return_expr)?;
                }
            }

            Stmt::Comment(_) => {}
        };

        Ok(())
    }

    fn generate_symbols_for_expression(&mut self, expr_id: ExprId) -> SemanticResult<()> {
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
                    return Err(SemanticError::UndeclaredVariable {
                        name: ident.name.clone(),
                        span: ident.span.clone(),
                    });
                }
            }

            Expr::PrefixOp { right, .. } => {
                self.generate_symbols_for_expression(*right)?;
            }

            Expr::BinaryOp { left, right, .. } => {
                self.generate_symbols_for_expression(*left)?;
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
    fn get_type_of_function_decl(&self, func: &FuncDecl) -> SemanticResult<PolyType> {
        // Get the type of the arguments
        let mut param_tys = Vec::new();
        for param in &func.args {
            let ty = self.type_for_annotation(&param.ty)?;
            param_tys.push(ty);
        }

        // Get the return type
        let return_ty = match &func.return_ty {
            Some(annotation) => self.type_for_annotation(annotation)?,
            None => MonoType::unit(),
        };

        let func_ty = FunctionAppType {
            params: param_tys,
            return_ty: Box::new(return_ty),
        };

        Ok(PolyType::Mono(MonoType::FunApp(func_ty)))
    }

    // Get the type of a type annotation by looking up the identifier in the type_symbols table
    pub fn type_for_annotation(&self, annotation: &TypeAnnotation) -> SemanticResult<MonoType> {
        self.type_symbols
            .get(&annotation.name)
            .and_then(|sym| sym.ty)
            .map_or_else(
                || {
                    Err(SemanticError::UndefinedType {
                        name: annotation.name.clone(),
                        span: annotation.span.clone(),
                    })
                },
                |ty| match ty {
                    PolyType::Mono(ty) => Ok(ty),
                    PolyType::Quantifier(_) => Err(SemanticError::InvalidTypeAnnotation {
                        name: annotation.name.clone(),
                        span: annotation.span.clone(),
                    }),
                },
            )
    }
}
