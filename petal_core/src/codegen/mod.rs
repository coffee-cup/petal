use crate::types::{FunctionAppType, HasType};

use self::context::CodegenContext;

use super::{
    ir::{IRBinOpType, IRExpression, IRProgram},
    types::MonoType,
};
use super::{
    ir::{IRFunctionSignature, IRStatement},
    wat::*,
};

pub mod context;

impl<'a> CodegenContext<'a> {
    pub fn new(ir: &'a IRProgram) -> Self {
        Self { ir }
    }

    pub fn generate_wat(&self) -> WatModule {
        let mut wat_funcs = Vec::new();

        for func in self.ir.functions.iter() {
            let locals = func
                .locals
                .iter()
                .map(|local| WatLocal {
                    name: local.name.clone(),
                    ty: self.type_for_monotype(&local.ty),
                })
                .collect::<Vec<_>>();

            let mut instructions = Vec::new();
            self.visit_statement(&func.body, &mut instructions);

            let func = WatFunction {
                signature: self.get_wat_signature(&func.signature),
                locals,
                instructions,
            };

            wat_funcs.push(func);
        }

        WatModule {
            functions: wat_funcs,
            main_func: self.ir.main_func.clone(),
        }
    }

    fn get_wat_signature(&self, ir_sig: &IRFunctionSignature) -> WatFunctionSignature {
        let return_ty = if ir_sig.return_type.is_unit() {
            None
        } else {
            Some(self.type_for_monotype(&ir_sig.return_type))
        };

        let params = ir_sig
            .params
            .iter()
            .map(|param| WatParam {
                name: param.name.clone(),
                ty: self.type_for_monotype(&param.ty),
            })
            .collect::<Vec<_>>();

        WatFunctionSignature {
            name: ir_sig.name.clone(),
            is_exported: ir_sig.is_exported,
            return_ty,
            params,
        }
    }

    fn visit_statement(&self, stmt: &IRStatement, instrs: &mut Vec<WatInstruction>) {
        match stmt {
            IRStatement::Let { name, ty: _, init } => {
                self.visit_expression(init, instrs);
                instrs.push(WatInstruction::SetLocal(name.clone()));
            }

            IRStatement::If {
                condition,
                then_block,
                else_block,
            } => {
                // Generate instructions for the if condition
                self.visit_expression(condition, instrs);

                // Generate instructions for the then and else blocks
                let mut then_instrs = Vec::new();
                self.visit_statement(then_block, &mut then_instrs);

                let mut else_instrs = Vec::new();
                if let Some(else_block) = else_block {
                    self.visit_statement(else_block, &mut else_instrs);
                }

                // Push the if instruction
                instrs.push(WatInstruction::If(then_instrs, else_instrs));
            }

            IRStatement::While {
                condition,
                body,
                uid,
            } => {
                let block_label = format!("o#{uid}");
                let loop_label = format!("i#{uid}");

                // Generate instructions for the while condition
                let mut body_instrs = Vec::new();
                self.visit_expression(condition, &mut body_instrs);

                // Branching instruction to exit the loop if the condition is true
                body_instrs.append(&mut vec![
                    // Invert the condition so that we jump out of the loop if the condition is false
                    WatInstruction::Const(WatValue::I32(0)),
                    WatInstruction::Equal(WatValueType::I32),
                    // Do the branch if the condition is false
                    WatInstruction::BrIf(block_label.clone()),
                ]);

                // Generate instructions for the loop body
                self.visit_statement(body, &mut body_instrs);

                // Loop back to the top of the inner loop
                body_instrs.push(WatInstruction::Br(loop_label.clone()));

                // Push the top level block instruction
                // This block is used to jump out of the loop if the condition is false
                instrs.push(WatInstruction::Block(
                    block_label,
                    vec![WatInstruction::Loop(loop_label, body_instrs)],
                ));
            }

            IRStatement::Return { expr } => {
                if let Some(expr) = expr {
                    self.visit_expression(expr, instrs);
                }
                instrs.push(WatInstruction::Return);
            }

            IRStatement::Block { statements } => {
                for stmt in statements {
                    self.visit_statement(stmt, instrs);
                }
            }

            IRStatement::Expr(x) => {
                self.visit_expression(x, instrs);
                instrs.push(WatInstruction::Drop)
            }

            _ => todo!(),
        };
    }

    fn visit_expression(&self, expr: &IRExpression, instrs: &mut Vec<WatInstruction>) {
        match expr {
            IRExpression::IntLiteral(n) => instrs.push(WatInstruction::Const(WatValue::I64(*n))),
            IRExpression::FloatLiteral(n) => instrs.push(WatInstruction::Const(WatValue::F64(*n))),
            IRExpression::BoolLiteral(b) => {
                instrs.push(WatInstruction::Const(WatValue::I32(*b as i32)))
            }
            IRExpression::StringLiteral(_) => todo!(),

            IRExpression::PrefixOp {
                op: _,
                right: _,
                ty: _,
            } => todo!(),

            IRExpression::BinOp {
                op,
                left,
                right,
                ty,
            } => {
                self.visit_expression(left, instrs);
                self.visit_expression(right, instrs);

                match op {
                    IRBinOpType::Add => {
                        instrs.push(WatInstruction::Add(self.type_for_monotype(ty)))
                    }
                    IRBinOpType::Sub => {
                        instrs.push(WatInstruction::Sub(self.type_for_monotype(ty)))
                    }
                    IRBinOpType::Mul => {
                        instrs.push(WatInstruction::Mult(self.type_for_monotype(ty)))
                    }
                    IRBinOpType::Div => {
                        instrs.push(WatInstruction::Div(self.type_for_monotype(ty)))
                    }
                    IRBinOpType::Mod => {
                        todo!()
                        // instrs.push(WatInstruction::Rem(self.type_for_monotype(ty)))
                    }
                    IRBinOpType::And => todo!(),
                    IRBinOpType::Or => todo!(),
                    IRBinOpType::Eq => {
                        instrs.push(WatInstruction::Equal(self.type_for_monotype(&left.ty())))
                    }
                    IRBinOpType::Neq => {
                        instrs.push(WatInstruction::NotEqual(self.type_for_monotype(&left.ty())))
                    }
                    IRBinOpType::Lt => {
                        instrs.push(WatInstruction::LessThan(self.type_for_monotype(&left.ty())))
                    }
                    IRBinOpType::Gt => instrs.push(WatInstruction::GreaterThan(
                        self.type_for_monotype(&left.ty()),
                    )),
                    IRBinOpType::Leq => instrs.push(WatInstruction::LessOrEqual(
                        self.type_for_monotype(&left.ty()),
                    )),
                    IRBinOpType::Geq => instrs.push(WatInstruction::GreaterOrEqual(
                        self.type_for_monotype(&left.ty()),
                    )),
                }
            }
            IRExpression::Ident { name, ty: _ } => {
                instrs.push(WatInstruction::GetLocal(name.clone()));
            }

            IRExpression::Assign { name, expr, .. } => {
                self.visit_expression(expr, instrs);
                instrs.push(WatInstruction::SetLocal(name.clone()));

                // We need to push the value back onto the stack so that it can be used in expressions
                instrs.push(WatInstruction::GetLocal(name.clone()));
            }

            IRExpression::Call { name, args, .. } => {
                for arg in args {
                    self.visit_expression(arg, instrs);
                }

                instrs.push(WatInstruction::Call(name.clone()));
            }
        }
    }

    fn type_for_monotype(&self, ty: &MonoType) -> WatValueType {
        match ty {
            MonoType::Variable(_) => unreachable!(),
            MonoType::Struct(struct_decl) => match struct_decl.name.as_str() {
                "Int" => WatValueType::I64,
                "Float" => WatValueType::F64,
                "Bool" => WatValueType::I32,
                _ => todo!(),
            },
            MonoType::FunApp(_) => unreachable!(),
        }
    }
}
