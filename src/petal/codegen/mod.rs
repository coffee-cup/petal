use self::context::CodegenContext;

use super::{
    ir::{IRExpression, IRProgram},
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

        let module = WatModule {
            functions: wat_funcs,
            main_func: self.ir.main_func.clone(),
        };

        module
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
            IRStatement::Let { name, ty, init } => {
                instrs.push(self.visit_expression(init));
                instrs.push(WatInstruction::SetLocal(name.clone()));
            }

            IRStatement::If { condition, then } => todo!(),

            IRStatement::Block { statements } => {
                for stmt in statements {
                    self.visit_statement(stmt, instrs);
                }
            }

            IRStatement::Expr(x) => instrs.push(self.visit_expression(x)),
        };
    }

    fn visit_expression(&self, expr: &IRExpression) -> WatInstruction {
        match expr {
            IRExpression::IntLiteral(n) => WatInstruction::Const(WatValue::I64(*n)),
            IRExpression::FloatLiteral(n) => WatInstruction::Const(WatValue::F64(*n)),
            IRExpression::BoolLiteral(b) => WatInstruction::Const(WatValue::I32(*b as i32)),
            IRExpression::StringLiteral(_) => todo!(),
            IRExpression::PrefixOp { op, right, ty } => todo!(),
            IRExpression::BinOp {
                op,
                left,
                right,
                ty,
            } => todo!(),
            IRExpression::Ident { name, ty } => todo!(),
            IRExpression::Call { name, args, ty } => todo!(),
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
            MonoType::FunApp(_) => todo!(),
        }
    }
}
