use self::context::CodegenContext;

use super::{ir::IRFunctionSignature, wat::*};
use super::{ir::IRProgram, types::MonoType};

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

            let func = WatFunction {
                signature: self.get_wat_signature(&func.signature),
                locals,
                instructions: Vec::new(),
            };

            wat_funcs.push(func);
        }

        let module = WatModule {
            functions: wat_funcs,
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
