use crate::petal::ir::IRProgram;

pub struct CodegenContext<'a> {
    pub ir: &'a IRProgram,
}
