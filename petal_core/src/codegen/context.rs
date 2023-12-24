use crate::ir::IRProgram;

pub struct CodegenContext<'a> {
    pub ir: &'a IRProgram,
}
