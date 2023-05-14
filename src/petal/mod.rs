use crate::petal::ir::IRGenerator;

use self::{errors::CompilerError, lexer::Lexer, wasm::Wasm};

mod ast;
mod codegen;
mod errors;
mod ir;
mod lexer;
mod parser;
mod positions;
mod precedence;
mod runner;
mod token;
mod wasm;

type CompilerResult<T> = Result<T, CompilerError>;

pub struct Compiler {}

impl Compiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile_file(&self, file: &str) -> CompilerResult<Wasm> {
        let file = std::fs::read_to_string(file).expect("Could not read file");

        let mut lexer = Lexer::new(&file);
        let mut parser = parser::Parser::new(&mut lexer);
        let program = parser.parse().map_err(CompilerError::ParserError)?;

        let mut ir_generator = IRGenerator::new();
        let ir_module = ir_generator.generate_ir_from_program(&program);

        let wasm = Wasm::new(&ir_module)
            .map_err(|(e, wat_string)| CompilerError::WasmGenerationError(e, wat_string))?;

        Ok(wasm)
    }
}
