use crate::petal::wat::IRGenerator;

use self::{errors::CompilerError, lexer::Lexer, wasm::Wasm};

mod ast;
mod codegen;
pub mod errors;
mod lexer;
mod parser;
mod positions;
mod precedence;
mod token;
mod wasm;
mod wat;

type CompilerResult<T> = Result<T, CompilerError>;

pub struct Compiler {}

impl Compiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile_file(&self, file: &str) -> CompilerResult<Wasm> {
        let file = std::fs::read_to_string(file).expect("Could not read file");

        let mut lexer1 = Lexer::new(&file);
        let tokens = lexer1.map(|t| t.unwrap()).collect::<Vec<_>>();
        println!("Tokens: {:?}", tokens);

        let mut lexer = Lexer::new(&file);
        let mut parser = parser::Parser::new(&mut lexer);
        let program = parser.parse().map_err(CompilerError::ParserError)?;

        println!("{:#?}", program);

        let mut ir_generator = IRGenerator::new();
        let ir_module = ir_generator.generate_ir_from_program(&program);

        // println!("{:#?}", ir_module);

        let wasm = Wasm::new(&ir_module)
            .map_err(|(e, wat_string)| CompilerError::WasmGenerationError(e, wat_string))?;

        wasm.validate()
            .map_err(|e| CompilerError::WasmValidationError(e.to_string(), wasm.print_wat()))?;

        Ok(wasm)
    }
}
