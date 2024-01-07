use log::{debug, log_enabled, Level};

use self::{errors::CompilerError, lexer::Lexer, semantics::context::SemanticContext, wasm::Wasm};

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate num_derive;

mod ast;
mod codegen;
pub mod errors;
mod ir;
mod lexer;
mod parser;
mod precedence;
mod semantics;
mod source_info;
mod token;
mod types;
pub mod wasm;
mod wat;

type CompilerResult<T> = Result<T, CompilerError>;

pub struct Compiler {}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Compiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile_file(&self, filename: &str) -> CompilerResult<Wasm> {
        let file = std::fs::read_to_string(filename).expect("Could not read file");
        self.compile_string(file)
    }

    pub fn compile_string(&self, file: String) -> CompilerResult<Wasm> {
        let mut lexer = Lexer::new(&file);
        let mut parser = parser::Parser::new(&mut lexer).map_err(CompilerError::ParserError)?;
        let mut program = parser.parse().map_err(CompilerError::ParserError)?;

        let mut semantics = SemanticContext::new(&mut program);
        semantics
            .analysis_program()
            .map_err(CompilerError::SemanticError)?;

        let ir_generator = ir::IRGeneration::new(&semantics);
        let ir = ir_generator.generate_ir();

        if log_enabled!(Level::Debug) {
            let mut ir_printer = ir::pretty_print::IRPrettyPrinter::new();
            debug!("--- IR\n{}", ir_printer.print_program(&ir));
        }

        let wat = codegen::context::CodegenContext::new(&ir).generate_wat();

        // debug!("--- WAT\n{}", wat);

        let wasm = Wasm::new(&wat).map_err(|(e, wat_string)| {
            CompilerError::WasmGenerationError(e.to_string(), wat_string)
        })?;

        // Validate the generated WASM to ensure it is valid
        wasm.validate()
            .map_err(|e| CompilerError::WasmValidationError(e.to_string(), wasm.print_wat()))?;

        Ok(wasm)
    }
}
