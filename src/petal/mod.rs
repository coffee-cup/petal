// use crate::petal::analysis::Analysis;

use self::{analysis::AnalysisContext, errors::CompilerError, lexer::Lexer};

mod analysis;
mod ast;
// mod codegen;
pub mod errors;
mod lexer;
mod parser;
mod precedence;
mod source_info;
mod token;
mod typechecker;
mod types;
// mod wasm;
// mod wat;

type CompilerResult<T> = Result<T, CompilerError>;

pub struct Compiler {}

impl Compiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile_file(&self, filename: &str) -> CompilerResult<()> {
        let file = std::fs::read_to_string(filename).expect("Could not read file");

        // let f2 = file.clone();
        // let lexer1 = Lexer::new(&f2);
        // match lexer1.collect::<Result<Vec<_>, _>>() {
        //     Ok(tokens) => {
        //         println!("Tokens: {:#?}", tokens);
        //     }
        //     Err(e) => {
        //         let report = miette::Report::from(e).with_source_code(f2);
        //         println!("{:?}", report);
        //     }
        // };

        // println!("Tokens: {:#?}", tokens);

        let mut lexer = Lexer::new(&file);
        let mut parser = parser::Parser::new(&mut lexer).map_err(CompilerError::ParserError)?;

        let mut program = parser.parse().map_err(CompilerError::ParserError)?;

        // println!("{:#?}", program);

        let mut analysis = AnalysisContext::new(&mut program);
        analysis
            .analysis_program()
            .map_err(CompilerError::AnalysisError)?;

        // let mut typechecker = Typechecker::new(&program);
        // typechecker.check().map_err(CompilerError::TypecheckError)?;

        Err(CompilerError::Unknown)

        // let mut ir_generator = IRGenerator::new();
        // let ir_module = ir_generator.generate_ir_from_program(&program);

        // println!("{:#?}", ir_module);

        // let wasm = Wasm::new(&ir_module)
        //     .map_err(|(e, wat_string)| CompilerError::WasmGenerationError(e, wat_string))?;

        // wasm.validate()
        //     .map_err(|e| CompilerError::WasmValidationError(e.to_string(), wasm.print_wat()))?;

        // Ok(wasm)
    }
}
