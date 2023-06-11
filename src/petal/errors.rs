use std::{io};



use super::{parser::ParserError, source_info::Span};
use miette::Diagnostic;
use thiserror::Error;



#[derive(Clone, Debug)]
pub struct ErrorContext {
    msg: String,
    span: Span,
}

#[derive(Clone, Debug)]
pub struct CompilerCodeError {
    context: Vec<ErrorContext>,
    error_msg: String,
    span: Span,
}

#[derive(Diagnostic, Error, Debug)]
pub enum CompilerError {
    #[error("Unknown compiler error")]
    Unknown,

    #[error("File read error: {0}")]
    FileReadError(#[from] io::Error),

    #[error("Parser error")]
    ParserError(ParserError),

    // #[error("Analysis error: {}", .0.kind)]
    // AnalysisError(AnalysisError),
    #[error("Failed to generate WASM binary: {0}\n\nThe generated wat...\n{1}")]
    WasmGenerationError(String, String),

    #[error("Failed to validate WASM binary: {0}\n\nThe generated wat...\n{1}")]
    WasmValidationError(String, String),
}

pub fn print_compiler_error(source: &str, error: CompilerError) {
    println!("ERROR: {:?}\n\n", error);

    match error {
        CompilerError::ParserError(ParserError::LexerError(e)) => {
            let report = miette::Report::from(e).with_source_code(source.to_string());
            eprintln!("{:?}", report);
        }
        CompilerError::ParserError(e) => {
            let report = miette::Report::from(e).with_source_code(source.to_string());
            eprintln!("{:?}", report);
        }
        _ => eprintln!("{}", error),
    }
}
