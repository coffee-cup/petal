use std::{cmp::min, io};

use crate::petal::lexer::LexerErrorKind;

use super::{parser::ParserErrorKind, source_info::Span, typechecker::TypecheckingError};
use miette::Diagnostic;
use thiserror::Error;

use colored::Colorize;

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
    ParserError(ParserErrorKind),

    // #[error("Analysis error: {}", .0.kind)]
    // AnalysisError(AnalysisError),
    #[error("Failed to generate WASM binary: {0}\n\nThe generated wat...\n{1}")]
    WasmGenerationError(String, String),

    #[error("Failed to validate WASM binary: {0}\n\nThe generated wat...\n{1}")]
    WasmValidationError(String, String),
}

pub fn print_compiler_error(source: &str, error: CompilerError) {
    use CompilerError::*;

    println!("ERROR: {:?}", error);

    match error {
        ParserError(ParserErrorKind::LexerError(e)) => {
            let report = miette::Report::from(e).with_source_code(source.to_string());
            println!("{:?}", report);
        }
        ParserError(e) => {
            let report = miette::Report::from(e).with_source_code(source.to_string());
            println!("{:?}", report);
        }
        _ => eprintln!("{}", error),
    }

    // if let Some(span) = &span {
    //     let source_lines = source.lines();
    //     let source_content = source_lines.collect::<Vec<_>>();
    //     let line_idx = min(span.start.line, source_content.len() - 1);
    //     let line = source_content[line_idx];
    //     let line_number = format!("{} | ", span.start.line);
    //     let line_number_len = line_number.len();

    //     let error_len = msg.len();

    //     println!("\n{}{}", line_number.purple(), line);
    //     println!(
    //         "{}{}{}",
    //         " ".repeat(line_number_len),
    //         " ".repeat(span.start.col - 1),
    //         "^".repeat(span.end.col + 1 - span.start.col).red()
    //     );

    //     let error_msg_pos: i32 = std::cmp::max(
    //         0,
    //         (line_number_len as i32) + (span.start.col as i32) - ((error_len as i32) / 2),
    //     );
    //     println!(
    //         "{} {}",
    //         " ".repeat(error_msg_pos as usize),
    //         msg.red().bold()
    //     );
    // } else {
    //     // No span information, just print the error
    //     println!("{}", msg.to_string().red().bold());
    // }
}
