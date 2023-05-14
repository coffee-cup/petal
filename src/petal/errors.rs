use std::io;

use super::parser::ParserError;
use thiserror::Error;

use colored::Colorize;

// pub trait CompilerError {
//     fn span(&self) -> Option<Span>;
//     fn msg(&self) -> String;
// }

// struct ErrorWithSpan {

// }

#[derive(Error, Debug)]
pub enum CompilerError {
    #[error("File read error: {0}")]
    FileReadError(#[from] io::Error),

    #[error("Parser error")]
    ParserError(ParserError),

    #[error("Failed to generate WASM binary: {0}\n\nThe generated wat...\n{1}")]
    WasmGenerationError(String, String),

    #[error("Failed to validate WASM binary: {0}\n\nThe generated wat...\n{1}")]
    WasmValidationError(String, String),
}

// pub struct CompilerError {
//     msg: String,
//     span: Option<Span>,
// }

// impl CompilerError {
//     pub fn new(msg: String, span: Option<Span>) -> Self {
//         Self { msg, span }
//     }
// }

pub fn print_error(source: &str, error: &ParserError) {
    if let Some(span) = &error.span {
        let msg = error.kind.to_string();
        let line = source.lines().nth(span.start.line).unwrap();
        let line_number = format!("{} | ", span.start.line);
        let line_number_len = line_number.len();

        let error_len = msg.len();

        println!("\n{}{}", line_number.dimmed(), line);

        println!(
            "{}{}{}",
            " ".repeat(line_number_len),
            " ".repeat(span.start.col),
            "^".red()
        );

        let error_msg_pos: i32 = std::cmp::max(
            0,
            (line_number_len as i32) + (span.start.col as i32) - ((error_len as i32) / 2),
        );
        println!(
            "{} {}",
            " ".repeat(error_msg_pos as usize),
            msg.red().bold()
        );
    } else {
        // No span information, just print the error
        println!("{}", error.kind.to_string().red().bold());
    }
}
