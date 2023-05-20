use std::{cmp::min, io};

use super::parser::ParserError;
use thiserror::Error;

use colored::Colorize;

#[derive(Error, Debug)]
pub enum CompilerError {
    #[error("File read error: {0}")]
    FileReadError(#[from] io::Error),

    #[error("Parser error: {}", .0.kind)]
    ParserError(ParserError),

    #[error("Failed to generate WASM binary: {0}\n\nThe generated wat...\n{1}")]
    WasmGenerationError(String, String),

    #[error("Failed to validate WASM binary: {0}\n\nThe generated wat...\n{1}")]
    WasmValidationError(String, String),
}

pub fn print_parser_error(source: &str, error: &ParserError) {
    if let Some(span) = &error.span {
        let msg = error.kind.to_string();
        let source_lines = source.lines();
        let source_content = source_lines.collect::<Vec<_>>();
        let line_idx = min(span.start.line, source_content.len() - 1);
        let line = source_content[line_idx];
        let line_number = format!("{} | ", span.start.line);
        let line_number_len = line_number.len();

        println!("Span: {:?}", span);

        let error_len = msg.len();

        println!("\n{}{}", line_number.purple(), line);

        println!(
            "{}{}{}",
            " ".repeat(line_number_len),
            " ".repeat(span.start.col - 1),
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
