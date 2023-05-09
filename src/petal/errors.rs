use super::{lexer::LexerError, positions::Span};

use colored::Colorize;

pub trait CompilerError {
    fn span(&self) -> Option<Span>;
    fn msg(&self) -> String;
}

pub fn print_error(source: &str, error: &dyn CompilerError) {
    if let Some(span) = error.span() {
        let msg = error.msg();
        let line = source.lines().nth(span.start.line).unwrap();
        let line_number = format!("{} | ", span.start.line);
        let line_number_len = line_number.len();

        let error_len = error.msg().len();

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
            error.msg().red().bold()
        );
    } else {
        // No span information, just print the error
        println!("{}", error.msg().red().bold());
    }
}
