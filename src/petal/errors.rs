use super::lexer::LexerError;

use colored::Colorize;

pub fn print_error(source: &str, error: &LexerError) {
    let line = source.lines().nth(error.pos.line).unwrap();
    let line_number = format!("{} | ", error.pos.line);
    let line_number_len = line_number.len();
    let error_len = error.msg.len();

    println!("\n{}{}", line_number.dimmed(), line);

    println!(
        "{}{}{}",
        " ".repeat(line_number_len),
        " ".repeat(error.pos.col),
        "^".red()
    );

    println!(
        "{} {}",
        " ".repeat(line_number_len + error.pos.col - (error_len / 2)),
        error.msg.red().bold()
    );
}
