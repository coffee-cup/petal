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

    let error_msg_pos: i32 = std::cmp::max(
        0,
        (line_number_len as i32) + (error.pos.col as i32) - ((error_len as i32) / 2),
    );
    println!(
        "{} {}",
        " ".repeat(error_msg_pos as usize),
        error.msg.red().bold()
    );
}
