use rustyline::{error::ReadlineError, DefaultEditor, Editor};

use super::scanner::Scanner;

static FAIRWELL: &str = "Fairwell :D";
static HISTORY: &str = "history.txt";

pub struct Repl {}

impl Repl {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run(&mut self) {
        let mut rl = DefaultEditor::new().unwrap();
        rl.load_history(HISTORY).unwrap_or(());

        loop {
            let readline = rl.readline(">> ");
            match readline {
                Ok(line) => {
                    rl.add_history_entry(line.as_str()).unwrap();
                    match self.exec_line(line) {
                        Ok(()) => {}
                        Err(e) => println!("{}", e),
                    }
                }
                Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => {
                    println!("{}", FAIRWELL);
                    break;
                }
                Err(err) => {
                    println!("Error: {:?}", err);
                    break;
                }
            }
        }
    }

    fn exec_line(&mut self, line: String) -> Result<(), String> {
        let tokens = Scanner::lex(&line).map_err(|errors| {
            errors
                .iter()
                .map(|e| format!("{}", e))
                .collect::<Vec<_>>()
                .join(",")
        })?;

        println!("{:?}", tokens);

        Ok(())
    }
}
