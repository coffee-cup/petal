use clap::{Parser, Subcommand};
use petal::lexer::Lexer;

use crate::petal::errors::print_error;

#[macro_use]
extern crate lazy_static;

mod petal;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Compile a petal file
    Build {
        /// The petal file to compile
        file: String,
    },
}

fn main() {
    let args = Cli::parse();

    match args.command {
        Commands::Build { file } => {
            let file = std::fs::read_to_string(file).expect("Could not read file");

            let tokens = match Lexer::lex(&file) {
                Ok(tokens) => tokens,
                Err(e) => {
                    print_error(&file, &e);
                    std::process::exit(1);
                }
            };

            println!("{:?}", tokens);
        }
    }
}
