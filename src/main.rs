use clap::{Parser, Subcommand};
use petal::lexer::Lexer;

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
            println!("File: {}", file);

            let tokens = match Lexer::lex(&file) {
                Ok(tokens) => tokens,
                Err(e) => {
                    eprintln!("{}", e);
                    std::process::exit(1);
                }
            };

            println!("{:?}", tokens);

            // let tokens = match Lexer::lex(&file) {
            //     Ok(tokens) => tokens,
            //     Err(errors) => {
            //         eprintln!(
            //             "{}",
            //             errors
            //                 .iter()
            //                 .map(|e| e.to_string())
            //                 .collect::<Vec<_>>()
            //                 .join("\n")
            //         );
            //         std::process::exit(1);
            //     }
            // };

            // println!("{:?}", tokens);
        }
    }
}
