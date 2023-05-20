use std::io::Write;

use clap::{Parser, Subcommand};
use petal::{errors::CompilerError, Compiler};

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate num_derive;

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

        /// Whether to output the WAT string
        #[arg(short = 'w', long = "wat")]
        wat: bool,

        /// Directory to output the compiled WASM to
        #[arg(short = 'o', long = "output", default_value = "build")]
        output: String,
    },
}

fn main() {
    let args = Cli::parse();

    match args.command {
        Commands::Build { file, wat, output } => {
            let compiler = Compiler::new();

            match compiler.compile_file(&file) {
                Err(CompilerError::ParserError(e)) => {
                    // We should not need to re-read the file here
                    let file = std::fs::read_to_string(file).expect("Could not read file");

                    petal::errors::print_parser_error(&file, &e);
                }
                Err(e) => {
                    println!("{}", e);
                }
                Ok(wasm) => {
                    if wat {
                        println!("{}", wasm.print_wat())
                    }

                    // Ensure that the output directory exists
                    std::fs::create_dir_all(&output).expect("Unable to create output directory");

                    let file_name = file.clone().replace("petal", "wasm");

                    // Save the WASM binary to the output directory
                    let mut file = std::fs::File::create(format!("{}/{}", output, file_name))
                        .expect("Unable to create output file");

                    file.write_all(wasm.bytes())
                        .expect("Unable to write the .wasm binary");
                }
            }
        }
    }
}
