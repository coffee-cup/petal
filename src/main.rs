use std::io::Write;

use clap::{Parser, Subcommand};
use petal::{run::run_wasm, Compiler};

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

    /// Run a petal file
    Run {
        /// The petal file to run
        file: String,

        /// Function in the WASM to run
        #[arg(short = 'f', long = "function")]
        function: Option<String>,
    },
}

fn main() {
    let args = Cli::parse();
    let compiler = Compiler::new();

    match args.command {
        Commands::Build {
            file,
            wat: _,
            output,
        } => {
            match compiler.compile_file(&file) {
                Err(e) => {
                    // We should not need to re-read the file here
                    let file = std::fs::read_to_string(file).expect("Could not read file");
                    petal::errors::print_compiler_error(&file, e);
                }
                Ok(wasm) => {
                    // Ensure that the output directory exists
                    std::fs::create_dir_all(output.clone())
                        .expect("Unable to create output directory");

                    let file_name = file.clone().replace("petal", "wasm");

                    // Save the WASM binary to the output directory
                    let mut file = std::fs::File::create(format!("{}/{}", output, file_name))
                        .expect("Unable to create output file");

                    file.write_all(wasm.bytes())
                        .expect("Unable to write the .wasm binary");
                }
            }
        }

        Commands::Run { file, function } => {
            match compiler.compile_file(&file) {
                Err(e) => {
                    // We should not need to re-read the file here
                    let file = std::fs::read_to_string(file).expect("Could not read file");

                    petal::errors::print_compiler_error(&file, e);
                }
                Ok(wasm) => {
                    run_wasm(wasm, function).expect("Failed to run wasm");
                }
            }
        }
    }
}
