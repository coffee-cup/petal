use std::io::Write;

use clap::{Parser, Subcommand};
use petal_core::Compiler;
use petal_runtime::{run_wasm, to_val_string};

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
                    petal_core::errors::print_compiler_error(&file, e);
                }
                Ok(wasm) => {
                    // Ensure that the output directory exists
                    std::fs::create_dir_all(output.clone())
                        .expect("Unable to create output directory");

                    // Save the WASM binary to the output directory
                    let wasm_file_name = file.clone().replace("petal", "wasm");
                    let mut wasm_file =
                        std::fs::File::create(format!("{}/{}", output, wasm_file_name))
                            .expect("Unable to create output wasm file");
                    wasm_file
                        .write_all(wasm.bytes())
                        .expect("Unable to write the .wasm binary");

                    // Additionally, save the wat text to the output directory
                    // This is useful for debugging
                    let wat_file_name = file.clone().replace("petal", "wat");
                    let mut wat_file =
                        std::fs::File::create(format!("{}/{}", output, wat_file_name))
                            .expect("Unable to create output wat file");
                    wat_file
                        .write_all(wasm.wat_string.as_bytes())
                        .expect("Unable to write the .wat binary");
                }
            }
        }

        Commands::Run { file, function } => {
            match compiler.compile_file(&file) {
                Err(e) => {
                    // We should not need to re-read the file here
                    let file = std::fs::read_to_string(file).expect("Could not read file");

                    petal_core::errors::print_compiler_error(&file, e);
                }
                Ok(wasm) => {
                    let results = run_wasm(wasm, function).expect("Failed to run wasm");
                    println!(
                        "{}",
                        results
                            .iter()
                            .map(to_val_string)
                            .collect::<Vec<_>>()
                            .join(",")
                    )
                }
            }
        }
    }
}
