

use clap::{Parser, Subcommand};
use petal::Compiler;



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
    },
}

fn main() {
    let args = Cli::parse();

    match args.command {
        Commands::Build { file, wat } => {
            let compiler = Compiler::new();

            match compiler.compile_file(&file) {
                Err(e) => {
                    println!("Error: {}", e);
                }
                Ok(wasm) => {
                    if wat {
                        println!("{}", wasm.print_wat())
                    }
                }
            }

            // let mut codegen = Codegen::new();
            // let wat_string = codegen.generate_module();

            // let buf = wast::parser::ParseBuffer::new(&wat_string).unwrap();
            // let wast_module = wast::parser::parse::<Wat>(&buf).unwrap();

            // let wasm_binary = wat::parse_str(&wat_string).unwrap();

            // let engine = wasmtime::Engine::default();
            // let module = wasmtime::Module::new(&engine, &wasm_binary).unwrap();

            // let mut linker = wasmtime::Linker::new(&engine);
            // linker
            //     .func_wrap(
            //         "host",
            //         "log",
            //         |caller: wasmtime::Caller<'_, u32>, param: i32| {
            //             println!("Got {} from WebAssembly", param);
            //         },
            //     )
            //     .unwrap();

            // let mut file = fs::File::create("test.wasm").expect("Unable to create test.wasm file");
            // file.write_all(&wasm_binary)
            //     .expect("Unable to write the .wasm binary");

            // let mut store = wasmtime::Store::new(&engine, 0);
            // let instance = linker.instantiate(&mut store, &module).unwrap();
            // let hello = instance
            //     .get_typed_func::<(), ()>(&mut store, "hello")
            //     .unwrap();
            // hello.call(&mut store, ()).unwrap();
        }
    }
}
