use clap::Parser;

#[macro_use]
extern crate lazy_static;

mod petal;

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// The file to compile and run with petal
    file: Option<String>,
}

fn main() {
    let args = Cli::parse();

    match args.file {
        Some(file) => {
            println!("File: {}", file);
        }
        None => {
            let mut repl = petal::repl::Repl::new();
            repl.run();
        }
    }
}
