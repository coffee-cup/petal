use petal_core::Compiler;
use petal_runtime::{linker::Runtime, run_wasm};
use rstest::*;
use std::path::PathBuf;

#[rstest]
fn for_each_file(#[files("cases/**/*.petal")] path: PathBuf) {
    let contents = std::fs::read_to_string(path.clone()).unwrap();

    let results_line = contents
        .lines()
        .find(|line| line.starts_with("# Results: "))
        .unwrap_or_else(|| panic!("Could not find Results line in `{}`", path.display()));

    let results = results_line
        .strip_prefix("# Results: ")
        .expect("Could not strip prefix");

    let expected_results: String = results
        .split(',')
        .map(|s| s.trim())
        .collect::<Vec<_>>()
        .join(", ");

    let compiler = Compiler::new();
    let wasm = compiler
        .compile_file(path.display().to_string().as_str())
        .unwrap();

    let results = run_wasm(wasm, Runtime::Tests, Some("test".to_string())).unwrap();
    let results_string = results
        .iter()
        .map(|val| petal_runtime::to_val_string(val).to_string())
        .collect::<Vec<_>>()
        .join(", ");

    assert_eq!(expected_results, results_string);
}
