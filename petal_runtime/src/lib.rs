use anyhow::{Context, Result};
use linker::{link_runtime, Runtime};
use petal_core::wasm::Wasm;
use state::RuntimeState;
use wasmtime::{Engine, Linker, Module, Store, Val};

pub mod linker;
mod state;

pub fn run_wasm(wasm: Wasm, runtime: Runtime, function: Option<String>) -> Result<Vec<Val>> {
    let engine = Engine::default();
    let module = Module::new(&engine, wasm.bytes()).context("creating wasm module")?;

    let mut linker = Linker::new(&engine);
    link_runtime(runtime, &mut linker).context("Linking runtime")?;

    let mut store = Store::new(&engine, RuntimeState::default());

    let instance = linker
        .instantiate(&mut store, &module)
        .context("creating wasmtime instance")?;

    // TODO: We should call the wasm.main_func here first always

    let func_name = function.unwrap_or_else(|| wasm.main_func.clone());

    let func = instance
        .get_func(&mut store, &func_name)
        .context(format!("getting petal function `{func_name}`"))?;

    let ty = func.ty(&store);

    // Create a vector to hold the results
    let mut results: Vec<Val> = Vec::new();
    (0..ty.results().len()).for_each(|_i| {
        results.push(Val::I32(0));
    });

    match func.call(&mut store, &[], &mut results) {
        Ok(()) => {}
        Err(trap) => {
            panic!("execution of `foo` resulted in a wasm trap: {}", trap);
        }
    };

    Ok(results)
}

pub fn to_val_string(val: &Val) -> String {
    match val {
        Val::I32(i) => format!("{}", i),
        Val::I64(i) => format!("{}", i),
        Val::F32(f) => format!("{}", f32::from_bits(*f)),
        Val::F64(f) => format!("{}", f64::from_bits(*f)),
        Val::V128(_) => String::from("v128"),
        Val::ExternRef(_) => String::from("externref"),
        Val::FuncRef(_) => String::from("funcref"),
    }
}
