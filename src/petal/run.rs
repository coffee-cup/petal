use anyhow::{Context, Result};
use wasmtime::{Caller, Engine, Func, Instance, Linker, Module, Store};

use super::wasm::Wasm;

struct HostState {
    pub value: i32,
}

pub fn run_wasm(wasm: Wasm, function: Option<String>) -> Result<()> {
    let engine = Engine::default();
    let module = Module::new(&engine, wasm.bytes()).context("creating wasm module")?;

    let mut store = Store::new(&engine, HostState { value: 42 });

    let hello_fun = Func::wrap(&mut store, |mut caller: Caller<'_, HostState>| {
        println!("Calling back...");
        println!("> {}", caller.data().value);
        caller.data_mut().value += 1;
    });

    let imports = [];
    let instance =
        Instance::new(&mut store, &module, &imports).context("creating wasmtime instance")?;

    // let run = instance
    //     .get_typed_func::<(), i64>(&mut store, &wasm.main_func)
    //     .context("getting main petal function")?;

    let func_name = function.unwrap_or_else(|| wasm.main_func.clone());

    let run = instance
        .get_func(&mut store, &func_name)
        .context(format!("getting petal function `{func_name}`"))?
        .typed::<(), i64>(&store)?;

    let result = run.call(&mut store, ()).context("running main func")?;

    println!("\n\n--- Result");
    println!("{result}",);

    Ok(())
}
