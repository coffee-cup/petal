use anyhow::{Context, Result};
use petal_core::wasm::Wasm;
use wasmtime::{Caller, Engine, Func, Instance, Linker, Module, Store, Val};

struct HostState {
    pub value: i32,
}

pub fn run_wasm(wasm: Wasm, function: Option<String>) -> Result<Vec<Val>> {
    let engine = Engine::default();
    let module = Module::new(&engine, wasm.bytes()).context("creating wasm module")?;

    let mut linker = Linker::new(&engine);
    linker.func_wrap(
        "env",
        "add",
        |_caller: Caller<'_, HostState>, a: i64, b: i64| {
            return a + b;
        },
    )?;

    linker.func_wrap(
        "env",
        "printInt",
        |_caller: Caller<'_, HostState>, param: i64| {
            println!("{param}");
        },
    )?;

    let mut store = Store::new(&engine, HostState { value: 42 });

    let instance = linker
        .instantiate(&mut store, &module)
        .context("creating wasmtime instance")?;

    // let _hello_fun = Func::wrap(&mut store, |mut caller: Caller<'_, HostState>| {
    //     println!("Calling back...");
    //     println!("> {}", caller.data().value);
    //     caller.data_mut().value += 1;
    // });

    // let print_func = Func::wrap(&mut store, |_caller: Caller<'_, HostState>, param: i64| {
    //     println!("{param}");
    // });

    // let add_func = Func::wrap(
    //     &mut store,
    //     |_caller: Caller<'_, HostState>, a: i64, b: i64| -> i64 {
    //         return a + b;
    //     },
    // );

    // let imports = [print_func.into(), add_func.into()];
    // let imports = [];
    // let instance =
    //     Instance::new(&mut store, &module, &imports).context("creating wasmtime instance")?;

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
