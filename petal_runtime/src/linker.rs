use anyhow::Result;
use wasmtime::{Caller, Linker};

use crate::state::RuntimeState;

pub enum Runtime {
    Tests,
    Petal,
}

pub fn link_runtime(runtime: Runtime, linker: &mut Linker<RuntimeState>) -> Result<()> {
    match runtime {
        Runtime::Tests => {
            linker.func_wrap(
                "env",
                "assert",
                |_caller: Caller<'_, RuntimeState>, a: i64, b: i64| {
                    assert_eq!(a, b);
                },
            )?;
        }

        Runtime::Petal => {
            linker.func_wrap(
                "env",
                "printInt",
                |_caller: Caller<'_, RuntimeState>, param: i64| {
                    println!("{param}");
                },
            )?;
        }
    }

    Ok(())
}
