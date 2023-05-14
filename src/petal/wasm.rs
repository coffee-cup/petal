use super::{codegen::ToWat, ir::IRModule};

pub struct Wasm {
    wasm_binary: Vec<u8>,
}

impl Wasm {
    pub fn new(ir_module: &IRModule) -> Result<Self, (String, String)> {
        let wat_string = ir_module.to_wat();

        let wasm_binary = wat::parse_str(&wat_string).map_err(|e| (e.to_string(), wat_string))?;

        Ok(Self { wasm_binary })
    }

    // pub fn validate(&self) -> Result<()> {
    //     wasmparser::validate(&self.wasm_binary)
    //         .map(|_v| ())
    //         .map_err(|e| e.into())
    // }

    pub fn print_wat(&self) -> String {
        wasmprinter::print_bytes(&self.wasm_binary).unwrap()
    }
}
