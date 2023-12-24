use wasmparser::BinaryReaderError;

use super::wat::WatModule;

pub struct Wasm {
    wasm_binary: Vec<u8>,
    pub wat_string: String,
    pub main_func: String,
}

impl Wasm {
    pub fn new(wat: &WatModule) -> Result<Self, (String, String)> {
        let wat_string = format!("{wat}");

        let wasm_binary =
            wat::parse_str(&wat_string.clone()).map_err(|e| (e.to_string(), wat_string.clone()))?;

        Ok(Self {
            wasm_binary,
            wat_string,
            main_func: wat.main_func.clone(),
        })
    }

    pub fn validate(&self) -> Result<(), BinaryReaderError> {
        wasmparser::validate(&self.wasm_binary).map(|_v| ())
    }

    pub fn print_wat(&self) -> String {
        wasmprinter::print_bytes(&self.wasm_binary).unwrap()
    }

    pub fn bytes(&self) -> &[u8] {
        &self.wasm_binary
    }
}
