hyperfine --shell none --warmup 3 'wasmtime build/test.wasm --invoke test' 'bun test.js'
