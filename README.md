# petal

[![CI](https://github.com/coffee-cup/petal/actions/workflows/ci.yml/badge.svg)](https://github.com/coffee-cup/petal/actions/workflows/ci.yml)

A programming language that compiles to web assembly.

```
# Mix of TypeScript and Rust syntax
fn add(a: Int, b: Int): Int  {
  return a + b
}

let three = add(1, 2)

# Exposed functions are exposed to the host environment in the compiled wasm module
# This function can be called from JavaScript on the web
# The goal is to automatically generate a TypeScript API for the wasm module
expose fn do_work(n: Int): Int {
  return n * 2
}
```

## todo

This is a rough list of everything that needs to be done

- [x] Compiler structure setup
  - [x] Errors
  - [x] Lexer/Parser
  - [x] IR generation
  - [x] Type checking
  - [x] Wat generation
  - [x] Wasm bytes generation
- [ ] Testing architecture
  - [x] Snapshot tests
  - [x] Output comparison
  - [ ] Assertions
- [x] Primitive constants
- [x] Binary operations
- [x] Unary operations
- [x] Control flow
  - [x] If
  - [x] Loops
  - [x] Return
- [x] Functions
  - [x] Creation
  - [x] Calling
  - [x] Return values
- [ ] Modules
- [x] Import host functions
- [ ] Encode wasm directly
- [ ] Number casting
- [ ] Structs
- [ ] Strings
- [ ] Arrays
- [ ] VSCode syntax highlighting
- [ ] TypeScript bindings
- [ ] Match expressions
