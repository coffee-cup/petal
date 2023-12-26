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
  - [ ] Assertions
  - [ ] Stdout comparison
  - [ ] Type assertions
- [x] Primitive constants
- [ ] Binary/unary operations
- [ ] Control flow
  - [ ] If
  - [ ] Match statements
- [ ] Functions
  - [x] Creation
  - [ ] Calling
  - [x] Return values
- [ ] Number casting
- [ ] Structs
- [ ] Strings
- [ ] Arrays
- [ ] TypeScript bindings
