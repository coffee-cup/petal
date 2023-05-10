# petal

[![CI](https://github.com/coffee-cup/petal/actions/workflows/ci.yml/badge.svg)](https://github.com/coffee-cup/petal/actions/workflows/ci.yml)

A programming language that compiles to web assembly.

```
# Mix of TypeScript and Rust syntax
fn add(a: int, b: int) -> int  {
  return a + b
}

let three = add(1, 2)
```

## todo

This is a rough list of everything that needs to be done

- [x] Lexer
- [ ] Parsing expressions
  - [x] Literals
  - [x] Mathematical
  - [x] Function calls
- [ ] Parsing statements
  - [ ] Conditionals
  - [ ] Declarations
- [ ] Parsing types
- [ ] Pattern matching
- [ ] Structs
- [ ] Traits
- [ ] Typechecker
  - [ ] Inference
- [ ] Code generation
  - [ ] Basic wasm module
  - [ ] Interaction with host
  - [ ] TypeScript API
- [ ] CLI
  - [ ] Ability to run compiled wasm modules
