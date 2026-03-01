# Petal

Statically-typed language compiling to WASM, written in Rust.

## Workspace

| Crate | Purpose |
|-------|---------|
| `petal_core` | Compiler pipeline: lexer → parser → semantics → IR → codegen → WASM |
| `petal_cli` | CLI (`build`, `run` subcommands) |
| `petal_runtime` | Wasmtime-based execution runtime |
| `petal_tests` | Golden tests (rstest) |

## Commands

```sh
mise run test       # run all tests (cargo test --all)
mise run build      # cargo build
mise run snapshot   # update insta snapshots (cargo insta test --review -p petal_core)
mise run fix        # fmt check + clippy autofix
```

```sh
cargo run -- build <file.petal>           # outputs .wasm + .wat to build/
cargo run -- build <file.petal> -o out/   # custom output dir
cargo run -- run <file.petal>             # compile and execute via wasmtime
```

## Compiler Pipeline

```
Source → Lexer → Parser → Semantics → IR → Codegen → WASM
         tokens   AST    typecheck   ir    .wat     .wasm
```

Semantics uses constraint-based type inference (HM-style): symbol generation → constraint collection → solving.

Key modules in `petal_core/src/`:
- `lexer.rs`, `token.rs` — tokenization
- `parser.rs`, `precedence.rs`, `ast.rs` — parsing
- `semantics/` — symbol table, constraint gen, typechecker
- `ir/` — intermediate representation
- `codegen/` — WAT generation
- `wasm.rs`, `wat.rs` — WASM binary output

## Language Syntax

```
# line comments

# types: Int, Float, Bool
let x = 42
let pi = 3.14
let flag = true

# functions with type annotations
fn add(a: Int, b: Int): Int {
  return a + b
}

# exported to WASM host
export fn work(n: Int): Int {
  return n * 2
}

# import host functions
import assert(a: Int, b: Int)

# control flow
if x > 0 { ... } else { ... }
while x > 0 { x = x - 1 }

# operators: + - * / == != < <= > >= ! (unary -)
```

## Testing

**Unit tests:** insta snapshot tests in `petal_core` (parser, IR, codegen stages).

**Golden tests:** `petal_tests/cases/*.petal` — each file has a `# Results: <expected>` comment at the end. The test harness compiles, runs the `test()` function, and compares output.

To add a golden test: create a `.petal` file in `petal_tests/cases/` with an `export fn test()` and a `# Results:` comment.

## Known Issues (fix.md)

- Assigning to expressions of `Unit` type
- Assigning functions to variables (`let x = foo`)
- Returning a value from a function that returns `Unit`
