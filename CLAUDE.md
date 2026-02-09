# AGI Compiler (agi-cc)

A C compiler written in Rust, built incrementally with test-driven development.

## Architecture

```
src/
  main.rs        - CLI entry point
  lib.rs         - Public API (compile function)
  lexer/         - Tokenizer (C source → tokens)
    mod.rs
  parser/        - Parser (tokens → AST)
    mod.rs
  codegen/       - Code generator (AST → x86-64 assembly)
    mod.rs
tests/
  integration.rs - End-to-end compile-and-run tests
  fixtures/      - C source files for testing
```

## Development Phases

Each phase must have comprehensive tests before moving to the next.

### Phase 1: Return constants (CURRENT)
- `int main() { return <0-255>; }` → x86-64 asm → assemble & link → run → check exit code

### Phase 2: Unary operators
- `-`, `~`, `!`

### Phase 3: Binary operators
- `+`, `-`, `*`, `/`, `%`
- Operator precedence

### Phase 4: Local variables & assignments
- `int` declarations, assignments, compound expressions

### Phase 5: Control flow
- `if/else`, ternary, `for`, `while`, `do-while`

### Phase 6: Functions
- Function declarations, calls, parameters, return values

### Phase 7: Pointers & arrays

### Phase 8: Strings & global variables

### Phase 9: Structs & unions

### Phase 10: Preprocessor

## Build & Test

```bash
cargo build
cargo test
```

## Testing Strategy

- Every language feature gets integration tests that compile C → asm → run → verify exit code
- Use GCC/clang as reference: compile with both, compare outputs
- Test edge cases and error cases explicitly
- Tests must pass before merging any feature

## Conventions

- Keep each module focused and small
- Use `Result<T, CompileError>` for error handling
- Write descriptive error messages with source locations
- No unsafe code unless absolutely necessary for codegen
