# AGI Compiler (agi-cc)

A C compiler written in Rust, built incrementally with test-driven development.

## Architecture

```
src/
  main.rs          - CLI entry point
  lib.rs           - Public API (compile function)
  preprocessor/    - Preprocessor (comments, macros, conditionals)
    mod.rs
  lexer/           - Tokenizer (C source → tokens)
    mod.rs
  parser/          - Parser (tokens → AST)
    mod.rs
  codegen/         - Code generator (AST → x86-64 assembly)
    mod.rs
tests/
  integration.rs   - End-to-end compile-and-run tests
  fixtures/        - C source files for testing
```

## Development Phases

Each phase must have comprehensive tests before moving to the next.

### Phase 1: Return constants (DONE)
- `int main() { return <0-255>; }` → x86-64 asm → assemble & link → run → check exit code

### Phase 2: Unary operators (DONE)
- `-`, `~`, `!`

### Phase 3: Binary operators (DONE)
- `+`, `-`, `*`, `/`, `%`
- Operator precedence

### Phase 4: Local variables & assignments (DONE)
- `int` declarations, assignments, compound expressions

### Phase 5: Control flow (DONE)
- `if/else`, ternary, `for`, `while`, `do-while`

### Phase 6: Functions (DONE)
- Function declarations, calls, parameters, return values
- x86-64 System V ABI calling convention (up to 6 register arguments)
- Stack alignment tracking for nested calls

### Phase 7: Pointers & arrays (DONE)
- Pointer declarations (`int *p`), address-of (`&x`), dereference (`*p`)
- Array declarations (`int arr[N]`), indexing (`arr[i]`)
- Pointer parameters, array decay to pointer
- Pointer arithmetic with sizeof(int) scaling
- Type system: VarType (Int/Pointer/Array), ExprType (Int/Pointer)

### Phase 8: Strings & global variables (DONE)
- Global variable declarations (initialized and uninitialized)
- `.data` section for initialized globals, `.comm` for BSS (uninitialized)
- String literals in `.section .rodata`, accessed via `leaq .LCn(%rip)`
- `char *` type (same codegen as `int *`)
- Global string initialization (`char *msg = "hello"` → `.quad .LCn`)
- RIP-relative addressing for global read/write (`name(%rip)`)
- VarLocation enum (Local vs Global) for unified variable access
- Section management: only emit `.data`/`.rodata`/`.text` when needed

### Phase 9: Structs & unions (DONE)
- Struct/union type definitions with member layout computation
- Struct alignment and padding (int=4 bytes, pointer=8 bytes)
- Union layout: all members at offset 0, size = max member size
- `.` (dot) member access on struct variables
- `->` (arrow) member access on struct/union pointers
- Assignment to struct/union members via `.` and `->`
- Address-of struct members (`&p.y`)
- Struct/union pointer parameters (`struct Point *p`)
- Global struct/union variables (`.comm` with correct size/alignment)
- CompositeLayout type registry for type-aware codegen
- ExprType::StructPtr/UnionPtr for typed struct pointer tracking

### Phase 10: Preprocessor (DONE)
- Comment stripping: `//` line comments, `/* */` block comments
- Respects string literals (no stripping inside `"..."`)
- `#define NAME value` macro definitions with identifier-level substitution
- `#undef NAME` to remove macros
- `#ifdef`/`#ifndef` conditional compilation with nesting
- `#else`/`#endif` conditional branches
- Chained macro expansion (expand until stable)
- Preprocessor runs before lexer in pipeline: source → preprocess → tokenize → parse → codegen

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
