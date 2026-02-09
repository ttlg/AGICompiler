# agi-cc

A C compiler written in Rust that compiles a subset of C to x86-64 assembly.

Built incrementally with test-driven development, one language feature at a time.

## Supported Features

- **Types**: `int`, `char *`, pointers (`int *`), arrays (`int arr[N]`), structs, unions
- **Operators**: arithmetic (`+`, `-`, `*`, `/`, `%`), comparison, logical (`&&`, `||`), unary (`-`, `~`, `!`)
- **Control flow**: `if`/`else`, ternary (`?:`), `for`, `while`, `do-while`
- **Functions**: declarations, calls, up to 6 parameters (System V ABI)
- **Pointers & arrays**: address-of, dereference, pointer arithmetic, array indexing
- **Structs & unions**: member access (`.`, `->`), alignment, padding
- **Global variables**: initialized (`.data`), uninitialized (`.bss`), string literals (`.rodata`)
- **Preprocessor**: `//` and `/* */` comments, `#define`/`#undef`, `#ifdef`/`#ifndef`/`#else`/`#endif`

## Usage

```bash
cargo build --release

# Compile C source to x86-64 assembly
./target/release/agi-cc input.c > output.s

# Assemble and link (using system assembler/linker)
as -o output.o output.s
ld -o output output.o -lc -dynamic-linker /lib64/ld-linux-x86-64.so.2
```

## Example

```c
#define SIZE 5

int sum(int *arr, int n) {
    int total = 0;
    for (int i = 0; i < n; i = i + 1)
        total = total + arr[i];
    return total;
}

int main() {
    int arr[SIZE];
    for (int i = 0; i < SIZE; i = i + 1)
        arr[i] = i * 10;
    return sum(arr, SIZE);
}
```

```bash
./target/release/agi-cc example.c > example.s
```

## Pipeline

```
C source  -->  preprocess  -->  tokenize  -->  parse  -->  codegen  -->  x86-64 asm
            (comments,       (tokens)     (AST)       (assembly)
             macros,
             conditionals)
```

## Project Structure

```
src/
  main.rs          - CLI entry point
  lib.rs           - Public API
  preprocessor/    - Comment stripping, macros, conditional compilation
  lexer/           - Tokenizer
  parser/          - Recursive descent parser
  codegen/         - x86-64 assembly generator
tests/
  integration.rs   - End-to-end tests (334 tests)
```

## Build & Test

```bash
cargo build
cargo test
```

## License

MIT
