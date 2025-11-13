# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
# Build the compiler in debug mode
cargo build

# Build in release mode for better performance
cargo build --release

# Run all tests
cargo test

# Run tests for specific modules
cargo test lexer
cargo test parser
cargo test semantic

# Run tests with verbose output
cargo test -- --nocapture
```

## Running the Compiler

The compiler provides multiple execution modes through the CLI:

```bash
# Direct execution (interpreter mode)
cargo run -- run program.nlang

# Compile to executable (via C codegen + GCC)
cargo run -- compile program.nlang -o program.exe

# Generate C code
cargo run -- generate-c program.nlang -o program.c

# Generate lexer tokens JSON
cargo run -- lex program.nlang -o tokens.json

# Generate AST JSON
cargo run -- gen-ast program.nlang -o ast.json
```

## Architecture Overview

Nlang is a statically-typed programming language compiler with Python-like syntax. It follows a multi-stage compilation pipeline:

1. **Lexical Analysis** (`src/lexer/`) - Tokenizes source code into lexical tokens
2. **Syntax Analysis** (`src/parser/`) - Parses tokens into an Abstract Syntax Tree (AST) defined in `src/ast/`
3. **Semantic Analysis** (`src/semantic/`) - Performs type checking, scope validation, and semantic error reporting
4. **Execution/Code Generation** - Three backends are supported:
   - **Interpreter** (`src/interpreter/`) - Direct AST execution for development
   - **C Code Generator** (`src/c_codegen/`) - Transpiles to C code for GCC compilation
   - **Execution Engine** (`src/execution_engine/`) - Unified interface coordinating all stages

### Key Components

- **ExecutionEngine**: Main orchestrator that coordinates lexer → parser → semantic analyzer → backend
- **Standard Library** (`src/std_lib/`) - Built-in functions and types for I/O, math, and string operations
- **CLI Interface** (`src/cli.rs`) - Command-line argument parsing and command dispatch

### Language Features

- Variables declared with `store` keyword (e.g., `store x = 42`)
- Static typing with type inference
- Functions with parameters and return values
- Control flow: `if`/`else`, `while` loops with `break`/`continue`
- Import system: `import math` or `from string { upper, lower }`
- Types: Integer, Float, String, Boolean, Null

### File Extensions

- Source files: `.nlang`
- Test files: Standard Rust `#[test]` attributes in `*_tests.rs` files

### Error Handling

The codebase uses `thiserror` and `anyhow` for comprehensive error handling. Each compilation stage has its own error type that propagates through the `ExecutionEngine`.

### Testing Strategy

- Unit tests for individual components in `*_tests.rs` files
- Integration tests via the `ExecutionEngine`
- JSON output modes for lexer tokens and AST enable external testing
- Use `cargo test` to run the full test suite

### Module Resolution

The compiler supports proper module resolution when given file paths, using the file's directory to resolve relative imports.