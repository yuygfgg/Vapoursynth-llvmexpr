# Vapoursynth-llvmexpr

A [VapourSynth](https://www.vapoursynth.com/) filter for evaluating complex, per-pixel mathematical or logical expressions. It utilizes an LLVM-based JIT (Just-In-Time) compiler to translate expressions into native code for better performance.

`llvmexpr.Expr` is fully compatible with the syntax of `akarin.Expr`, with additional extensions. While being a bit slower (llvmexpr with LLVM21 is ~15% slower than akarin with LLVM15), `llvmexpr` is Turing-complete and might be used in more scenarios.

## Core Components

This project consists of two main parts: the core C++ plugin and a supporting Python utility library.

### 1. `llvmexpr` (C++ VapourSynth Plugin)

This is the core engine of the project. It is a VapourSynth filter that accepts expression strings written in **postfix notation** (also known as Reverse Polish Notation, or RPN). At runtime, it JIT-compiles these expressions into highly efficient machine code and applies them to each pixel of the video frame.

**Function Signature:**
```
llvmexpr.Expr(clip[] clips, string[] expr[, int format, int boundary=0, string dump_ir="", int opt_level=5, bint approx_math=True])
```

### 2. `exprutils` (Python Utility Library)

This is a companion Python library designed to assist with writing expressions. It provides:

*   **`infix2postfix`**: A transpiler that converts C-style **infix expressions** into the postfix format required by the C++ plugin.
*   **`postfix2infix`**: A reverse converter for debugging or understanding existing postfix expressions; Its output is guaranteed to be compatible with the `infix2postfix` transpiler.

The `exprutils` library provides an alternative way to generate the expression strings used by the `llvmexpr` plugin.

## Documentation

*   **[Infix Syntax](docs/infix.md)**: Describes the C-style syntax for use with the `exprutils.infix2postfix` transpiler.
*   **[Postfix Syntax](docs/postfix.md)**: The core RPN syntax and operator reference for the `llvmexpr` plugin.

## Dependencies

*   A C++23 compliant compiler (e.g., Clang, GCC)
*   [VapourSynth](https://www.vapoursynth.com/) SDK (headers)
*   [LLVM](https://llvm.org/) development libraries (>= 20.0.0)
*   [Meson](https://mesonbuild.com/) build system

## Building and installing

### Build and install the plugin

1.  **Configure the build directory:**
    ```sh
    meson setup builddir
    ```

2.  **Compile and install the plugin:**
    ```sh
    ninja -C builddir install
    ```

### Install the python library

```sh
pip install .
```

## Testing

To run the tests, you need to have VapourSynth installed.

This project uses pytest for testing.

```sh
pytest .
```