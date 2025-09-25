# Vapoursynth-llvmexpr

A [VapourSynth](https://www.vapoursynth.com/) filter for evaluating complex, per-pixel mathematical or logical expressions. It utilizes an LLVM-based JIT (Just-In-Time) compiler to translate expressions into native code.

`llvmexpr.Expr` is designed to be a powerful and feature-rich alternative to `akarin.Expr`. It is fully compatible with `akarin`'s syntax and extends it with additional features, most notably Turing-complete control flow and advanced math functions.

In terms of performance, `llvmexpr` may excel at complex mathematical computations. However, its performance can be limited by memory access patterns. In scenarios involving heavy random memory access or specific spatial operations (see `static relative access` in benchmarks), `akarin.Expr` may offer better performance.

## Benchmark

[benchmarks/benchmark.py](benchmarks/benchmark.py)

Benchmark on Apple M2 Pro with 32GB RAM.

| Test Case | llvmexpr | akarin |
|---|---|---|
| simple arithmetic | 3436.28 FPS | 3313.28 FPS |
| logical condition | 3335.33 FPS | 3356.79 FPS |
| data range clamp | 3322.70 FPS | 3297.24 FPS |
| complex math chain | 1371.12 FPS | 1276.12 FPS |
| trigonometry coords | 2076.42 FPS | FAILED (Error) |
| power function | 3429.03 FPS | 3256.96 FPS |
| stack dup | 3440.84 FPS | 3325.12 FPS |
| named variables | 3297.12 FPS | 3356.67 FPS |
| static relative access | 2422.24 FPS | 3205.19 FPS |
| dynamic absolute access | 3091.64 FPS | 3038.17 FPS |
| bitwise and | 3365.81 FPS | 3353.05 FPS |
| gain | 1470.07 FPS | 1762.66 FPS |
| power with loop | 3348.47 FPS | FAILED (Error) |
| 3D rendering | 377.50 FPS | 194.69 FPS |
| 3D rendering 2 (icosahedron) | 559.52 FPS | 328.41 FPS |
| rotate clip | 216.35 FPS | 351.75 FPS |
| 8x8 dct | 181.06 FPS | 185.77 FPS |
| 8x8 idct | 193.94 FPS | 166.80 FPS |

## Core Components

This project consists of two main parts: the core C++ plugin and a supporting Python utility library.

### 1. `llvmexpr` (C++ VapourSynth Plugin)

This is the core engine of the project. It is a VapourSynth filter that accepts expression strings written in **postfix notation** (also known as Reverse Polish Notation, or RPN). At runtime, it JIT-compiles these expressions into highly efficient machine code and applies them to each pixel of the video frame.

**Function Signature:**
```
llvmexpr.Expr(clip[] clips, string[] expr[, int format, int boundary=0, string dump_ir="", int opt_level=5, int approx_math=2])
```

**Parameters:**
- `clips`: Input video clips
- `expr`: Expression strings in postfix notation (one per plane)
- `format`: Output format (optional)
- `boundary`: Boundary handling mode (0=clamp, 1=mirror)
- `dump_ir`: Path to dump LLVM IR for debugging (optional)
- `opt_level`: Optimization level (> 0, default: 5)
- `approx_math`: Approximate math mode (default: 2)
  - `0`: Disabled – use precise LLVM intrinsics for all math operations
  - `1`: Enabled – use fast approximate implementations for `exp`, `log`, `sin`, `cos`, `tan`
  - `2`: Auto (recommended) – first tries with approximate math enabled; if LLVM reports that the inner loop cannot be vectorized, the compiler automatically recompiles the same function with approximate math disabled and JITs that precise version instead.

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