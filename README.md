# Vapoursynth-llvmexpr

A [VapourSynth](https://www.vapoursynth.com/) filter for evaluating complex, per-pixel mathematical or logical expressions. It utilizes an LLVM-based JIT (Just-In-Time) compiler to translate expressions into native code for better performance.

`llvmexpr.Expr` is fully compatible with the syntax of `akarin.Expr`, with additional extensions. Compared to `akarin`, `llvmexpr` is faster, more accurate and Turing-complete.

## Benchmark

[benchmarks/benchmark.py](benchmarks/benchmark.py)

Benchmark on Apple M2 Pro with 32GB RAM.

| Test Case | llvmexpr | akarin |
|---|---|---|
| simple arithmetic | 3346.11 FPS | 3192.03 FPS |
| logical condition | 3201.96 FPS | 3095.45 FPS |
| data range clamp | 3336.29 FPS | 3296.98 FPS |
| complex math chain | 1367.20 FPS | 1271.05 FPS |
| trigonometry coords | 1449.27 FPS | FAILED (Error) |
| power function | 3276.86 FPS | 3184.17 FPS |
| stack dup | 3116.86 FPS | 3192.57 FPS |
| named variables | 3152.67 FPS | 3317.73 FPS |
| static relative access | 2377.03 FPS | 3024.50 FPS |
| dynamic absolute access | 2955.05 FPS | 3036.40 FPS |
| bitwise and | 3295.08 FPS | 3096.99 FPS |
| gain | 1465.71 FPS | 1731.89 FPS |
| power with loop | 3181.33 FPS | FAILED (Error) |
| 3D rendering | 373.50 FPS | 194.39 FPS |
| 3D rendering 2 (icosahedron) | 556.56 FPS | 329.31 FPS |
| rotate clip | 214.62 FPS | 348.76 FPS |
| 8x8 dct | 179.82 FPS | 185.07 FPS |
| 8x8 idct | 192.41 FPS | 166.62 FPS |

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