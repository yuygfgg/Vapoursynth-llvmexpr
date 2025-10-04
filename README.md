# Vapoursynth-llvmexpr

A [VapourSynth](https://www.vapoursynth.com/) filter for evaluating complex mathematical or logical expressions. It utilizes an LLVM-based JIT (Just-In-Time) compiler to translate expressions into native code.

The plugin provides two main functions:
*   `llvmexpr.Expr`: Evaluates an expression for **every pixel** in a frame, ideal for spatial filtering and general image manipulation.
*   `llvmexpr.SingleExpr`: Evaluates an expression only **once per frame**, designed for tasks like calculating frame-wide statistics, reading specific pixels, and writing to frame properties or arbitrary pixel locations.

`llvmexpr.Expr` is designed to be a powerful and feature-rich alternative to `akarin.Expr`. It is (almost) fully compatible with `akarin`'s syntax and extends it with additional features, most notably Turing-complete control flow and advanced math functions. See [Migrating From Akarin](docs/migrating_from_akarin.md) for a detailed comparison.

In terms of performance, `llvmexpr` may excel at complex mathematical computations. However, its performance can be limited by memory access patterns. In scenarios involving heavy random memory access or specific spatial operations (see `rotate clip` in benchmarks), `akarin.Expr` may offer better performance.

## Benchmark

[benchmarks/benchmark.py](benchmarks/benchmark.py)

Benchmark on Apple M2 Pro with 32GB RAM.

| Test Case | llvmexpr | akarin |
|---|---|---|
| simple arithmetic | 3394.66 FPS | 3178.16 FPS |
| logical condition | 3268.11 FPS | 3347.38 FPS |
| data range clamp | 3354.53 FPS | 3304.06 FPS |
| complex math chain | 1366.07 FPS | 1274.04 FPS |
| trigonometry coords | 2090.50 FPS | FAILED (Error) |
| power function | 3238.64 FPS | 3138.22 FPS |
| stack dup | 3404.09 FPS | 3328.36 FPS |
| named variables | 3349.18 FPS | 3271.53 FPS |
| static relative access | 3026.66 FPS | 3085.37 FPS |
| dynamic absolute access | 2980.11 FPS | 3015.62 FPS |
| bitwise and | 3314.40 FPS | 3254.09 FPS |
| gain | 1456.08 FPS | 1722.37 FPS |
| power with loop | 3275.87 FPS | FAILED (Error) |
| 3D rendering | 371.87 FPS | 192.80 FPS |
| 3D rendering 2 (icosahedron) | 552.66 FPS | 324.18 FPS |
| rotate clip | 215.35 FPS | 347.64 FPS |
| 8x8 dct | 180.91 FPS | 183.51 FPS |
| 8x8 idct | 192.42 FPS | 164.70 FPS |

Geometric mean FPS (common successful tests only):
  llvmexpr: 1352.85 FPS
  akarin: 1280.74 FPS

Performance ratios (relative to 'llvmexpr'): 
  llvmexpr: 1.000x
  akarin: 0.947x

## Core Components

This project consists of two main parts: the core C++ plugin and a supporting Python utility library.

### 1. `llvmexpr` (C++ VapourSynth Plugin)

This is the core engine of the project. It is a VapourSynth filter that accepts expression strings written in **postfix notation** (also known as Reverse Polish Notation, or RPN). At runtime, it JIT-compiles these expressions into highly efficient machine code.

#### `llvmexpr.Expr` (Per-Pixel)

This function applies an expression to each pixel of the video frame.

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
  - `1`: Enabled – use fast approximate implementations for `exp`, `log`, `sin`, `cos`, `tan`, `acos`, `atan`, `asin`, `atan2`.
  - `2`: Auto (recommended) – first tries with approximate math enabled; if LLVM reports that the inner loop cannot be vectorized, the compiler automatically recompiles the same function with approximate math disabled and JITs that precise version instead.

#### `llvmexpr.SingleExpr` (Per-Frame)

This function executes an expression only once per frame. It is not suitable for typical image filtering but is powerful for tasks that involve reading from arbitrary coordinates, calculating frame-wide metrics, and writing results to other pixels or to frame properties.

**Function Signature:**
```
llvmexpr.SingleExpr(clip[] clips, string expr[, int boundary=0, string dump_ir="", int opt_level=5, int approx_math=2])
```

**Parameters:**
- `clips`: Input video clips.
- `expr`: A single expression string in postfix notation. Unlike `Expr`, only one string is accepted for all planes.
- `boundary`: Boundary handling mode for pixel reads (0=clamp, 1=mirror). This does not affect writes.
- `dump_ir`: Path to dump LLVM IR for debugging (optional).
- `opt_level`: Optimization level (> 0, default: 5).
- `approx_math`: Approximate math mode (default: 2). See description under `Expr` for details.

### 2. `exprutils` (Python Utility Library)

This is a companion Python library designed to assist with writing expressions. It provides:

*   **`infix2postfix`**: A transpiler that converts C-style **infix expressions** into the postfix format required by the C++ plugin.
*   **`postfix2infix`**: A reverse converter for debugging or understanding existing postfix expressions; Its output is guaranteed to be compatible with the `infix2postfix` transpiler.

The `exprutils` library provides an alternative way to generate the expression strings used by the `llvmexpr` plugin.

#### Examples

See [examples](examples) for examples of infix code.

- [8x8 DCT](examples/8x8dct.expr)
- [8x8 IDCT](examples/8x8idct.expr)
- [NL-Means](examples/nl-means.expr)

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