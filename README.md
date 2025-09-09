# Vapoursynth-llvmexpr

A [VapourSynth](https://www.vapoursynth.com/) filter that evaluates per-pixel mathematical or logical expressions. It uses an LLVM-based JIT (Just-In-Time) compiler for high performance.

`yuygfgg.Expr` is fully compatible with `akarin.Expr`, with other extensions in addition.

## Documentation

For a complete guide to the expression syntax and all available operators, please see the [**Documentation**](docs/specification.md).

## Dependencies

*   A C++23 compliant compiler (e.g., Clang, GCC)
*   [VapourSynth](https://www.vapoursynth.com/) SDK (headers)
*   [LLVM](https://llvm.org/) development libraries (>= 20.0.0)
*   [Meson](https://mesonbuild.com/) build system

## Building

This project uses the Meson build system.

1.  **Configure the build directory:**
    ```sh
    meson setup builddir
    ```

2.  **Compile and install the plugin:**
    ```sh
    ninja -C builddir install
    ```
