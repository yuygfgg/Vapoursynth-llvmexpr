### Migrating from `akarin.Expr` to `llvmexpr`

This document serves as a guide for users familiar with `akarin.Expr` who are transitioning to `llvmexpr.Expr`.

While `llvmexpr` is designed for full syntax compatibility with `akarin`'s RPN, its enhanced feature set introduce new possibilities and some important distinctions.

### 1. Major New Capabilities

These are fundamental features in `llvmexpr` that enable entirely new types of operations not possible in `akarin.Expr`.

*   **Turing-Complete Control Flow:** `llvmexpr` allows for complex, iterative algorithms using labels and conditional jumps, making the expression engine Turing-complete.
    *   **Labels (`#label_name`):** Defines a destination for a jump.
    *   **Conditional Jumps (`label_name#`):** Pops a value from the stack. If the value is true (> 0), execution jumps to the corresponding label. This enables the creation of loops and complex conditional logic.

*   **Direct Output Control:** Provides operators to write to arbitrary pixel locations and suppress the default output, allowing for advanced spatial operations like rotations or custom warps directly within an expression.
    *   **`@[]`:** Pops a value and `x`, `y` coordinates (`val absX absY @[]`), and writes the value to the output frame at `[absX, absY]`.
    *   **`^exit^`:** A special marker that suppresses the default write to the current pixel `[X, Y]` if it (or a variable it was assigned to) is the only item left on the stack after evaluation.

### 2. Core Engine and Usability Enhancements

*   **`exprutils` Python Library:** A companion library is provided to improve the user experience:
    *   **`infix2postfix`:** A transpiler to convert C-style infix expressions into the RPN format.
    *   **`postfix2infix`:** A reverse transpiler for debugging and understanding RPN expressions.
*   **Advanced Function Parameters:** Offers more fine-grained control over compilation and execution:
    *   `dump_ir`: Dumps the intermediate LLVM IR for debugging and performance analysis.
    *   `opt_level`: Provides explicit control over the LLVM optimization level.
    *   `approx_math`: An intelligent mode for using fast, approximate math functions, with an automatic fallback to precise versions if the LLVM vectorizer fails.

### 3. Expanded Syntax and Function Library

`llvmexpr` extends `akarin`'s syntax with a significantly larger set of mathematical functions.

*   **Expanded Mathematical Functions:**
    *   **Trigonometry:** `tan`, `asin`, `acos`, `atan`, `atan2`
    *   **Hyperbolic:** `sinh`, `cosh`, `tanh`
    *   **Logarithmic:** `log2`, `log10`
    *   **Exponential:** `exp2`
    *   **Rounding:** `ceil`
    *   **Miscellaneous:** `copysign`, `fma` (fused multiply-add), `neg`, `sgn` (signum).
*   **Enhanced Pixel Access Modifiers:** Extends absolute pixel access with explicit boundary mode control.
    *   `absX absY clip[]:m`: Forces mirrored boundary for absolute access.
    *   `absX absY clip[]:b`: Forces the use of the filter's global `boundary` parameter for absolute access.
    *   `absX absY clip[]:c`: Forces clamped boundary for absolute access (default when not specified, same as akarin.Expr).
*   **Relaxed Offset Range for Static Pixel Access:**: While `akarin.Expr` requires `-width < relX < width && -height < relY < height` in `x[relX,relY]` syntax, `llvmexpr` allows `relX` and `relY` to be any integer value.

### 4. Behavioral Differences: Type-Aware vs. Always-Float Evaluation

The most significant difference between the two filters lies in their core evaluation engines. `llvmexpr` operates exclusively in a **32-bit always-float mode**. In contrast, `akarin.Expr` uses a **type-aware, integer-preserving engine** that only promotes values to floating-point when an operation requires it. The `opt` parameter in `akarin` is the crucial switch that controls how this engine treats pixel data loaded from integer-format clips.

#### 4.1. Integer Wrap-around Behavior

`akarin`'s engine tracks the type of each value. If an operation involves only integers, it performs the calculation using 32-bit signed two's complement arithmetic, which enables integer wrap-around.

*   **In `akarin`**: Wrap-around occurs whenever an operation's inputs are all integers. This is true for integer constants even with `opt=0`.
    *   `'2147483647 1 +'` → `int` + `int` → wraps to `-2147483648`.

`llvmexpr` does not have this type-aware behavior. It promotes all numeric inputs to 32-bit floats at the earliest opportunity.

*   **In `llvmexpr`**: The expression `'2147483647 1 +'` will always be evaluated using floating-point math, resulting in `2147483648.0`. Wrap-around is impossible.

**Implication:** Any expression that relies on integer overflow as a mechanism is **not** directly portable and will produce fundamentally different results in `llvmexpr`.

#### 4.2. Integer Precision & The Critical Role of `akarin`'s `opt` Parameter

A standard 32-bit float can only represent all consecutive integers exactly up to **$2^{24}$**.

The `opt` parameter controls the **initial type** of pixel data loaded from integer-format clips (`x`, `y`, etc.):

*   **`opt=0` (default):** Loads integer pixel data **as floats**. In this mode, if a clip has more than 24 bits of integer precision, that extra precision is **lost immediately upon loading**, before any operations are performed.
*   **`opt=1`:** Loads integer pixel data **as integers**. This mode **preserves the full bit-depth** of the source clip, allowing for precise calculations on values up to 32 bits.

This leads to a stark difference in capabilities:

| Feature                                    | `llvmexpr`                                  | `akarin.Expr (opt=0)`                       | `akarin.Expr (opt=1)`                         |
| :----------------------------------------- | :------------------------------------------ | :------------------------------------------ | :-------------------------------------------- |
| **Model**                                  | Always-Float                                | Type-Aware                                  | Type-Aware, Integer-Preserving                |
| **Loading `x` from a 32-bit integer clip** | Converted to float, **precision lost**      | Converted to float, **precision lost**      | Loaded as 32-bit integer, **full precision**  |
| **Bitwise Ops on >24bit `x`**              | **Incorrect** (operates on imprecise float) | **Incorrect** (operates on imprecise float) | **Correct** (operates on precise integer)     |
| **Arithmetic on >24bit `x`**               | Imprecise float arithmetic                  | Imprecise float arithmetic                  | Precise integer arithmetic (with wrap-around) |

**Conclusion for Migration:**

*   `llvmexpr`'s behavior is simple and consistent: everything is a float, and precision is always limited to 24 bits for integer values.
*   `akarin` with `opt=0` behaves like `llvmexpr` *when dealing with pixel data*, as the initial conversion to float limits precision. However, it still performs integer math on integer constants.
*   `akarin` with `opt=1` unlocks a integer-only, high-precision pipeline that `llvmexpr` cannot replicate.

Users migrating from `akarin.Expr` must be mindful of this core difference. **Expressions that depend on the precise representation of integers greater than 24 bits (especially for bitwise operations) or on integer wrap-around behavior will not work as expected in `llvmexpr` and may require algorithmic changes.**