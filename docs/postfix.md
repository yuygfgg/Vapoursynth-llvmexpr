### VapourSynth Expr Syntax Documentation

`Expr` is a powerful VapourSynth function that evaluates a per-pixel mathematical or logical expression. Its core is the `expr` string, which uses Reverse Polish Notation (RPN). This guide details the syntax of that string.

---

### **1. Core Concepts**

#### **1.1. Reverse Polish Notation (RPN)**

Instead of the conventional `A + B`, RPN places operators _after_ their operands: `A B +`. The expression is evaluated using a stack. Values are pushed onto the stack, and operators pop values, perform a calculation, and push the result back onto the stack.

- **Example:** To calculate `(5 + 3) * 2`, the RPN expression is `5 3 + 2 *`.
  1.  `5`: Push 5. Stack: `[5]`
  2.  `3`: Push 3. Stack: `[5, 3]`
  3.  `+`: Pop 3 and 5, calculate 8, push 8. Stack: `[8]`
  4.  `2`: Push 2. Stack: `[8, 2]`
  5.  `*`: Pop 2 and 8, calculate 16, push 16. Stack: `[16]`

The final value on the stack is the result for the pixel.

#### **1.2. Data Ranges**

`Expr` does not normalize input clip values. You must account for the native range of the pixel format.

- **8-bit integer:** 0 to 255
- **10-bit integer:** 0 to 1023
- **16-bit integer:** 0 to 65535
- **32-bit float:** Typically 0.0 to 1.0 for Luma (Y) and Alpha, and -0.5 to 0.5 for Chroma (U/V).

When mixing formats, scale values accordingly. For example, to add an 8-bit value to a 10-bit value, you might use `x y 4 * +` (multiplying the 8-bit value `y` by 4 to scale it to the 10-bit range).

---

### **2. Operands: Pushing Values onto the Stack**

#### **2.1. Clip Identifiers**

Input clips are the primary source of pixel values. They can be referenced in two ways:

- **By Letter (up to 26 clips):**
  - `x`: The first input clip.
  - `y`: The second input clip.
  - `z`: The third input clip.
  - `a` to `w`: The 4th to 26th clips.

- **By Index (arbitrary number of clips):**
  - `srcN`: Accesses the N-th input clip (0-indexed).
  - `src0` is equivalent to `x`.
  - `src1` is equivalent to `y`.
  - `src26` accesses the 27th clip.

#### **2.2. Numeric Constants**

Literals are pushed directly onto the stack.

- **Standard:** `128`, `3.14`, `-0.5`
- **Hexadecimal:** `0x10` (16), `0xFF` (255).
- **Octal:** `010` (8). Note that invalid octal numbers like `09` are parsed as floats (`9.0`).

**Example:** `x 128 -` (Subtracts 128 from each pixel value of the first clip).

#### **2.3. Special Constants & Coordinates**

These operators push a specific value onto the stack without needing an operand.

- `pi`: The mathematical constant π (approximately 3.14159).
- `N`: The current frame number.
- `X`: The current pixel's column coordinate.
- `Y`: The current pixel's row coordinate.
- `width`: The width of the frame.
- `height`: The height of the frame.

---

### **3. Operators: Manipulating the Stack**

#### **3.1. Arithmetic Operators**

| Operator | Operands | Description |
| :--- | :--- | :--- |
| `+` | 2 | Addition |
| `-` | 2 | Subtraction |
| `*` | 2 | Multiplication |
| `/` | 2 | Division |
| `%` | 2 | C's `fmodf`. `x 1.0 %` gives the fractional part of `x`. |

#### **3.2. Comparison & Logical Operators**

These operators treat any value greater than 0 as `true`. They return `1.0` for true and `0.0` for false.

| Operator | Operands | Description |
| :--- | :--- | :--- |
| `>` | 2 | Greater than |
| `<` | 2 | Less than |
| `=` | 2 | Equal to |
| `>=` | 2 | Greater than or equal to |
| `<=` | 2 | Less than or equal to |
| `and` | 2 | Logical AND |
| `or` | 2 | Logical OR |
| `xor` | 2 | Logical XOR |
| `not` | 1 | Logical NOT |

#### **3.3. Mathematical Functions**

| Function | Operands | Description |
| :--- | :--- | :--- |
| **Power & Root** | | |
| `pow` or `**` | 2 | `x y pow` raises `x` to the power of `y`. `**` is an alias. |
| `sqrt` | 1 | `x sqrt` is the square root of `x`. |
| **Exponential & Logarithmic** | | |
| `exp` | 1 | `x exp` is e^x. |
| `exp2` | 1 | `x exp2` computes 2^x. |
| `log` | 1 | `x log` is the natural logarithm of `x`. |
| `log2` | 1 | `x log2` computes the base-2 logarithm of `x`. |
| `log10` | 1 | `x log10` computes the base-10 logarithm of `x`. |
| **Trigonometric** | | |
| `sin`, `cos`, `tan` | 1 | Sine, Cosine, Tangent. |
| `asin`, `acos`, `atan` | 1 | Arc Sine, Arc Cosine, Arc Tangent. |
| `atan2` | 2 | `y x atan2` computes `atan(y/x)` using signs to find the correct quadrant. |
| `sinh`, `cosh`, `tanh` | 1 | Hyperbolic Sine, Cosine, Tangent. |
| **Rounding** | | |
| `floor` | 1 | Rounds down to the nearest integer. |
| `ceil` | 1 | Rounds up to the nearest integer. |
| `round` | 1 | Rounds to the nearest integer. |
| `trunc` | 1 | Truncates towards zero. |
| **Other** | | |
| `abs` | 1 | `x abs` is the absolute value of `x`. |
| `copysign` | 2 | `x y copysign` returns a value with the magnitude of `x` and the sign of `y`. |
| `fma` | 3 | `a b c fma` computes `(a * b) + c` as a single fused multiply-add. |

#### **3.4. Conditional Operator**

- `?`: A ternary operator. `C A B ?` is equivalent to `C ? A : B`. If `C` is true (non-zero), `A` is evaluated and its result is pushed. Otherwise, `B` is evaluated and its result is pushed.
  - **Example:** `x 128 > x 0 ?` (If the pixel value is greater than 128, keep it, otherwise set it to 0).

#### **3.5. Min/Max & Clamping**

| Operator | Operands | Description |
| :--- | :--- | :--- |
| `max` | 2 | Returns the larger of the two values. |
| `min` | 2 | Returns the smaller of the two values. |
| `clip` or `clamp` | 3 | `x min_val max_val clip` clamps `x` to the range `[min_val, max_val]`. |

**Example:** `x 16 235 clip` clamps the pixel value to the broadcast-safe range [16, 235].

#### **3.6. Bitwise Operators**

These operators truncate floating-point values to integers before the operation.

| Operator | Description |
| :--- | :--- |
| `bitand` | Bitwise AND |
| `bitor` | Bitwise OR |
| `bitxor` | Bitwise XOR |
| `bitnot` | Bitwise NOT |

---

### **4. Advanced Operations**

#### **4.1. Stack Manipulation**

| Operator | Description | Example |
| :--- | :--- | :--- |
| `dup` | (1 operand) Duplicates the top item. | `x dup *` is equivalent to `x 2 pow`. |
| `swap` | (2 operands) Swaps the top two items. | `x y swap -` is equivalent to `y x -`. |
| `dupN` | Duplicates the item N positions from the top. `dup0` is `dup`. | |
| `swapN` | Swaps the top item with the item N positions down. `swap1` is `swap`. | |
| `drop` / `dropN` | Drops the top N items. `drop` is an alias for `drop1`. | `1 2 3 drop2` results in a stack of `[1]`. |
| `sortN` | Sorts the top N items, with the smallest value ending up on top. | `3 1 2 sort3` results in a stack of `[3, 2, 1]`. |

#### **4.2. Named Variables**

- `var!`: Pops the top value from the stack and stores it in a variable named `var`.
- `var@`: Pushes the value of the variable `var` onto the stack.

**Variable Initialization:** All variables are automatically allocated and initialized to `0.0` at the beginning of expression evaluation, regardless of where they are first used in the code. However, the plugin will try its best to report an error if a variable is used before its first store (not guaranteed in complex control flows).

**Example:** `x 2 / my_var! my_var@ my_var@ *` (Calculates `(x/2)^2`).

#### **4.3. Data Access**

##### **Pixel Access**

Access pixels from any clip at absolute or relative coordinates.

- **Static Relative Access:** `clip[relX, relY]`
  - Accesses a pixel relative to the current coordinate. `relX` and `relY` must be constants.
  - **Example:** `y[-1, 0]` accesses the pixel to the immediate left in the second clip (`y`).
  - **Boundary Suffixes:**
    - `:c`: Clamped boundary (edge pixels are repeated). This is the default.
    - `:m`: Mirrored boundary.

- **Dynamic Absolute Access:** `absX absY clip[]`
  - Accesses a pixel at an absolute coordinate, where `absX` and `absY` can be computed by expressions.
  - **Warning:** Dynamic Absolute Access may not be vectorized by the JIT compiler if coordinates are computed at runtime, causing severe performance degradation.
  - **Example:** `X 2 / Y x[]` reads the pixel at half the current X coordinate from the first clip.

> Note: `X 2 + Y 3 - x[]` is equivalent to `x[2,-3]`.

##### **Frame Property Access**

- `clip.PropertyName`: Loads a scalar numerical frame property. `clip` can be any clip identifier (`x`, `y`, `srcN`, etc.).
  - **Example:** `x.PlaneStatsAverage` pushes the value of the `PlaneStatsAverage` property from the first clip's frame properties.
  - If the property is not a scalar numerical property, its value will be its first byte.
  - If the property does not exist, its value will be `NaN` (Not a Number).

#### 4.4. Direct Output Control

These operators provide fine-grained control over pixel output, allowing expressions to write to arbitrary locations or to conditionally skip writing altogether.

| Operator | Operands | Description |
| :--- | :--- | :--- |
| `@[]` | 3 | `val absX absY @[]` pops a value `val` and two coordinates `absX`, `absY`, and writes `val` to the output pixel at `[absX, absY]`. This allows an expression for one pixel to write to another. |
| `^exit^` | 0 | Pushes a special marker value onto the stack. If, after the entire expression is evaluated, this marker is the *only* item remaining on the stack, the default write to the current pixel `[X, Y]` is suppressed. This is useful in expressions that only use `@[]` to write to other pixels. |

**Undefined Behavior Warning:**

The behavior of memory writes is undefined under the following conditions:
- If a pixel receives more than one write from any source (default write or `@[]`) during the processing of a single frame.
- If a pixel is not written to at all.

For example, an expression like `val x y @[] ^exit^` is valid: it writes `val` to `[x, y]` and then suppresses the default write. An expression like `val x y @[]` is **invalid** because it leaves the stack empty; it should be `val x y @[] ^exit^` to be valid if no default write is desired. If a default write is also desired, one could do `val x y @[] some_other_val`.

---

### **5. Control Flow (Turing-Complete Operations)**

The RPN engine is Turing-complete, allowing for arbitrary loops and conditional branching using labels and jumps. This enables complex iterative algorithms directly within an expression.

**Warning:** With great power comes great responsibility. An infinite loop in your expression will cause the filter to hang indefinitely.

#### **5.1. Labels**

-   `#label_name`: Defines a jump destination. The `#` must be the first character of the token. `label_name` can be any string of characters without whitespace. The label definition itself is a no-op during execution; it only marks a position for jumps to target.

#### **5.2. Conditional Jumps**

-   `label_name#`: Performs a conditional jump. The `#` must be the last character of the token.
    -   It pops the top value from the stack.
    -   If the value is greater than `0` (true), execution jumps to the corresponding `#label_name` and continues from the instruction immediately following the label.
    -   If the value is `0` or less (false), execution ignores the jump and continues with the next instruction in the expression.

#### **5.3. Example: Power Calculation via Loop**

The following expression calculates `x` to the power of 4, equivalent to `x 4 pow`, but demonstrates a loop using variables.

**Expression:** `x base! 1 result! 4 counter! #loop result@ base@ * result! counter@ 1 - counter! counter@ loop# result@`

**Execution Trace:**

1.  **Pre-initialization (automatic):**
    -   All variables (`base`, `result`, `counter`) are automatically allocated and initialized to `0.0`.

2.  **Initialization:**
    -   `x base!`: Stores the pixel value of clip `x` into the variable `base`.
    -   `1 result!`: Initializes `result` to 1.
    -   `4 counter!`: Initializes a loop `counter` to 4.
    -   Stack is now empty.

3.  **Loop Start (`#loop`):**
    -   `result@ base@ * result!`: `result` becomes `result * base`.
    -   `counter@ 1 - counter!`: Decrements the `counter`.
    -   `counter@`: Pushes the current value of `counter` onto the stack.
    -   `loop#`: Pops `counter`.
        -   If `counter` was > 0, execution jumps back to `#loop`.
        -   If `counter` was 0, the jump is not taken.

4.  **Termination:**
    -   After the loop finishes (when `counter` reaches 0), the expression continues.
    -   `result@`: The final calculated value is pushed onto the stack, becoming the output for the pixel.