# Infix Syntax Documentation for llvmexpr

## 1. Introduction

The language provides a familiar C-style infix syntax that supports variables, operators, user-defined functions, and special constructs for video processing. It is designed to be expressive and readable, abstracting away the complexities of writing raw postfix expressions.

The infix transpiler supports two distinct execution models, corresponding to VapourSynth's `Expr` and `SingleExpr` functions.

### 1.1. `Expr` Mode: Per-Pixel Execution

This is the traditional model. The script is executed once for *every single pixel* of the output frame. It is ideal for standard image filtering, such as applying a brightness curve, combining clips, or spatial filtering.

- **Key Characteristics:**
    - Operates on a "current pixel" concept, with `$X` and `$Y` coordinates available.
    - The final value assigned to the special `RESULT` variable is implicitly written to the current pixel's location.

### 1.2. `SingleExpr` Mode: Per-Frame Execution

This is a specialized model where the script is executed only *once for the entire frame*. This model is not suitable for general image processing but excels at tasks that require summarizing or distributing data across a frame, such as calculating average brightness or copying frame properties.

- **Key Characteristics:**
    - No "current pixel" concept; `$X` and `$Y` are unavailable.
    - All output must be performed explicitly by writing to absolute pixel coordinates (`store()`) or by writing to frame properties (`set_prop()`).
    - The special `RESULT` variable has no effect on the output frame.

## 2. Preprocessor

The infix2postfix tool includes a preprocessor that runs before parsing. This allows for macro definitions and conditional compilation, providing powerful code organization and configuration capabilities.

### 2.1. Overview

The preprocessor processes directives at the beginning of lines (marked with `@`) before the source code is tokenized and parsed. All preprocessing is completed before semantic analysis begins.

#### Predefined Macros

The following macros are automatically defined:

**Mode-Specific Macros:**
- `__EXPR__` - Defined when compiling in `Expr` mode (per-pixel execution)
- `__SINGLEEXPR__` - Defined when compiling in `SingleExpr` mode (per-frame execution)

These macros can be used with `@ifdef` to write mode-specific or format-specific code.

**Context Macros (when using `infix=1` in VapourSynth):**
- `__WIDTH__` - Output frame width (integer) (sub-pixeling not counted)
- `__HEIGHT__` - Output frame height (integer) (sub-pixeling not counted)
- `__INPUT_NUM__` - Number of input clips (integer)
- `__OUTPUT_BITDEPTH__` - Output bit depth
- `__INPUT_BITDEPTH_0__` - Bit depth of first input clip
- `__INPUT_BITDEPTH_1__` - Bit depth of second input clip (if present)
- `__INPUT_BITDEPTH_N__` - Bit depth of (N+1)-th input clip
- `__SUBSAMPLE_W__` - Horizontal chroma subsampling (0 for 4:4:4, 1 for 4:2:0/4:2:2)
- `__SUBSAMPLE_H__` - Vertical chroma subsampling (0 for 4:4:4/4:2:2, 1 for 4:2:0)
- **Expr only:** `__PLANE_NO__` - Current plane being processed (0, 1, or 2)

### 2.2. Preprocessor Directives

All preprocessor directives start with `@` at the beginning of a line (after any whitespace):

| Directive | Syntax                      | Description                                                                    |
| :-------- | :-------------------------- | :----------------------------------------------------------------------------- |
| `@define` | `@define NAME [value]`      | Define a macro. The value can be a constant expression that will be evaluated. |
| `@undef`  | `@undef NAME`               | Undefine a macro                                                               |
| `@ifdef`  | `@ifdef NAME ... @endif`    | Conditional compilation (if defined)                                           |
| `@ifndef` | `@ifndef NAME ... @endif`   | Conditional compilation (if not defined)                                       |
| `@if`     | `@if expression ... @endif` | Conditional compilation based on an expression's value.                        |
| `@else`   | `@else`                     | Alternative branch (optional)                                                  |
| `@endif`  | `@endif`                    | End conditional block                                                          |
| `@error`  | `@error [message]`          | Emit compilation error if enabled                                              |

### 2.3. Macro Definitions

Define constants or feature flags using the `@define` directive:

**Syntax:** `@define NAME [value]`

- If a value is provided, all occurrences of `NAME` as a token will be replaced with the value
- If the value is a constant expression that can be evaluated at compile time, it will be replaced with the result. For example, `@define SIX (2 * 3)` will cause `SIX` to expand to `6`.
- If no value is provided, `NAME` is just marked as defined (useful for feature flags)
- Macro names must start with a letter or underscore
- Macro names can contain letters, digits, and underscores

### 2.4. Undefining Macros

Remove a macro definition using the `@undef` directive:

**Syntax:** `@undef NAME`

### 2.5. Conditional Compilation

#### @ifdef - If Defined

Include code only when a macro is defined:

**Syntax:**
```
@ifdef NAME
  # Code included if NAME is defined
@else
  # Code included if NAME is not defined
@endif
```

The `@else` block is optional.

**Example:**

```
@define DEBUG_MODE

@ifdef DEBUG_MODE
  # This code will be included
  debug_value = 42;
@else
  # This code will be excluded
  debug_value = 0;
@endif
```

#### @ifndef - If Not Defined

Include code only when a macro is NOT defined:

**Syntax:**
```
@ifndef NAME
  # Code included if NAME is NOT defined
@else
  # Code included if NAME is defined
@endif
```

### 2.6. @if Directive

The `@if` directive allows for conditional compilation based on the result of a constant expression.

**Syntax:**
```
@if expression
  # Code included if the expression evaluates to a non-zero value
@else
  # Optional: code included otherwise
@endif
```

The expression can contain:
- Integer and floating-point literals.
- Previously defined macros, which will be expanded.
- The `defined(MACRO)` operator, which returns 1 if `MACRO` is defined, and 0 otherwise.
- Arithmetic operators: `+`, `-`, `*`, `/`, `%`, `**`
- Comparison operators: `==`, `!=`, `>`, `<`, `>=`, `<=`
- Logical operators: `&&`, `||`, `!`
- Parentheses `()` for grouping.


### 2.7. Error Directive

Use `@error` to emit a compilation error if the directive is encountered in active code:

**Syntax:** `@error [message]`

This is useful for preventing compilation in certain configurations or warning about deprecated features. The `@error` directive only triggers when it's in an active code block.

**Example:**

```
@ifdef __EXPR__
  @error This expr should not be used in Expr mode
@endif
```

### 2.8. Macro Expansion

Macros are expanded during preprocessing:

1. Only complete tokens (identifiers) are replaced
2. Macro names must start with a letter or underscore
3. Macro names can contain letters, digits, and underscores
4. Macro expansion is simple text replacement (no arguments)
5. Macros are not expanded recursively

**Example:**

```
@define MAX 100
@define SQUARE 2 ** 2        # Evaluates to: 4
@define LARGE_VALUE 2 ** 10  # Evaluates to: 1024

x = MAX;        # Expands to: x = 100;
y = MAX + 1;    # Expands to: y = 100 + 1;
z = SQUARE;     # Expands to: z = 4;
```

**Notes:**
- The `**` operator (power/exponentiation) is right-associative, meaning `2 ** 3 ** 2` is evaluated as `2 ** (3 ** 2)` = `2 ** 9` = `512`.
- Power operations always produce floating-point results in the preprocessor.

## 3. Lexical Structure

### 3.1. Comments

Comments begin with a `#` character and extend to the end of the line. They are ignored by the parser.

```
# This is a comment.
a = 1 # This is an inline comment.
```

### 3.2. Whitespace

Whitespace characters (spaces, tabs, newlines) are used to separate tokens. Multiple whitespace characters are treated as a single separator. Newlines are significant as they terminate statements.

### 3.3. Identifiers

Identifiers are used for naming variables and functions.

- **Rules:** Must begin with a letter (`a-z`, `A-Z`) or an underscore (`_`) and can be followed by any number of letters, digits, or underscores. The pattern is `[a-zA-Z_]\w*`.
- **Case-Sensitivity:** Identifiers are case-sensitive. `myVar` and `myvar` are different.
- **Reserved Prefix:** Identifiers starting with `__internal_` are reserved for internal use by the transpiler and must not be used in user code.

### 3.4. Literals

#### Numeric Literals

The language supports several formats for numeric constants:

- **Decimal:** Standard integers (`100`, `-42`) and floating-point numbers (`3.14`, `-0.5`, `1.2e-5`). The decimal separator is always a period (`.`), regardless of the system's locale settings.
- **Hexadecimal:** Prefixed with `0x`. Can include a fractional part and a binary exponent (`p-exponent`). Examples: `0xFF`, `0x1.9p-2`.
- **Octal:** Prefixed with a leading `0`. Example: `0755`.

## 4. Program Structure

A program is a script composed of one or more statements.

### 4.1. Statements

- Statements can be terminated by either a newline or a semicolon (`;`).
- Semicolons are optional if a statement is on its own line.
- To place multiple statements on a single line, you must separate them with semicolons.

#### Assignment Statements

An assignment statement computes the value of an expression and stores it in a variable.

- **Syntax:** `variable = expression`
- The stack must be balanced after an assignment (i.e., the expression must result in exactly one value).

#### Expression Statements

A statement can consist of a single expression, such as a function call that does not return a value. The expression is evaluated, and its result is discarded. The net stack effect of such a statement must be zero.

```
# Assignment statement (newline terminated)
my_var = 10 * 2

# Expression statement (semicolon terminated)
my_func(my_var);

# Multiple statements on one line
a = 1; b = 2; c = a + b
```

### 4.2. Final Result (`Expr` mode)

In `Expr` mode, the final output for each pixel is determined by the value assigned to the special `RESULT` variable. This assignment is typically the last statement in the global scope.

```
# ... calculations ...
RESULT = final_value
```

In `SingleExpr` mode, assigning to `RESULT` has no effect. Output in this mode is handled explicitly via the `store()` and `set_prop()` functions.

## 5. Variables and Constants

### 5.1. Variables

- **Declaration:** Variables are declared implicitly upon their first assignment.
- **Usage:** A variable must be guaranteed to be assigned a value before it is used in an expression. The transpiler performs a static analysis to ensure a variable is defined on all possible execution paths before any use. Referencing a variable that is not guaranteed to be initialized will result in a syntax error.

### 5.2. Constants

Constants represent fixed values and are **always** identified by a `$` prefix. They are treated as literal values and do not require prior assignment.

### 5.3. Built-in Constants

The language provides several built-in constants.

| Constant  | Description                                                                                                                               | Availability |
| :-------- | :---------------------------------------------------------------------------------------------------------------------------------------- | :----------- |
| `$pi`     | The value of π.                                                                                                                           | Both         |
| `$N`      | The current frame number (0-based).                                                                                                       | Both         |
| `$X`      | The current column coordinate (chroma-subsampling counted).                                                                               | `Expr` only  |
| `$Y`      | The current row coordinate (chroma-subsampling counted).                                                                                  | `Expr` only  |
| `$width`  | The width of the video plane. In `Expr`, this is the width of the current plane. In `SingleExpr`, this is the width of the luma plane.    | Both         |
| `$height` | The height of the video plane. In `Expr`, this is the height of the current plane. In `SingleExpr`, this is the height of the luma plane. | Both         |

### 5.4. Source Clips

Source clips are special constants used to reference input video clips. They must be prefixed with a `$`.

Source clips can be referenced in two equivalent ways:

1.  **Single Letters:** The lowercase letters `xyzabcdefghijklmnopqrstuvw` are aliases for `src0` through `src25` respectively:

- `$x` is equivalent to `$src0`
- `$y` is equivalent to `$src1`
- `$z` is equivalent to `$src2`
- `$a` is equivalent to `$src3`
- And so on through `$w` which is equivalent to `$src25`

2.  **`src` Prefixed:** The identifier `src` followed by one or more digits, prefixed with `$` (e.g., `$src0`, `$src1`).

## 6. Operators

Operators are left-associative, except for the unary and ternary operators. The following table lists operators in order of precedence, from lowest to highest.

| Precedence | Operator            | Description              | Arity      | Associativity |
| :--------- | :------------------ | :----------------------- | :--------- | :------------ |
| 1          | `                   | `                        | Logical OR | Binary        | Left |
| 2          | `&&`                | Logical AND              | Binary     | Left          |
| 3          | `                   | `                        | Bitwise OR | Binary        | Left |
|            | `^`                 | Bitwise XOR              | Binary     | Left          |
|            | `&`                 | Bitwise AND              | Binary     | Left          |
|            | `~`                 | Bitwise NOT              | Unary      | Right         |
| 4          | `<`, `<=`, `>` `>=` | Relational               | Binary     | Left          |
| 5          | `==`, `!=`          | Equality                 | Binary     | Left          |
| 6          | `+`, `-`            | Addition, Subtraction    | Binary     | Left          |
| 7          | `*`, `/`            | Multiplication, Division | Binary     | Left          |
|            | `%`                 | Modulus                  | Binary     | Left          |
| 8          | `**`                | Exponentiation (Power)   | Binary     | Left          |
| 9          | `-`                 | Negation                 | Unary      | Right         |
|            | `!`                 | Logical NOT              | Unary      | Right         |
| 10         | `? :`               | Ternary Conditional      | Ternary    | Right         |

Note: Bitwise operators operates on integer values. When operating on floating-point values, operands are first rounded to the nearest integer.
Note: Logical operators treat any value greater than 0 as `true`. They return `1.0` for true and `0.0` for false.

## 7. Data Access

Data access methods for pixels and frame properties differ significantly between `Expr` and `SingleExpr` modes.

### 7.1. Frame Property Access

#### Reading (Both Modes)

Read a property from a clip's frame properties using `clip.propname` syntax.

- **Syntax:** `clip.propname`
- `clip` must be a valid source clip identifier (e.g., `a`, `src1`). The `$` prefix is required. For example: `$a.propname` or `$src1._Matrix`.

#### Writing (`SingleExpr` only)

Write a frame property using the `set_prop()` built-in function.

- **Signature:** `set_prop(property_name, value)`
- `property_name`: Property name as an identifier (not a string).
- `value`: The value to write, which can be the result of an expression.

```
# Write a simple value
set_prop(MyProperty, 123.456);

# Write computed value
w = frame.width[0];
h = frame.height[0];
avg = (dyn(x, 0, 0, 0) + dyn(x, w-1, h-1, 0)) / 2;
set_prop(AverageValue, avg);
```

### 7.2. Pixel Access (`Expr` mode)

In `Expr` mode, you can access pixels from source clips relative to the current pixel or at absolute coordinates.

#### Static Relative Pixel Access

Access a pixel at a fixed, constant offset from the current coordinate (`$X`, `$Y`).

- **Syntax:** `$clip[offsetX, offsetY]` or `$clip[offsetX, offsetY]:m` or `$clip[offsetX, offsetY]:c`
- `$clip` must be a source clip constant.
- `offsetX` and `offsetY` must be integer literals.
- **Boundary Suffixes:**
  - `:c`: Forces clamped boundary (edge pixels are repeated).
  - `:m`: Forces mirrored boundary.
  - If omitted, the filter's global boundary parameter is used.

#### Dynamic Absolute Pixel Access

Access a pixel at a dynamically calculated coordinate using the 3-argument `dyn()` function. See section 7.2 for details.

### 7.3. Pixel and Data I/O (`SingleExpr` mode)

In `SingleExpr` mode, all data I/O is explicit and uses absolute coordinates.

#### Plane-Specific Dimensions

Access the width and height of specific planes using `frame.width[N]` and `frame.height[N]`.

```
w0 = frame.width[0];   # Width of plane 0 (luma)
h1 = frame.height[1];  # Height of plane 1 (chroma U)
```
**Note:** The plane index `N` must be an literal constant.

#### Absolute Pixel Reading

Read pixels from specific coordinates and planes using the 4-argument version of `dyn()`. See section 7.2 for details.

#### Absolute Pixel Writing

Write values to specific output frame locations using the 4-argument version of `store()`. See section 7.2 for details.

## 8. Functions

### 8.1. Function Calls

Functions are called using standard syntax: `functionName(argument1, argument2, ...)`

### 8.2. Built-in Functions

| Function                          | Arity               | Description                                                                                             |
| :-------------------------------- | :------------------ | :------------------------------------------------------------------------------------------------------ |
| `sin`, `cos`, `tan`               | 1                   | Trigonometric sine, cosine, tangent.                                                                    |
| `asin`, `acos`, `atan`            | 1                   | Inverse trigonometric functions.                                                                        |
| `atan2`                           | 2                   | Two-argument arctangent; `atan2(y, x)`.                                                                 |
| `exp`, `exp2`                     | 1                   | Exponential functions `e^x`, `2^x`.                                                                     |
| `log`, `log2`, `log10`            | 1                   | Natural/base-2/base-10 logarithms.                                                                      |
| `sqrt`                            | 1                   | Square root.                                                                                            |
| `abs`                             | 1                   | Absolute value.                                                                                         |
| `sgn`                             | 1                   | Signum function: -1 if x < 0, 0 if x == 0, 1 if x > 0.                                                  |
| `floor`, `ceil`, `round`, `trunc` | 1                   | Rounding family.                                                                                        |
| `min`, `max`                      | 2                   | Minimum/maximum.                                                                                        |
| `copysign`                        | 2                   | Magnitude of first operand, sign of second.                                                             |
| `clamp`                           | 3                   | `clamp(x, lo, hi)`; clamps to `[lo, hi]`.                                                               |
| `fma`                             | 3                   | Fused multiply-add: `(a * b) + c`.                                                                      |
| `nth_N`                           | `M` (where `M ≥ N`) | `nth_3(a, b, c, d)` returns the 3rd smallest of the 4 values.                                           |
| `new`                             | 1                   | Allocates an array. In `Expr` mode, size must be a literal. In `SingleExpr`, size can be an expression. |
| `resize`                          | 1                   | Resizes an array. Alias for `new()` in `SingleExpr` mode only.                                          |

Notes:

- All built-ins are recognized by name and arity; wrong arity will raise a syntax error.
- `nth_N(...)` supports any `N ≥ 1`. It sorts its `M` arguments internally and returns the `N`-th smallest. This compiles to stack ops using `sortM`/`dropK` under the hood.

### 8.3. Mode-Specific Functions

#### `dyn()` - Dynamic Pixel Access

The `dyn()` function has different signatures for `Expr` and `SingleExpr` modes.

- **`Expr` mode:** `dyn($clip, x_expr, y_expr, [boundary_mode])`
  - Accesses a pixel at a dynamically calculated coordinate.
  - `$clip` must be a source clip constant.
  - `x_expr` and `y_expr` can be any valid expressions. If the coordinates are not integers, they will be rounded half to even.
  - `boundary_mode` is an optional integer constant that specifies the boundary handling:
    - `0`: Use the filter's global boundary parameter.
    - `1`: Mirrored boundary.
    - `2`: Clamped boundary (this is the default if the argument is omitted).
  - `dyn($src0, $X + 2, $Y + 3, 0)` is roughly equivalent to `$src0[2, 3]`.

- **`SingleExpr` mode:** `dyn($clip, x, y, plane)`
  - Reads a pixel from an absolute coordinate on a specific plane.
  - **Signature:** `dyn($clip, x, y, plane)`
    - `$clip`: Clip constant (e.g., `$x`, `$y`, `$z`, or `$srcN`).
    - `x`, `y`: Absolute coordinates (can be expressions).
    - `plane`: Plane index (must be an literal constant).
  - **Example:** `val = dyn($x, 100, 200, 0);`

#### `store()` - Pixel Writing

The `store()` function has different signatures for `Expr` and `SingleExpr` modes.

- **`Expr` mode:** `store(x, y, val)`
  - Writes `val` to the output pixel at `[trunc(x), trunc(y)]`.
  - This allows an expression for one pixel to write to another location.

- **`SingleExpr` mode:** `store(x, y, plane, value)`
  - Writes a value to an absolute coordinate on a specific output plane.
  - **Signature:** `store(x, y, plane, value)`
    - `x`, `y`: Absolute coordinates (can be expressions).
    - `plane`: Plane index (must be an literal constant).
    - `value`: Value to write (can be an expression).
  - **Example:** `store(0, 0, 0, 255);`
  - **Warning:** No boundary checking is performed. Writing outside valid frame dimensions causes undefined behavior.

#### `exit()` (`Expr` only)

- **Signature:** `exit()`
- Suppresses the default pixel write to the current coordinate (`$X`, `$Y`). This is useful when an expression only writes to other pixels using `store()`.

### 8.4. User-Defined Functions

- **Definition:**

Functions are defined using the `function` keyword. The function name cannot conflict with any built-in function names. If a function has no type specifier, it is assumed to be `Value`.

```
function functionName(Type1 param1, Type2 param2) {
    # Body of the function
    local_var = param1 * param2
    return local_var + 10
}
```

- **Parameter Types:**
  - `Value`: A standard floating-point value, the most general type.
  - `Clip`: A source clip constant (e.g., `$a`, `$src1`).
  - `Literal`: A literal constant value (numeric literal).
  - `Array`: A reference to an array created with `new()`.

- **Return Statement:** The `return` statement exits a function. It can appear multiple times within a function body and can be used with or without a value.

  - `return expression;`: Exits the function and provides a return value.
  - `return;`: Exits a function that does not return a value (a "void" function).

  The language enforces the following rules for `return` statements:

  1.  **Consistency:** Within a single function, all `return` statements must be consistent. You cannot mix `return <value>;` and `return;`.
  2.  **Completeness for Value-Returning Functions:** If a function returns a value (i.e., contains at least one `return <value>;`), the compiler will verify that **all possible control flow paths** end in a `return` statement. If any path can exit without returning a value, a compile-time error is raised.
  3.  **Flexibility for Void Functions:** If a function does not return a value (it only contains empty `return;` statements or no `return` statements at all), it is not required for all paths to have a `return`. The function can simply "fall off" the end when its last statement is executed.

- **Inlining:** Function calls are effectively inlined at compile time. Recursion is not supported.
- **Nesting:** Function definitions cannot be nested.

### 8.5. Function Overloading

The language supports function overloading, allowing multiple functions to share the same name as long as their parameter lists are different in number or type.

When an overloaded function is called, the compiler selects the best-matching overload based on the provided arguments:

1.  **Exact Match:** An overload with parameter types that exactly match the argument types is always chosen.
2.  **Best Fit (Fewest Conversions):** If no exact match is found, the compiler chooses the overload that requires the minimum number of implicit type conversions (e.g., from `Clip` to `Value`).
3.  **Tie-Breaking:** If multiple overloads have the same number of conversions, the one where the first conversion occurs on a later argument (further to the right in the parameter list) is selected.
4.  **Ambiguity:** If a single best overload cannot be determined from these rules, a compile-time error is raised.

**Example:**
```
# Overloaded function 'process'
function process(Clip c) {
    # processes a clip
    return c * 2 - 1
}

function process(Value v) {
    # processes a numeric value
    return v * 2
}

# Calling the overloads
a = process($x)  # Calls process(Clip c)
b = process(10.0)  # Calls process(Value v)
```

## 9. Scope and Globals

### 9.1. Scopes

- **Global Scope:** Variables defined at the top level of the script.
- **Function Scope:** Each function has its own local scope. This includes its parameters and any variables assigned within its body.
- **Block Scope:** Variables defined inside `if`, `else`, and `while` blocks are local to that specific block. They cannot be accessed outside of the block they are defined in.

**Example of Block Scope:**
```
if ($x > 10) {
    a = 5  # 'a' is defined only inside this block
}

# Attempting to use 'a' here would cause a syntax error
# because 'a' is not defined in the global scope.
# RESULT = a
```
### 9.2. Global Variable Access

By default, functions operate in an isolated scope and cannot access global variables. This behavior can be modified with a global declaration placed immediately before a function definition.

- **`<global.none>`:** (Default) The function cannot access any global variables.
- **`<global.all>`:** The function can access any global variable defined in the script at the time of the function's call.
- **`<global<var1><var2>...>`:** The function can access only the specified global variables.

**Example:**

```
<global<my_global>>
function useGlobal(x) {
    return x + my_global # Accesses global 'my_global'
}

my_global = 100
RESULT = useGlobal(5) # Evaluates to 105
```

- Any global variable a function depends on must be defined before that function is called.

## 10. Control Flow (if/else/while and Labels)

The infix syntax supports structured conditionals, loops, and low-level jumps at both global and function scope. These compile to RPN labels and jumps.

### 10.1. If / Else Blocks

- **Syntax:**

```
if (condition) {
    # statements
} else {
    # statements
}
```

- `else` is optional. Nested blocks are supported.
- The condition is any valid expression; non-zero is treated as true.
- Each block is a sequence of normal statements (assignments or expressions).

### 10.2. Goto and Labels

- **Define a label:** `label_name:` (at the start of a line)
- **Unconditional jump:** `goto label_name`

Constraints and rules:

- Jumps may cross `if/else` braces (C-like). Targets are resolved against any label defined in the global scope of the script.
- The target label must exist somewhere in the script; otherwise a syntax error is raised.
- Labels only mark positions; they do not execute anything by themselves.

### 10.3. While Loops

A `while` loop provides a C-style syntax for repeated execution as long as a condition is true.

- **Syntax:**

```
while (condition) {
    # statements
}
```

- The `condition` is any valid expression; a non-zero result is treated as true.
- The loop continues to execute as long as the condition evaluates to true.
- This is syntactic sugar for a structure using labels and conditional `goto`.

**Example:**

A countdown loop using `while`:

```
counter = 4
while (counter > 0) {
    counter = counter - 1
}
RESULT = counter # will be 0
```

## 11. Arrays

Arrays are collections of values that can be created and accessed by an index. They are especially useful in `SingleExpr` mode for tasks like building lookup tables, histograms, or buffering data for complex calculations.

### 11.1. Declaration and Initialization

Arrays are created by calling the built-in `new()` function and assigning the result to a variable.

- **`Expr` Mode (Fixed-Size Only):**
  - The size must be a numeric literal.
  - `my_lut = new(256);`

- **`SingleExpr` Mode (Fixed or Dynamic Size):**
  - Fixed size: `my_array = new(100);`
  - Dynamic size: The size can be any expression that results in a value.
    ```
    # Create an array to hold a value for each pixel
    frame_size = $width * $height;
    pixel_buffer = new(frame_size);
    ```
  - The `resize()` function can be used to change an array's size.
    ```
    pixel_buffer = resize(new_size);
    ```
    Note: An array must be allocated using `new()` before it can be resized.

### 11.2. Element Access

C-style square brackets `[]` are used to read from and write to array elements.

- **Writing:** `my_array[index_expression] = value_expression;`
- **Reading:** `value = my_array[index_expression];`

**Syntax Disambiguation:** Array access is distinguished from relative pixel access by the number of arguments in the brackets.
- `my_array[i]` (1 argument) is an array access.
- `$x[0, 1]` (2 arguments) is a pixel access.

**Example (`SingleExpr` mode):**
```
# Create and populate a lookup table
lut = new(256);
i = 0;
while (i < 256) {
    lut[i] = i * i; # Store the square of the index
    i = i + 1;
}

# In a later part of the script, use the LUT
some_value = dyn($x, 10, 10, 0);
result_from_lut = lut[some_value]; # Read from the LUT
```

### 11.3. Arrays as Function Parameters

Arrays can be passed to user-defined functions by specifying the `Array` type in the function signature. The array is passed by reference, meaning the function can modify the original array.

**Example:**
```
# Function to fill an array with a value
function fill_array(Array a, Value fill_val, Value len) {
    i = 0;
    while (i < len) {
        a[i] = fill_val;
        i = i + 1;
    }
}

# Usage
my_data = new(10);
fill_array(my_data, 3.14, 10); # my_data is now filled with 3.14
```

### 11.4. Scope and Lifetime

The lifetime of an array depends on the execution mode:

- **`Expr` mode:** Arrays are temporary and exist only for the evaluation of a single pixel. They are re-created and destroyed for each pixel and cannot be used to share data between pixels.
- **`SingleExpr` mode:** Arrays are persistent and exist for the lifetime of the filter instance. Their contents are preserved between frame evaluations. **Warning:** Due to VapourSynth's parallel processing, you should not rely on arrays for communication *between* frames, as frame evaluation order is not guaranteed.

### 11.5. Safety

**Warning:** The language does not perform runtime bounds checking on array access. Accessing an index outside the allocated size (e.g., `my_array[-1]` or `my_array[size]`) will result in **undefined behavior**, which may include crashes or memory corruption. It is the script author's responsibility to ensure all access is within bounds.