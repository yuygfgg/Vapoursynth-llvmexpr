"""
Copyright (C) 2025 yuygfgg

This file is part of Vapoursynth-llvmexpr.

Vapoursynth-llvmexpr is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Vapoursynth-llvmexpr is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Vapoursynth-llvmexpr.  If not, see <https://www.gnu.org/licenses/>.
"""

import subprocess
import tempfile
import os
from pathlib import Path

BUILDDIR = Path(__file__).parent.parent / "builddir"
INFIX2POSTFIX = BUILDDIR / "infix2postfix"

if not INFIX2POSTFIX.exists():
    BUILDDIR = Path(__file__).parent.parent / "build"
    INFIX2POSTFIX = BUILDDIR / "infix2postfix"


def run_infix2postfix(infix_code: str, mode: str = "expr") -> tuple[bool, str]:
    with tempfile.NamedTemporaryFile(mode="w", suffix=".infix", delete=False) as f_in:
        f_in.write(infix_code)
        input_file = f_in.name

    with tempfile.NamedTemporaryFile(mode="r", suffix=".expr", delete=False) as f_out:
        output_file = f_out.name

    try:
        result = subprocess.run(
            [str(INFIX2POSTFIX), input_file, "-m", mode, "-o", output_file],
            capture_output=True,
            text=True,
            timeout=5,
        )

        if result.returncode == 0:
            with open(output_file, "r") as f:
                return True, f.read().strip()
        else:
            return False, result.stderr
    except subprocess.TimeoutExpired:
        return False, "Timeout"
    finally:
        try:
            os.unlink(input_file)
            os.unlink(output_file)
        except Exception:
            pass


class TestSingleExprMode:
    """Test SingleExpr-specific features."""

    def test_frame_width_height(self):
        """Test frame.width[N] and frame.height[N] syntax."""
        infix = """
w0 = frame.width[0]
h0 = frame.height[0]
w1 = frame.width[1]
"""
        success, output = run_infix2postfix(infix, "single")
        assert success, f"Failed to convert: {output}"
        assert "width^0" in output
        assert "height^0" in output
        assert "width^1" in output
        assert "w0!" in output
        assert "h0!" in output

    def test_dyn_four_args(self):
        """Test dyn() with 4 arguments (clip, x, y, plane)."""
        infix = """
val = dyn($x, 100, 200, 0)
val2 = dyn($src0, 50, 50, 1)
"""
        success, output = run_infix2postfix(infix, "single")
        assert success, f"Failed to convert: {output}"
        assert "100 200 x^0[]" in output
        assert "50 50 src0^1[]" in output

    def test_store_four_args(self):
        """Test store() with 4 arguments (x, y, plane, value)."""
        infix = """
store(10, 20, 0, 255)
store(5, 5, 1, 128)
"""
        success, output = run_infix2postfix(infix, "single")
        assert success, f"Failed to convert: {output}"
        assert "255 10 20 @[]^0" in output
        assert "128 5 5 @[]^1" in output

    def test_set_prop(self):
        """Test set_prop() for frame property writing."""
        infix = """
set_prop(MyProperty, 123.456)
set_prop(TestProp, 42)
"""
        success, output = run_infix2postfix(infix, "single")
        assert success, f"Failed to convert: {output}"
        assert "123.456 MyProperty$" in output
        assert "42 TestProp$" in output

    def test_set_prop_with_variable(self):
        """Test set_prop() with a variable as the value."""
        infix = """
my_value = 42
set_prop(MyProperty, my_value)
"""
        success, output = run_infix2postfix(infix, "single")
        assert success, f"Failed to convert: {output}"
        assert "42 my_value!" in output
        assert "my_value@ MyProperty$" in output

    def test_complex_single_expr(self):
        """Test a complex SingleExpr script."""
        infix = """
# Get frame dimensions
w = frame.width[0]
h = frame.height[0]

# Read corner pixels
top_left = dyn($x, 0, 0, 0)
bottom_right = dyn($x, w - 1, h - 1, 0)

# Write to center
center_val = 255
store(w / 2, h / 2, 0, center_val)

# Save statistics
set_prop(TopLeft, top_left)
set_prop(BottomRight, bottom_right)
"""
        success, output = run_infix2postfix(infix, "single")
        assert success, f"Failed to convert: {output}"
        assert "width^0" in output
        assert "height^0" in output
        assert "x^0[]" in output
        assert "@[]^0" in output
        assert "TopLeft$" in output
        assert "BottomRight$" in output


class TestExprMode:
    """Test Expr-specific features."""

    def test_xy_coordinates(self):
        """Test X and Y coordinate access."""
        infix = """
coord_sum = $X + $Y
RESULT = coord_sum
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "X Y +" in output
        assert "RESULT!" in output
        assert "RESULT@" in output

    def test_static_relative_pixel_access(self):
        """Test static relative pixel access like $x[1, 0]."""
        infix = """
right = $x[1, 0]
left = $x[-1, 0]
top = $y[0, -1]
RESULT = right + left + top
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "x[1,0]" in output
        assert "x[-1,0]" in output
        assert "y[0,-1]" in output

    def test_dyn_three_args(self):
        """Test dyn() with 3 arguments (clip, x, y) in Expr mode."""
        infix = """
val = dyn($x, $X * 2, $Y / 2)
RESULT = val
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "X 2 *" in output
        assert "Y 2 /" in output
        assert "x[]" in output

    def test_store_three_args(self):
        """Test store() with 3 arguments (x, y, value) in Expr mode."""
        infix = """
store($X + 1, $Y + 1, 128)
RESULT = $X
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "128" in output
        assert "X 1 +" in output
        assert "Y 1 +" in output
        assert "@[]" in output

    def test_complex_expr(self):
        """Test a complex Expr script with multiple features."""
        infix = """
# Neighbor average
top = $x[0, -1]
bottom = $x[0, 1]
left = $x[-1, 0]
right = $x[1, 0]
avg = (top + bottom + left + right) / 4

# Apply threshold
threshold = 128
result = avg > threshold

RESULT = -result
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "x[0,-1]" in output
        assert "x[0,1]" in output
        assert "x[-1,0]" in output
        assert "x[1,0]" in output
        assert "4 /" in output
        assert ">" in output
        assert "neg" in output


class TestModeChecking:
    """Test mode-specific restrictions."""

    def test_xy_in_single_mode_error(self):
        """Test that using X/Y in SingleExpr mode causes an error."""
        infix = """
result = $X + $Y
"""
        success, output = run_infix2postfix(infix, "single")
        assert not success, "Should have failed"
        assert "only available in Expr mode" in output

    def test_frame_width_in_expr_mode_error(self):
        """Test that using frame.width[N] in Expr mode causes an error."""
        infix = """
w = frame.width[0]
RESULT = w
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success, "Should have failed"
        assert "only available in SingleExpr mode" in output

    def test_dyn_wrong_args_count_single(self):
        """Test that dyn() with 3 args in SingleExpr mode causes an error."""
        infix = """
val = dyn($x, 10, 20)
"""
        success, output = run_infix2postfix(infix, "single")
        assert not success, "Should have failed"

    def test_store_wrong_args_count_expr(self):
        """Test that store() with 4 args in Expr mode causes an error."""
        infix = """
store(10, 20, 0, 255)
RESULT = $X
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success, "Should have failed"

    def test_store_wrong_args_count_single(self):
        """Test that store() with 3 args in SingleExpr mode causes an error."""
        infix = """
store(10, 20, 255)
"""
        success, output = run_infix2postfix(infix, "single")
        assert not success, "Should have failed"

    def test_set_prop_in_expr_mode_error(self):
        """Test that set_prop() in Expr mode causes an error."""
        infix = """
set_prop(MyProp, 123)
RESULT = $X
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success, "Should have failed"

    def test_exit_in_single_mode_error(self):
        """Test that exit() in SingleExpr mode causes an error."""
        infix = """
exit()
"""
        success, output = run_infix2postfix(infix, "single")
        assert not success, "Should have failed"

    def test_static_pixel_access_in_single_mode_error(self):
        """Test that static pixel access $x[1,0] in SingleExpr mode causes an error."""
        infix = """
val = $x[1, 0]
"""
        success, output = run_infix2postfix(infix, "single")
        assert not success, "Should have failed"


class TestBasicSyntax:
    """Test basic infix syntax conversion."""

    def test_arithmetic_operators(self):
        """Test basic arithmetic operators."""
        infix = """
a = 10 + 20
b = 30 - 15
c = 5 * 6
d = 100 / 4
e = a + b + c + d
RESULT = atan(e)
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "10 20 +" in output
        assert "30 15 -" in output
        assert "5 6 *" in output
        assert "100 4 /" in output
        assert "atan" in output

    def test_comparison_operators(self):
        """Test comparison operators."""
        infix = """
a = 10 > 5
b = 20 < 30
c = 15 >= 15
d = 10 <= 11
e = 5 == 5
RESULT = a
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "10 5 >" in output
        assert "20 30 <" in output
        assert "15 15 >=" in output
        assert "10 11 <=" in output
        assert "5 5 =" in output

    def test_function_calls(self):
        """Test function calls."""
        infix = """
a = sqrt(16)
b = abs(-10)
c = max(5, 10)
d = min(20, 15)
RESULT = a + b
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "16 sqrt" in output
        assert "10 neg abs" in output or "-10 abs" in output
        assert "5 10 max" in output
        assert "20 15 min" in output

    def test_ternary_operator(self):
        """Test ternary operator."""
        infix = """
condition = $X > 100
result = condition ? 255 : 0
RESULT = result
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "?" in output

    def test_control_flow(self):
        """Test if-else control flow."""
        infix = """
x_coord = $X
val = -1
if (x_coord > 50) {
    val = 255
} else {
    val = 0
}
RESULT = val
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "#" in output

    def test_if_else_true_value(self):
        """Test if-else with true value."""
        infix = """
a = -1
if (-1) {
    a = 255
} else {
    a = 0
}
RESULT = a
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert (
            output.strip()
            == "-1 a! -1 0 = __internal_else_0# 255 a! 1 __internal_endif_1# #__internal_else_0 0 a! #__internal_endif_1 a@ RESULT! RESULT@"
        )

    def test_while_loop(self):
        """Test while loop."""
        infix = """
counter = 5
sum = 0
while (counter > 0) {
    sum = sum + counter
    counter = counter - 1
}
RESULT = sum
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "#" in output

    def test_function_variable_scope(self):
        """Test function variable scope."""
        infix = """
function test_function(x) {
    return x + var
}
var = 1
RESULT = test_function(10)
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success, f"Should fail, got {output}"

    def test_if_else_scope(self):
        """Test that a variable defined in an if-block is not accessible outside."""
        # var is not defined for outer scope
        infix = """
if (1) {
    var = 1
}
RESULT = var
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success, f"Should fail, got {output}"
        assert "Variable 'var' is used before being defined" in output


class TestEdgeCases:
    """Test edge cases and error handling."""

    def test_empty_input(self):
        """Test empty input."""
        infix = ""
        success, output = run_infix2postfix(infix, "expr")
        assert not success, "Empty input should fail"

    def test_missing_result_expr_mode(self):
        """Test that Expr mode requires RESULT assignment."""
        infix = """
a = 10 + 20
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success, "Should fail without RESULT"
        assert "RESULT" in output

    def test_comments(self):
        """Test that comments are handled correctly."""
        infix = """
# This is a comment
a = 10 # inline comment
# Another comment
RESULT = a
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "10 a!" in output

    def test_clip_identifiers(self):
        """Test various clip identifiers."""
        infix = """
val1 = $x
val2 = $y
val3 = $z
val4 = $src0
val5 = $src10
RESULT = val1
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert " x " in output or output.startswith("x ")
        assert " y " in output
        assert " z " in output
        assert "src0" in output
        assert "src10" in output

    def test_builtin_constants(self):
        """Test built-in constants."""
        infix = """
pi_val = $pi
frame_num = $N
w = $width
h = $height
RESULT = pi_val
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "pi" in output
        assert "N" in output
        assert "width" in output
        assert "height" in output

    def test_undefined_function(self):
        """Test undefined function."""
        infix = """
RESULT = atan2($x)
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success, "Should fail"

    def test_conflict_with_builtin_function(self):
        """Test user defined function conflicts with built-in function."""
        infix = """
function sin(a, b, c, d) {
    return a + b + c + d
}
RESULT = sin(4, 3, 2, 1)
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success, "Should fail"
        assert "conflicts with a built-in function" in output


class TestFunctions:
    def test_untyped_function(self):
        """Test that untyped function parameters are allowed and default to Value."""
        infix = """
function add(x, y) {
    return x + y
}
RESULT = add(10, 20)
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert (
            output.strip()
            == "10 __internal_func_add_x! 20 __internal_func_add_y! __internal_func_add_x@ __internal_func_add_y@ + RESULT! RESULT@"
        )

    def test_typed_function_and_global(self):
        """Test typed function and global declaration."""
        infix = """
<global<global_var>>
function test_function(Value x) {
    d = x + global_var
    return d
}
global_var = 1
RESULT = test_function(10)
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "1 global_var!" in output
        assert "10 __internal_func_test_function_x!" in output
        assert "__internal_func_test_function_x@ global_var@ +" in output
        assert "RESULT!" in output

    def test_typed_function_param_substitution(self):
        """Test typed function parameter substitution."""
        infix = """
function test_function(Value x, Clip clip, Clip a) {
    return x + x + clip.prop + dyn(a, 1, 1)
}
RESULT = test_function(10, $src0, $x)
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "src0.prop" in output
        assert "1 1 x[]:c" in output
        assert "$" not in output
        assert "a[]" not in output

    def test_nth_N_function(self):
        """Test nth_N function."""
        infix = """
RESULT = nth_3(4, 3, 2, 1)
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "4 3 2 1 sort4 drop2 swap1 drop1 RESULT! RESULT@" in output

    def test_nth_N_function_with_invalid_name(self):
        """Test nth_N function with invalid name."""
        infix = """
RESULT = nth_0(4, 3, 2, 1)
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success, "Should fail"
        assert "Invalid nth_N function name 'nth_0'" in output


class TestFunctionOverloading:
    def test_overload_by_type(self):
        """Test function overloading based on parameter types."""
        infix = """
function process(Clip c) {
    return c[1,1] * 2
}
function process(Value v) {
    return v * 2
}
RESULT = process($x) + process(10.0)
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert (
            "x[1,1] 2 * 10.0 __internal_func_process_v! __internal_func_process_v@ 2 * + RESULT! RESULT@"
            in output
        )

    def test_overload_by_arity(self):
        """Test function overloading based on number of parameters."""
        infix = """
function f(Value x) { return x }
function f(Value x, Value y) { return x + y }
RESULT = f(1) + f(2, 3)
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert (
            "1 __internal_func_f_x! __internal_func_f_x@ 2 __internal_func_f_x! 3 __internal_func_f_y! __internal_func_f_x@ __internal_func_f_y@ + + RESULT! RESULT@"
            in output
        )

    def test_overload_best_fit(self):
        """Test that the best-fit overload (fewest conversions) is chosen."""
        infix = """
function best(Value v, Clip c) { return 1 }
function best(Value v, Value v2) { return 2 }
RESULT = best(1.0, 2.0)
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert (
            output.strip()
            == "1.0 __internal_func_best_v! 2.0 __internal_func_best_v2! 2 RESULT! RESULT@"
        )

    def test_overload_tie_break(self):
        """Test overload resolution tie-breaking rule."""
        infix = """
function tie(Clip c, Value v) { return 1 }
function tie(Value v, Clip c) { return 2 }
RESULT = tie($x, $y)
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert output.strip() == "y __internal_func_tie_v! 1 RESULT! RESULT@"

    def test_duplicate_function_error(self):
        """Test that defining a function with the same signature twice errors."""
        infix = """
function dup(Value a) {
    return a + 1
}
function dup(Value b) {
    return b / 2
}
RESULT = dup(4)
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success, "Should fail due to duplicate function definition"
        assert "Duplicate function signature" in output

    def test_untyped_duplicate_function_error(self):
        """Test that defining an untyped function with the same signature twice errors."""
        infix = """
function dup(a) {
    return a + 1
}
function dup(b) {
    return b / 2
}
RESULT = dup(4)
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success, "Should fail due to duplicate function definition"
        assert "Duplicate function signature" in output


class TestStatementTermination:
    """Tests for statement termination rules (newlines and semicolons)."""

    def test_semicolon_termination(self):
        """Test that statements can be terminated by semicolons."""
        infix = """
a = 10;
b = 20;
RESULT = a + b;
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "10 a!" in output
        assert "20 b!" in output
        assert "a@ b@ + RESULT!" in output

    def test_multiple_statements_on_one_line(self):
        """Test that multiple statements can be on one line separated by semicolons."""
        infix = "a = 1; b = 2; c = a + b; RESULT = c"
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "1 a! 2 b! a@ b@ + c! c@ RESULT!" in output

    def test_mixed_termination(self):
        """Test a mix of newline and semicolon terminations."""
        infix = """
a = 1;
b = 2
c = a + b; RESULT = c
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "1 a! 2 b! a@ b@ + c! c@ RESULT!" in output

    def test_empty_statements(self):
        """Test that empty statements (extra semicolons) are allowed."""
        infix = """
a = 1;
;
b = 2;;
RESULT = a + b
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "1 a! 2 b! a@ b@ + RESULT!" in output

    def test_missing_terminator_error(self):
        """Test that two statements on one line without a separator is an error."""
        infix = "a = 1 b = 2"
        success, output = run_infix2postfix(infix, "expr")
        assert not success, "Should have failed"
        assert "Expected newline or semicolon after statement" in output

    def test_junk_after_statement_error(self):
        """Test that junk after a statement on the same line is an error."""
        infix = "a = 1 2"
        success, output = run_infix2postfix(infix, "expr")
        assert not success, "Should have failed"
        assert "Expected newline or semicolon after statement" in output

    def test_invalid_case(self):
        """Test invalid case."""
        infix = "RESULT = 1 v = 5 + 8"
        success, output = run_infix2postfix(infix, "expr")
        assert not success, "Should have failed"
        assert "Expected newline or semicolon after statement" in output


class TestSyntaxAndScopeValidation:
    """Tests for new brace, scope and RESULT definition rules."""

    def test_if_without_braces_fails(self):
        """Test that an if statement without braces is an error."""
        infix = "if (1) RESULT = 1"
        success, output = run_infix2postfix(infix, "expr")
        assert not success
        assert "must be a block statement" in output

    def test_while_without_braces_fails(self):
        """Test that a while statement without braces is an error."""
        infix = """
RESULT = 1
while (0) RESULT = 2
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success
        assert "must be a block statement" in output

    def test_else_without_braces_fails(self):
        """Test that an else statement without braces is an error."""
        infix = """
RESULT = 0
if (0) {
    RESULT = 1
} else RESULT = 2
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success
        assert "must be a block statement" in output

    def test_else_if_is_allowed(self):
        """Test that 'else if' chain is still allowed."""
        infix = """
RESULT = 0
if (0) {
    RESULT = 1
} else if (1) {
    RESULT = 2
} else {
    RESULT = 3
}
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, output

    def test_variable_leaks_from_while_fails(self):
        """Test that a variable defined in a while loop is not accessible outside."""
        infix = """
while (0) {
    a = 10
}
RESULT = a
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success
        assert "Variable 'a' is used before being defined" in output

    def test_outer_scope_var_modification_is_ok(self):
        """Test that modifying an outer scope variable inside a block is allowed."""
        infix = """
a = 1
if (1) {
    a = 2
}
RESULT = a
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, output

    def test_result_only_in_block_fails(self):
        """Test that RESULT must be defined in the global scope in expr mode."""
        infix = """
if (1) {
    RESULT = 1
}
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success
        assert "'RESULT' must be defined in the global scope" in output

    def test_result_in_global_and_block_is_ok(self):
        """Test that RESULT can be defined globally and modified in a block."""
        infix = """
RESULT = 0
if (1) {
    RESULT = 1
}
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, output

    def test_standalone_block_fails(self):
        """Test that a standalone block is not allowed."""
        infix = """
RESULT = 0
{
    a = 1
}
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success
        assert "Standalone blocks are not allowed" in output


class TestArrays:
    """Test array functionality."""

    def test_array_creation_expr_mode(self):
        """Test array creation with literal size in Expr mode."""
        infix = """
arr = new(10)
arr[0] = 255
RESULT = arr[0]
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "arr{}^10" in output
        assert "255 0 arr{}!" in output
        assert "0 arr{}@" in output

    def test_array_creation_single_mode_literal(self):
        """Test array creation with literal size in SingleExpr mode."""
        infix = """
buffer = new(256)
buffer[0] = 128
val = buffer[0]
"""
        success, output = run_infix2postfix(infix, "single")
        assert success, f"Failed to convert: {output}"
        assert "256 buffer{}^" in output
        assert "128 0 buffer{}!" in output
        assert "0 buffer{}@" in output

    def test_array_creation_single_mode_dynamic(self):
        """Test array creation with dynamic size in SingleExpr mode."""
        infix = """
size = $width * $height
buffer = new(size)
buffer[0] = 255
"""
        success, output = run_infix2postfix(infix, "single")
        assert success, f"Failed to convert: {output}"
        assert "width height * size!" in output
        assert "size@ buffer{}^" in output
        assert "255 0 buffer{}!" in output

    def test_array_resize_single_mode(self):
        """Test array resize in SingleExpr mode."""
        infix = """
arr = new(10)
arr[0] = 100
arr = resize(20)
arr[10] = 200
"""
        success, output = run_infix2postfix(infix, "single")
        assert success, f"Failed to convert: {output}"
        assert "10 arr{}^" in output
        assert "20 arr{}^" in output
        assert "100 0 arr{}!" in output
        assert "200 10 arr{}!" in output

    def test_array_element_access_in_expression(self):
        """Test array element access in expressions."""
        infix = """
arr = new(5)
arr[0] = 10
arr[1] = 20
arr[2] = 30
result = arr[0] + arr[1] * arr[2]
RESULT = result
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "0 arr{}@" in output
        assert "1 arr{}@" in output
        assert "2 arr{}@" in output
        assert "+" in output
        assert "*" in output

    def test_array_with_loop(self):
        """Test array usage in a loop."""
        infix = """
arr = new(5)
i = 0
while (i < 5) {
    arr[i] = i * 10
    i = i + 1
}
RESULT = arr[3]
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "arr{}^5" in output
        assert "i@ arr{}!" in output
        assert "3 arr{}@" in output

    def test_array_as_function_parameter(self):
        """Test passing array to function."""
        infix = """
function fill_array(Array a, Value size, Value fill_val) {
    i = 0
    while (i < size) {
        a[i] = fill_val
        i = i + 1
    }
}

my_array = new(3)
fill_array(my_array, 3, 100)
RESULT = my_array[0] + my_array[1] + my_array[2]
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "my_array{}^3" in output
        assert "my_array{}!" in output
        assert "my_array{}@" in output

    def test_array_function_modifies_original(self):
        """Test that array modifications in function affect original."""
        infix = """
function set_first(Array a, Value val) {
    a[0] = val
}

arr = new(1)
arr[0] = 10
set_first(arr, 99)
RESULT = arr[0]
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "arr{}^1" in output
        # The function stores val parameter then writes to array
        assert "__internal_func_set_first_val@" in output
        assert "0 arr{}!" in output

    def test_array_with_expression_index(self):
        """Test array access with computed index."""
        infix = """
arr = new(10)
idx = 3 + 2
arr[idx] = 42
val = arr[5]
RESULT = val
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "3 2 + idx!" in output
        assert "42 idx@ arr{}!" in output
        assert "5 arr{}@" in output

    def test_multiple_arrays(self):
        """Test using multiple arrays."""
        infix = """
arr1 = new(2)
arr2 = new(2)
arr1[0] = 10
arr1[1] = 20
arr2[0] = arr1[0] * 2
arr2[1] = arr1[1] * 2
RESULT = arr2[0] + arr2[1]
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "arr1{}^2" in output
        assert "arr2{}^2" in output
        assert "0 arr1{}@" in output
        assert "0 arr2{}!" in output

    def test_array_in_single_expr_with_store(self):
        """Test array in SingleExpr mode with store operations."""
        infix = """
lut = new(256)
i = 0
while (i < 256) {
    lut[i] = i * i
    i = i + 1
}
val = lut[128]
store(0, 0, 0, val)
"""
        success, output = run_infix2postfix(infix, "single")
        assert success, f"Failed to convert: {output}"
        assert "256 lut{}^" in output
        assert "i@ lut{}!" in output
        assert "128 lut{}@" in output

    def test_array_distinguish_from_pixel_access(self):
        """Test that array access is distinguished from pixel access."""
        infix = """
arr = new(5)
arr[3] = $x[1, 0]
RESULT = arr[3]
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "arr{}^5" in output
        assert "x[1,0]" in output  # Pixel access
        assert "3 arr{}!" in output  # Array write
        assert "3 arr{}@" in output  # Array read

    # Error cases

    def test_array_expr_mode_requires_literal_size(self):
        """Test that Expr mode requires literal size for new()."""
        infix = """
size = 10
arr = new(size)
RESULT = 1
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success, "Should fail with non-literal size in Expr mode"
        assert "array size must be a numeric literal" in output

    def test_array_resize_not_in_expr_mode(self):
        """Test that resize() is not available in Expr mode."""
        infix = """
arr = new(10)
arr = resize(20)
RESULT = 1
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success, "Should fail using resize() in Expr mode"
        assert "resize() is only available in SingleExpr mode" in output

    def test_array_cannot_be_reallocated_with_new(self):
        """Test that an array cannot be allocated twice with new()."""
        infix = """
arr = new(10)
arr = new(20)
RESULT = arr[5]
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success, "Should fail reallocating array with new()"
        assert "Cannot reallocate array" in output

    def test_array_can_be_resized_in_single_mode(self):
        """Test that an array can be resized with resize() in SingleExpr mode."""
        infix = """
arr = new(10)
arr[0] = 100
arr = resize(20)
arr[15] = 200
"""
        success, output = run_infix2postfix(infix, "single")
        assert success, f"Failed to convert: {output}"
        assert "10 arr{}^" in output
        assert "20 arr{}^" in output

    def test_resize_requires_prior_allocation(self):
        """Test that resize() requires the array to be allocated first."""
        infix = """
arr = resize(10)
"""
        success, output = run_infix2postfix(infix, "single")
        assert not success, "Should fail using resize() on unallocated variable"
        assert "is undefined or not an array." in output

    def test_resize_on_non_array_variable(self):
        """Test that resize() cannot be used on non-array variables."""
        infix = """
val = 42
val = resize(10)
"""
        success, output = run_infix2postfix(infix, "single")
        assert not success, "Should fail using resize() on non-array"
        assert "not an array" in output

    def test_array_cannot_be_reassigned_to_value(self):
        """Test that an array variable cannot be reassigned to a non-array value."""
        infix = """
arr = new(10)
arr = 42
"""
        success, output = run_infix2postfix(infix, "single")
        assert not success, "Should fail reassigning array to value"
        assert "is an array and cannot be reassigned" in output

    def test_array_cannot_be_reassigned_to_value_expr_mode(self):
        """Test that an array variable cannot be reassigned to a value in Expr mode."""
        infix = """
arr = new(10)
arr = 42
RESULT = arr
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success, "Should fail reassigning array to value"
        assert "is an array and cannot be reassigned" in output

    def test_array_cannot_be_assigned_to_variable(self):
        """Test that arrays cannot be assigned to other variables."""
        infix = """
arr1 = new(10)
arr2 = arr1
RESULT = 1
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success, "Should fail assigning array to variable"
        assert "Cannot assign array" in output

    def test_array_cannot_be_assigned_in_function(self):
        """Test that arrays cannot be assigned to variables in functions."""
        infix = """
function process(Array a) {
    tmp = a
    return tmp[0]
}
arr = new(5)
RESULT = process(arr)
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success, "Should fail assigning array parameter to variable"
        assert "Cannot assign array" in output

    def test_function_cannot_return_array(self):
        """Test that functions cannot return arrays."""
        infix = """
function get_array() {
    arr = new(5)
    return arr
}
RESULT = get_array()
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success, "Should fail returning array from function"
        assert "Functions cannot return arrays" in output

    def test_function_cannot_return_array_parameter(self):
        """Test that functions cannot return array parameters."""
        infix = """
function return_param(Array a) {
    return a
}
arr = new(5)
RESULT = return_param(arr)
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success, "Should fail returning array parameter"
        assert "Functions cannot return arrays" in output

    def test_array_access_undefined_variable(self):
        """Test accessing undefined array."""
        infix = """
val = undefined_array[0]
RESULT = val
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success, "Should fail accessing undefined array"
        assert "is used before being defined" in output

    def test_array_access_on_non_array(self):
        """Test array access on non-array variable."""
        infix = """
not_array = 42
val = not_array[0]
RESULT = val
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success, "Should fail accessing non-array as array"
        assert "is not an array" in output

    def test_array_assign_to_non_array(self):
        """Test array assignment to non-array variable."""
        infix = """
not_array = 42
not_array[0] = 10
RESULT = not_array
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success, "Should fail assigning to non-array as array"
        assert "is not an array" in output

    def test_array_function_param_type_mismatch(self):
        """Test passing non-array to Array parameter."""
        infix = """
function process(Array a) {
    return a[0]
}
val = 42
RESULT = process(val)
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success, "Should fail passing non-array to Array parameter"

    def test_array_function_param_requires_variable(self):
        """Test that Array parameter requires a variable, not expression."""
        infix = """
function process(Array a) {
    return a[0]
}
arr = new(5)
arr[0] = 10
RESULT = process(arr[0])
"""
        success, output = run_infix2postfix(infix, "expr")
        assert (
            not success
        ), "Should fail passing non-variable expression to Array parameter"

    def test_array_overloading_by_type(self):
        """Test function overloading with Array type."""
        infix = """
function process(Array a) {
    return a[0]
}
function process(Value v) {
    return v * 2
}
arr = new(2)
arr[0] = 10
result1 = process(arr)
result2 = process(20)
RESULT = result1 + result2
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "arr{}^2" in output
        assert "0 arr{}@" in output
        assert "__internal_func_process_v@ 2 *" in output

    def test_array_complex_scenario_expr(self):
        """Test complex array scenario in Expr mode."""
        infix = """
lut = new(256)
i = 0
while (i < 256) {
    lut[i] = sqrt(i)
    i = i + 1
}

pixel = $x
idx = pixel > 255 ? 255 : pixel
RESULT = lut[idx]
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "lut{}^256" in output
        assert "sqrt" in output
        assert "?" in output

    def test_array_complex_scenario_single(self):
        """Test complex array scenario in SingleExpr mode."""
        infix = """
w = frame.width[0]
h = frame.height[0]
histogram = new(256)

i = 0
while (i < 256) {
    histogram[i] = 0
    i = i + 1
}

val1 = dyn($x, 0, 0, 0)
val2 = dyn($x, w - 1, 0, 0)
val3 = dyn($x, 0, h - 1, 0)
val4 = dyn($x, w - 1, h - 1, 0)

set_prop(Histogram0, histogram[0])
set_prop(Histogram255, histogram[255])
"""
        success, output = run_infix2postfix(infix, "single")
        assert success, f"Failed to convert: {output}"
        assert "256 histogram{}^" in output
        # Array write is: value index array{}!
        assert "i@ histogram{}!" in output
        assert "0 histogram{}@" in output
        assert "255 histogram{}@" in output

    def test_array_nested_function_calls(self):
        """Test arrays with nested function calls."""
        infix = """
function get_value(Array a, Value idx) {
    return a[idx]
}

function set_value(Array a, Value idx, Value val) {
    a[idx] = val
}

arr = new(3)
set_value(arr, 0, 100)
set_value(arr, 1, 200)
result = get_value(arr, 0) + get_value(arr, 1)
RESULT = result
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "arr{}^3" in output
        assert "arr{}!" in output
        assert "arr{}@" in output


class TestControlFlowValidation:
    # GOTO related tests
    def test_goto_forbidden_label_name(self):
        infix = """
__internal_label:
    RESULT = 1
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success
        assert "cannot start with '__internal_'" in output

    def test_goto_forbidden_target_name(self):
        infix = """
    goto __internal_label
__internal_label:
    RESULT = 1
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success
        assert "cannot start with '__internal_'" in output

    def test_goto_to_nonexistent_label_global(self):
        infix = """
    goto missing_label
    RESULT = 1
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success
        assert "goto target 'missing_label' not found in global scope" in output

    def test_goto_to_nonexistent_label_function(self):
        infix = """
function f() {
    goto missing_label
}
RESULT = f()
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success
        assert "goto target 'missing_label' not found in function 'f'" in output

    def test_duplicate_label_global(self):
        infix = """
my_label:
    a = 1
my_label:
    a = 2
RESULT = a
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success
        assert "Duplicate label 'my_label' in global scope" in output

    def test_duplicate_label_function(self):
        infix = """
function f() {
my_label:
    a = 1
my_label:
    a = 2
    return a
}
RESULT = f()
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success
        assert "Duplicate label 'my_label' in function 'f'" in output

    def test_goto_from_function_to_global_is_disallowed(self):
        infix = """
function f() {
    goto global_label
}
global_label:
RESULT = f()
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success
        assert (
            "goto from function 'f' to global label 'global_label' is not allowed"
            in output
        )

    def test_valid_goto_in_function(self):
        infix = """
function f(a) {
    if (a > 0) {
        goto end
    }
    a = a + 1
end:
    return a
}
RESULT = f(10)
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed: {output}"

    # RETURN related tests
    def test_return_in_global_scope(self):
        infix = "return 10"
        success, output = run_infix2postfix(infix, "expr")
        assert not success
        assert "'return' statements are not allowed in the global scope" in output

    def test_return_not_last_statement(self):
        infix = """
function f() {
    return 1
    a = 2
}
RESULT = f()
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success
        assert "'return' must be the last statement in a function body" in output

    def test_multiple_return_statements(self):
        infix = """
function f() {
    return 1
    return 2
}
RESULT = f()
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success
        assert "'return' must be the last statement in a function body" in output

    def test_return_in_if_block(self):
        infix = """
function f(a) {
    if (a > 0) {
        return 1
    }
    return 0
}
RESULT = f(1)
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success
        assert "'return' is not allowed inside blocks within a function" in output

    def test_return_in_while_block(self):
        infix = """
function f(a) {
    while(a > 0) {
        return 1
    }
    return 0
}
RESULT = f(1)
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success
        assert "'return' is not allowed inside blocks within a function" in output

    def test_valid_return(self):
        infix = """
function f(a) {
    b = a + 1
    return b
}
RESULT = f(10)
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed: {output}"
