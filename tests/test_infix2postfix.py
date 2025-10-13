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

import pytest
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
        assert output.strip() == "-1 a! -1 0 = __internal_else_0# 255 a! 1 __internal_endif_1# #__internal_else_0 0 a! #__internal_endif_1 a@ RESULT! RESULT@"

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
        """Test if-else scope."""
        # x is not defined
        # var is not defined for outer scope
        infix = """
if (x > 10) {
    var = 1
}
RESULT = var
"""
        success, output = run_infix2postfix(infix, "expr")
        assert not success, f"Should fail, got {output}"


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
        assert output.strip() == "10 20 + RESULT! RESULT@"

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
        assert "10 global_var@ +" in output
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


class TestFunctionOverloading:
    def test_overload_by_type(self):
        """Test function overloading based on parameter types."""
        infix = """
function process(Clip c) {
    return c[0,0] * 2
}
function process(Value v) {
    return v * 2
}
RESULT = process($x) + process(10.0)
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "x[0,0] 2 * 10.0 2 * + RESULT! RESULT@" in output

    def test_overload_by_arity(self):
        """Test function overloading based on number of parameters."""
        infix = """
function f(Value x) { return x }
function f(Value x, Value y) { return x + y }
RESULT = f(1) + f(2, 3)
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert "1 2 3 + + RESULT! RESULT@" in output

    def test_overload_best_fit(self):
        """Test that the best-fit overload (fewest conversions) is chosen."""
        infix = """
function best(Value v, Clip c) { return 1 }
function best(Value v, Value v2) { return 2 }
RESULT = best(1.0, 2.0)
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert output.strip() == "2 RESULT! RESULT@"

    def test_overload_tie_break(self):
        """Test overload resolution tie-breaking rule."""
        infix = """
function tie(Clip c, Value v) { return 1 }
function tie(Value v, Clip c) { return 2 }
RESULT = tie($x, $y)
"""
        success, output = run_infix2postfix(infix, "expr")
        assert success, f"Failed to convert: {output}"
        assert output.strip() == "1 RESULT! RESULT@"

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


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
