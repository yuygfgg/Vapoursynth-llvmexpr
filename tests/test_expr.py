"""
Copyright (C) 2025 sgt0
Copyright (C) 2025 yuygfgg

This file is part of Vapoursynth-llvmexpr.

This file is derived from and has been modified from code
originally contributed by sgt0 in a pull request to the akarin-vapoursynth-plugin
project (https://github.com/Jaded-Encoding-Thaumaturgy/akarin-vapoursynth-plugin/pull/12),
which was licensed under LGPLv3.

As Vapoursynth-llvmexpr is licensed under the GNU General Public License v3, this
modified file is also distributed under the same license.

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
import vapoursynth as vs
import numpy as np

core = vs.core


@pytest.mark.parametrize(
    "input_format, a, b, expr, expected",
    [
        (vs.GRAYS, 2.0, 3.0, "x y +", 5.0),
        (vs.GRAYS, 7.0, 5.0, "x y -", 2.0),
        (vs.GRAYS, 4.0, 2.5, "x y *", 10.0),
        (vs.GRAYS, 7.5, 2.5, "x y /", 3.0),
        (vs.GRAYS, 5.25, 1.0, "x 1.0 %", 0.25),
    ],
)
def test_arithmetic(
    input_format: int, a: float, b: float, expr: str, expected: float
) -> None:
    c1 = core.std.BlankClip(format=input_format, color=a)
    c2 = core.std.BlankClip(format=input_format, color=b)
    res = core.llvmexpr.Expr([c1, c2], expr, vs.GRAYS)
    assert res.get_frame(0)[0][0, 0] == pytest.approx(expected)


@pytest.mark.parametrize(
    "a, b, expr, expected",
    [
        (3.0, 2.0, "x y >", 1.0),
        (2.0, 3.0, "x y <", 1.0),
        (3.0, 3.0, "x y =", 1.0),
        (3.0, 3.0, "x y >=", 1.0),
        (2.0, 3.0, "x y <=", 1.0),
        (1.0, 0.0, "x y and", 0.0),
        (1.0, 0.0, "x y or", 1.0),
        (1.0, 1.0, "x y xor", 0.0),
        (0.0, 0.0, "x not", 1.0),
        (-2, -3, "x y and", 0.0),
        (-2, -1, "x y or", 0.0),
        (-2, -1, "x y xor", 0.0),
        (-2, 3, "x y xor", 1.0),
    ],
)
def test_comparison_and_logical(a: float, b: float, expr: str, expected: float) -> None:
    c1 = core.std.BlankClip(format=vs.GRAYS, color=a)
    c2 = core.std.BlankClip(format=vs.GRAYS, color=b)
    res = core.llvmexpr.Expr([c1, c2], expr, vs.GRAYS)
    assert res.get_frame(0)[0][0, 0] == pytest.approx(expected)


@pytest.mark.parametrize(
    "input_format, input_value, expr, expected",
    [
        (vs.GRAY8, 0, "0 exp", 1.0),
        (vs.GRAY8, 0, "x exp", 1.0),
        (vs.GRAY8, 1, "x exp", 2.71828),
        (vs.GRAYS, 0.5, "x exp", 1.64872),
    ],
)
def test_exp(input_format: int, input_value: int, expr: str, expected: float) -> None:
    clip = core.std.BlankClip(format=input_format, color=input_value)
    result = core.llvmexpr.Expr(clip, expr, vs.GRAYS)
    assert result.get_frame(0)[0][0, 0] == pytest.approx(expected)


@pytest.mark.parametrize(
    "input_format, input_value, expr, expected",
    [
        (vs.GRAY8, 0, "0 log", -87.3365478515625),
        (
            vs.GRAY8,
            0,
            "x log",
            -87.3365478515625,
        ),
        (vs.GRAY8, 1, "x log", 0),
        (vs.GRAYS, 7.38905, "x log", 2),
    ],
)
def test_log(input_format: int, input_value: int, expr: str, expected: float) -> None:
    clip = core.std.BlankClip(format=input_format, color=input_value)
    result = core.llvmexpr.Expr(clip, expr, vs.GRAYS)
    assert result.get_frame(0)[0][0, 0] == pytest.approx(expected)


@pytest.mark.parametrize(
    "val, expr, expected",
    [
        (1.9, "x floor", 1.0),
        (1.1, "x ceil", 2.0),
        (2.49, "x round", 2.0),
        (2.5, "x round", 3.0),
        (-2.9, "x trunc", -2.0),
        (2.9, "x abs", 2.9),
        (3.0, "x neg", -3.0),
        (-5.0, "x sgn", -1.0),
        (0.0, "x sgn", 0.0),
        (7.0, "x sgn", 1.0),
        (2.0, "x -3 copysign", -2.0),
        (2.0, "2 3 4 fma", 10.0),
    ],
)
def test_rounding_and_misc(val: float, expr: str, expected: float) -> None:
    c = core.std.BlankClip(format=vs.GRAYS, color=val)
    res = core.llvmexpr.Expr(c, expr, vs.GRAYS)
    assert res.get_frame(0)[0][0, 0] == pytest.approx(expected)


@pytest.mark.parametrize("input_format", [vs.GRAY8, vs.GRAY16, vs.GRAYS])
@pytest.mark.parametrize(
    "expr, expected",
    [
        ("x 1.5 pow", 0.0),
    ],
)
def test_pow(input_format: int, expr: str, expected: float) -> None:
    clip = core.std.BlankClip(format=input_format, color=0)
    result = core.llvmexpr.Expr(clip, expr, vs.GRAYS)
    assert result.get_frame(0)[0][0, 0] == pytest.approx(expected)


@pytest.mark.parametrize(
    "input_format, input_value, expr, expected",
    [
        (vs.GRAY8, 0, "x sin", 0),
        (vs.GRAY8, 1, "x sin", 0.8414709568023682),
        (vs.GRAY8, 2, "x sin", 0.9092974066734314),
    ],
)
def test_sin(input_format: int, input_value: int, expr: str, expected: float) -> None:
    clip = core.std.BlankClip(format=input_format, color=input_value)
    result = core.llvmexpr.Expr(clip, expr, vs.GRAYS)
    assert result.get_frame(0)[0][0, 0] == pytest.approx(expected)


def test_gh_11() -> None:
    clip = core.std.BlankClip(format=vs.GRAY8, color=0)
    result = core.llvmexpr.Expr(clip, "x 128 / 0.86 pow 255 *")
    assert result.get_frame(0)[0][0, 0] == pytest.approx(6.122468756907559e-31)

    clip = core.std.BlankClip(format=vs.GRAY16, color=0)
    result = core.llvmexpr.Expr(clip, "x 32768 / 0.86 pow 65535 *")
    assert result.get_frame(0)[0][0, 0] == pytest.approx(1.5734745330615421e-28)


@pytest.mark.parametrize(
    "val, min_v, max_v, expected_max, expected_min",
    [
        (5.0, 2.0, 8.0, 8.0, 2.0),
        (1.0, 2.0, 8.0, 2.0, 1.0),
    ],
)
def test_min_max_clip(
    val: float, min_v: float, max_v: float, expected_max: float, expected_min: float
) -> None:
    c = core.std.BlankClip(format=vs.GRAYS, color=val)
    res_max = core.llvmexpr.Expr(c, "x 3 max", vs.GRAYS)
    res_min = core.llvmexpr.Expr(c, "x 3 min", vs.GRAYS)
    res_clip = core.llvmexpr.Expr(c, f"x {min_v} {max_v} clip", vs.GRAYS)
    assert res_max.get_frame(0)[0][0, 0] == pytest.approx(max(val, 3.0))
    assert res_min.get_frame(0)[0][0, 0] == pytest.approx(min(val, 3.0))
    assert res_clip.get_frame(0)[0][0, 0] == pytest.approx(min(max(val, min_v), max_v))


@pytest.mark.parametrize(
    "a, b, expr, expected",
    [
        (5.9, 1.0, "x y bitand", 1.0),
        (5.0, 2.0, "x y bitor", 7.0),
        (5.0, 1.0, "x y bitxor", 4.0),
        (
            5.0,
            0.0,
            "x bitnot",
            (
                float(2**32 - 1 - 5)
                if vs.core.core_version.release_major
                >= 64  # pyright: ignore[reportOperatorIssue]
                else float(~5 & 0xFFFFFFFF)
            ),
        ),
    ],
)
def test_bitwise(a: float, b: float, expr: str, expected: float) -> None:
    c1 = core.std.BlankClip(format=vs.GRAYS, color=a)
    c2 = core.std.BlankClip(format=vs.GRAYS, color=b)
    res = core.llvmexpr.Expr([c1, c2], expr, vs.GRAYS)
    # bitnot result depends on implementation width; for generality, compare masked
    out = res.get_frame(0)[0][0, 0]
    if "bitnot" in expr:
        assert int(out) & 0xFFFFFFFF == (~int(a)) & 0xFFFFFFFF
    else:
        assert out == pytest.approx(expected)


def test_stack_manipulation() -> None:
    c = core.std.BlankClip(format=vs.GRAYS, color=3.0)
    res_dup = core.llvmexpr.Expr(c, "x dup *", vs.GRAYS)
    assert res_dup.get_frame(0)[0][0, 0] == pytest.approx(9.0)
    c0 = core.std.BlankClip(format=vs.GRAYS, color=0.0)
    res_swap = core.llvmexpr.Expr(c0, "2 5 swap -", vs.GRAYS)
    assert res_swap.get_frame(0)[0][0, 0] == pytest.approx(3.0)
    res_drop = core.llvmexpr.Expr(c0, "1 2 3 drop2", vs.GRAYS)
    assert res_drop.get_frame(0)[0][0, 0] == pytest.approx(1.0)
    res_sort = core.llvmexpr.Expr(c0, "3 1 2 sort3 drop2", vs.GRAYS)
    assert res_sort.get_frame(0)[0][0, 0] == pytest.approx(3.0)


def test_named_variables_and_loop_power() -> None:
    c = core.std.BlankClip(format=vs.GRAYS, color=2.0)
    y = core.std.BlankClip(format=vs.GRAYS, color=4.0)
    expr = "x base! 1 result! y counter! #loop result@ base@ * result! counter@ 1 - counter! counter@ loop# result@"
    res = core.llvmexpr.Expr([c, y], expr, vs.GRAYS)
    assert res.get_frame(0)[0][0, 0] == pytest.approx(16.0)


def test_constants_and_coords() -> None:
    c0 = core.std.BlankClip(format=vs.GRAYS, color=0.0, width=3, height=2)
    res_pi = core.llvmexpr.Expr(c0, "pi", vs.GRAYS)
    assert res_pi.get_frame(0)[0][0, 0] == pytest.approx(3.14159265, rel=1e-6)
    res_N = core.llvmexpr.Expr(c0, "N", vs.GRAYS)
    assert res_N.get_frame(3)[0][0, 0] == pytest.approx(3.0)
    res_wh = core.llvmexpr.Expr(c0, "width height +", vs.GRAYS)
    assert res_wh.get_frame(0)[0][0, 0] == pytest.approx(5.0)
    res_X = core.llvmexpr.Expr(c0, "X", vs.GRAYS)
    res_Y = core.llvmexpr.Expr(c0, "Y", vs.GRAYS)
    fX = res_X.get_frame(0)
    fY = res_Y.get_frame(0)
    assert fX[0][1, 2] == pytest.approx(2.0)
    assert fY[0][1, 2] == pytest.approx(1.0)


def test_conditional_ternary() -> None:
    c = core.std.BlankClip(format=vs.GRAYS, color=10.0)
    res = core.llvmexpr.Expr(c, "x 5 > 1 0 ?", vs.GRAYS)
    assert res.get_frame(0)[0][0, 0] == pytest.approx(1.0)


def test_pixel_access_static_and_dynamic() -> None:
    base = core.std.BlankClip(format=vs.GRAYS, color=0.0, width=4, height=2)
    ramp = core.llvmexpr.Expr(base, "X", vs.GRAYS)
    src = core.std.BlankClip(format=vs.GRAYS, color=99.0, width=4, height=2)
    expr_rel = "y[-1,0]"
    res_rel = core.llvmexpr.Expr([src, ramp], expr_rel, vs.GRAYS)
    f = res_rel.get_frame(0)
    assert f[0][0, 2] == pytest.approx(1.0)
    expr_abs = "1 1 y[]"
    res_abs = core.llvmexpr.Expr([src, ramp], expr_abs, vs.GRAYS)
    assert res_abs.get_frame(0)[0][0, 0] == pytest.approx(1.0)


def test_frame_property_access() -> None:
    c = core.std.BlankClip(format=vs.GRAYS, color=0.0)
    c = core.std.SetFrameProps(c, _TestProp=0.25)
    res = core.llvmexpr.Expr(c, "x._TestProp", vs.GRAYS)
    assert res.get_frame(0)[0][0, 0] == pytest.approx(0.25)


def test_direct_output_write_and_exit() -> None:
    base = core.std.BlankClip(format=vs.GRAYS, color=0.0, width=4, height=4)
    expr = "X 1 = Y 2 = and 5 1 2 @[] ^exit^ 0 ?"
    res = core.llvmexpr.Expr(base, expr, vs.GRAYS)
    fr = res.get_frame(0)
    assert fr[0][2, 1] == pytest.approx(5.0)
    assert fr[0][0, 0] == pytest.approx(0.0)


@pytest.fixture(scope="module")
def ramp_clip() -> vs.VideoNode:
    width, height = 4, 4
    base = core.std.BlankClip(format=vs.GRAYS, width=width, height=height, color=0.0)

    def ramp_frame(n, f):
        fout = f.copy()
        arr = np.asarray(fout[0])
        for y in range(height):
            for x in range(width):
                arr[y, x] = y * width + x
        return fout

    return core.std.ModifyFrame(base, clips=base, selector=ramp_frame)


boundary_test_cases = [
    # Default boundary (clamp)
    pytest.param("x[-1,-1]", None, 0, 0, 0.0, id="clamp_default_topleft"),
    pytest.param("x[1,1]", None, 3, 3, 15.0, id="clamp_default_bottomright"),
    pytest.param("x[-2,0]", None, 1, 1, 4.0, id="clamp_default_rel"),
    # Explicit clamp with boundary parameter
    pytest.param("x[-1,-1]", 0, 0, 0, 0.0, id="clamp_param_topleft"),
    pytest.param("x[1,1]", 0, 3, 3, 15.0, id="clamp_param_bottomright"),
    # Explicit clamp with :c suffix
    pytest.param("x[-1,-1]:c", None, 0, 0, 0.0, id="clamp_suffix_topleft"),
    pytest.param("x[1,1]:c", None, 3, 3, 15.0, id="clamp_suffix_bottomright"),
    # Mirror with boundary parameter
    pytest.param("x[-1,-1]", 1, 0, 0, 5.0, id="mirror_param_topleft"),
    pytest.param("x[1,1]", 1, 3, 3, 10.0, id="mirror_param_bottomright"),
    pytest.param("x[-2,0]", 1, 1, 1, 5.0, id="mirror_param_rel"),
    # Mirror with :m suffix
    pytest.param("x[-1,-1]:m", None, 0, 0, 5.0, id="mirror_suffix_topleft"),
    pytest.param("x[1,1]:m", None, 3, 3, 10.0, id="mirror_suffix_bottomright"),
    # Override behavior
    pytest.param("x[-1,-1]:m", 0, 0, 0, 5.0, id="override_clamp_with_mirror"),
    pytest.param("x[-1,-1]:c", 1, 0, 0, 0.0, id="override_mirror_with_clamp"),
    # More mirror tests
    pytest.param("x[4,4]", 1, 0, 0, 10.0, id="mirror_param_far_coord1"),
    pytest.param("x[5,5]", 1, 0, 0, 5.0, id="mirror_param_far_coord2"),
    pytest.param("x[-4,-4]", 1, 0, 0, 10.0, id="mirror_param_far_coord3"),
]


@pytest.mark.parametrize("expr, boundary, x, y, expected", boundary_test_cases)
def test_boundary_conditions(
    ramp_clip: vs.VideoNode,
    expr: str,
    boundary: int | None,
    x: int,
    y: int,
    expected: float,
) -> None:
    if boundary:
        res = core.llvmexpr.Expr(ramp_clip, expr, boundary=boundary)
    else:
        res = core.llvmexpr.Expr(ramp_clip, expr)

    frame = res.get_frame(0)
    assert frame[0][y, x] == pytest.approx(expected)


# Tests for absolute pixel access boundary conditions
abs_boundary_test_cases = [
    # Default is clamp
    pytest.param("-1 -1 x[]", None, 0.0, id="abs_default_clamp_topleft"),
    pytest.param("4 4 x[]", None, 15.0, id="abs_default_clamp_bottomright"),
    # Default clamp should ignore boundary=1 (mirror)
    pytest.param("-1 -1 x[]", 1, 0.0, id="abs_default_clamp_overrides_mirror_param"),
    # Explicit clamp :c
    pytest.param("-1 -1 x[]:c", None, 0.0, id="abs_explicit_clamp_topleft"),
    # Explicit clamp should ignore boundary=1 (mirror)
    pytest.param("-1 -1 x[]:c", 1, 0.0, id="abs_explicit_clamp_overrides_mirror_param"),
    # Explicit mirror :m
    pytest.param("-1 -1 x[]:m", None, 5.0, id="abs_explicit_mirror_topleft"),
    pytest.param("4 4 x[]:m", None, 10.0, id="abs_explicit_mirror_bottomright"),
    # Explicit mirror should ignore boundary=0 (clamp)
    pytest.param("-1 -1 x[]:m", 0, 5.0, id="abs_explicit_mirror_overrides_clamp_param"),
    # Use boundary param :b
    pytest.param("-1 -1 x[]:b", 0, 0.0, id="abs_b_uses_clamp_param"),
    pytest.param("-1 -1 x[]:b", 1, 5.0, id="abs_b_uses_mirror_param"),
    pytest.param("4 4 x[]:b", 0, 15.0, id="abs_b_uses_clamp_param_br"),
    pytest.param("4 4 x[]:b", 1, 10.0, id="abs_b_uses_mirror_param_br"),
]


@pytest.mark.parametrize("expr, boundary, expected", abs_boundary_test_cases)
def test_abs_boundary_conditions(
    ramp_clip: vs.VideoNode, expr: str, boundary: int | None, expected: float
) -> None:
    if boundary is not None:
        res = core.llvmexpr.Expr(ramp_clip, expr, boundary=boundary)
    else:
        res = core.llvmexpr.Expr(ramp_clip, expr)

    # We test at a single pixel, since the coordinates are absolute
    frame = res.get_frame(0)
    assert frame[0][0, 0] == pytest.approx(expected)


def test_non_integer_coordinate_rounding() -> None:
    c = core.std.BlankClip(format=vs.GRAYS, color=0.0, width=4, height=2)
    c = core.llvmexpr.Expr(c, "X")
    res = core.llvmexpr.Expr(c, "X 0.5 + Y 0.5 + x[]", vs.GRAYS)
    assert res.get_frame(0)[0][0, 0] == pytest.approx(0.0)
    assert res.get_frame(0)[0][0, 1] == pytest.approx(2.0)
    assert res.get_frame(0)[0][0, 2] == pytest.approx(2.0)
    assert res.get_frame(0)[0][0, 3] == pytest.approx(3.0)
