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

import numpy as np
import pytest
import vapoursynth as vs


core = vs.core


def _eval_exp(x: float) -> float:
    """Helper function to evaluate exp(x) using llvmexpr."""
    clip = vs.core.std.BlankClip(width=1, height=1, format=vs.GRAYS, length=1, color=x)
    clip = vs.core.llvmexpr.Expr(clip, "x exp", approx_math=1)
    frame = clip.get_frame(0)
    return float(frame[0][0, 0])


def _eval_log(x: float) -> float:
    """Helper function to evaluate log(x) using llvmexpr."""
    clip = vs.core.std.BlankClip(width=1, height=1, format=vs.GRAYS, length=1, color=x)
    clip = vs.core.llvmexpr.Expr(clip, "x log", approx_math=1)
    frame = clip.get_frame(0)
    return float(frame[0][0, 0])


@pytest.mark.parametrize(
    "x",
    [
        0.0,
        1.0,
        -1.0,
        2.0,
        -2.0,
        10.0,
        -10.0,
        np.e,
        -np.e,
        0.1,
        -0.1,
        0.001,
        -0.001,
    ],
)
def test_exp_special_cases(x: float) -> None:
    out = _eval_exp(x)
    expected = float(np.exp(x))
    # Relax precision for approximate math implementation
    # Use larger relative tolerance for larger values
    if abs(expected) > 1000:
        rel_tol = 1e-2  # 1% for very large values
    elif abs(expected) > 10:
        rel_tol = 1e-3  # 0.1% for large values
    else:
        rel_tol = 1e-3  # 0.1% for normal values

    abs_tol = max(1e-4, abs(expected) * 1e-4)  # Scale absolute tolerance
    assert out == pytest.approx(expected, rel=rel_tol, abs=abs_tol)


@pytest.mark.parametrize("x", np.linspace(-10.0, 10.0, num=21))
def test_exp_grid(x: float) -> None:
    out = _eval_exp(x)
    expected = float(np.exp(x))
    if abs(expected) > 1000:
        rel_tol = 1e-2  # 1% for very large values
    elif abs(expected) > 10:
        rel_tol = 1e-3  # 0.1% for large values
    else:
        rel_tol = 1e-3  # 0.1% for normal values

    abs_tol = max(1e-4, abs(expected) * 1e-4)  # Scale absolute tolerance
    assert out == pytest.approx(expected, rel=rel_tol, abs=abs_tol)


_rng_exp = np.random.default_rng(12345)
_EXP_RANDOM_VALUES = _rng_exp.uniform(-20.0, 20.0, size=50)


@pytest.mark.parametrize("x", _EXP_RANDOM_VALUES)
def test_exp_random_values(x: float) -> None:
    out = _eval_exp(float(x))
    expected = float(np.exp(x))
    if abs(expected) > 1000:
        rel_tol = 1e-2  # 1% for very large values
    elif abs(expected) > 10:
        rel_tol = 1e-3  # 0.1% for large values
    else:
        rel_tol = 1e-3  # 0.1% for normal values

    abs_tol = max(1e-4, abs(expected) * 1e-4)  # Scale absolute tolerance
    assert out == pytest.approx(expected, rel=rel_tol, abs=abs_tol)


@pytest.mark.parametrize(
    "x",
    [
        1.0,
        2.0,
        10.0,
        100.0,
        np.e,
        0.1,
        0.001,
        0.5,
        1.5,
        2.5,
    ],
)
def test_log_special_cases(x: float) -> None:
    out = _eval_log(x)
    expected = float(np.log(x))
    assert out == pytest.approx(expected, rel=1e-5, abs=1e-6)


@pytest.mark.parametrize("x", np.logspace(-3, 3, num=21))  # 0.001 to 1000
def test_log_positive_grid(x: float) -> None:
    out = _eval_log(x)
    expected = float(np.log(x))
    assert out == pytest.approx(expected, rel=1e-5, abs=1e-6)


_rng_log = np.random.default_rng(12345)
_LOG_RANDOM_VALUES = _rng_log.uniform(0.001, 1000.0, size=50)


@pytest.mark.parametrize("x", _LOG_RANDOM_VALUES)
def test_log_random_values(x: float) -> None:
    out = _eval_log(float(x))
    expected = float(np.log(x))
    assert out == pytest.approx(expected, rel=1e-5, abs=1e-6)


def _eval_trig(op: str, x: float) -> float:
    """Helper function to evaluate trig functions using llvmexpr."""
    c = core.std.BlankClip(format=vs.GRAYS, color=x)
    res = core.llvmexpr.Expr(c, f"x {op}", vs.GRAYS, approx_math=1)
    return float(res.get_frame(0)[0][0, 0])


TRIG_SPECIAL_CASES = [
    0.0,
    np.pi / 6,
    np.pi / 4,
    np.pi / 3,
    np.pi / 2,
    2 * np.pi / 3,
    3 * np.pi / 4,
    5 * np.pi / 6,
    np.pi,
    7 * np.pi / 6,
    5 * np.pi / 4,
    4 * np.pi / 3,
    3 * np.pi / 2,
    5 * np.pi / 3,
    7 * np.pi / 4,
    11 * np.pi / 6,
    2 * np.pi,
]
TRIG_SPECIAL_CASES += [-x for x in TRIG_SPECIAL_CASES if x != 0.0]
TRIG_SPECIAL_CASES += [100.0, -100.0, 1000.0, -1000.0]  # Test range reduction


@pytest.mark.parametrize(
    "op, func", [("sin", np.sin), ("cos", np.cos), ("tan", np.tan)]
)
@pytest.mark.parametrize("x", TRIG_SPECIAL_CASES)
def test_trig_special_cases(op: str, func, x: float) -> None:
    if op == "tan" and np.isclose(np.cos(x), 0.0):
        pytest.skip("Skipping tan test at asymptote")

    out = _eval_trig(op, x)
    expected = float(func(x))
    assert out == pytest.approx(expected, abs=9e-3)


_rng_trig = np.random.default_rng(54321)
_TRIG_RANDOM_VALUES = _rng_trig.uniform(-1000.0, 1000.0, size=100)


@pytest.mark.parametrize(
    "op, func", [("sin", np.sin), ("cos", np.cos), ("tan", np.tan)]
)
@pytest.mark.parametrize("x", _TRIG_RANDOM_VALUES)
def test_trig_random_values(op: str, func, x: float) -> None:
    # Don't test tan near its asymptotes where behavior is chaotic
    if op == "tan" and np.isclose(np.cos(float(x)), 0.0, atol=1e-2):
        pytest.skip("Skipping tan test near asymptote")

    out = _eval_trig(op, float(x))
    expected = float(func(x))
    if op == "sin":
        assert out == pytest.approx(expected, abs=9e-4)
    elif op == "cos":
        assert out == pytest.approx(expected, abs=3e-4)
    else:  # tan
        assert out == pytest.approx(expected, abs=0.01)


def _eval_atan(x: float) -> float:
    """Helper function to evaluate atan(x) using llvmexpr."""
    c = core.std.BlankClip(format=vs.GRAYS, color=x)
    res = core.llvmexpr.Expr(c, "x atan", vs.GRAYS, approx_math=1)
    return float(res.get_frame(0)[0][0, 0])


def _eval_atan2(y: float, x: float) -> float:
    """Helper function to evaluate atan2(y, x) using llvmexpr."""
    c_y = core.std.BlankClip(format=vs.GRAYS, color=y)
    c_x = core.std.BlankClip(format=vs.GRAYS, color=x)
    res = core.llvmexpr.Expr([c_y, c_x], "x y atan2", vs.GRAYS, approx_math=1)
    return float(res.get_frame(0)[0][0, 0])


ATAN_SPECIAL_CASES = [
    0.0,
    1.0,
    -1.0,
    0.5,
    -0.5,
    2.0,
    -2.0,
    10.0,
    -10.0,
    1000.0,
    -1000.0,
]


@pytest.mark.parametrize("x", ATAN_SPECIAL_CASES)
def test_atan_special_cases(x: float) -> None:
    out = _eval_atan(x)
    expected = float(np.arctan(x))
    assert out == pytest.approx(expected, abs=1e-4)


_rng_atan = np.random.default_rng(67890)
_ATAN_RANDOM_VALUES = _rng_atan.uniform(-1000.0, 1000.0, size=100)


@pytest.mark.parametrize("x", _ATAN_RANDOM_VALUES)
def test_atan_random_values(x: float) -> None:
    out = _eval_atan(float(x))
    expected = float(np.arctan(float(x)))
    assert out == pytest.approx(expected, abs=1e-4)


def _eval_acos(x: float) -> float:
    """Helper function to evaluate acos(x) using llvmexpr."""
    c = core.std.BlankClip(format=vs.GRAYS, color=x)
    res = core.llvmexpr.Expr(c, "x acos", vs.GRAYS, approx_math=1)
    return float(res.get_frame(0)[0][0, 0])


ACOS_SPECIAL_CASES = [
    -1.0,
    -0.75,
    -0.5,
    -0.25,
    0.0,
    0.25,
    0.5,
    0.75,
    1.0,
]


@pytest.mark.parametrize("x", ACOS_SPECIAL_CASES)
def test_acos_special_cases(x: float) -> None:
    out = _eval_acos(x)
    expected = float(np.arccos(x))
    assert out == pytest.approx(expected, abs=0.0004333)


_rng_acos = np.random.default_rng(98765)
_ACOS_RANDOM_VALUES = _rng_acos.uniform(-1.0, 1.0, size=100)


@pytest.mark.parametrize("x", _ACOS_RANDOM_VALUES)
def test_acos_random_values(x: float) -> None:
    out = _eval_acos(float(x))
    expected = float(np.arccos(float(x)))
    assert out == pytest.approx(expected, abs=0.0004333)


ATAN2_SPECIAL_CASES = [
    (0.0, 0.0),
    (1.0, 0.0),
    (-1.0, 0.0),
    (0.0, 1.0),
    (0.0, -1.0),
    (1.0, 1.0),
    (1.0, -1.0),
    (-1.0, 1.0),
    (-1.0, -1.0),
    (100.0, 100.0),
    (100.0, -100.0),
    (-100.0, 100.0),
    (-100.0, -100.0),
]


@pytest.mark.parametrize("y, x", ATAN2_SPECIAL_CASES)
def test_atan2_special_cases(y: float, x: float) -> None:
    out = _eval_atan2(y, x)
    expected = float(np.arctan2(y, x))
    assert out == pytest.approx(expected, abs=1e-4)


_rng_atan2 = np.random.default_rng(13579)
_ATAN2_Y_RANDOM_VALUES = _rng_atan2.uniform(-1000.0, 1000.0, size=100)
_ATAN2_X_RANDOM_VALUES = _rng_atan2.uniform(-1000.0, 1000.0, size=100)
_ATAN2_RANDOM_PAIRS = [
    (float(y), float(x))
    for y, x in zip(_ATAN2_Y_RANDOM_VALUES, _ATAN2_X_RANDOM_VALUES)
    if not (y == 0.0 and x == 0.0)
]


@pytest.mark.parametrize("y, x", _ATAN2_RANDOM_PAIRS)
def test_atan2_random_values(y: float, x: float) -> None:
    out = _eval_atan2(y, x)
    expected = float(np.arctan2(y, x))
    assert out == pytest.approx(expected, abs=1e-4)


def _eval_asin(x: float) -> float:
    """Helper function to evaluate asin(x) using llvmexpr."""
    c = core.std.BlankClip(format=vs.GRAYS, color=x)
    res = core.llvmexpr.Expr(c, "x asin", vs.GRAYS, approx_math=1)
    return float(res.get_frame(0)[0][0, 0])


ASIN_SPECIAL_CASES = [
    -1.0,
    -0.75,
    -0.5,
    -0.25,
    0.0,
    0.25,
    0.5,
    0.75,
    1.0,
]


@pytest.mark.parametrize("x", ASIN_SPECIAL_CASES)
def test_asin_special_cases(x: float) -> None:
    out = _eval_asin(x)
    expected = float(np.arcsin(x))
    assert out == pytest.approx(expected, abs=5e-4)


_rng_asin = np.random.default_rng(98766)
_ASIN_RANDOM_VALUES = _rng_asin.uniform(-1.0, 1.0, size=100)


@pytest.mark.parametrize("x", _ASIN_RANDOM_VALUES)
def test_asin_random_values(x: float) -> None:
    out = _eval_asin(float(x))
    expected = float(np.arcsin(float(x)))
    assert out == pytest.approx(expected, abs=5e-4)
