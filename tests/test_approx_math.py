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
    # Relax precision for approximate math implementation
    if abs(expected) > 1000:
        rel_tol = 1e-2  # 1% for very large values
    elif abs(expected) > 10:
        rel_tol = 1e-3  # 0.1% for large values
    else:
        rel_tol = 1e-3  # 0.1% for normal values

    abs_tol = max(1e-4, abs(expected) * 1e-4)  # Scale absolute tolerance
    assert out == pytest.approx(expected, rel=rel_tol, abs=abs_tol)


def test_exp_random_values() -> None:
    rng = np.random.default_rng(12345)
    values = rng.uniform(-20.0, 20.0, size=50)
    for x in values:
        out = _eval_exp(float(x))
        expected = float(np.exp(x))
        # Relax precision for approximate math implementation
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


def test_log_random_values() -> None:
    rng = np.random.default_rng(12345)
    values = rng.uniform(0.001, 1000.0, size=50)
    for x in values:
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


@pytest.mark.parametrize(
    "op, func", [("sin", np.sin), ("cos", np.cos), ("tan", np.tan)]
)
def test_trig_random_values(op: str, func) -> None:
    rng = np.random.default_rng(54321)
    values = rng.uniform(-1000.0, 1000.0, size=100)
    for x in values:
        # Don't test tan near its asymptotes where behavior is chaotic
        if op == "tan" and np.isclose(np.cos(float(x)), 0.0, atol=1e-2):
            continue

        out = _eval_trig(op, float(x))
        expected = float(func(x))
        if op == "sin":
            assert out == pytest.approx(expected, abs=9e-4)
        elif op == "cos":
            assert out == pytest.approx(expected, abs=3e-4)
        else:  # tan
            assert out == pytest.approx(expected, abs=0.01)
