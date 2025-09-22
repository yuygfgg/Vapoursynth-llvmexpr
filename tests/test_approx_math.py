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


def _eval_pow(a: float, b: float) -> float:
    c1 = core.std.BlankClip(format=vs.GRAYS, color=a)
    c2 = core.std.BlankClip(format=vs.GRAYS, color=b)
    res = core.llvmexpr.Expr([c1, c2], "x y pow", vs.GRAYS)
    return float(res.get_frame(0)[0][0, 0])


@pytest.mark.parametrize(
    "a, b",
    [
        (1.0, -5.0),
        (1.0, 0.0),
        (1.0, 3.14159),
        (2.0, 0.0),
        (2.0, 1.0),
        (0.0, 2.5),
        (0.0, 1.0),
        (0.0, 0.0),
        (-2.0, 2.0),
        (-2.0, 3.0),
        (-2.0, -2.0),
        (-2.0, -3.0),
        (np.e, 2.0),
        (10.0, -3.0),
    ],
)
def test_pow_special_cases(a: float, b: float) -> None:
    out = _eval_pow(a, b)
    expected = float(np.power(a, b))
    assert out == pytest.approx(expected, rel=1e-5, abs=1e-6)


@pytest.mark.parametrize("a", np.linspace(1e-3, 100.0, num=9))
@pytest.mark.parametrize("b", [-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 2.5, 3.0])
def test_pow_nonnegative_base_grid(a: float, b: float) -> None:
    out = _eval_pow(a, b)
    expected = float(np.power(a, b))
    assert out == pytest.approx(expected, rel=1e-5, abs=1e-6)


@pytest.mark.parametrize("a", [-0.1, -0.5, -1.0, -2.0, -10.0])
@pytest.mark.parametrize("b", [-5, -3, -2, -1, 0, 1, 2, 3, 4, 5])
def test_pow_negative_base_integer_exponent(a: float, b: int) -> None:
    out = _eval_pow(a, float(b))
    expected = float(np.power(a, b))
    assert out == pytest.approx(expected, rel=1e-5, abs=1e-6)


def test_pow_random_values() -> None:
    rng = np.random.default_rng(12345)
    bases = rng.uniform(1e-6, 1000.0, size=50)
    exps = rng.uniform(-4.0, 4.0, size=50)
    for a, b in zip(bases, exps):
        out = _eval_pow(float(a), float(b))
        expected = float(np.power(a, b))
        assert out == pytest.approx(expected, rel=1e-5, abs=1e-6)



def _eval_exp(x: float) -> float:
    """Helper function to evaluate exp(x) using llvmexpr."""
    clip = vs.core.std.BlankClip(width=1, height=1, format=vs.GRAYS, length=1)
    clip = vs.core.llvmexpr.Expr(clip, f"{x} exp")
    frame = clip.get_frame(0)
    return float(frame[0][0, 0])


def _eval_log(x: float) -> float:
    """Helper function to evaluate log(x) using llvmexpr."""
    clip = vs.core.std.BlankClip(width=1, height=1, format=vs.GRAYS, length=1)
    clip = vs.core.llvmexpr.Expr(clip, f"{x} log")
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
