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

import glob
import os
import uuid

import pytest
import vapoursynth as vs

core = vs.core

TEST_IR_DIR = os.path.join(os.path.dirname(__file__), "..", ".test_ir_dumps")


def get_ir_content(dump_path):
    ir_files = [
        f for f in glob.glob(dump_path.replace(".ll", ".*.ll")) if f.endswith(".pre.ll")
    ]
    if not ir_files:
        pytest.fail(f"No .pre.ll IR file found for {dump_path}")
    with open(ir_files[0], "r") as f:
        return f.read()


def count_array_allocations(ir_content, array_name=None):
    """Count llvmexpr_ensure_buffer calls, optionally filtered by array name."""
    if array_name:
        return ir_content.count(f"@{array_name}_name, i64")
    return ir_content.count("call ptr @llvmexpr_ensure_buffer")


def test_static_array_opt_converts_dynamic_to_static():
    """
    StaticArrayOptPass: Dynamic allocation with constant size should be
    converted to static allocation.
    """
    clip = core.std.BlankClip(width=64, height=64, format=vs.GRAY8, length=1)

    os.makedirs(TEST_IR_DIR, exist_ok=True)
    dump_path = os.path.join(TEST_IR_DIR, f"test_static_opt_{uuid.uuid4().hex[:8]}.ll")

    try:
        result = core.llvmexpr.SingleExpr(
            [clip],
            expr="10 buffer{}^ 5.0 0 buffer{}! 0 buffer{}@ result$",
            dump_ir=dump_path,
        )
        result.get_frame(0)

        ir_content = get_ir_content(dump_path)

        assert count_array_allocations(ir_content) == 1
        assert "i64 10" in ir_content
        assert count_array_allocations(ir_content, "buffer") == 1

    finally:
        for f in glob.glob(dump_path.replace(".ll", "*")):
            try:
                os.remove(f)
            except FileNotFoundError:
                pass


def test_dynamic_array_opt_eliminates_redundant_allocation():
    """
    DynamicArrayAllocOptPass: Redundant allocation with no usage between
    two allocations should be eliminated.
    """
    clip = core.std.BlankClip(width=64, height=64, format=vs.GRAY8, length=1)

    os.makedirs(TEST_IR_DIR, exist_ok=True)
    dump_path = os.path.join(
        TEST_IR_DIR, f"test_dyn_opt_elim_{uuid.uuid4().hex[:8]}.ll"
    )

    try:
        result = core.llvmexpr.SingleExpr(
            [clip],
            expr="10 buffer{}^ 20 buffer{}^ 5.0 0 buffer{}! 0 buffer{}@ result$",
            dump_ir=dump_path,
        )
        result.get_frame(0)

        ir_content = get_ir_content(dump_path)

        assert count_array_allocations(ir_content) == 1
        assert "i64 20" in ir_content
        assert "i64 10" not in ir_content

    finally:
        for f in glob.glob(dump_path.replace(".ll", "*")):
            try:
                os.remove(f)
            except FileNotFoundError:
                pass


def test_dynamic_array_opt_keeps_used_allocations():
    """
    DynamicArrayAllocOptPass: Both allocations should be kept if array
    is used between them.
    """
    clip = core.std.BlankClip(width=64, height=64, format=vs.GRAY8, length=1)

    os.makedirs(TEST_IR_DIR, exist_ok=True)
    dump_path = os.path.join(
        TEST_IR_DIR, f"test_dyn_opt_keep_{uuid.uuid4().hex[:8]}.ll"
    )

    try:
        result = core.llvmexpr.SingleExpr(
            [clip],
            expr="10 buffer{}^ 5.0 0 buffer{}! 20 buffer{}^ 10.0 0 buffer{}! 0 buffer{}@ drop1",
            dump_ir=dump_path,
        )
        result.get_frame(0)

        ir_content = get_ir_content(dump_path)

        assert count_array_allocations(ir_content) == 2
        assert "i64 10" in ir_content
        assert "i64 20" in ir_content

    finally:
        for f in glob.glob(dump_path.replace(".ll", "*")):
            try:
                os.remove(f)
            except FileNotFoundError:
                pass


def test_array_opt_multiple_arrays():
    """
    Test mixed optimization: multiple arrays with different optimization
    opportunities.
    """
    clip = core.std.BlankClip(width=64, height=64, format=vs.GRAY8, length=1)

    os.makedirs(TEST_IR_DIR, exist_ok=True)
    dump_path = os.path.join(TEST_IR_DIR, f"test_multi_arr_{uuid.uuid4().hex[:8]}.ll")

    try:
        result = core.llvmexpr.SingleExpr(
            [clip],
            expr="10 arr1{}^ 20 arr2{}^ 30 arr1{}^ 5.0 0 arr2{}! 0 arr2{}@ result$",
            dump_ir=dump_path,
        )
        result.get_frame(0)

        ir_content = get_ir_content(dump_path)

        assert count_array_allocations(ir_content) == 2
        assert count_array_allocations(ir_content, "arr1") == 1
        assert "i64 30" in ir_content
        assert count_array_allocations(ir_content, "arr2") == 1
        assert "i64 20" in ir_content

    finally:
        for f in glob.glob(dump_path.replace(".ll", "*")):
            try:
                os.remove(f)
            except FileNotFoundError:
                pass


def test_array_opt_with_control_flow():
    """
    Test that optimization correctly handles control flow.
    """
    clip = core.std.BlankClip(width=64, height=64, format=vs.GRAY8, length=1)

    os.makedirs(TEST_IR_DIR, exist_ok=True)
    dump_path = os.path.join(TEST_IR_DIR, f"test_cf_{uuid.uuid4().hex[:8]}.ll")

    try:
        result = core.llvmexpr.SingleExpr(
            [clip],
            expr="10 arr{}^ 1 skip# 5.0 0 arr{}! #skip 20 arr{}^ 0 arr{}@ result$",
            dump_ir=dump_path,
        )
        result.get_frame(0)

        ir_content = get_ir_content(dump_path)
        assert count_array_allocations(ir_content) == 2

    finally:
        for f in glob.glob(dump_path.replace(".ll", "*")):
            try:
                os.remove(f)
            except FileNotFoundError:
                pass


def test_combined_optimizations():
    """
    Test that both StaticArrayOpt and DynamicArrayAllocOpt work together.
    """
    clip = core.std.BlankClip(width=64, height=64, format=vs.GRAY8, length=1)

    os.makedirs(TEST_IR_DIR, exist_ok=True)
    dump_path = os.path.join(TEST_IR_DIR, f"test_combined_{uuid.uuid4().hex[:8]}.ll")

    try:
        result = core.llvmexpr.SingleExpr(
            [clip],
            expr="100 data{}^ 200 data{}^ 42.0 0 data{}! 0 data{}@ result$",
            dump_ir=dump_path,
        )
        result.get_frame(0)

        ir_content = get_ir_content(dump_path)

        assert count_array_allocations(ir_content) == 1
        assert "i64 200" in ir_content
        assert "i64 100" not in ir_content

    finally:
        for f in glob.glob(dump_path.replace(".ll", "*")):
            try:
                os.remove(f)
            except FileNotFoundError:
                pass
