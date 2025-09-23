/**
 * Copyright (C) 2025 yuygfgg
 * 
 * This file is part of Vapoursynth-llvmexpr.
 * 
 * Vapoursynth-llvmexpr is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Vapoursynth-llvmexpr is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with Vapoursynth-llvmexpr.  If not, see <https://www.gnu.org/licenses/>.
 */

#ifndef LLVMEXPR_MATH_CONFIG_HPP
#define LLVMEXPR_MATH_CONFIG_HPP

#include <utility>

enum class MathOp { Exp, Log, Sin, Cos, Tan };

// VectorWidth = 1 is used for scalar version, this file defines vector versions.
#ifdef __x86_64__
using SupportedVectorWidths = std::integer_sequence<int, 4, 8, 16>;
#else
using SupportedVectorWidths = std::integer_sequence<int, 4>;
#endif

#endif // LLVMEXPR_MATH_CONFIG_HPP
