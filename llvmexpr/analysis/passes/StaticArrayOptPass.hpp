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

#ifndef LLVMEXPR_ANALYSIS_STATIC_ARRAY_OPT_PASS_HPP
#define LLVMEXPR_ANALYSIS_STATIC_ARRAY_OPT_PASS_HPP

#include "../framework/Pass.hpp"
#include <map>
#include <string>

namespace analysis {

struct StaticArrayOptResult {
    std::map<std::string, int> static_array_sizes;
};

/**
    Identifies arrays that can be allocated on the stack instead of the heap.
    This optimization requires:
    1. Array size must be known at compile time (via constant propagation)
    2. Array size must be <= threshold (1024 elements)
    3. Array must be allocated only once in the entire expression
    Depends on: ConstPropPass
 */
class StaticArrayOptPass
    : public AnalysisPass<StaticArrayOptPass, StaticArrayOptResult> {
  public:
    using Result = StaticArrayOptResult;

    [[nodiscard]] const char* getName() const override { return "Static Array Optimization Pass"; }

    Result run(const std::vector<Token>& tokens, AnalysisManager& am) override;
};

} // namespace analysis

#endif // LLVMEXPR_ANALYSIS_STATIC_ARRAY_OPT_PASS_HPP
