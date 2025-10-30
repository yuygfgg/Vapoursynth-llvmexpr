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

#ifndef LLVMEXPR_ANALYSIS_DYNAMIC_ARRAY_ALLOC_OPT_PASS_HPP
#define LLVMEXPR_ANALYSIS_DYNAMIC_ARRAY_ALLOC_OPT_PASS_HPP

#include "../framework/Pass.hpp"
#include <vector>

namespace analysis {

/**
    Eliminates redundant dynamic array allocations.
    If between two allocations of the same array, there are no reads or writes
    to that array on all possible control flow paths, the first allocation is
    replaced with a DROP instruction to maintain stack effects.
    
    Depends on: BlockAnalysisPass, BuildCFGPass
 */
class DynamicArrayAllocOptPass : public TransformationPass {
  public:
    [[nodiscard]] const char* getName() const override {
        return "Dynamic Array Allocation Optimization Pass";
    }

    PreservedAnalyses run(std::vector<Token>& tokens,
                          AnalysisManager& am) override;
};

} // namespace analysis

#endif // LLVMEXPR_ANALYSIS_DYNAMIC_ARRAY_ALLOC_OPT_PASS_HPP
