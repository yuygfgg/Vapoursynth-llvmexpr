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

#ifndef LLVMEXPR_ANALYSIS_PASSES_RELACCESSANALYSISPASS_HPP
#define LLVMEXPR_ANALYSIS_PASSES_RELACCESSANALYSISPASS_HPP

#include "../framework/DataStructures.hpp"
#include "../framework/Pass.hpp"

namespace analysis {

struct RelAccessAnalysisResult {
    std::vector<RelYAccess> unique_rel_y_accesses;
    int min_rel_x{0};
    int max_rel_x{0};
    bool mirror_boundary{false};
};

/**
    Analyzes the expression to identify all relative clip accesses.
    Collects:
    - All unique relative y-accesses and their mirroring modes.
    - The minimum and maximum relative x-accesses across the expression.
    This information is used by the IR generator to optimize memory access patterns.
    Depends on: None
 */
class RelAccessAnalysisPass
    : public AnalysisPass<RelAccessAnalysisPass, RelAccessAnalysisResult> {
  public:
    RelAccessAnalysisResult run(const std::vector<Token>& tokens,
                                AnalysisManager& am) override;

    [[nodiscard]] const char* getName() const override {
        return "RelAccessAnalysisPass";
    }
};

} // namespace analysis

#endif // LLVMEXPR_ANALYSIS_PASSES_RELACCESSANALYSISPASS_HPP
