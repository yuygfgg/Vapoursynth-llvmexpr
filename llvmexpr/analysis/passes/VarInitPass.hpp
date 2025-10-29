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

#ifndef LLVMEXPR_ANALYSIS_VAR_INIT_PASS_HPP
#define LLVMEXPR_ANALYSIS_VAR_INIT_PASS_HPP

#include "../framework/Pass.hpp"
#include <set>
#include <string>
#include <vector>

namespace analysis {

struct VarInitResult {
    std::vector<std::set<std::string>> var_init_in;
    std::vector<std::set<std::string>> var_init_out;
};

/**
    Performs forward dataflow analysis to determine which variables and arrays
    are definitely initialized at each program point. Uses intersection semantics
    at merge points (a variable is only considered initialized if it's initialized
    on ALL incoming paths).
    Depends on: BlockAnalysisPass
 */
class VarInitPass : public AnalysisPass<VarInitPass, VarInitResult> {
  public:
    using Result = VarInitResult;

    [[nodiscard]] const char* getName() const override {
        return "Variable Initialization Pass";
    }

    Result run(const std::vector<Token>& tokens, AnalysisManager& am) override;
};

} // namespace analysis

#endif // LLVMEXPR_ANALYSIS_VAR_INIT_PASS_HPP
