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

#ifndef LLVMEXPR_ANALYSIS_PASSES_COORDINATEUSAGEPASS_HPP
#define LLVMEXPR_ANALYSIS_PASSES_COORDINATEUSAGEPASS_HPP

#include "../framework/Pass.hpp"

namespace analysis {

struct CoordinateUsageResult {
    bool uses_x{false};
    bool uses_y{false};
};

/**
    Analyzes the expression to determine if 'x' or 'y' coordinates are used.
    Collects:
    - A boolean indicating if 'x' is used.
    - A boolean indicating if 'y' is used.
    This information is used by the IR generator to avoid generating unnecessary
    coordinate variables and calculations.
    Depends on: None
 */
class CoordinateUsagePass
    : public AnalysisPass<CoordinateUsagePass, CoordinateUsageResult> {
  public:
    CoordinateUsageResult run(const std::vector<Token>& tokens,
                              AnalysisManager& am) override;

    [[nodiscard]] const char* getName() const override {
        return "CoordinateUsagePass";
    }
};

} // namespace analysis

#endif // LLVMEXPR_ANALYSIS_PASSES_COORDINATEUSAGEPASS_HPP
