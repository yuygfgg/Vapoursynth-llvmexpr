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

#include "AnalysisManager.hpp"

namespace analysis {

AnalysisManager::AnalysisManager(const std::vector<Token>& tokens_in,
                                 bool mirror_boundary_in,
                                 int expected_final_depth_in)
    : tokens(tokens_in), mirror_boundary(mirror_boundary_in),
      expected_final_depth(expected_final_depth_in) {}

void AnalysisManager::invalidate(const PreservedAnalyses& pa) {
    if (pa.preservedAll()) {
        return;
    }

    std::vector<std::type_index> to_invalidate;

    for (const auto& [pass_type, _] : results.getAll()) {
        if (!pa.isPreserved(pass_type)) {
            to_invalidate.push_back(pass_type);
        }
    }

    for (const auto& pass_type : to_invalidate) {
        results.invalidate(pass_type);
    }
}

} // namespace analysis
