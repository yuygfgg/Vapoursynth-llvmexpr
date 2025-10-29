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

#ifndef LLVMEXPR_ANALYSIS_FRAMEWORK_DATA_STRUCTURES_HPP
#define LLVMEXPR_ANALYSIS_FRAMEWORK_DATA_STRUCTURES_HPP

#include <tuple>
#include <vector>

namespace analysis {

struct CFGBlock {
    int start_token_idx;
    int end_token_idx; // exclusive
    std::vector<int> successors;
    std::vector<int> predecessors;

    int stack_effect = 0;
    int min_stack_needed =
        0; // min stack depth during the block, relative to start
};

struct RelYAccess {
    int clip_idx;
    int rel_y;
    bool use_mirror;

    bool operator<(const RelYAccess& other) const {
        return std::tie(clip_idx, rel_y, use_mirror) <
               std::tie(other.clip_idx, other.rel_y, other.use_mirror);
    }
};

} // namespace analysis

#endif // LLVMEXPR_ANALYSIS_FRAMEWORK_DATA_STRUCTURES_HPP
