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

#ifndef LLVMEXPR_ANALYSIS_HPP
#define LLVMEXPR_ANALYSIS_HPP

#include <map>
#include <string>
#include <tuple>
#include <vector>

#include "Tokenizer.hpp"

struct CFGBlock {
    int start_token_idx;
    int end_token_idx; // exclusive
    std::vector<int> successors;
    std::vector<int> predecessors;

    int stack_effect = 0;
    int min_stack_needed =
        0; // min stack depth *during* the block, relative to start
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

struct ExpressionAnalysisResults {
    std::vector<CFGBlock> cfg_blocks;
    std::map<std::string, int> label_to_block_idx;
    std::vector<int> stack_depth_in;
};

class ExpressionAnalyser {
  public:
    ExpressionAnalyser(const std::vector<Token>& tokens,
                       int expected_final_depth = 1);

    // Run the analysis
    void run();

    // Getter for results
    const ExpressionAnalysisResults& getResults() const { return results; }

  private:
    const std::vector<Token>& tokens;
    ExpressionAnalysisResults results;
    int expected_final_depth;

    void validate_and_build_cfg();
};

#endif // LLVMEXPR_ANALYSIS_HPP
