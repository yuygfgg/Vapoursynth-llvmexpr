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

#include "StaticArrayOptPass.hpp"
#include "../../frontend/Tokenizer.hpp"
#include "../framework/AnalysisManager.hpp"
#include "BlockAnalysisPass.hpp"
#include "ConstPropPass.hpp"
#include <algorithm>
#include <map>
#include <optional>

namespace {
constexpr int SINGLEEXPR_STACK_ALLOC_THRESHOLD = 1024;
} // namespace

namespace analysis {

StaticArrayOptPass::Result
StaticArrayOptPass::run(const std::vector<Token>& tokens, AnalysisManager& am) {
    const auto& const_result = am.getResult<ConstPropPass>();
    const auto& in_stacks = const_result.const_stack_in;

    const auto& block_result = am.getResult<BlockAnalysisPass>();
    const auto& cfg_blocks = block_result.cfg_blocks;

    Result result;

    std::map<std::string, int> array_alloc_count;
    for (const auto& token : tokens) {
        if (token.type == TokenType::ARRAY_ALLOC_STATIC ||
            token.type == TokenType::ARRAY_ALLOC_DYN) {
            const auto& payload = std::get<TokenPayload_ArrayOp>(token.payload);
            array_alloc_count[payload.name]++;
        }
    }

    for (size_t block_idx = 0; block_idx < cfg_blocks.size(); ++block_idx) {
        const auto& block = cfg_blocks[block_idx];

        auto current_stack = in_stacks[block_idx];

        for (int token_idx = block.start_token_idx;
             token_idx < block.end_token_idx; ++token_idx) {
            const auto& token = tokens[token_idx];
            const auto behavior = get_token_behavior(token);

            while (current_stack.size() < static_cast<size_t>(behavior.arity)) {
                current_stack.insert(current_stack.begin(), std::nullopt);
            }

            std::vector<std::optional<double>> args;
            for (int k = 0; k < behavior.arity; ++k) {
                args.push_back(current_stack.back());
                current_stack.pop_back();
            }
            std::ranges::reverse(args);

            if (token.type == TokenType::ARRAY_ALLOC_STATIC) {
                const auto& payload =
                    std::get<TokenPayload_ArrayOp>(token.payload);
                // Allocate on stack if size <= threshold and only allocated once
                if (payload.static_size <= SINGLEEXPR_STACK_ALLOC_THRESHOLD &&
                    array_alloc_count[payload.name] == 1) {
                    result.static_array_sizes[payload.name] =
                        payload.static_size;
                }
            } else if (token.type == TokenType::ARRAY_ALLOC_DYN) {
                const auto& payload =
                    std::get<TokenPayload_ArrayOp>(token.payload);
                // Allocate on stack if:
                // 1. Size is a compile-time constant (from ConstPropPass)
                // 2. Size <= threshold
                // 3. Only allocated once
                if (args[0].has_value() &&
                    array_alloc_count[payload.name] == 1) {
                    int size = static_cast<int>(args[0].value());
                    if (size <= SINGLEEXPR_STACK_ALLOC_THRESHOLD) {
                        result.static_array_sizes[payload.name] = size;
                    }
                }
            }

            for (int k = 0; k < behavior.arity + behavior.stack_effect; ++k) {
                current_stack.emplace_back(std::nullopt);
            }
        }
    }

    return result;
}

} // namespace analysis
