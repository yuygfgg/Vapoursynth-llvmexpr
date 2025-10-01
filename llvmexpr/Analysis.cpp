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

#include "Analysis.hpp"

#include <algorithm>
#include <format>
#include <iterator>
#include <set>
#include <stdexcept>

ExpressionAnalyser::ExpressionAnalyser(const std::vector<Token>& tokens_in)
    : tokens(tokens_in) {}

void ExpressionAnalyser::run() { validate_and_build_cfg(); }

void ExpressionAnalyser::validate_and_build_cfg() {
    if (tokens.empty()) {
        throw std::runtime_error("Expression cannot be empty.");
    }

    // Clear previous state
    cfg_blocks.clear();
    label_to_block_idx.clear();
    stack_depth_in.clear();

    std::map<int, int> token_idx_to_block_idx;

    int current_token_idx = 0;
    while (static_cast<size_t>(current_token_idx) < tokens.size()) {
        int block_idx = cfg_blocks.size();
        CFGBlock block;
        block.start_token_idx = current_token_idx;

        int block_start_idx = current_token_idx;
        token_idx_to_block_idx[block_start_idx] = block_idx;

        if (tokens[current_token_idx].type == TokenType::LABEL_DEF) {
            const auto& payload =
                std::get<TokenPayload_Label>(tokens[current_token_idx].payload);
            if (label_to_block_idx.count(payload.name)) {
                throw std::runtime_error(
                    std::format("Duplicate label: {} (idx {})", payload.name,
                                current_token_idx));
            }
            label_to_block_idx[payload.name] = block_idx;
        }

        int scan_idx = current_token_idx;
        while (static_cast<size_t>(scan_idx) < tokens.size()) {
            const auto& token = tokens[scan_idx];
            if (token.type == TokenType::JUMP) {
                scan_idx++;
                break;
            }
            if (scan_idx > block_start_idx &&
                token.type == TokenType::LABEL_DEF) {
                break;
            }
            scan_idx++;
        }
        block.end_token_idx = scan_idx;
        cfg_blocks.push_back(block);
        current_token_idx = scan_idx;
    }

    for (size_t i = 0; i < cfg_blocks.size(); ++i) {
        CFGBlock& block = cfg_blocks[i];
        int current_stack = 0;
        int min_stack_in_block = 0;
        for (int j = block.start_token_idx; j < block.end_token_idx; ++j) {
            const auto& token = tokens[j];
            const auto behavior = get_token_behavior(token);

            int items_needed = behavior.arity;
            if (current_stack < items_needed) {
                min_stack_in_block =
                    std::max(min_stack_in_block, items_needed - current_stack);
            }

            current_stack += behavior.stack_effect;
        }
        block.stack_effect = current_stack;
        block.min_stack_needed = min_stack_in_block;

        const auto& last_token = tokens[block.end_token_idx - 1];
        if (last_token.type == TokenType::JUMP) {
            const auto& payload =
                std::get<TokenPayload_Label>(last_token.payload);
            if (label_to_block_idx.find(payload.name) ==
                label_to_block_idx.end()) {
                throw std::runtime_error(
                    std::format("Undefined label for jump: {} (idx {})",
                                payload.name, block.end_token_idx - 1));
            }
            int target_block_idx = label_to_block_idx.at(payload.name);
            block.successors.push_back(target_block_idx);
            cfg_blocks[target_block_idx].predecessors.push_back(i);

            if (static_cast<size_t>(block.end_token_idx) < tokens.size()) {
                int fallthrough_block_idx =
                    token_idx_to_block_idx.at(block.end_token_idx);
                block.successors.push_back(fallthrough_block_idx);
                cfg_blocks[fallthrough_block_idx].predecessors.push_back(i);
            }
        } else {
            if (static_cast<size_t>(block.end_token_idx) < tokens.size()) {
                int fallthrough_block_idx =
                    token_idx_to_block_idx.at(block.end_token_idx);
                block.successors.push_back(fallthrough_block_idx);
                cfg_blocks[fallthrough_block_idx].predecessors.push_back(i);
            }
        }
    }

    // Data-flow analysis for initialized variables
    std::set<std::string> all_vars;
    for (const auto& token : tokens) {
        if (token.type == TokenType::VAR_STORE ||
            token.type == TokenType::VAR_LOAD) {
            all_vars.insert(std::get<TokenPayload_Var>(token.payload).name);
        }
    }

    std::vector<std::set<std::string>> gen_sets(cfg_blocks.size());
    for (size_t i = 0; i < cfg_blocks.size(); ++i) {
        for (int j = cfg_blocks[i].start_token_idx;
             j < cfg_blocks[i].end_token_idx; ++j) {
            if (tokens[j].type == TokenType::VAR_STORE) {
                gen_sets[i].insert(
                    std::get<TokenPayload_Var>(tokens[j].payload).name);
            }
        }
    }

    std::vector<std::set<std::string>> in_sets(cfg_blocks.size());
    std::vector<std::set<std::string>> out_sets(cfg_blocks.size(), all_vars);

    bool changed = true;
    while (changed) {
        changed = false;
        for (size_t i = 0; i < cfg_blocks.size(); ++i) {
            // IN[i] = Intersection of OUT[p] for all predecessors p
            std::set<std::string> new_in;
            if (cfg_blocks[i].predecessors.empty()) {
                // Entry block or unreachable. IN set is empty.
                new_in.clear();
            } else {
                new_in = out_sets[cfg_blocks[i].predecessors[0]];
                for (size_t j = 1; j < cfg_blocks[i].predecessors.size(); ++j) {
                    int p_idx = cfg_blocks[i].predecessors[j];
                    std::set<std::string> temp_intersect;
                    std::set_intersection(
                        new_in.begin(), new_in.end(), out_sets[p_idx].begin(),
                        out_sets[p_idx].end(),
                        std::inserter(temp_intersect, temp_intersect.begin()));
                    new_in = temp_intersect;
                }
            }
            in_sets[i] = new_in;

            // OUT[i] = GEN[i] U IN[i]
            std::set<std::string> new_out = gen_sets[i];
            new_out.insert(in_sets[i].begin(), in_sets[i].end());

            if (new_out != out_sets[i]) {
                out_sets[i] = new_out;
                changed = true;
            }
        }
    }

    // Validate variables
    for (size_t i = 0; i < cfg_blocks.size(); ++i) {
        std::set<std::string> defined_in_block = in_sets[i];
        for (int j = cfg_blocks[i].start_token_idx;
             j < cfg_blocks[i].end_token_idx; ++j) {
            const auto& token = tokens[j];
            if (token.type == TokenType::VAR_LOAD) {
                const auto& payload = std::get<TokenPayload_Var>(token.payload);
                if (defined_in_block.find(payload.name) ==
                    defined_in_block.end()) {
                    throw std::runtime_error(
                        std::format("Variable is uninitialized: {} (idx {})",
                                    payload.name, j));
                }
            } else if (token.type == TokenType::VAR_STORE) {
                defined_in_block.insert(
                    std::get<TokenPayload_Var>(token.payload).name);
            }
        }
    }

    stack_depth_in.assign(cfg_blocks.size(), -1);
    std::vector<int> worklist;

    if (!cfg_blocks.empty()) {
        stack_depth_in[0] = 0;
        worklist.push_back(0);
    }

    size_t processed_count = 0;
    while (!worklist.empty()) {
        processed_count++;
        if (processed_count >
            cfg_blocks.size() * cfg_blocks.size()) { // Heuristic
            throw std::runtime_error(std::format(
                "Failed to prove stack safety; check loops. "
                "processed_count = {}, "
                "cfg_blocks = {}, heuristic_limit = {}, worklist_size = "
                "{}, next_block_candidate = {}",
                processed_count, cfg_blocks.size(),
                cfg_blocks.size() * cfg_blocks.size(), worklist.size(),
                worklist.empty() ? -1 : worklist.back()));
        }

        int block_idx = worklist.back();
        worklist.pop_back();

        const auto& block = cfg_blocks[block_idx];
        int depth_in = stack_depth_in[block_idx];

        if (depth_in < block.min_stack_needed) {
            throw std::runtime_error(std::format(
                "Stack underflow before executing block {}: depth_in = {}, "
                "min_needed = {}. "
                "start token '{}' (idx {}).",
                block_idx, depth_in, block.min_stack_needed,
                tokens[block.start_token_idx].text, block.start_token_idx));
        }

        int depth_out = depth_in + block.stack_effect;

        for (int succ_idx : block.successors) {
            if (stack_depth_in[succ_idx] == -1) {
                stack_depth_in[succ_idx] = depth_out;
                worklist.push_back(succ_idx);
            } else if (stack_depth_in[succ_idx] != depth_out) {
                throw std::runtime_error(std::format(
                    "Stack depth mismatch on converging paths: block {} -> "
                    "successor {}. "
                    "incoming depth = {}, recorded successor depth = {}. "
                    "current start token '{}' (idx {}), successor start "
                    "token '{}' (idx {}).",
                    block_idx, succ_idx, depth_out, stack_depth_in[succ_idx],
                    tokens[block.start_token_idx].text, block.start_token_idx,
                    tokens[cfg_blocks[succ_idx].start_token_idx].text,
                    cfg_blocks[succ_idx].start_token_idx));
            }
        }
    }

    for (size_t i = 0; i < cfg_blocks.size(); ++i) {
        if (stack_depth_in[i] != -1 &&
            cfg_blocks[i].successors.empty()) { // Reachable terminal block
            int final_depth = stack_depth_in[i] + cfg_blocks[i].stack_effect;
            if (final_depth != 1) {
                throw std::runtime_error(std::format(
                    "Expression stack not balanced on "
                    "reachable terminal block {}: "
                    "final depth = {}, expected = 1. start "
                    "token '{}' (idx {}).",
                    i, final_depth, tokens[cfg_blocks[i].start_token_idx].text,
                    cfg_blocks[i].start_token_idx));
            }
        }
    }
}
