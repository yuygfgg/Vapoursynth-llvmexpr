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
#include <cmath>
#include <format>
#include <iterator>
#include <numbers>
#include <optional>
#include <set>
#include <stdexcept>

namespace {
constexpr int SINGLEEXPR_STACK_ALLOC_THRESHOLD = 1024;
} // namespace

ExpressionAnalyser::ExpressionAnalyser(const std::vector<Token>& tokens_in,
                                       int expected_final_depth_in)
    : tokens(tokens_in), expected_final_depth(expected_final_depth_in) {}

void ExpressionAnalyser::run() { validate_and_build_cfg(); }

void ExpressionAnalyser::validate_and_build_cfg() {
    if (tokens.empty()) {
        if (expected_final_depth == 0) {
            return; // SingleExpr
        }
        throw std::runtime_error("Expression cannot be empty.");
    }

    // Clear previous state
    results.cfg_blocks.clear();
    results.label_to_block_idx.clear();
    results.stack_depth_in.clear();

    std::map<int, int> token_idx_to_block_idx;

    int current_token_idx = 0;
    while (static_cast<size_t>(current_token_idx) < tokens.size()) {
        int block_idx = static_cast<int>(results.cfg_blocks.size());
        CFGBlock block;
        block.start_token_idx = current_token_idx;

        int block_start_idx = current_token_idx;
        token_idx_to_block_idx[block_start_idx] = block_idx;

        if (tokens[current_token_idx].type == TokenType::LABEL_DEF) {
            const auto& payload =
                std::get<TokenPayload_Label>(tokens[current_token_idx].payload);
            if (results.label_to_block_idx.contains(payload.name)) {
                throw std::runtime_error(
                    std::format("Duplicate label: {} (idx {})", payload.name,
                                current_token_idx));
            }
            results.label_to_block_idx[payload.name] = block_idx;
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
        results.cfg_blocks.push_back(block);
        current_token_idx = scan_idx;
    }

    for (size_t i = 0; i < results.cfg_blocks.size(); ++i) {
        CFGBlock& block = results.cfg_blocks[i];
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
            if (!results.label_to_block_idx.contains(payload.name)) {
                throw std::runtime_error(
                    std::format("Undefined label for jump: {} (idx {})",
                                payload.name, block.end_token_idx - 1));
            }
            int target_block_idx = results.label_to_block_idx.at(payload.name);
            block.successors.push_back(target_block_idx);
            results.cfg_blocks[target_block_idx].predecessors.push_back(
                static_cast<int>(i));

            if (static_cast<size_t>(block.end_token_idx) < tokens.size()) {
                int fallthrough_block_idx =
                    token_idx_to_block_idx.at(block.end_token_idx);
                block.successors.push_back(fallthrough_block_idx);
                results.cfg_blocks[fallthrough_block_idx]
                    .predecessors.push_back(static_cast<int>(i));
            }
        } else {
            if (static_cast<size_t>(block.end_token_idx) < tokens.size()) {
                int fallthrough_block_idx =
                    token_idx_to_block_idx.at(block.end_token_idx);
                block.successors.push_back(fallthrough_block_idx);
                results.cfg_blocks[fallthrough_block_idx]
                    .predecessors.push_back(static_cast<int>(i));
            }
        }
    }

    // Data-flow analysis for initialized variables and arrays
    std::set<std::string> all_vars;
    for (const auto& token : tokens) {
        if (token.type == TokenType::VAR_STORE ||
            token.type == TokenType::VAR_LOAD) {
            all_vars.insert(std::get<TokenPayload_Var>(token.payload).name);
        } else if (token.type == TokenType::ARRAY_ALLOC_STATIC ||
                   token.type == TokenType::ARRAY_ALLOC_DYN ||
                   token.type == TokenType::ARRAY_STORE ||
                   token.type == TokenType::ARRAY_LOAD) {
            all_vars.insert(std::get<TokenPayload_ArrayOp>(token.payload).name +
                            "{}");
        }
    }

    std::vector<std::set<std::string>> gen_sets(results.cfg_blocks.size());
    for (size_t i = 0; i < results.cfg_blocks.size(); ++i) {
        for (int j = results.cfg_blocks[i].start_token_idx;
             j < results.cfg_blocks[i].end_token_idx; ++j) {
            if (tokens[j].type == TokenType::VAR_STORE) {
                gen_sets[i].insert(
                    std::get<TokenPayload_Var>(tokens[j].payload).name);
            } else if (tokens[j].type == TokenType::ARRAY_ALLOC_STATIC ||
                       tokens[j].type == TokenType::ARRAY_ALLOC_DYN) {
                gen_sets[i].insert(
                    std::get<TokenPayload_ArrayOp>(tokens[j].payload).name +
                    "{}");
            }
        }
    }

    std::vector<std::set<std::string>> static_alloc_gen_sets(
        results.cfg_blocks.size());
    for (size_t i = 0; i < results.cfg_blocks.size(); ++i) {
        for (int j = results.cfg_blocks[i].start_token_idx;
             j < results.cfg_blocks[i].end_token_idx; ++j) {
            if (tokens[j].type == TokenType::ARRAY_ALLOC_STATIC) {
                static_alloc_gen_sets[i].insert(
                    std::get<TokenPayload_ArrayOp>(tokens[j].payload).name +
                    "{}");
            }
        }
    }

    std::vector<std::set<std::string>> in_sets(results.cfg_blocks.size());
    std::vector<std::set<std::string>> out_sets(results.cfg_blocks.size(),
                                                all_vars);

    bool changed = true;
    while (changed) {
        changed = false;
        for (size_t i = 0; i < results.cfg_blocks.size(); ++i) {
            // IN[i] = Intersection of OUT[p] for all predecessors p
            std::set<std::string> new_in;
            if (results.cfg_blocks[i].predecessors.empty()) {
                // Entry block or unreachable. IN set is empty.
                new_in.clear();
            } else {
                new_in = out_sets[results.cfg_blocks[i].predecessors[0]];
                for (size_t j = 1;
                     j < results.cfg_blocks[i].predecessors.size(); ++j) {
                    int p_idx = results.cfg_blocks[i].predecessors[j];
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

    std::map<std::string, int> array_alloc_count;
    for (const auto& token : tokens) {
        if (token.type == TokenType::ARRAY_ALLOC_STATIC ||
            token.type == TokenType::ARRAY_ALLOC_DYN) {
            const auto& payload = std::get<TokenPayload_ArrayOp>(token.payload);
            array_alloc_count[payload.name]++;
        }
    }

    using ConstStack = std::vector<std::optional<double>>;
    std::vector<ConstStack> in_stacks(results.cfg_blocks.size());
    std::vector<ConstStack> out_stacks(results.cfg_blocks.size());

    if (!results.cfg_blocks.empty()) {
        in_stacks[0] = ConstStack{};
    }

    bool const_prop_changed = true;
    while (const_prop_changed) {
        const_prop_changed = false;
        for (size_t i = 0; i < results.cfg_blocks.size(); ++i) {
            // IN[i] = Merge of OUT[p] for all predecessors p
            ConstStack new_in;
            if (i > 0 && !results.cfg_blocks[i].predecessors.empty()) {
                new_in = out_stacks[results.cfg_blocks[i].predecessors[0]];
                for (size_t j = 1;
                     j < results.cfg_blocks[i].predecessors.size(); ++j) {
                    int p_idx = results.cfg_blocks[i].predecessors[j];
                    const auto& other_stack = out_stacks[p_idx];

                    if (new_in.size() != other_stack.size()) {
                        throw std::runtime_error(
                            std::format("Stack depth mismatch during constant "
                                        "propagation at "
                                        "block {}: {} vs {}",
                                        i, new_in.size(), other_stack.size()));
                    }

                    for (size_t k = 0; k < new_in.size(); ++k) {
                        if (new_in[k] != other_stack[k]) {
                            new_in[k] = std::nullopt;
                        }
                    }
                }
            } else {
                new_in = ConstStack{};
            }

            if (new_in != in_stacks[i]) {
                in_stacks[i] = new_in;
            }

            // Transfer function: OUT[i] = F(IN[i])
            ConstStack current_stack = in_stacks[i];
            for (int j = results.cfg_blocks[i].start_token_idx;
                 j < results.cfg_blocks[i].end_token_idx; ++j) {
                const auto& token = tokens[j];
                const auto behavior = get_token_behavior(token);

                if (current_stack.size() <
                    static_cast<size_t>(behavior.arity)) {
                    for (size_t k = 0; k < static_cast<size_t>(behavior.arity) -
                                               current_stack.size();
                         ++k) {
                        current_stack.insert(current_stack.begin(),
                                             std::nullopt);
                    }
                }

                std::vector<std::optional<double>> args;
                for (int k = 0; k < behavior.arity; ++k) {
                    args.push_back(current_stack.back());
                    current_stack.pop_back();
                }
                std::ranges::reverse(args);

                auto all_const = [&](const auto& a) { return a.has_value(); };
                bool can_compute = std::ranges::all_of(args, all_const);

                auto push_result = [&](std::optional<double> res) {
                    current_stack.push_back(res);
                };

                auto push_n = [&](int n, std::optional<double> val) {
                    for (int k = 0; k < n; ++k) {
                        current_stack.push_back(val);
                    }
                };

                switch (token.type) {
                case TokenType::NUMBER:
                    push_result(
                        std::get<TokenPayload_Number>(token.payload).value);
                    break;
                case TokenType::CONSTANT_PI:
                    push_result(std::numbers::pi_v<double>);
                    break;
                case TokenType::ADD:
                    push_result(can_compute ? std::optional(args[0].value() +
                                                            args[1].value())
                                            : std::nullopt);
                    break;
                case TokenType::SUB:
                    push_result(can_compute ? std::optional(args[0].value() -
                                                            args[1].value())
                                            : std::nullopt);
                    break;
                case TokenType::MUL:
                    push_result(can_compute ? std::optional(args[0].value() *
                                                            args[1].value())
                                            : std::nullopt);
                    break;
                case TokenType::DIV:
                    push_result(can_compute ? std::optional(args[0].value() /
                                                            args[1].value())
                                            : std::nullopt);
                    break;
                case TokenType::POW:
                    push_result(can_compute
                                    ? std::optional(
                                          pow(args[0].value(), args[1].value()))
                                    : std::nullopt);
                    break;
                case TokenType::ARRAY_ALLOC_STATIC: {
                    const auto& payload =
                        std::get<TokenPayload_ArrayOp>(token.payload);
                    // Allocate on stack if size <= threshold
                    if (payload.static_size <=
                            SINGLEEXPR_STACK_ALLOC_THRESHOLD &&
                        array_alloc_count[payload.name] == 1) {
                        results.static_array_sizes[payload.name] =
                            payload.static_size;
                    }
                } break;
                case TokenType::ARRAY_ALLOC_DYN: {
                    const auto& payload =
                        std::get<TokenPayload_ArrayOp>(token.payload);
                    // Allocate on stack if:
                    // 1. Size is a compile-time constant
                    // 2. Size <= threshold
                    // 3. Only allocated once
                    if (args[0].has_value() &&
                        array_alloc_count[payload.name] == 1) {
                        int size = static_cast<int>(args[0].value());
                        if (size <= SINGLEEXPR_STACK_ALLOC_THRESHOLD) {
                            results.static_array_sizes[payload.name] = size;
                        }
                    }
                } break;
                default:
                    push_n(behavior.arity + behavior.stack_effect,
                           std::nullopt);
                    break;
                }
            }

            if (current_stack != out_stacks[i]) {
                out_stacks[i] = current_stack;
                const_prop_changed = true;
            }
        }
    }

    // Data-flow analysis for statically allocated arrays.
    std::vector<std::set<std::string>> static_alloc_in_sets(
        results.cfg_blocks.size());
    std::vector<std::set<std::string>> static_alloc_out_sets(
        results.cfg_blocks.size());

    changed = true;
    while (changed) {
        changed = false;
        for (size_t i = 0; i < results.cfg_blocks.size(); ++i) {
            // IN[i] = Union of OUT[p] for all predecessors p
            std::set<std::string> new_in;
            for (int p_idx : results.cfg_blocks[i].predecessors) {
                new_in.insert(static_alloc_out_sets[p_idx].begin(),
                              static_alloc_out_sets[p_idx].end());
            }

            if (new_in != static_alloc_in_sets[i]) {
                static_alloc_in_sets[i] = new_in;
                changed =
                    true; // Not strictly needed, but helps convergence view
            }

            // OUT[i] = GEN[i] U IN[i]
            std::set<std::string> new_out = static_alloc_gen_sets[i];
            new_out.insert(static_alloc_in_sets[i].begin(),
                           static_alloc_in_sets[i].end());

            if (new_out != static_alloc_out_sets[i]) {
                static_alloc_out_sets[i] = new_out;
                changed = true;
            }
        }
    }

    // Validate variables and arrays
    for (size_t i = 0; i < results.cfg_blocks.size(); ++i) {
        std::set<std::string> defined_in_block = in_sets[i];
        std::set<std::string> static_in_block = static_alloc_in_sets[i];
        for (int j = results.cfg_blocks[i].start_token_idx;
             j < results.cfg_blocks[i].end_token_idx; ++j) {
            const auto& token = tokens[j];
            if (token.type == TokenType::VAR_LOAD) {
                const auto& payload = std::get<TokenPayload_Var>(token.payload);
                if (!defined_in_block.contains(payload.name)) {
                    throw std::runtime_error(
                        std::format("Variable is uninitialized: {} (idx {})",
                                    payload.name, j));
                }
            } else if (token.type == TokenType::VAR_STORE) {
                defined_in_block.insert(
                    std::get<TokenPayload_Var>(token.payload).name);
            } else if (token.type == TokenType::ARRAY_LOAD ||
                       token.type == TokenType::ARRAY_STORE) {
                const auto& payload =
                    std::get<TokenPayload_ArrayOp>(token.payload);
                std::string array_name = payload.name + "{}";
                if (!defined_in_block.contains(array_name)) {
                    throw std::runtime_error(std::format(
                        "Array is uninitialized: {} (idx {})", array_name, j));
                }
            } else if (token.type == TokenType::ARRAY_ALLOC_STATIC ||
                       token.type == TokenType::ARRAY_ALLOC_DYN) {
                const auto& payload =
                    std::get<TokenPayload_ArrayOp>(token.payload);
                std::string array_name = payload.name + "{}";
                if (static_in_block.contains(array_name)) {
                    throw std::runtime_error(
                        std::format("Statically allocated array cannot be "
                                    "reallocated: {} (idx {})",
                                    payload.name, j));
                }
                if (token.type == TokenType::ARRAY_ALLOC_STATIC) {
                    static_in_block.insert(array_name);
                }
                defined_in_block.insert(array_name);
            }
        }
    }

    results.stack_depth_in.assign(results.cfg_blocks.size(), -1);
    std::vector<int> worklist;

    if (!results.cfg_blocks.empty()) {
        results.stack_depth_in[0] = 0;
        worklist.push_back(0);
    }

    size_t processed_count = 0;
    while (!worklist.empty()) {
        processed_count++;
        if (processed_count > results.cfg_blocks.size() *
                                  results.cfg_blocks.size()) { // Heuristic
            throw std::runtime_error(std::format(
                "Failed to prove stack safety; check loops. "
                "processed_count = {}, "
                "cfg_blocks = {}, heuristic_limit = {}, worklist_size = "
                "{}, next_block_candidate = {}",
                processed_count, results.cfg_blocks.size(),
                results.cfg_blocks.size() * results.cfg_blocks.size(),
                worklist.size(), worklist.empty() ? -1 : worklist.back()));
        }

        int block_idx = worklist.back();
        worklist.pop_back();

        const auto& block = results.cfg_blocks[block_idx];
        int depth_in = results.stack_depth_in[block_idx];

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
            if (results.stack_depth_in[succ_idx] == -1) {
                results.stack_depth_in[succ_idx] = depth_out;
                worklist.push_back(succ_idx);
            } else if (results.stack_depth_in[succ_idx] != depth_out) {
                throw std::runtime_error(std::format(
                    "Stack depth mismatch on converging paths: block {} -> "
                    "successor {}. "
                    "incoming depth = {}, recorded successor depth = {}. "
                    "current start token '{}' (idx {}), successor start "
                    "token '{}' (idx {}).",
                    block_idx, succ_idx, depth_out,
                    results.stack_depth_in[succ_idx],
                    tokens[block.start_token_idx].text, block.start_token_idx,
                    tokens[results.cfg_blocks[succ_idx].start_token_idx].text,
                    results.cfg_blocks[succ_idx].start_token_idx));
            }
        }
    }

    for (size_t i = 0; i < results.cfg_blocks.size(); ++i) {
        if (results.stack_depth_in[i] != -1 &&
            results.cfg_blocks[i]
                .successors.empty()) { // Reachable terminal block
            int final_depth =
                results.stack_depth_in[i] + results.cfg_blocks[i].stack_effect;
            if (final_depth != expected_final_depth) {
                throw std::runtime_error(std::format(
                    "Expression stack not balanced on "
                    "reachable terminal block {}: "
                    "final depth = {}, expected = {}. start "
                    "token '{}' (idx {}).",
                    i, final_depth, expected_final_depth,
                    tokens[results.cfg_blocks[i].start_token_idx].text,
                    results.cfg_blocks[i].start_token_idx));
            }
        }
    }
}
