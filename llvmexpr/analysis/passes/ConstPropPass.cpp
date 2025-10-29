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

#include "ConstPropPass.hpp"
#include "../../frontend/Tokenizer.hpp"
#include "../framework/AnalysisError.hpp"
#include "../framework/AnalysisManager.hpp"
#include "BlockAnalysisPass.hpp"
#include <algorithm>
#include <cmath>
#include <format>
#include <numbers>

namespace analysis {

ConstPropPass::Result ConstPropPass::run(const std::vector<Token>& tokens,
                                         AnalysisManager& am) {
    using ConstStack = std::vector<std::optional<double>>;

    // Depend on BlockAnalysisPass for CFG structure
    const auto& block_result = am.getResult<BlockAnalysisPass>();
    const auto& cfg_blocks = block_result.cfg_blocks;

    Result result;

    // Initialize const stack states in results
    result.const_stack_in.resize(cfg_blocks.size());
    result.const_stack_out.resize(cfg_blocks.size());

    if (!cfg_blocks.empty()) {
        result.const_stack_in[0] = ConstStack{};
    }

    bool changed = true;
    while (changed) {
        changed = false;
        for (size_t i = 0; i < cfg_blocks.size(); ++i) {
            // IN[i] = Merge of OUT[p] for all predecessors p
            ConstStack new_in;
            if (i > 0 && !cfg_blocks[i].predecessors.empty()) {
                new_in =
                    result.const_stack_out[cfg_blocks[i].predecessors[0]];
                for (size_t j = 1; j < cfg_blocks[i].predecessors.size(); ++j) {
                    int p_idx = cfg_blocks[i].predecessors[j];
                    const auto& other_stack = result.const_stack_out[p_idx];

                    if (new_in.size() != other_stack.size()) {
                        throw AnalysisError(
                            std::format("Stack depth mismatch during constant "
                                       "propagation at block {}: {} vs {}",
                                       i, new_in.size(), other_stack.size()));
                    }

                    // Merge: if values differ, mark as non-constant
                    for (size_t k = 0; k < new_in.size(); ++k) {
                        if (new_in[k] != other_stack[k]) {
                            new_in[k] = std::nullopt;
                        }
                    }
                }
            } else {
                new_in = ConstStack{};
            }

            if (new_in != result.const_stack_in[i]) {
                result.const_stack_in[i] = new_in;
            }

            // Transfer function: OUT[i] = F(IN[i])
            ConstStack current_stack = result.const_stack_in[i];
            for (int j = cfg_blocks[i].start_token_idx;
                 j < cfg_blocks[i].end_token_idx; ++j) {
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

                // Evaluate constant expressions
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
                case TokenType::MOD:
                    push_result(can_compute
                                    ? std::optional(std::fmod(args[0].value(),
                                                              args[1].value()))
                                    : std::nullopt);
                    break;

                case TokenType::GT:
                    push_result(
                        can_compute
                            ? std::optional(
                                  args[0].value() > args[1].value() ? 1.0 : 0.0)
                            : std::nullopt);
                    break;
                case TokenType::LT:
                    push_result(
                        can_compute
                            ? std::optional(
                                  args[0].value() < args[1].value() ? 1.0 : 0.0)
                            : std::nullopt);
                    break;
                case TokenType::EQ:
                    push_result(
                        can_compute
                            ? std::optional(args[0].value() == args[1].value()
                                                ? 1.0
                                                : 0.0)
                            : std::nullopt);
                    break;
                case TokenType::GE:
                    push_result(
                        can_compute
                            ? std::optional(args[0].value() >= args[1].value()
                                                ? 1.0
                                                : 0.0)
                            : std::nullopt);
                    break;
                case TokenType::LE:
                    push_result(
                        can_compute
                            ? std::optional(args[0].value() <= args[1].value()
                                                ? 1.0
                                                : 0.0)
                            : std::nullopt);
                    break;

                // Logical operators
                case TokenType::AND:
                    push_result(can_compute
                                    ? std::optional((args[0].value() > 0 &&
                                                     args[1].value() > 0)
                                                        ? 1.0
                                                        : 0.0)
                                    : std::nullopt);
                    break;
                case TokenType::OR:
                    push_result(can_compute
                                    ? std::optional((args[0].value() > 0 ||
                                                     args[1].value() > 0)
                                                        ? 1.0
                                                        : 0.0)
                                    : std::nullopt);
                    break;
                case TokenType::XOR:
                    push_result(can_compute
                                    ? std::optional(((args[0].value() > 0) !=
                                                     (args[1].value() > 0))
                                                        ? 1.0
                                                        : 0.0)
                                    : std::nullopt);
                    break;
                case TokenType::NOT:
                    push_result(
                        can_compute
                            ? std::optional(args[0].value() > 0 ? 0.0 : 1.0)
                            : std::nullopt);
                    break;

                case TokenType::POW:
                    push_result(can_compute
                                    ? std::optional(std::pow(args[0].value(),
                                                             args[1].value()))
                                    : std::nullopt);
                    break;
                case TokenType::SQRT:
                    push_result(can_compute
                                    ? std::optional(std::sqrt(args[0].value()))
                                    : std::nullopt);
                    break;

                case TokenType::EXP:
                    push_result(can_compute
                                    ? std::optional(std::exp(args[0].value()))
                                    : std::nullopt);
                    break;
                case TokenType::EXP2:
                    push_result(can_compute
                                    ? std::optional(std::exp2(args[0].value()))
                                    : std::nullopt);
                    break;
                case TokenType::LOG:
                    push_result(can_compute
                                    ? std::optional(std::log(args[0].value()))
                                    : std::nullopt);
                    break;
                case TokenType::LOG2:
                    push_result(can_compute
                                    ? std::optional(std::log2(args[0].value()))
                                    : std::nullopt);
                    break;
                case TokenType::LOG10:
                    push_result(can_compute
                                    ? std::optional(std::log10(args[0].value()))
                                    : std::nullopt);
                    break;

                case TokenType::SIN:
                    push_result(can_compute
                                    ? std::optional(std::sin(args[0].value()))
                                    : std::nullopt);
                    break;
                case TokenType::COS:
                    push_result(can_compute
                                    ? std::optional(std::cos(args[0].value()))
                                    : std::nullopt);
                    break;
                case TokenType::TAN:
                    push_result(can_compute
                                    ? std::optional(std::tan(args[0].value()))
                                    : std::nullopt);
                    break;
                case TokenType::ASIN:
                    push_result(can_compute
                                    ? std::optional(std::asin(args[0].value()))
                                    : std::nullopt);
                    break;
                case TokenType::ACOS:
                    push_result(can_compute
                                    ? std::optional(std::acos(args[0].value()))
                                    : std::nullopt);
                    break;
                case TokenType::ATAN:
                    push_result(can_compute
                                    ? std::optional(std::atan(args[0].value()))
                                    : std::nullopt);
                    break;
                case TokenType::ATAN2:
                    push_result(can_compute
                                    ? std::optional(std::atan2(args[0].value(),
                                                               args[1].value()))
                                    : std::nullopt);
                    break;

                case TokenType::SINH:
                    push_result(can_compute
                                    ? std::optional(std::sinh(args[0].value()))
                                    : std::nullopt);
                    break;
                case TokenType::COSH:
                    push_result(can_compute
                                    ? std::optional(std::cosh(args[0].value()))
                                    : std::nullopt);
                    break;
                case TokenType::TANH:
                    push_result(can_compute
                                    ? std::optional(std::tanh(args[0].value()))
                                    : std::nullopt);
                    break;

                case TokenType::FLOOR:
                    push_result(can_compute
                                    ? std::optional(std::floor(args[0].value()))
                                    : std::nullopt);
                    break;
                case TokenType::CEIL:
                    push_result(can_compute
                                    ? std::optional(std::ceil(args[0].value()))
                                    : std::nullopt);
                    break;
                case TokenType::ROUND:
                    push_result(can_compute
                                    ? std::optional(std::round(args[0].value()))
                                    : std::nullopt);
                    break;
                case TokenType::TRUNC:
                    push_result(can_compute
                                    ? std::optional(std::trunc(args[0].value()))
                                    : std::nullopt);
                    break;

                case TokenType::ABS:
                    push_result(can_compute
                                    ? std::optional(std::abs(args[0].value()))
                                    : std::nullopt);
                    break;
                case TokenType::NEG:
                    push_result(can_compute ? std::optional(-args[0].value())
                                            : std::nullopt);
                    break;
                case TokenType::SGN:
                    push_result(can_compute ? std::optional([&]() -> double {
                        double val = args[0].value();
                        if (val < 0) {
                            return -1.0;
                        }
                        if (val > 0) {
                            return 1.0;
                        }
                        return 0.0;
                    }())
                                            : std::nullopt);
                    break;
                case TokenType::COPYSIGN:
                    push_result(can_compute
                                    ? std::optional(std::copysign(
                                          args[0].value(), args[1].value()))
                                    : std::nullopt);
                    break;
                case TokenType::FMA:
                    push_result(can_compute
                                    ? std::optional(std::fma(args[0].value(),
                                                             args[1].value(),
                                                             args[2].value()))
                                    : std::nullopt);
                    break;

                case TokenType::MAX:
                    push_result(can_compute
                                    ? std::optional(std::max(args[0].value(),
                                                             args[1].value()))
                                    : std::nullopt);
                    break;
                case TokenType::MIN:
                    push_result(can_compute
                                    ? std::optional(std::min(args[0].value(),
                                                             args[1].value()))
                                    : std::nullopt);
                    break;
                case TokenType::CLAMP:
                    push_result(can_compute
                                    ? std::optional(std::clamp(args[0].value(),
                                                               args[1].value(),
                                                               args[2].value()))
                                    : std::nullopt);
                    break;

                case TokenType::TERNARY:
                    push_result(can_compute
                                    ? std::optional(args[0].value() > 0
                                                        ? args[1].value()
                                                        : args[2].value())
                                    : std::nullopt);
                    break;

                case TokenType::BITAND:
                    push_result(can_compute
                                    ? std::optional(static_cast<double>(
                                          static_cast<int64_t>(
                                              std::nearbyint(args[0].value())) &
                                          static_cast<int64_t>(
                                              std::nearbyint(args[1].value()))))
                                    : std::nullopt);
                    break;
                case TokenType::BITOR:
                    push_result(can_compute
                                    ? std::optional(static_cast<double>(
                                          static_cast<int64_t>(
                                              std::nearbyint(args[0].value())) |
                                          static_cast<int64_t>(
                                              std::nearbyint(args[1].value()))))
                                    : std::nullopt);
                    break;
                case TokenType::BITXOR:
                    push_result(can_compute
                                    ? std::optional(static_cast<double>(
                                          static_cast<int64_t>(
                                              std::nearbyint(args[0].value())) ^
                                          static_cast<int64_t>(
                                              std::nearbyint(args[1].value()))))
                                    : std::nullopt);
                    break;
                case TokenType::BITNOT:
                    push_result(can_compute
                                    ? std::optional(static_cast<double>(
                                          ~static_cast<int64_t>(
                                              std::nearbyint(args[0].value()))))
                                    : std::nullopt);
                    break;

                default:
                    push_n(behavior.arity + behavior.stack_effect,
                           std::nullopt);
                    break;
                }
            }

            if (current_stack != result.const_stack_out[i]) {
                result.const_stack_out[i] = current_stack;
                changed = true;
            }
        }
    }

    return result;
}

} // namespace analysis
