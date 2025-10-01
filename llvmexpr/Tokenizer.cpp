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

#include "Tokenizer.hpp"

#include <algorithm>
#include <format>
#include <functional>
#include <optional>
#include <regex>
#include <sstream>
#include <stdexcept>
#include <string_view>
#include <utility>

namespace {

using TokenParser = std::function<std::optional<Token>(std::string_view)>;

struct TokenInfo {
    TokenType type;
    std::string_view name;
    BehaviorResolver behavior;
    TokenParser parser;
};

TokenInfo define_keyword(TokenType type, std::string_view keyword,
                         TokenBehavior behavior) {
    return {type, keyword, behavior,
            [type, keyword](std::string_view input) -> std::optional<Token> {
                if (input == keyword) {
                    return Token{type, std::string(input), std::monostate{}};
                }
                return std::nullopt;
            }};
}

TokenInfo define_regex(
    TokenType type, std::string_view name, BehaviorResolver behavior,
    const std::regex& re,
    std::function<Token::PayloadVariant(const std::smatch&)> payload_builder) {
    return {type, name, behavior,
            [type, re,
             payload_builder](std::string_view input) -> std::optional<Token> {
                std::string s(input);
                std::smatch match;
                if (std::regex_match(s, match, re)) {
                    return Token{type, s, payload_builder(match)};
                }
                return std::nullopt;
            }};
}

static const std::vector<TokenInfo> token_definitions = {
    define_keyword(TokenType::ADD, "+", {2, -1}),

    define_keyword(TokenType::SUB, "-", {2, -1}),

    define_keyword(TokenType::MUL, "*", {2, -1}),

    define_keyword(TokenType::DIV, "/", {2, -1}),

    define_keyword(TokenType::MOD, "%", {2, -1}),

    define_keyword(TokenType::GT, ">", {2, -1}),

    define_keyword(TokenType::LT, "<", {2, -1}),

    define_keyword(TokenType::EQ, "=", {2, -1}),

    define_keyword(TokenType::TERNARY, "?", {3, -2}),

    define_keyword(TokenType::CONSTANT_X, "X", {0, 1}),

    define_keyword(TokenType::CONSTANT_Y, "Y", {0, 1}),

    define_keyword(TokenType::CONSTANT_N, "N", {0, 1}),

    define_keyword(TokenType::GE, ">=", {2, -1}),

    define_keyword(TokenType::LE, "<=", {2, -1}),

    define_keyword(TokenType::POW, "**", {2, -1}),

    define_keyword(TokenType::OR, "or", {2, -1}),

    define_keyword(TokenType::CONSTANT_PI, "pi", {0, 1}),

    define_keyword(TokenType::AND, "and", {2, -1}),

    define_keyword(TokenType::XOR, "xor", {2, -1}),

    define_keyword(TokenType::NOT, "not", {1, 0}),

    define_keyword(TokenType::POW, "pow", {2, -1}),

    define_keyword(TokenType::MIN, "min", {2, -1}),

    define_keyword(TokenType::MAX, "max", {2, -1}),

    define_keyword(TokenType::FMA, "fma", {3, -2}),

    define_keyword(TokenType::EXP, "exp", {1, 0}),

    define_keyword(TokenType::LOG, "log", {1, 0}),

    define_keyword(TokenType::ABS, "abs", {1, 0}),

    define_keyword(TokenType::SIN, "sin", {1, 0}),

    define_keyword(TokenType::COS, "cos", {1, 0}),

    define_keyword(TokenType::TAN, "tan", {1, 0}),

    define_keyword(TokenType::SGN, "sgn", {1, 0}),

    define_keyword(TokenType::NEG, "neg", {1, 0}),

    define_keyword(TokenType::STORE_ABS, "@[]", {3, -3}),

    define_keyword(TokenType::CLIP, "clip", {3, -2}),

    define_keyword(TokenType::SQRT, "sqrt", {1, 0}),

    define_keyword(TokenType::CEIL, "ceil", {1, 0}),

    define_keyword(TokenType::ASIN, "asin", {1, 0}),

    define_keyword(TokenType::ACOS, "acos", {1, 0}),

    define_keyword(TokenType::ATAN, "atan", {1, 0}),

    define_keyword(TokenType::EXP2, "exp2", {1, 0}),

    define_keyword(TokenType::LOG2, "log2", {1, 0}),

    define_keyword(TokenType::SINH, "sinh", {1, 0}),

    define_keyword(TokenType::COSH, "cosh", {1, 0}),

    define_keyword(TokenType::TANH, "tanh", {1, 0}),

    define_keyword(TokenType::BITOR, "bitor", {2, -1}),

    define_keyword(TokenType::ATAN2, "atan2", {2, -1}),

    define_keyword(TokenType::CLAMP, "clamp", {3, -2}),

    define_keyword(TokenType::FLOOR, "floor", {1, 0}),

    define_keyword(TokenType::TRUNC, "trunc", {1, 0}),

    define_keyword(TokenType::ROUND, "round", {1, 0}),

    define_keyword(TokenType::LOG10, "log10", {1, 0}),

    define_keyword(TokenType::CONSTANT_WIDTH, "width", {0, 1}),

    define_keyword(TokenType::BITAND, "bitand", {2, -1}),

    define_keyword(TokenType::BITXOR, "bitxor", {2, -1}),

    define_keyword(TokenType::BITNOT, "bitnot", {1, 0}),

    define_keyword(TokenType::CONSTANT_HEIGHT, "height", {0, 1}),

    define_keyword(TokenType::EXIT_NO_WRITE, "^exit^", {0, 1}),

    define_keyword(TokenType::COPYSIGN, "copysign", {2, -1}),

    define_regex(
        TokenType::DUP, "dupN",
        [](const Token& t) -> TokenBehavior {
            const auto& payload = std::get<TokenPayload_StackOp>(t.payload);
            return {.arity = payload.n + 1, .stack_effect = 1};
        },
        std::regex(R"(^dup(\d*)$)"),
        [](const std::smatch& m) -> Token::PayloadVariant {
            int n = m[1].str().empty() ? 0 : std::stoi(m[1].str());
            if (n < 0)
                throw std::runtime_error("Invalid dupN value");
            return TokenPayload_StackOp{n};
        }),

    define_regex(
        TokenType::DROP, "dropN",
        [](const Token& t) -> TokenBehavior {
            const auto& payload = std::get<TokenPayload_StackOp>(t.payload);
            return {.arity = payload.n, .stack_effect = -payload.n};
        },
        std::regex(R"(^drop(\d*)$)"),
        [](const std::smatch& m) -> Token::PayloadVariant {
            int n = m[1].str().empty() ? 1 : std::stoi(m[1].str());
            if (n < 0)
                throw std::runtime_error("Invalid dropN value");
            return TokenPayload_StackOp{n};
        }),

    define_regex(
        TokenType::SWAP, "swapN",
        [](const Token& t) -> TokenBehavior {
            const auto& payload = std::get<TokenPayload_StackOp>(t.payload);
            return {.arity = payload.n + 1, .stack_effect = 0};
        },
        std::regex(R"(^swap(\d*)$)"),
        [](const std::smatch& m) -> Token::PayloadVariant {
            int n = m[1].str().empty() ? 1 : std::stoi(m[1].str());
            if (n < 0)
                throw std::runtime_error("Invalid swapN value");
            return TokenPayload_StackOp{n};
        }),

    define_regex(
        TokenType::SORTN, "sortN",
        [](const Token& t) -> TokenBehavior {
            const auto& payload = std::get<TokenPayload_StackOp>(t.payload);
            return {.arity = payload.n, .stack_effect = 0};
        },
        std::regex(R"(^sort(\d+)$)"),
        [](const std::smatch& m) -> Token::PayloadVariant {
            int n = std::stoi(m[1].str());
            if (n < 0)
                throw std::runtime_error("Invalid sortN value");
            return TokenPayload_StackOp{n};
        }),

    define_regex(TokenType::LABEL_DEF, "label_def", TokenBehavior{0, 0},
                 std::regex(R"(^#(.+)$)"),
                 [](const std::smatch& m) -> Token::PayloadVariant {
                     return TokenPayload_Label{.name = m[1].str()};
                 }),

    define_regex(TokenType::JUMP, "jump", TokenBehavior{1, -1},
                 std::regex(R"(^(.+)#$)"),
                 [](const std::smatch& m) -> Token::PayloadVariant {
                     return TokenPayload_Label{.name = m[1].str()};
                 }),

    define_regex(TokenType::VAR_STORE, "var_store", TokenBehavior{1, -1},
                 std::regex(R"(^(.+)!$)"),
                 [](const std::smatch& m) -> Token::PayloadVariant {
                     return TokenPayload_Var{.name = m[1].str()};
                 }),

    define_regex(TokenType::VAR_LOAD, "var_load", TokenBehavior{0, 1},
                 std::regex(R"(^(.+)@$)"),
                 [](const std::smatch& m) -> Token::PayloadVariant {
                     return TokenPayload_Var{.name = m[1].str()};
                 }),

    define_regex(
        TokenType::CLIP_REL, "clip_rel", TokenBehavior{0, 1},
        std::regex(
            R"(^(?:src(\d+)|([x-za-w]))\[\s*(-?\d+)\s*,\s*(-?\d+)\s*\](?::([cm]))?$)"),
        [](const std::smatch& m) -> Token::PayloadVariant {
            TokenPayload_ClipAccess data;
            if (m[1].matched) {
                data.clip_idx = std::stoi(m[1].str());
            } else if (m[2].matched) {
                char c = m[2].str()[0];
                if (c >= 'x' && c <= 'z') {
                    data.clip_idx = c - 'x';
                } else {
                    data.clip_idx = c - 'a' + 3;
                }
            }
            data.rel_x = std::stoi(m[3].str());
            data.rel_y = std::stoi(m[4].str());
            if (m[5].matched) {
                data.has_mode = true;
                data.use_mirror = (m[5].str() == "m");
            }
            return data;
        }),

    define_regex(TokenType::CLIP_ABS, "clip_abs", TokenBehavior{2, -1},
                 std::regex(R"(^(?:src(\d+)|([x-za-w]))\[\](?::([mcb]))?$)"),
                 [](const std::smatch& m) -> Token::PayloadVariant {
                     TokenPayload_ClipAccess data;
                     if (m[1].matched) {
                         data.clip_idx = std::stoi(m[1].str());
                     } else if (m[2].matched) {
                         char c = m[2].str()[0];
                         if (c >= 'x' && c <= 'z') {
                             data.clip_idx = c - 'x';
                         } else {
                             data.clip_idx = c - 'a' + 3;
                         }
                     }
                     if (m[3].matched) {
                         char mode_char = m[3].str()[0];
                         if (mode_char == 'm') {
                             data.has_mode = true;
                             data.use_mirror = true;
                         } else if (mode_char == 'c') {
                             data.has_mode = true;
                             data.use_mirror = false;
                         } else if (mode_char == 'b') {
                             data.has_mode = false;
                         }
                     } else {
                         data.has_mode = true;
                         data.use_mirror = false;
                     }
                     return data;
                 }),

    define_regex(TokenType::CLIP_CUR, "clip_cur", TokenBehavior{0, 1},
                 std::regex(R"(^(?:src(\d+)|([x-za-w]))$)"),
                 [](const std::smatch& m) -> Token::PayloadVariant {
                     TokenPayload_ClipAccess data;
                     if (m[1].matched) {
                         data.clip_idx = std::stoi(m[1].str());
                     } else if (m[2].matched) {
                         char c = m[2].str()[0];
                         if (c >= 'x' && c <= 'z') {
                             data.clip_idx = c - 'x';
                         } else {
                             data.clip_idx = c - 'a' + 3;
                         }
                     }
                     return data;
                 }),

    define_regex(
        TokenType::PROP_ACCESS, "prop_access", TokenBehavior{0, 1},
        std::regex(R"(^(?:src(\d+)|([x-za-w]))\.([a-zA-Z_][a-zA-Z0-9_]*)$)"),
        [](const std::smatch& m) -> Token::PayloadVariant {
            TokenPayload_PropAccess data;
            if (m[1].matched) {
                data.clip_idx = std::stoi(m[1].str());
            } else if (m[2].matched) {
                char c = m[2].str()[0];
                if (c >= 'x' && c <= 'z') {
                    data.clip_idx = c - 'x';
                } else {
                    data.clip_idx = c - 'a' + 3;
                }
            }
            data.prop_name = m[3].str();
            return data;
        }),

    define_regex(
        TokenType::NUMBER, "number", TokenBehavior{0, 1},
        std::regex(
            R"(^(?:(0x[0-9a-fA-F]+(?:\.[0-9a-fA-F]+(?:p[+\-]?\d+)?)?)|(0[0-7]+)|([+\-]?\d+(?:\.\d+)?(?:[eE][+\-]?\d+)?))$)"),
        [](const std::smatch& m) -> Token::PayloadVariant {
            std::string s = m.str();
            double val;
            if (m[2].matched) { // Octal integer
                val = static_cast<double>(std::stoll(s, nullptr, 0));
            } else { // Hex or decimal float/integer
                val = std::stod(s);
            }
            return TokenPayload_Number{val};
        })};

} // anonymous namespace

std::vector<Token> tokenize(const std::string& expr, int num_inputs) {
    std::vector<Token> tokens;
    std::stringstream ss(expr);
    std::string str_token;
    int idx = 0;

    while (ss >> str_token) {
        std::optional<Token> parsed_token;

        for (const auto& definition : token_definitions) {
            if ((parsed_token = definition.parser(str_token))) {
                break;
            }
        }

        if (!parsed_token) {
            throw std::runtime_error(
                std::format("Invalid token: {} (idx {})", str_token, idx));
        }

        // Post-parse validation for clip indices
        if (parsed_token->type == TokenType::CLIP_REL ||
            parsed_token->type == TokenType::CLIP_ABS ||
            parsed_token->type == TokenType::CLIP_CUR) {
            if (std::get<TokenPayload_ClipAccess>(parsed_token->payload)
                        .clip_idx < 0 ||
                std::get<TokenPayload_ClipAccess>(parsed_token->payload)
                        .clip_idx >= num_inputs) {
                throw std::runtime_error(
                    std::format("Invalid clip index in token: {} (idx {})",
                                str_token, idx));
            }
        } else if (parsed_token->type == TokenType::PROP_ACCESS) {
            if (std::get<TokenPayload_PropAccess>(parsed_token->payload)
                        .clip_idx < 0 ||
                std::get<TokenPayload_PropAccess>(parsed_token->payload)
                        .clip_idx >= num_inputs) {
                throw std::runtime_error(
                    std::format("Invalid clip index in token: {} (idx {})",
                                str_token, idx));
            }
        }

        tokens.push_back(*parsed_token);
        idx++;
    }
    return tokens;
}

TokenBehavior get_token_behavior(const Token& token) {
    auto it =
        std::find_if(token_definitions.begin(), token_definitions.end(),
                     [&](const auto& def) { return def.type == token.type; });

    if (it == token_definitions.end()) {
        std::unreachable();
    }

    return std::visit(
        [&token](auto&& arg) -> TokenBehavior {
            using T = std::decay_t<decltype(arg)>;
            if constexpr (std::is_same_v<T, TokenBehavior>) {
                return arg;
            } else if constexpr (std::is_same_v<T, DynamicBehaviorFn>) {
                return arg(token);
            }
        },
        it->behavior);
}
