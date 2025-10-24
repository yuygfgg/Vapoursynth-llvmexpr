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
#include <cmath>
#include <format>
#include <functional>
#include <locale>
#include <optional>
#include <ranges>
#include <regex>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace {

using TokenParser = std::function<std::optional<Token>(std::string_view)>;

// TODO: Use std::from_chars when libc++ supports it.
inline double locale_independent_stod(const std::string& s) {
    std::istringstream iss(s);
    iss.imbue(std::locale::classic());
    double val = NAN;
    if (!(iss >> val) || !iss.eof()) {
        throw std::runtime_error(std::format("Failed to parse number: {}", s));
    }
    return val;
}

struct TokenInfo {
    TokenType type;
    std::string_view name;
    BehaviorResolver behavior;
    TokenParser parser;
};

TokenInfo define_keyword(TokenType type, std::string_view keyword,
                         TokenBehavior behavior) {
    return {.type = type,
            .name = keyword,
            .behavior = behavior,
            .parser = [type, keyword](
                          std::string_view input) -> std::optional<Token> {
                if (input == keyword) {
                    return Token{.type = type,
                                 .text = std::string(input),
                                 .payload = std::monostate{}};
                }
                return std::nullopt;
            }};
}

TokenInfo
define_regex(TokenType type, std::string_view name, BehaviorResolver behavior,
             const std::regex& re,
             const std::function<Token::PayloadVariant(const std::smatch&)>&
                 payload_builder) {
    return {.type = type,
            .name = name,
            .behavior = behavior,
            .parser = [type, re, payload_builder](
                          std::string_view input) -> std::optional<Token> {
                std::string s(input);
                std::smatch match;
                if (std::regex_match(s, match, re)) {
                    return Token{.type = type,
                                 .text = s,
                                 .payload = payload_builder(match)};
                }
                return std::nullopt;
            }};
}

const std::vector<TokenInfo>& get_token_definitions() {
    static const std::vector<TokenInfo> token_definitions = {
        define_keyword(TokenType::ADD, "+", {.arity = 2, .stack_effect = -1}),

        define_keyword(TokenType::SUB, "-", {.arity = 2, .stack_effect = -1}),

        define_keyword(TokenType::MUL, "*", {.arity = 2, .stack_effect = -1}),

        define_keyword(TokenType::DIV, "/", {.arity = 2, .stack_effect = -1}),

        define_keyword(TokenType::MOD, "%", {.arity = 2, .stack_effect = -1}),

        define_keyword(TokenType::GT, ">", {.arity = 2, .stack_effect = -1}),

        define_keyword(TokenType::LT, "<", {.arity = 2, .stack_effect = -1}),

        define_keyword(TokenType::EQ, "=", {.arity = 2, .stack_effect = -1}),

        define_keyword(TokenType::TERNARY, "?",
                       {.arity = 3, .stack_effect = -2}),

        define_keyword(TokenType::CONSTANT_X, "X",
                       {.arity = 0, .stack_effect = 1}),

        define_keyword(TokenType::CONSTANT_Y, "Y",
                       {.arity = 0, .stack_effect = 1}),

        define_keyword(TokenType::CONSTANT_N, "N",
                       {.arity = 0, .stack_effect = 1}),

        define_keyword(TokenType::GE, ">=", {.arity = 2, .stack_effect = -1}),

        define_keyword(TokenType::LE, "<=", {.arity = 2, .stack_effect = -1}),

        define_keyword(TokenType::POW, "**", {.arity = 2, .stack_effect = -1}),

        define_keyword(TokenType::OR, "or", {.arity = 2, .stack_effect = -1}),

        define_keyword(TokenType::CONSTANT_PI, "pi",
                       {.arity = 0, .stack_effect = 1}),

        define_keyword(TokenType::AND, "and", {.arity = 2, .stack_effect = -1}),

        define_keyword(TokenType::XOR, "xor", {.arity = 2, .stack_effect = -1}),

        define_keyword(TokenType::NOT, "not", {.arity = 1, .stack_effect = 0}),

        define_keyword(TokenType::POW, "pow", {.arity = 2, .stack_effect = -1}),

        define_keyword(TokenType::MIN, "min", {.arity = 2, .stack_effect = -1}),

        define_keyword(TokenType::MAX, "max", {.arity = 2, .stack_effect = -1}),

        define_keyword(TokenType::FMA, "fma", {.arity = 3, .stack_effect = -2}),

        define_keyword(TokenType::EXP, "exp", {.arity = 1, .stack_effect = 0}),

        define_keyword(TokenType::LOG, "log", {.arity = 1, .stack_effect = 0}),

        define_keyword(TokenType::ABS, "abs", {.arity = 1, .stack_effect = 0}),

        define_keyword(TokenType::SIN, "sin", {.arity = 1, .stack_effect = 0}),

        define_keyword(TokenType::COS, "cos", {.arity = 1, .stack_effect = 0}),

        define_keyword(TokenType::TAN, "tan", {.arity = 1, .stack_effect = 0}),

        define_keyword(TokenType::SGN, "sgn", {.arity = 1, .stack_effect = 0}),

        define_keyword(TokenType::NEG, "neg", {.arity = 1, .stack_effect = 0}),

        define_keyword(TokenType::STORE_ABS, "@[]",
                       {.arity = 3, .stack_effect = -3}),

        define_keyword(TokenType::CLIP, "clip",
                       {.arity = 3, .stack_effect = -2}),

        define_keyword(TokenType::SQRT, "sqrt",
                       {.arity = 1, .stack_effect = 0}),

        define_keyword(TokenType::CEIL, "ceil",
                       {.arity = 1, .stack_effect = 0}),

        define_keyword(TokenType::ASIN, "asin",
                       {.arity = 1, .stack_effect = 0}),

        define_keyword(TokenType::ACOS, "acos",
                       {.arity = 1, .stack_effect = 0}),

        define_keyword(TokenType::ATAN, "atan",
                       {.arity = 1, .stack_effect = 0}),

        define_keyword(TokenType::EXP2, "exp2",
                       {.arity = 1, .stack_effect = 0}),

        define_keyword(TokenType::LOG2, "log2",
                       {.arity = 1, .stack_effect = 0}),

        define_keyword(TokenType::SINH, "sinh",
                       {.arity = 1, .stack_effect = 0}),

        define_keyword(TokenType::COSH, "cosh",
                       {.arity = 1, .stack_effect = 0}),

        define_keyword(TokenType::TANH, "tanh",
                       {.arity = 1, .stack_effect = 0}),

        define_keyword(TokenType::BITOR, "bitor",
                       {.arity = 2, .stack_effect = -1}),

        define_keyword(TokenType::ATAN2, "atan2",
                       {.arity = 2, .stack_effect = -1}),

        define_keyword(TokenType::CLAMP, "clamp",
                       {.arity = 3, .stack_effect = -2}),

        define_keyword(TokenType::FLOOR, "floor",
                       {.arity = 1, .stack_effect = 0}),

        define_keyword(TokenType::TRUNC, "trunc",
                       {.arity = 1, .stack_effect = 0}),

        define_keyword(TokenType::ROUND, "round",
                       {.arity = 1, .stack_effect = 0}),

        define_keyword(TokenType::LOG10, "log10",
                       {.arity = 1, .stack_effect = 0}),

        define_keyword(TokenType::CONSTANT_WIDTH, "width",
                       {.arity = 0, .stack_effect = 1}),

        define_keyword(TokenType::BITAND, "bitand",
                       {.arity = 2, .stack_effect = -1}),

        define_keyword(TokenType::BITXOR, "bitxor",
                       {.arity = 2, .stack_effect = -1}),

        define_keyword(TokenType::BITNOT, "bitnot",
                       {.arity = 1, .stack_effect = 0}),

        define_keyword(TokenType::CONSTANT_HEIGHT, "height",
                       {.arity = 0, .stack_effect = 1}),

        define_regex(TokenType::CONSTANT_PLANE_WIDTH, "plane_width",
                     TokenBehavior{.arity = 0, .stack_effect = 1},
                     std::regex(R"(^width\^(\d+)$)"),
                     [](const std::smatch& m) -> Token::PayloadVariant {
                         return TokenPayload_PlaneDim{
                             .plane_idx = std::stoi(m[1].str())};
                     }),

        define_regex(TokenType::CONSTANT_PLANE_HEIGHT, "plane_height",
                     TokenBehavior{.arity = 0, .stack_effect = 1},
                     std::regex(R"(^height\^(\d+)$)"),
                     [](const std::smatch& m) -> Token::PayloadVariant {
                         return TokenPayload_PlaneDim{
                             .plane_idx = std::stoi(m[1].str())};
                     }),

        define_keyword(TokenType::EXIT_NO_WRITE, "^exit^",
                       {.arity = 0, .stack_effect = 1}),

        define_keyword(TokenType::COPYSIGN, "copysign",
                       {.arity = 2, .stack_effect = -1}),

        define_regex(
            TokenType::DUP, "dupN",
            [](const Token& t) -> TokenBehavior {
                const auto& payload = std::get<TokenPayload_StackOp>(t.payload);
                return {.arity = payload.n + 1, .stack_effect = 1};
            },
            std::regex(R"(^dup(\d*)$)"),
            [](const std::smatch& m) -> Token::PayloadVariant {
                int n = m[1].str().empty() ? 0 : std::stoi(m[1].str());
                if (n < 0) {
                    throw std::runtime_error("Invalid dupN value");
                }
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
                if (n < 0) {
                    throw std::runtime_error("Invalid dropN value");
                }
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
                if (n < 0) {
                    throw std::runtime_error("Invalid swapN value");
                }
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
                if (n < 0) {
                    throw std::runtime_error("Invalid sortN value");
                }
                return TokenPayload_StackOp{n};
            }),

        define_regex(TokenType::LABEL_DEF, "label_def",
                     TokenBehavior{.arity = 0, .stack_effect = 0},
                     std::regex(R"(^#(.+)$)"),
                     [](const std::smatch& m) -> Token::PayloadVariant {
                         return TokenPayload_Label{.name = m[1].str()};
                     }),

        define_regex(TokenType::JUMP, "jump",
                     TokenBehavior{.arity = 1, .stack_effect = -1},
                     std::regex(R"(^(.+)#$)"),
                     [](const std::smatch& m) -> Token::PayloadVariant {
                         return TokenPayload_Label{.name = m[1].str()};
                     }),

        define_regex(TokenType::VAR_STORE, "var_store",
                     TokenBehavior{.arity = 1, .stack_effect = -1},
                     std::regex(R"(^([a-zA-Z_][a-zA-Z0-9_]*)!$)"),
                     [](const std::smatch& m) -> Token::PayloadVariant {
                         return TokenPayload_Var{.name = m[1].str()};
                     }),

        define_regex(TokenType::VAR_LOAD, "var_load",
                     TokenBehavior{.arity = 0, .stack_effect = 1},
                     std::regex(R"(^([a-zA-Z_][a-zA-Z0-9_]*)@$)"),
                     [](const std::smatch& m) -> Token::PayloadVariant {
                         return TokenPayload_Var{.name = m[1].str()};
                     }),

        define_regex(TokenType::ARRAY_ALLOC_STATIC, "array_alloc_static",
                     TokenBehavior{.arity = 0, .stack_effect = 0},
                     std::regex(R"(^([a-zA-Z_][a-zA-Z0-9_]*)\{\}\^(\d+)$)"),
                     [](const std::smatch& m) -> Token::PayloadVariant {
                         return TokenPayload_ArrayOp{.name = m[1].str(),
                                                     .static_size =
                                                         std::stoi(m[2].str())};
                     }),

        define_regex(TokenType::ARRAY_ALLOC_DYN, "array_alloc_dyn",
                     TokenBehavior{.arity = 1, .stack_effect = -1},
                     std::regex(R"(^([a-zA-Z_][a-zA-Z0-9_]*)\{\}\^$)"),
                     [](const std::smatch& m) -> Token::PayloadVariant {
                         return TokenPayload_ArrayOp{.name = m[1].str()};
                     }),

        define_regex(TokenType::ARRAY_STORE, "array_store",
                     TokenBehavior{.arity = 2, .stack_effect = -2},
                     std::regex(R"(^([a-zA-Z_][a-zA-Z0-9_]*)\{\}!$)"),
                     [](const std::smatch& m) -> Token::PayloadVariant {
                         return TokenPayload_ArrayOp{.name = m[1].str()};
                     }),

        define_regex(TokenType::ARRAY_LOAD, "array_load",
                     TokenBehavior{.arity = 1, .stack_effect = 0},
                     std::regex(R"(^([a-zA-Z_][a-zA-Z0-9_]*)\{\}@$)"),
                     [](const std::smatch& m) -> Token::PayloadVariant {
                         return TokenPayload_ArrayOp{.name = m[1].str()};
                     }),

        define_regex(
            TokenType::CLIP_REL, "clip_rel",
            TokenBehavior{.arity = 0, .stack_effect = 1},
            std::regex(
                R"(^(?:src(\d+)|([x-za-w]))\[\s*(-?\d+)\s*,\s*(-?\d+)\s*\](?::([cm]))?$)"),
            [](const std::smatch& m) -> Token::PayloadVariant {
                TokenPayload_ClipAccess data;
                if (m[1].matched) {
                    data.clip_idx = std::stoi(m[1].str());
                } else if (m[2].matched) {
                    data.clip_idx = parse_std_clip_idx(m[2].str()[0]);
                }
                data.rel_x = std::stoi(m[3].str());
                data.rel_y = std::stoi(m[4].str());
                if (m[5].matched) { // NOLINT(cppcoreguidelines-avoid-magic-numbers)
                    data.has_mode = true;
                    data.use_mirror =
                        (m[5].str() == // NOLINT(cppcoreguidelines-avoid-magic-numbers)
                         "m");
                }
                return data;
            }),

        define_regex(
            TokenType::CLIP_ABS, "clip_abs",
            TokenBehavior{.arity = 2, .stack_effect = -1},
            std::regex(R"(^(?:src(\d+)|([x-za-w]))\[\](?::([mcb]))?$)"),
            [](const std::smatch& m) -> Token::PayloadVariant {
                TokenPayload_ClipAccess data;
                if (m[1].matched) {
                    data.clip_idx = std::stoi(m[1].str());
                } else if (m[2].matched) {
                    data.clip_idx = parse_std_clip_idx(m[2].str()[0]);
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

        define_regex(TokenType::CLIP_CUR, "clip_cur",
                     TokenBehavior{.arity = 0, .stack_effect = 1},
                     std::regex(R"(^(?:src(\d+)|([x-za-w]))$)"),
                     [](const std::smatch& m) -> Token::PayloadVariant {
                         TokenPayload_ClipAccess data;
                         if (m[1].matched) {
                             data.clip_idx = std::stoi(m[1].str());
                         } else if (m[2].matched) {
                             data.clip_idx = parse_std_clip_idx(m[2].str()[0]);
                         }
                         return data;
                     }),

        define_regex(
            TokenType::PROP_ACCESS, "prop_access",
            TokenBehavior{.arity = 0, .stack_effect = 1},
            std::regex(
                R"(^(?:src(\d+)|([x-za-w]))\.([a-zA-Z_][a-zA-Z0-9_]*)$)"),
            [](const std::smatch& m) -> Token::PayloadVariant {
                TokenPayload_PropAccess data;
                if (m[1].matched) {
                    data.clip_idx = std::stoi(m[1].str());
                } else if (m[2].matched) {
                    data.clip_idx = parse_std_clip_idx(m[2].str()[0]);
                }
                data.prop_name = m[3].str();
                return data;
            }),

        // SingleExpr specific
        define_regex(TokenType::CLIP_ABS_PLANE, "clip_abs_plane",
                     TokenBehavior{.arity = 2, .stack_effect = -1},
                     std::regex(R"(^(?:src(\d+)|([x-za-w]))\^(\d+)\[\]$)"),
                     [](const std::smatch& m) -> Token::PayloadVariant {
                         TokenPayload_ClipAccessPlane data;
                         if (m[1].matched) {
                             data.clip_idx = std::stoi(m[1].str());
                         } else if (m[2].matched) {
                             data.clip_idx = parse_std_clip_idx(m[2].str()[0]);
                         }
                         data.plane_idx = std::stoi(m[3].str());
                         return data;
                     }),

        define_regex(TokenType::STORE_ABS_PLANE, "store_abs_plane",
                     TokenBehavior{.arity = 3, .stack_effect = -3},
                     std::regex(R"(^@\[\]\^(\d+)$)"),
                     [](const std::smatch& m) -> Token::PayloadVariant {
                         return TokenPayload_StoreAbsPlane{
                             .plane_idx = std::stoi(m[1].str())};
                     }),

        define_regex(TokenType::PROP_STORE, "prop_store",
                     TokenBehavior{.arity = 1, .stack_effect = -1},
                     std::regex(R"(^([a-zA-Z_][a-zA-Z0-9_]*)\$$)"),
                     [](const std::smatch& m) -> Token::PayloadVariant {
                         return TokenPayload_PropStore{.prop_name = m[1].str()};
                     }),

        define_regex(
            TokenType::NUMBER, "number",
            TokenBehavior{.arity = 0, .stack_effect = 1},
            std::regex(
                R"(^(?:(0x[0-9a-fA-F]+(?:\.[0-9a-fA-F]+(?:p[+\-]?\d+)?)?)|(0[0-7]+)|([+\-]?\d+(?:\.\d+)?(?:[eE][+\-]?\d+)?))$)"),
            [](const std::smatch& m) -> Token::PayloadVariant {
                std::string s = m.str();
                double val = NAN;
                if (m[2].matched) { // Octal integer
                    val = static_cast<double>(std::stoll(s, nullptr, 0));
                } else { // Hex or decimal float/integer
                    val = locale_independent_stod(s);
                }
                return TokenPayload_Number{val};
            })};
    return token_definitions;
}

} // anonymous namespace

std::vector<Token> tokenize(const std::string& expr, int num_inputs,
                            ExprMode mode) {
    std::vector<Token> tokens;
    int idx = 0;

    auto is_space = [](char c) { return std::isspace(c); };
    auto to_string_view = [](auto r) {
        return std::string_view(r.begin(), r.end());
    };

    for (const auto str_token_view :
         expr | std::views::chunk_by([=](char a, char b) {
             return is_space(a) == is_space(b);
         }) | std::views::filter([=](auto r) { return !is_space(r.front()); }) |
             std::views::transform(to_string_view)) {
        std::optional<Token> parsed_token;

        std::string str_token{str_token_view};

        for (const auto& definition : get_token_definitions()) {
            // TODO: handle mode-specific tokens in a more elegant way.
            bool skip = false;
            switch (definition.type) {
            case TokenType::CONSTANT_X:
            case TokenType::CONSTANT_Y:
            case TokenType::CLIP_REL:
            case TokenType::CLIP_CUR:
            case TokenType::EXIT_NO_WRITE:
            case TokenType::STORE_ABS:
                if (mode == ExprMode::SINGLE_EXPR) {
                    skip = true;
                }
                break;
            case TokenType::CLIP_ABS_PLANE:
            case TokenType::STORE_ABS_PLANE:
            case TokenType::PROP_STORE:
            case TokenType::CONSTANT_PLANE_WIDTH:
            case TokenType::CONSTANT_PLANE_HEIGHT:
            case TokenType::ARRAY_ALLOC_DYN:
                if (mode == ExprMode::EXPR) {
                    skip = true;
                }
                break;
            default:
                break;
            }
            if (skip) {
                continue;
            }

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
        } else if (parsed_token->type == TokenType::CLIP_ABS_PLANE) {
            if (std::get<TokenPayload_ClipAccessPlane>(parsed_token->payload)
                        .clip_idx < 0 ||
                std::get<TokenPayload_ClipAccessPlane>(parsed_token->payload)
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
    auto it = std::find_if(
        get_token_definitions().begin(), get_token_definitions().end(),
        [&](const auto& def) { return def.type == token.type; });

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
