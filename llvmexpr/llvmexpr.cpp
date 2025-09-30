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

#include <algorithm>
#include <atomic>
#include <cctype>
#include <cstdint>
#include <format>
#include <functional>
#include <map>
#include <memory>
#include <mutex>
#include <numbers>
#include <numeric>
#include <optional>
#include <regex>
#include <sstream>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>
#include <variant>
#include <vector>

#include "VSHelper.h"
#include "VapourSynth.h"

#include "llvm/ADT/SmallVector.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/Frontend/Driver/CodeGenOptions.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/DiagnosticPrinter.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/CodeGen.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"
#include "llvm/TargetParser/Triple.h"

#include "math_functions.hpp"
#include "optimal_sorting_networks.hpp"

constexpr unsigned ALIGNMENT = 32; // Vapoursynth should guarantee this

namespace {

constexpr uint32_t EXIT_NAN_PAYLOAD = 0x7FC0E71F; // qNaN with payload 0xE71F
constexpr uint32_t PROP_NAN_PAYLOAD = 0x7FC0BEEF; // qNaN with payload 0xBEEF

enum class TokenType {
    // Literals & Constants
    NUMBER,
    CONSTANT_X,
    CONSTANT_Y,
    CONSTANT_WIDTH,
    CONSTANT_HEIGHT,
    CONSTANT_N,
    CONSTANT_PI,

    // Variable Ops
    VAR_STORE, // my_var!
    VAR_LOAD,  // my_var@

    // Data Access
    CLIP_REL,    // src[x,y]
    CLIP_ABS,    // src[]
    CLIP_CUR,    // src
    PROP_ACCESS, // src.prop

    // Binary Operators
    ADD,
    SUB,
    MUL,
    DIV,
    MOD,
    GT,
    LT,
    GE,
    LE,
    EQ,
    AND,
    OR,
    XOR,
    BITAND,
    BITOR,
    BITXOR,
    POW,
    ATAN2,
    COPYSIGN,
    MIN,
    MAX,

    // Unary Operators
    NOT,
    BITNOT,
    SQRT,
    EXP,
    LOG,
    ABS,
    FLOOR,
    CEIL,
    TRUNC,
    ROUND,
    SIN,
    COS,
    TAN,
    ASIN,
    ACOS,
    ATAN,
    EXP2,
    LOG10,
    LOG2,
    SINH,
    COSH,
    TANH,
    SGN,
    NEG,

    // Ternary and other multi-arg
    TERNARY, // ?
    CLIP,
    CLAMP, // same op, 3 args
    FMA,   // 3 args

    // Stack manipulation
    DUP,
    DROP,
    SWAP,
    SORTN,

    // Control Flow
    LABEL_DEF, // #my_label
    JUMP,      // my_label#

    // Custom output control
    EXIT_NO_WRITE, // ^exit^
    STORE_ABS,     // @[]
};

struct TokenPayload_Number {
    double value;
};
struct TokenPayload_Var {
    std::string name;
};
struct TokenPayload_Label {
    std::string name;
};
struct TokenPayload_StackOp {
    int n;
};
struct TokenPayload_ClipAccess {
    int clip_idx;
    int rel_x = 0;
    int rel_y = 0;
    bool use_mirror = false;
    bool has_mode = false;
};
struct TokenPayload_PropAccess {
    int clip_idx;
    std::string prop_name;
};

struct Token {
    using PayloadVariant =
        std::variant<std::monostate, TokenPayload_Number, TokenPayload_Var,
                     TokenPayload_Label, TokenPayload_StackOp,
                     TokenPayload_ClipAccess, TokenPayload_PropAccess>;

    TokenType type;
    std::string text;
    PayloadVariant payload;
};

struct TokenBehavior {
    int arity;
    int stack_effect;
};

using DynamicBehaviorFn = TokenBehavior (*)(const Token&);

using BehaviorResolver = std::variant<TokenBehavior, DynamicBehaviorFn>;

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
        // TODO: This should be done in the token definitions, not here
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

void oem_merge_pairs(std::vector<std::pair<int, int>>& pairs, int lo, int n,
                     int r) {
    int m = r * 2;
    if (m < n) {
        oem_merge_pairs(pairs, lo, n, m);
        oem_merge_pairs(pairs, lo + r, n, m);
        for (int i = lo + r; i < lo + n - r; i += m) {
            pairs.push_back({i, i + r});
        }
    } else {
        pairs.push_back({lo, lo + r});
    }
}

void generate_oem_sort_pairs(std::vector<std::pair<int, int>>& pairs, int lo,
                             int n) {
    if (n > 1) {
        int m = n / 2;
        generate_oem_sort_pairs(pairs, lo, m);
        generate_oem_sort_pairs(pairs, lo + m, m);
        oem_merge_pairs(pairs, lo, n, 1);
    }
}

class OrcJit {
  private:
    std::unique_ptr<llvm::orc::LLJIT> lljit;

  public:
    explicit OrcJit(bool no_nans_fp_math) {
        static struct LLVMInitializer {
            LLVMInitializer() {
                llvm::InitializeNativeTarget();
                llvm::InitializeNativeTargetAsmPrinter();
                llvm::InitializeNativeTargetAsmParser();
            }
        } initializer;

        auto jtmb =
            llvm::cantFail(llvm::orc::JITTargetMachineBuilder::detectHost());
        jtmb.setCodeGenOptLevel(llvm::CodeGenOptLevel::Aggressive);

        llvm::StringMap<bool> host_features = llvm::sys::getHostCPUFeatures();
        if (host_features.size() > 0) {
            std::vector<std::string> features;
            for (auto& f : host_features) {
                if (f.getValue()) {
                    features.push_back("+" + f.getKey().str());
                }
            }
            jtmb.addFeatures(features);
        }

        llvm::TargetOptions Opts;
        Opts.AllowFPOpFusion = llvm::FPOpFusion::Fast;
        Opts.UnsafeFPMath = true;
        Opts.NoInfsFPMath = true;
        Opts.NoNaNsFPMath = no_nans_fp_math;
        jtmb.setOptions(Opts);

        auto jit_builder = llvm::orc::LLJITBuilder();
        jit_builder.setJITTargetMachineBuilder(std::move(jtmb));
        auto temp_jit = jit_builder.create();
        if (!temp_jit) {
            llvm::errs() << "Failed to create LLJIT instance: "
                         << llvm::toString(temp_jit.takeError()) << "\n";
            throw std::runtime_error("LLJIT creation failed");
        }
        lljit = std::move(*temp_jit);
    }

    const llvm::DataLayout& getDataLayout() const {
        return lljit->getDataLayout();
    }

    const llvm::Triple& getTargetTriple() const {
        return lljit->getTargetTriple();
    }

    void addModule(std::unique_ptr<llvm::Module> M,
                   std::unique_ptr<llvm::LLVMContext> Ctx) {
        std::vector<llvm::Function*> functions_to_remove;
        for (auto& F : *M) {
            if (!F.isDeclaration()) {
                std::string func_name = F.getName().str();

                if (func_name.find("process_plane_") != 0) {
                    if (func_name.find("fast_") == 0 ||
                        func_name.find("_v4") != std::string::npos ||
                        func_name.find("_v8") != std::string::npos ||
                        func_name.find("_v16") != std::string::npos) {

                        // Try a test lookup to see if symbol exists
                        auto test_sym = lljit->lookup(func_name);
                        if (test_sym) {
                            // Symbol already exists, mark for removal
                            functions_to_remove.push_back(&F);
                            // llvm::errs() << "Removing duplicate function: " << func_name << "\n";
                            continue;
                        }
                        // If test lookup failed, the symbol doesn't exist, so we can add it
                        llvm::consumeError(test_sym.takeError());
                    }
                }

                // llvm::errs() << "Adding function: " << func_name << "\n";
            }
        }

        // Remove duplicate functions from the module
        for (auto* F : functions_to_remove) {
            F->eraseFromParent();
        }

        auto TSM = llvm::orc::ThreadSafeModule(std::move(M), std::move(Ctx));
        auto Err = lljit->addIRModule(std::move(TSM));
        if (Err) {
            llvm::errs() << "Failed to add IR module: "
                         << llvm::toString(std::move(Err)) << "\n";
            throw std::runtime_error("Failed to add IR module to JIT");
        }

        // llvm::errs() << "Module added successfully\n";
    }

    void* getFunctionAddress(const std::string& name) {
        // Try to lookup the symbol
        auto sym = lljit->lookup(name);
        if (!sym) {
            llvm::errs() << "Failed to find symbol '" << name
                         << "': " << llvm::toString(sym.takeError()) << "\n";
            return nullptr;
        }
        return sym->toPtr<void*>();
    }
};

using ProcessProc = void (*)(uint8_t** rwptrs, const int* strides,
                             const float* props);

struct CompiledFunction {
    ProcessProc func_ptr = nullptr;
};

std::unordered_map<std::string, CompiledFunction> jit_cache;
std::mutex cache_mutex;
OrcJit global_jit_fast(true);
OrcJit global_jit_nan_safe(false);

class VectorizationDiagnosticHandler {
  public:
    VectorizationDiagnosticHandler()
        : vectorization_failed(false), original_handler(nullptr),
          original_context(nullptr) {}

    void
    setOriginalHandler(llvm::DiagnosticHandler::DiagnosticHandlerTy handler,
                       void* context) {
        original_handler = handler;
        original_context = context;
    }

    void handleDiagnostic(const llvm::DiagnosticInfo& DI) {
        bool should_suppress = false;

        if (DI.getSeverity() == llvm::DS_Remark ||
            DI.getSeverity() == llvm::DS_Warning) {
            std::string msg;
            llvm::raw_string_ostream stream(msg);
            llvm::DiagnosticPrinterRawOStream printer(stream);
            DI.print(printer);

            if (msg.find("loop not vectorized") != std::string::npos) {
                vectorization_failed.store(true);
                should_suppress = true;
            }
        }

        // Call original handler for all diagnostics except "loop not vectorized"
        if (!should_suppress && original_handler) {
            original_handler(&DI, original_context);
        }
    }

    bool hasVectorizationFailed() const { return vectorization_failed.load(); }

    void reset() { vectorization_failed.store(false); }

    static void diagnosticHandlerCallback(const llvm::DiagnosticInfo* DI,
                                          void* Context) {
        static_cast<VectorizationDiagnosticHandler*>(Context)->handleDiagnostic(
            *DI);
    }

  private:
    std::atomic<bool> vectorization_failed;
    llvm::DiagnosticHandler::DiagnosticHandlerTy original_handler;
    void* original_context;
};

class Compiler {
  private:
    std::vector<Token> tokens;
    const VSVideoInfo* vo;
    const std::vector<const VSVideoInfo*>& vi;
    int num_inputs;
    int width;
    int height;
    bool mirror_boundary;
    std::string dump_ir_path;
    const std::map<std::pair<int, std::string>, int>& prop_map;
    std::string func_name;
    bool uses_x = false;
    bool uses_y = false;
    bool validate_only;
    int opt_level;
    int approx_math;

    std::unique_ptr<llvm::LLVMContext> context;
    std::unique_ptr<llvm::Module> module;
    llvm::IRBuilder<> builder;
    MathLibraryManager math_manager;
    VectorizationDiagnosticHandler diagnostic_handler;

    llvm::Function* func;
    llvm::Value* rwptrs_arg;
    llvm::Value* strides_arg;
    llvm::Value* props_arg;

    // Loop-invariant caches
    std::vector<llvm::Value*> preloaded_base_ptrs;
    std::vector<llvm::Value*> preloaded_strides;

    // Alias scope metadata (per rwptrs element)
    llvm::MDNode* alias_scope_domain = nullptr;
    std::vector<llvm::MDNode*> alias_scopes; // distinct scopes for each pointer
    std::vector<llvm::MDNode*> alias_scope_lists;   // !alias.scope lists (self)
    std::vector<llvm::MDNode*> noalias_scope_lists; // !noalias lists (others)

    // CFG and validation
    struct CFGBlock {
        int start_token_idx;
        int end_token_idx; // exclusive
        std::vector<int> successors;
        std::vector<int> predecessors;

        int stack_effect = 0;
        int min_stack_needed =
            0; // min stack depth *during* the block, relative to start
    };
    std::vector<CFGBlock> cfg_blocks;
    std::map<std::string, int> label_to_block_idx;
    struct RelYAccess {
        int clip_idx;
        int rel_y;
        bool use_mirror;

        bool operator<(const RelYAccess& other) const {
            return std::tie(clip_idx, rel_y, use_mirror) <
                   std::tie(other.clip_idx, other.rel_y, other.use_mirror);
        }
    };
    std::vector<RelYAccess> unique_rel_y_accesses;
    std::map<RelYAccess, llvm::Value*> row_ptr_cache;
    int min_rel_x = 0;
    int max_rel_x = 0;

    std::vector<int> stack_depth_in;

  public:
  private:
    // Base constructor
    Compiler(std::vector<Token>&& tokens_in, const VSVideoInfo* out_vi,
             const std::vector<const VSVideoInfo*>& in_vi, int width_in,
             int height_in, bool mirror,
             const std::map<std::pair<int, std::string>, int>& p_map,
             bool is_validation, std::string dump_path = {},
             std::string function_name = {}, int opt_level_in = 0,
             int approx_math_in = 0)
        : tokens(std::move(tokens_in)), vo(out_vi), vi(in_vi),
          num_inputs(in_vi.size()), width(width_in), height(height_in),
          mirror_boundary(mirror), dump_ir_path(std::move(dump_path)),
          prop_map(p_map), func_name(std::move(function_name)),
          validate_only(is_validation), opt_level(opt_level_in),
          approx_math(approx_math_in),
          context(std::make_unique<llvm::LLVMContext>()),
          module(std::make_unique<llvm::Module>(
              is_validation ? "ValidationModule" : "ExprJITModule", *context)),
          builder(*context), math_manager(module.get(), *context) {
        for (const auto& token : tokens) {
            if (token.type == TokenType::CONSTANT_X)
                uses_x = true;
            if (token.type == TokenType::CONSTANT_Y)
                uses_y = true;
            if (uses_x && uses_y)
                break;
        }
    }

  public:
    // Constructor for compilation
    Compiler(std::vector<Token>&& tokens_in, const VSVideoInfo* out_vi,
             const std::vector<const VSVideoInfo*>& in_vi, int width_in,
             int height_in, bool mirror, std::string dump_path,
             const std::map<std::pair<int, std::string>, int>& p_map,
             std::string function_name, int opt_level_in, int approx_math_in)
        : Compiler(std::move(tokens_in), out_vi, in_vi, width_in, height_in,
                   mirror, p_map, false, std::move(dump_path),
                   std::move(function_name), opt_level_in, approx_math_in) {}

    // Constructor for validation only
    Compiler(std::vector<Token>&& tokens_in, const VSVideoInfo* out_vi,
             const std::vector<const VSVideoInfo*>& in_vi, int width_in,
             int height_in, bool mirror,
             const std::map<std::pair<int, std::string>, int>& p_map)
        : Compiler(std::move(tokens_in), out_vi, in_vi, width_in, height_in,
                   mirror, p_map, true) {}

    CompiledFunction compile() {
        if (validate_only) {
            throw std::runtime_error("Cannot compile in validation mode");
        }
        if (approx_math == 2) {
            return compile_with_approx_math(1);
        }
        return compile_with_approx_math(approx_math);
    }

    // Validate expression syntax and CFG without compiling
    void validate() { validate_and_build_cfg(); }

    CompiledFunction compile_with_approx_math(int actual_approx_math) {
        bool needs_nans = false;
        for (const auto& token : tokens) {
            if (token.type ==
                TokenType::EXIT_NO_WRITE) { // Akarin.Expr uses fast math even
                                            // when props not exist, so we only
                                            // check for `^exit^`.
                needs_nans = true;
                break;
            }
        }

        OrcJit& jit = needs_nans ? global_jit_nan_safe : global_jit_fast;

        diagnostic_handler.reset();

        context->setDiagnosticHandlerCallBack(
            VectorizationDiagnosticHandler::diagnosticHandlerCallback,
            &diagnostic_handler);

        llvm::FastMathFlags FMF;
        FMF.setFast();
        FMF.setNoNaNs(!needs_nans);
        builder.setFastMathFlags(FMF);

        validate_and_build_cfg();
        collect_rel_y_accesses();
        collect_rel_x_accesses();

        define_function_signature();

        // Store the original approx_math value and update it for this compilation
        int original_approx_math = approx_math;
        approx_math = actual_approx_math;

        {
            llvm::AttrBuilder FuncAttrs(func->getContext());
            if (FMF.allowContract()) {
                FuncAttrs.addAttribute("fp-contract", "fast");
            }
            if (FMF.approxFunc()) {
                FuncAttrs.addAttribute("approx-func-fp-math", "true");
            }
            if (FMF.noInfs()) {
                FuncAttrs.addAttribute("no-infs-fp-math", "true");
            }
            if (FMF.noNaNs()) {
                FuncAttrs.addAttribute("no-nans-fp-math", "true");
            }
            if (FMF.noSignedZeros()) {
                FuncAttrs.addAttribute("no-signed-zeros-fp-math", "true");
            }
            if (FMF.allowReciprocal()) {
                FuncAttrs.addAttribute("allow-reciprocal-fp-math", "true");
            }

            FuncAttrs.addAttribute(llvm::Attribute::NoUnwind);
            FuncAttrs.addAttribute(llvm::Attribute::WillReturn);
            func->addFnAttrs(FuncAttrs);
        }

        generate_loops();

        module->setDataLayout(jit.getDataLayout());

        if (llvm::verifyModule(*module, &llvm::errs())) {
            module->print(llvm::errs(), nullptr);
            throw std::runtime_error(
                "LLVM module verification failed (pre-opt).");
        }

        std::string plane_specific_dump_path;
        if (!dump_ir_path.empty()) {
            plane_specific_dump_path = dump_ir_path;
            size_t dot_pos = plane_specific_dump_path.rfind('.');
            if (dot_pos != std::string::npos) {
                plane_specific_dump_path.insert(dot_pos, "." + func_name);
            } else {
                plane_specific_dump_path += "." + func_name;
            }
        }

        // Dump pre-optimization IR
        if (!plane_specific_dump_path.empty()) {
            std::error_code EC;
            std::string pre_path = plane_specific_dump_path + ".pre.ll";
            llvm::raw_fd_ostream dest_pre(pre_path, EC, llvm::sys::fs::OF_None);
            if (!EC) {
                module->print(dest_pre, nullptr);
                dest_pre.flush();
            }
        }

        {
            llvm::LoopAnalysisManager LAM;
            llvm::FunctionAnalysisManager FAM;
            llvm::CGSCCAnalysisManager CGAM;
            llvm::ModuleAnalysisManager MAM;

            llvm::PassBuilder PB;
            PB.registerModuleAnalyses(MAM);
            PB.registerFunctionAnalyses(FAM);
            PB.registerCGSCCAnalyses(CGAM);
            PB.registerLoopAnalyses(LAM);
            PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

            llvm::ModulePassManager MPM;
            std::string pipeline;
            if (opt_level > 0) {
                pipeline = "default<O3>";
                for (int i = 1; i < opt_level; ++i) {
                    pipeline += ",default<O3>";
                }
            }
            // TODO: Figure out how to enable polly optimization, and if that helps
            if (auto Err = PB.parsePassPipeline(MPM, pipeline)) {
                llvm::errs()
                    << "Failed to parse '" << pipeline
                    << "' pipeline: " << llvm::toString(std::move(Err)) << "\n";
                throw std::runtime_error(
                    "Failed to create default optimization pipeline.");
            }
            MPM.run(*module, MAM);
        }

        if (llvm::verifyModule(*module, &llvm::errs())) {
            module->print(llvm::errs(), nullptr);
            throw std::runtime_error("LLVM module verification failed.");
        }

        if (!plane_specific_dump_path.empty()) {
            std::error_code EC;
            llvm::raw_fd_ostream dest(plane_specific_dump_path, EC,
                                      llvm::sys::fs::OF_None);
            if (EC) {
                throw std::runtime_error(
                    "Could not open file: " + EC.message() +
                    " for writing IR to " + plane_specific_dump_path);
            } else {
                module->print(dest, nullptr);
                dest.flush();
            }
        }

        if (diagnostic_handler.hasVectorizationFailed() &&
            original_approx_math == 2 && actual_approx_math == 1) {
            approx_math = original_approx_math;
            Compiler fallback_compiler(std::vector<Token>(tokens), vo, vi,
                                       width, height, mirror_boundary,
                                       dump_ir_path, prop_map, func_name,
                                       opt_level, approx_math);
            return fallback_compiler.compile_with_approx_math(0);
        }

        CompiledFunction compiled;

        jit.addModule(std::move(module), std::move(context));
        void* func_addr = jit.getFunctionAddress(func_name);

        if (!func_addr) {
            // Restore original approx_math value before throwing
            approx_math = original_approx_math;
            throw std::runtime_error("Failed to get JIT'd function address.");
        }

        compiled.func_ptr = reinterpret_cast<ProcessProc>(func_addr);

        // Restore original approx_math value
        approx_math = original_approx_math;
        return compiled;
    }

  private:
    llvm::AllocaInst* createAllocaInEntry(llvm::Type* type,
                                          const std::string& name) {
        llvm::IRBuilder<> entryBuilder(&func->getEntryBlock(),
                                       func->getEntryBlock().begin());
        return entryBuilder.CreateAlloca(type, nullptr, name);
    }

    template <typename... Args>
    llvm::Value* createIntrinsicCall(llvm::Intrinsic::ID intrinsic_id,
                                     Args... args) {
        static_assert(sizeof...(Args) >= 1, "At least one argument required");
        llvm::SmallVector<llvm::Value*, 4> arg_vec{args...};
        auto* callee = llvm::Intrinsic::getOrInsertDeclaration(
            module.get(), intrinsic_id, {arg_vec[0]->getType()});
        auto* call = builder.CreateCall(callee, arg_vec);
        call->setFastMathFlags(builder.getFastMathFlags());
        return call;
    }

    void assumeAligned(llvm::Value* ptrValue, unsigned alignment) {
        llvm::Function* assumeFn = llvm::Intrinsic::getOrInsertDeclaration(
            module.get(), llvm::Intrinsic::assume);
        llvm::Value* cond = builder.getInt1(true);
        llvm::SmallVector<llvm::Value*, 2> args;
        args.push_back(ptrValue);
        args.push_back(builder.getInt64(static_cast<uint64_t>(alignment)));
        llvm::OperandBundleDefT<llvm::Value*> alignBundle("align", args);
        builder.CreateCall(assumeFn, {cond}, {alignBundle});
    }

    template <typename MemInstT>
    void setMemoryInstAttrs(MemInstT* inst, unsigned alignment,
                            int rwptr_index) {
        inst->setAlignment(llvm::Align(alignment));
        inst->setMetadata(llvm::LLVMContext::MD_alias_scope,
                          alias_scope_lists[rwptr_index]);
        inst->setMetadata(llvm::LLVMContext::MD_noalias,
                          noalias_scope_lists[rwptr_index]);
    }

    void define_function_signature() {
        llvm::Type* void_ty = llvm::Type::getVoidTy(*context);
        llvm::Type* ptr_ty = llvm::PointerType::get(*context, 0);
        llvm::Type* i8_ptr_ptr_ty =
            ptr_ty; // opaque pointer (represents uint8_t**)
        llvm::Type* i32_ptr_ty = ptr_ty; // opaque pointer (represents int32_t*)
        llvm::Type* float_ptr_ty = ptr_ty; // opaque pointer (represents float*)

        llvm::FunctionType* func_ty = llvm::FunctionType::get(
            void_ty, {i8_ptr_ptr_ty, i32_ptr_ty, float_ptr_ty}, false);

        func = llvm::Function::Create(func_ty, llvm::Function::ExternalLinkage,
                                      func_name, module.get());
        func->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::None);

        auto args = func->arg_begin();
        rwptrs_arg = &*args++;
        rwptrs_arg->setName("rwptrs");
        strides_arg = &*args++;
        strides_arg->setName("strides");
        props_arg = &*args++;
        props_arg->setName("props");

        func->addParamAttr(1, llvm::Attribute::ReadOnly); // strides (int32_t*)
        func->addParamAttr(2, llvm::Attribute::ReadOnly); // props (float*)
    }

    void collect_rel_y_accesses() {
        std::set<RelYAccess> seen;
        for (const auto& token : tokens) {
            if (token.type == TokenType::CLIP_REL) {
                const auto& payload =
                    std::get<TokenPayload_ClipAccess>(token.payload);
                bool use_mirror =
                    payload.has_mode ? payload.use_mirror : mirror_boundary;
                RelYAccess access{payload.clip_idx, payload.rel_y, use_mirror};
                if (seen.find(access) == seen.end()) {
                    seen.insert(access);
                    unique_rel_y_accesses.push_back(access);
                }
            } else if (token.type == TokenType::CLIP_CUR) {
                const auto& payload =
                    std::get<TokenPayload_ClipAccess>(token.payload);
                RelYAccess access{payload.clip_idx, 0, mirror_boundary};
                if (seen.find(access) == seen.end()) {
                    seen.insert(access);
                    unique_rel_y_accesses.push_back(access);
                }
            }
        }
    }

    void collect_rel_x_accesses() {
        for (const auto& token : tokens) {
            if (token.type == TokenType::CLIP_REL) {
                const auto& payload =
                    std::get<TokenPayload_ClipAccess>(token.payload);
                min_rel_x = std::min(min_rel_x, payload.rel_x);
                max_rel_x = std::max(max_rel_x, payload.rel_x);
            }
        }
    }

    llvm::Value* get_final_coord(llvm::Value* coord, llvm::Value* max_dim,
                                 bool use_mirror) {
        llvm::Value* zero = builder.getInt32(0);
        llvm::Value* one = builder.getInt32(1);

        llvm::Value* result;
        if (use_mirror) {
            // idx = abs(idx); N = 2*(len-1); if (N==0) N=1; r = idx % N;
            // result = (len-1) - abs(r - (len-1))
            llvm::Function* abs_func = llvm::Intrinsic::getOrInsertDeclaration(
                module.get(), llvm::Intrinsic::abs, {builder.getInt32Ty()});
            auto abs_coord =
                builder.CreateCall(abs_func, {coord, builder.getInt1(false)});

            auto dim_minus_1 = builder.CreateSub(max_dim, one);
            auto twice_dim_minus_1 =
                builder.CreateMul(dim_minus_1, builder.getInt32(2));

            // base = base | (base==0 ? 1 : 0)
            auto base_is_zero = builder.CreateICmpEQ(twice_dim_minus_1, zero);
            auto one_if_zero =
                builder.CreateZExt(base_is_zero, builder.getInt32Ty());
            auto safe_mod_base =
                builder.CreateOr(twice_dim_minus_1, one_if_zero);

            auto rem = builder.CreateURem(abs_coord, safe_mod_base);

            // result = (len-1) - abs(rem - (len-1))
            auto diff = builder.CreateSub(rem, dim_minus_1);
            auto abs_diff =
                builder.CreateCall(abs_func, {diff, builder.getInt1(false)});
            result = builder.CreateSub(dim_minus_1, abs_diff);
        } else { // Clamping
            // clamp(coord, 0, max_dim - 1)
            auto dim_minus_1 = builder.CreateSub(max_dim, one);

            llvm::Function* smax_func = llvm::Intrinsic::getOrInsertDeclaration(
                module.get(), llvm::Intrinsic::smax, {builder.getInt32Ty()});
            llvm::Function* smin_func = llvm::Intrinsic::getOrInsertDeclaration(
                module.get(), llvm::Intrinsic::smin, {builder.getInt32Ty()});

            auto clamped_at_zero = builder.CreateCall(smax_func, {coord, zero});
            result =
                builder.CreateCall(smin_func, {clamped_at_zero, dim_minus_1});
        }

        return result;
    }

    llvm::Value* generate_load_from_row_ptr(llvm::Value* row_ptr, int clip_idx,
                                            llvm::Value* x, int rel_x,
                                            bool use_mirror,
                                            bool no_x_bounds_check) {
        const VSVideoInfo* vinfo = vi[clip_idx];
        llvm::Value* coord_x = builder.CreateAdd(x, builder.getInt32(rel_x));
        llvm::Value* final_x;
        if (no_x_bounds_check) {
            final_x = coord_x;
        } else {
            final_x =
                get_final_coord(coord_x, builder.getInt32(width), use_mirror);
        }

        const VSFormat* format = vinfo->format;
        int bpp = format->bytesPerSample;
        int vs_clip_idx = clip_idx + 1;

        llvm::Value* x_offset =
            builder.CreateMul(final_x, builder.getInt32(bpp));
        llvm::Value* pixel_addr =
            builder.CreateGEP(builder.getInt8Ty(), row_ptr, x_offset);

        int pixel_align = std::gcd(ALIGNMENT, bpp);
        assumeAligned(pixel_addr, static_cast<unsigned>(pixel_align));

        llvm::Value* loaded_val;
        if (format->sampleType == stInteger) {
            llvm::Type* load_type =
                bpp == 1
                    ? builder.getInt8Ty()
                    : (bpp == 2 ? builder.getInt16Ty() : builder.getInt32Ty());
            llvm::LoadInst* li = builder.CreateLoad(load_type, pixel_addr);
            setMemoryInstAttrs(li, static_cast<unsigned>(pixel_align),
                               vs_clip_idx);
            loaded_val = builder.CreateZExtOrBitCast(li, builder.getInt32Ty());
            return builder.CreateUIToFP(loaded_val, builder.getFloatTy());
        } else { // stFloat
            if (bpp == 4) {
                llvm::LoadInst* li =
                    builder.CreateLoad(builder.getFloatTy(), pixel_addr);
                setMemoryInstAttrs(li, static_cast<unsigned>(pixel_align),
                                   vs_clip_idx);
                return li;
            } else if (bpp == 2) {
                llvm::LoadInst* li =
                    builder.CreateLoad(builder.getHalfTy(), pixel_addr);
                setMemoryInstAttrs(li, static_cast<unsigned>(pixel_align),
                                   vs_clip_idx);
                return builder.CreateFPExt(li, builder.getFloatTy());
            } else {
                throw std::runtime_error("Unsupported float sample size.");
            }
        }
    }

    void add_vectorization_metadata(llvm::BranchInst* loop_br) {
        llvm::StringMap<bool> host_features = llvm::sys::getHostCPUFeatures();
        unsigned simd_width = 4;
        if (!host_features.empty()) {
            if (host_features["avx512f"]) {
                simd_width = 16;
            } else if (host_features["avx2"]) {
                simd_width = 8;
            }
        }

        llvm::Metadata* vec_width_md[] = {
            llvm::MDString::get(*context, "llvm.loop.vectorize.width"),
            llvm::ConstantAsMetadata::get(llvm::ConstantInt::get(
                llvm::Type::getInt32Ty(*context), simd_width))};
        llvm::MDNode* vec_width_node =
            llvm::MDNode::get(*context, vec_width_md);

        llvm::Metadata* enable_vec[] = {
            llvm::MDString::get(*context, "llvm.loop.vectorize.enable"),
            llvm::ConstantAsMetadata::get(
                llvm::ConstantInt::get(llvm::Type::getInt1Ty(*context), 1))};
        llvm::MDNode* enable_vec_node = llvm::MDNode::get(*context, enable_vec);

        llvm::Metadata* interleave_md[] = {
            llvm::MDString::get(*context, "llvm.loop.interleave.count"),
            llvm::ConstantAsMetadata::get(
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context), 4))};
        llvm::MDNode* interleave_node =
            llvm::MDNode::get(*context, interleave_md);

        llvm::SmallVector<llvm::Metadata*, 5> loop_md_elems;
        loop_md_elems.push_back(nullptr); // to be replaced with self reference
        loop_md_elems.push_back(enable_vec_node);
        loop_md_elems.push_back(vec_width_node);
        loop_md_elems.push_back(interleave_node);
        llvm::MDNode* loop_id =
            llvm::MDNode::getDistinct(*context, loop_md_elems);
        loop_id->replaceOperandWith(0, loop_id);

        loop_br->setMetadata(llvm::LLVMContext::MD_loop, loop_id);
    }

    void generate_loops() {
        llvm::BasicBlock* entry_bb =
            llvm::BasicBlock::Create(*context, "entry", func);
        builder.SetInsertPoint(entry_bb);

        llvm::Function* parent_func = builder.GetInsertBlock()->getParent();
        llvm::BasicBlock* loop_y_header =
            llvm::BasicBlock::Create(*context, "loop_y_header", parent_func);
        llvm::BasicBlock* loop_y_body =
            llvm::BasicBlock::Create(*context, "loop_y_body", parent_func);
        llvm::BasicBlock* loop_y_exit =
            llvm::BasicBlock::Create(*context, "loop_y_exit", parent_func);

        llvm::Value* y_var =
            builder.CreateAlloca(builder.getInt32Ty(), nullptr, "y.var");
        llvm::Value* x_var =
            builder.CreateAlloca(builder.getInt32Ty(), nullptr, "x.var");
        builder.CreateStore(builder.getInt32(0), y_var);

        llvm::Value* x_fp_var = nullptr;
        if (uses_x) {
            x_fp_var = createAllocaInEntry(builder.getFloatTy(), "x_fp.var");
        }
        llvm::Value* y_fp_var = nullptr;
        if (uses_y) {
            y_fp_var = createAllocaInEntry(builder.getFloatTy(), "y_fp.var");
            builder.CreateStore(
                llvm::ConstantFP::get(builder.getFloatTy(), 0.0), y_fp_var);
        }

        // Index 0 = dst, 1..num_inputs = sources
        preloaded_base_ptrs.resize(num_inputs + 1);
        preloaded_strides.resize(num_inputs + 1);
        for (int i = 0; i <= num_inputs; ++i) {
            llvm::Value* base_ptr_i = builder.CreateLoad(
                llvm::PointerType::get(*context, 0),
                builder.CreateGEP(llvm::PointerType::get(*context, 0),
                                  rwptrs_arg, builder.getInt32(i)));
            llvm::Value* stride_i = builder.CreateLoad(
                builder.getInt32Ty(),
                builder.CreateGEP(builder.getInt32Ty(), strides_arg,
                                  builder.getInt32(i)));
            preloaded_base_ptrs[i] = base_ptr_i;
            preloaded_strides[i] = stride_i;

            // Assume each base pointer is 32-byte aligned (VapourSynth
            // guarantee)
            assumeAligned(base_ptr_i, ALIGNMENT);
        }

        // Build alias scopes so distinct rwptrs[i] are considered noalias
        alias_scope_domain = llvm::MDNode::getDistinct(*context, {});
        alias_scopes.resize(num_inputs + 1);
        for (int i = 0; i <= num_inputs; ++i) {
            // Create self-referential scope nodes
            llvm::SmallVector<llvm::Metadata*, 2> elems;
            elems.push_back(nullptr); // placeholder for self-reference
            // Wrap the string in an MDNode
            llvm::Metadata* name_node = llvm::MDNode::get(
                *context, {llvm::MDString::get(
                              *context, std::format("rwptrs_{}", i).c_str())});
            elems.push_back(name_node);
            alias_scopes[i] = llvm::MDNode::getDistinct(*context, elems);
            // Replace the placeholder with self-reference
            alias_scopes[i]->replaceOperandWith(0, alias_scopes[i]);
        }
        alias_scope_lists.resize(num_inputs + 1);
        noalias_scope_lists.resize(num_inputs + 1);
        for (int i = 0; i <= num_inputs; ++i) {
            std::vector<llvm::Metadata*> self_list = {alias_scopes[i]};
            alias_scope_lists[i] = llvm::MDNode::get(*context, self_list);
            std::vector<llvm::Metadata*> others;
            for (int j = 0; j <= num_inputs; ++j) {
                if (j == i)
                    continue;
                others.push_back(alias_scopes[j]);
            }
            noalias_scope_lists[i] = llvm::MDNode::get(*context, others);
        }

        builder.CreateBr(loop_y_header);

        builder.SetInsertPoint(loop_y_header);
        llvm::Value* y_val =
            builder.CreateLoad(builder.getInt32Ty(), y_var, "y");
        llvm::Value* y_cond =
            builder.CreateICmpSLT(y_val, builder.getInt32(height), "y.cond");
        builder.CreateCondBr(y_cond, loop_y_body, loop_y_exit);

        builder.SetInsertPoint(loop_y_body);

        // Pre-calculate and cache row pointers for all unique relative Y accesses
        row_ptr_cache.clear();
        for (const auto& access : unique_rel_y_accesses) {
            const VSVideoInfo* vinfo = vi[access.clip_idx];
            llvm::Value* clip_height = builder.getInt32(vinfo->height);
            llvm::Value* coord_y =
                builder.CreateAdd(y_val, builder.getInt32(access.rel_y));
            llvm::Value* final_y =
                get_final_coord(coord_y, clip_height, access.use_mirror);

            int vs_clip_idx = access.clip_idx + 1;
            llvm::Value* base_ptr = preloaded_base_ptrs[vs_clip_idx];
            llvm::Value* stride = preloaded_strides[vs_clip_idx];

            llvm::Value* y_offset = builder.CreateMul(final_y, stride);
            llvm::Value* row_ptr = builder.CreateGEP(
                builder.getInt8Ty(), base_ptr, y_offset, "row_ptr");
            row_ptr_cache[access] = row_ptr;
        }

        // Main loop structure
        llvm::Value* width_val = builder.getInt32(width);
        llvm::Value* start_main_x = builder.getInt32(-min_rel_x);
        llvm::Value* end_main_x = builder.getInt32(width - max_rel_x);

        bool has_left_peel = min_rel_x < 0;
        bool has_right_peel = max_rel_x > 0;

        llvm::BasicBlock* loop_x_start_bb =
            llvm::BasicBlock::Create(*context, "loop_x_start", parent_func);
        llvm::BasicBlock* loop_x_exit_bb =
            llvm::BasicBlock::Create(*context, "loop_x_exit", parent_func);

        builder.CreateBr(loop_x_start_bb);
        builder.SetInsertPoint(loop_x_start_bb);

        // Initialize x variables at the start of the row
        builder.CreateStore(builder.getInt32(0), x_var);
        if (uses_x) {
            builder.CreateStore(
                llvm::ConstantFP::get(builder.getFloatTy(), 0.0), x_fp_var);
        }

        if (has_left_peel) {
            // Left peel loop
            llvm::BasicBlock* left_peel_header = llvm::BasicBlock::Create(
                *context, "left_peel_header", parent_func);
            llvm::BasicBlock* left_peel_body = llvm::BasicBlock::Create(
                *context, "left_peel_body", parent_func);
            llvm::BasicBlock* after_left_peel = llvm::BasicBlock::Create(
                *context, "after_left_peel", parent_func);

            builder.CreateBr(left_peel_header);
            builder.SetInsertPoint(left_peel_header);
            llvm::Value* x_val =
                builder.CreateLoad(builder.getInt32Ty(), x_var, "x_peel_l");
            llvm::Value* cond = builder.CreateICmpSLT(x_val, start_main_x);
            llvm::BranchInst* left_peel_br =
                builder.CreateCondBr(cond, left_peel_body, after_left_peel);
            add_vectorization_metadata(left_peel_br);

            builder.SetInsertPoint(left_peel_body);
            generate_x_loop_body(x_var, x_fp_var, y_var, y_fp_var, false);
            builder.CreateBr(left_peel_header);

            builder.SetInsertPoint(after_left_peel);
        }

        // Main vectorized loop
        llvm::BasicBlock* main_loop_header =
            llvm::BasicBlock::Create(*context, "main_loop_header", parent_func);
        llvm::BasicBlock* main_loop_body =
            llvm::BasicBlock::Create(*context, "main_loop_body", parent_func);
        llvm::BasicBlock* after_main_loop =
            llvm::BasicBlock::Create(*context, "after_main_loop", parent_func);

        builder.CreateBr(main_loop_header);
        builder.SetInsertPoint(main_loop_header);
        llvm::Value* x_val_main =
            builder.CreateLoad(builder.getInt32Ty(), x_var, "x_main");
        llvm::Value* main_cond = builder.CreateICmpSLT(x_val_main, end_main_x);

        // Add vectorization metadata to the main loop branch
        llvm::BranchInst* loop_br =
            builder.CreateCondBr(main_cond, main_loop_body, after_main_loop);
        add_vectorization_metadata(loop_br);

        builder.SetInsertPoint(main_loop_body);
        generate_x_loop_body(x_var, x_fp_var, y_var, y_fp_var, true);
        builder.CreateBr(main_loop_header);

        builder.SetInsertPoint(after_main_loop);

        if (has_right_peel) {
            // Right peel loop
            llvm::BasicBlock* right_peel_header = llvm::BasicBlock::Create(
                *context, "right_peel_header", parent_func);
            llvm::BasicBlock* right_peel_body = llvm::BasicBlock::Create(
                *context, "right_peel_body", parent_func);

            builder.CreateBr(right_peel_header);
            builder.SetInsertPoint(right_peel_header);
            llvm::Value* x_val =
                builder.CreateLoad(builder.getInt32Ty(), x_var, "x_peel_r");
            llvm::Value* cond = builder.CreateICmpSLT(x_val, width_val);
            llvm::BranchInst* right_peel_br =
                builder.CreateCondBr(cond, right_peel_body, loop_x_exit_bb);
            add_vectorization_metadata(right_peel_br);

            builder.SetInsertPoint(right_peel_body);
            generate_x_loop_body(x_var, x_fp_var, y_var, y_fp_var, false);
            builder.CreateBr(right_peel_header);
        } else {
            builder.CreateBr(loop_x_exit_bb);
        }

        builder.SetInsertPoint(loop_x_exit_bb);
        llvm::Value* y_next = builder.CreateAdd(y_val, builder.getInt32(1));
        builder.CreateStore(y_next, y_var);
        if (uses_y) {
            llvm::Value* y_fp_val =
                builder.CreateLoad(builder.getFloatTy(), y_fp_var);
            llvm::Value* y_fp_next = builder.CreateFAdd(
                y_fp_val, llvm::ConstantFP::get(builder.getFloatTy(), 1.0));
            builder.CreateStore(y_fp_next, y_fp_var);
        }
        builder.CreateBr(loop_y_header);

        builder.SetInsertPoint(loop_y_exit);
        builder.CreateRetVoid();
    }

    void generate_x_loop_body(llvm::Value* x_var, llvm::Value* x_fp_var,
                              llvm::Value* y_var, llvm::Value* y_fp_var,
                              bool no_x_bounds_check) {
        llvm::Value* x_val =
            builder.CreateLoad(builder.getInt32Ty(), x_var, "x");
        llvm::Value* y_val =
            builder.CreateLoad(builder.getInt32Ty(), y_var, "y_in_x_loop");

        llvm::Value* x_fp = nullptr;
        if (uses_x) {
            x_fp = builder.CreateLoad(builder.getFloatTy(), x_fp_var, "x_fp");
        }
        llvm::Value* y_fp = nullptr;
        if (uses_y) {
            y_fp = builder.CreateLoad(builder.getFloatTy(), y_fp_var, "y_fp");
        }

        generate_ir_from_tokens(x_val, y_val, x_fp, y_fp, no_x_bounds_check);

        llvm::Value* x_next = builder.CreateAdd(x_val, builder.getInt32(1));
        builder.CreateStore(x_next, x_var);
        if (uses_x) {
            llvm::Value* x_fp_next = builder.CreateFAdd(
                x_fp, llvm::ConstantFP::get(builder.getFloatTy(), 1.0));
            builder.CreateStore(x_fp_next, x_fp_var);
        }
    }

    void validate_and_build_cfg() {
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
                const auto& payload = std::get<TokenPayload_Label>(
                    tokens[current_token_idx].payload);
                if (label_to_block_idx.count(payload.name)) {
                    throw std::runtime_error(
                        std::format("Duplicate label: {} (idx {})",
                                    payload.name, current_token_idx));
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
                    min_stack_in_block = std::max(min_stack_in_block,
                                                  items_needed - current_stack);
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
        std::vector<std::set<std::string>> out_sets(cfg_blocks.size(),
                                                    all_vars);

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
                    for (size_t j = 1; j < cfg_blocks[i].predecessors.size();
                         ++j) {
                        int p_idx = cfg_blocks[i].predecessors[j];
                        std::set<std::string> temp_intersect;
                        std::set_intersection(
                            new_in.begin(), new_in.end(),
                            out_sets[p_idx].begin(), out_sets[p_idx].end(),
                            std::inserter(temp_intersect,
                                          temp_intersect.begin()));
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
                    const auto& payload =
                        std::get<TokenPayload_Var>(token.payload);
                    if (defined_in_block.find(payload.name) ==
                        defined_in_block.end()) {
                        throw std::runtime_error(std::format(
                            "Variable is uninitialized: {} (idx {})",
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
                        block_idx, succ_idx, depth_out,
                        stack_depth_in[succ_idx],
                        tokens[block.start_token_idx].text,
                        block.start_token_idx,
                        tokens[cfg_blocks[succ_idx].start_token_idx].text,
                        cfg_blocks[succ_idx].start_token_idx));
                }
            }
        }

        for (size_t i = 0; i < cfg_blocks.size(); ++i) {
            if (stack_depth_in[i] != -1 &&
                cfg_blocks[i].successors.empty()) { // Reachable terminal block
                int final_depth =
                    stack_depth_in[i] + cfg_blocks[i].stack_effect;
                if (final_depth != 1) {
                    throw std::runtime_error(
                        std::format("Expression stack not balanced on "
                                    "reachable terminal block {}: "
                                    "final depth = {}, expected = 1. start "
                                    "token '{}' (idx {}).",
                                    i, final_depth,
                                    tokens[cfg_blocks[i].start_token_idx].text,
                                    cfg_blocks[i].start_token_idx));
                }
            }
        }
    }

    void generate_ir_from_tokens(llvm::Value* x, llvm::Value* y,
                                 llvm::Value* x_fp, llvm::Value* y_fp,
                                 bool no_x_bounds_check) {
        llvm::Type* float_ty = builder.getFloatTy();
        llvm::Type* i32_ty = builder.getInt32Ty();
        llvm::Function* parent_func = builder.GetInsertBlock()->getParent();

        bool use_approx_math = false;
        if (approx_math == 1) {
            use_approx_math = true;
        } else if (approx_math == 2) {
            // In auto mode, always try approx math first
            use_approx_math = true;
        }

        if (tokens.empty()) {
            generate_pixel_store(llvm::ConstantFP::get(float_ty, 0.0), x, y);
            return;
        }

        // Pre-scan all variables and allocate them in the entry block
        std::unordered_map<std::string, llvm::Value*> named_vars;
        std::set<std::string> all_vars;

        for (const auto& token : tokens) {
            if (token.type == TokenType::VAR_STORE ||
                token.type == TokenType::VAR_LOAD) {
                const auto& payload = std::get<TokenPayload_Var>(token.payload);
                all_vars.insert(payload.name);
            }
        }

        // Allocate all variables in the entry block
        for (const std::string& var_name : all_vars) {
            named_vars[var_name] = createAllocaInEntry(float_ty, var_name);
        }

        // Create all Basic Blocks
        std::map<int, llvm::BasicBlock*> llvm_blocks;
        for (size_t i = 0; i < cfg_blocks.size(); ++i) {
            std::string name = "b" + std::to_string(i);
            for (const auto& [label_name, block_idx] : label_to_block_idx) {
                if (block_idx == static_cast<int>(i)) {
                    name = label_name;
                    break;
                }
            }
            llvm_blocks[i] =
                llvm::BasicBlock::Create(*context, name, parent_func);
        }
        llvm::BasicBlock* exit_bb =
            llvm::BasicBlock::Create(*context, "exit", parent_func);

        // Branch from current block to the first CFG block
        builder.CreateBr(llvm_blocks[0]);

        // Initial PHI generation for merge blocks
        std::map<int, std::vector<llvm::Value*>> block_initial_stacks;
        for (size_t i = 0; i < cfg_blocks.size(); ++i) {
            if (cfg_blocks[i].predecessors.size() > 1) {
                builder.SetInsertPoint(llvm_blocks[i]);
                std::vector<llvm::Value*> initial_stack;
                int depth = stack_depth_in[i];
                for (int j = 0; j < depth; ++j) {
                    initial_stack.push_back(builder.CreatePHI(
                        float_ty, cfg_blocks[i].predecessors.size()));
                }
                block_initial_stacks[i] = initial_stack;
            }
        }

        // Process blocks
        std::map<int, std::vector<llvm::Value*>> block_final_stacks;

        for (size_t i = 0; i < cfg_blocks.size(); ++i) {
            const auto& block_info = cfg_blocks[i];
            builder.SetInsertPoint(llvm_blocks[i]);

            std::vector<llvm::Value*> rpn_stack;
            if (block_info.predecessors.empty()) {
                // Entry block, empty stack
            } else if (block_info.predecessors.size() == 1) {
                int pred_idx = block_info.predecessors[0];
                // This can fail on first pass of a loop, so we use
                // get-or-create
                if (block_final_stacks.count(pred_idx)) {
                    rpn_stack = block_final_stacks.at(pred_idx);
                }
            } else {
                rpn_stack = block_initial_stacks.at(i);
            }

            for (int j = block_info.start_token_idx;
                 j < block_info.end_token_idx; ++j) {
                const auto& token = tokens[j];

                auto applyStackOp = [&]<size_t ARITY>(auto&& op) {
                    std::array<llvm::Value*, ARITY> args;
                    for (size_t i = ARITY; i > 0; --i) {
                        args[i - 1] = rpn_stack.back();
                        rpn_stack.pop_back();
                    }

                    rpn_stack.push_back(std::apply(op, args));
                };

                auto applyIntrinsic =
                    [&]<size_t ARITY>(llvm::Intrinsic::ID id) {
                        applyStackOp.operator()<ARITY>([&](auto... args) {
                            return createIntrinsicCall(id, args...);
                        });
                    };

                auto applyBinaryOp = [&](auto opCallable) {
                    applyStackOp.operator()<2>(
                        [&](auto a, auto b) { return opCallable(a, b); });
                };

                auto applyBinaryCmp = [&](llvm::CmpInst::Predicate pred) {
                    applyStackOp.operator()<2>([&](auto a, auto b) {
                        auto cmp = builder.CreateFCmp(pred, a, b);
                        return builder.CreateSelect(
                            cmp, llvm::ConstantFP::get(float_ty, 1.0),
                            llvm::ConstantFP::get(float_ty, 0.0));
                    });
                };

                enum class BoolBinOp { And, Or, Xor };
                auto applyLogicalOp = [&](BoolBinOp which) {
                    applyStackOp.operator()<2>([&](auto a_val, auto b_val) {
                        auto a_bool = builder.CreateFCmpOGT(
                            a_val, llvm::ConstantFP::get(float_ty, 0.0));
                        auto b_bool = builder.CreateFCmpOGT(
                            b_val, llvm::ConstantFP::get(float_ty, 0.0));
                        llvm::Value* logic_res = nullptr;
                        switch (which) {
                        case BoolBinOp::And:
                            logic_res = builder.CreateAnd(a_bool, b_bool);
                            break;
                        case BoolBinOp::Or:
                            logic_res = builder.CreateOr(a_bool, b_bool);
                            break;
                        case BoolBinOp::Xor:
                            logic_res = builder.CreateXor(a_bool, b_bool);
                            break;
                        }
                        return builder.CreateSelect(
                            logic_res, llvm::ConstantFP::get(float_ty, 1.0),
                            llvm::ConstantFP::get(float_ty, 0.0));
                    });
                };

                enum class IntBinOp { And, Or, Xor };
                auto applyBitwiseOp = [&](IntBinOp which) {
                    applyStackOp.operator()<2>([&](auto a, auto b) {
                        auto ai = builder.CreateFPToSI(a, i32_ty);
                        auto bi = builder.CreateFPToSI(b, i32_ty);
                        llvm::Value* resi = nullptr;
                        switch (which) {
                        case IntBinOp::And:
                            resi = builder.CreateAnd(ai, bi);
                            break;
                        case IntBinOp::Or:
                            resi = builder.CreateOr(ai, bi);
                            break;
                        case IntBinOp::Xor:
                            resi = builder.CreateXor(ai, bi);
                            break;
                        }
                        return builder.CreateSIToFP(resi, float_ty);
                    });
                };

                auto applyApproxMathOp = [&]<size_t ARITY>(
                                             MathOp math_op,
                                             llvm::Intrinsic::ID intrinsic_id) {
                    static_assert(ARITY == 1 || ARITY == 2,
                                  "Only unary or binary operations supported");

                    std::array<llvm::Value*, ARITY> args;
                    for (size_t i = ARITY; i > 0; --i) {
                        args[i - 1] = rpn_stack.back();
                        rpn_stack.pop_back();
                    }

                    if (use_approx_math) {
                        auto* callee = math_manager.getFunction(math_op);
                        llvm::SmallVector<llvm::Value*, 2> call_args(
                            args.begin(), args.end());
                        auto* call = builder.CreateCall(callee, call_args);
                        call->setFastMathFlags(builder.getFastMathFlags());
                        rpn_stack.push_back(call);
                    } else {
                        rpn_stack.push_back(std::apply(
                            [&](auto... args) {
                                return createIntrinsicCall(intrinsic_id,
                                                           args...);
                            },
                            args));
                    }
                };

                switch (token.type) {
                // Literals & Constants
                case TokenType::NUMBER: {
                    const auto& payload =
                        std::get<TokenPayload_Number>(token.payload);
                    rpn_stack.push_back(
                        llvm::ConstantFP::get(float_ty, payload.value));
                    break;
                }
                case TokenType::CONSTANT_X:
                    rpn_stack.push_back(x_fp);
                    break;
                case TokenType::CONSTANT_Y:
                    rpn_stack.push_back(y_fp);
                    break;
                case TokenType::CONSTANT_WIDTH:
                    rpn_stack.push_back(builder.CreateSIToFP(
                        builder.getInt32(width), float_ty));
                    break;
                case TokenType::CONSTANT_HEIGHT:
                    rpn_stack.push_back(builder.CreateSIToFP(
                        builder.getInt32(height), float_ty));
                    break;
                case TokenType::CONSTANT_N:
                    rpn_stack.push_back(builder.CreateLoad(
                        float_ty, builder.CreateGEP(float_ty, props_arg,
                                                    builder.getInt32(0))));
                    break;
                case TokenType::CONSTANT_PI:
                    rpn_stack.push_back(
                        llvm::ConstantFP::get(float_ty, std::numbers::pi));
                    break;

                // Variable Ops
                case TokenType::VAR_STORE: {
                    const auto& payload =
                        std::get<TokenPayload_Var>(token.payload);
                    llvm::Value* val_to_store = rpn_stack.back();
                    rpn_stack.pop_back();
                    llvm::Value* var_ptr = named_vars[payload.name];
                    builder.CreateStore(val_to_store, var_ptr);
                    break;
                }
                case TokenType::VAR_LOAD: {
                    const auto& payload =
                        std::get<TokenPayload_Var>(token.payload);
                    llvm::Value* var_ptr = named_vars[payload.name];
                    rpn_stack.push_back(builder.CreateLoad(float_ty, var_ptr));
                    break;
                }

                // Data Access
                case TokenType::CLIP_REL: {
                    const auto& payload =
                        std::get<TokenPayload_ClipAccess>(token.payload);
                    bool use_mirror =
                        payload.has_mode ? payload.use_mirror : mirror_boundary;
                    RelYAccess access{payload.clip_idx, payload.rel_y,
                                      use_mirror};
                    llvm::Value* row_ptr = row_ptr_cache.at(access);
                    rpn_stack.push_back(generate_load_from_row_ptr(
                        row_ptr, payload.clip_idx, x, payload.rel_x, use_mirror,
                        no_x_bounds_check));
                    break;
                }
                case TokenType::CLIP_ABS: {
                    const auto& payload =
                        std::get<TokenPayload_ClipAccess>(token.payload);
                    llvm::Value* coord_y_f = rpn_stack.back();
                    rpn_stack.pop_back();
                    llvm::Value* coord_x_f = rpn_stack.back();
                    rpn_stack.pop_back();

                    llvm::Value* coord_y = builder.CreateCall(
                        llvm::Intrinsic::getOrInsertDeclaration(
                            module.get(), llvm::Intrinsic::rint, {float_ty}),
                        {coord_y_f});
                    coord_y = builder.CreateFPToSI(coord_y, i32_ty);

                    llvm::Value* coord_x = builder.CreateCall(
                        llvm::Intrinsic::getOrInsertDeclaration(
                            module.get(), llvm::Intrinsic::rint, {float_ty}),
                        {coord_x_f});
                    coord_x = builder.CreateFPToSI(coord_x, i32_ty);

                    bool use_mirror_final;
                    if (payload.has_mode) {
                        use_mirror_final = payload.use_mirror;
                    } else {
                        use_mirror_final = mirror_boundary;
                    }

                    rpn_stack.push_back(generate_pixel_load(
                        payload.clip_idx, coord_x, coord_y, use_mirror_final));
                    break;
                }
                case TokenType::CLIP_CUR: {
                    const auto& payload =
                        std::get<TokenPayload_ClipAccess>(token.payload);
                    RelYAccess access{payload.clip_idx, 0, mirror_boundary};
                    llvm::Value* row_ptr = row_ptr_cache.at(access);
                    rpn_stack.push_back(generate_load_from_row_ptr(
                        row_ptr, payload.clip_idx, x, 0, mirror_boundary,
                        no_x_bounds_check));
                    break;
                }
                case TokenType::PROP_ACCESS: {
                    const auto& payload =
                        std::get<TokenPayload_PropAccess>(token.payload);
                    auto key =
                        std::make_pair(payload.clip_idx, payload.prop_name);
                    int prop_idx = prop_map.at(key);
                    llvm::Value* prop_val = builder.CreateLoad(
                        float_ty,
                        builder.CreateGEP(float_ty, props_arg,
                                          builder.getInt32(prop_idx)));
                    rpn_stack.push_back(prop_val);
                    break;
                }

                // Binary Operators
                case TokenType::ADD: {
                    applyBinaryOp([&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateFAdd(a, b);
                    });
                    break;
                }
                case TokenType::SUB: {
                    applyBinaryOp([&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateFSub(a, b);
                    });
                    break;
                }
                case TokenType::MUL: {
                    applyBinaryOp([&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateFMul(a, b);
                    });
                    break;
                }
                case TokenType::DIV: {
                    applyBinaryOp([&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateFDiv(a, b);
                    });
                    break;
                }
                case TokenType::MOD: {
                    applyBinaryOp([&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateFRem(a, b);
                    });
                    break;
                }
                case TokenType::POW:
                    applyIntrinsic.operator()<2>(llvm::Intrinsic::pow);
                    break;
                case TokenType::ATAN2:
                    applyApproxMathOp.operator()<2>(MathOp::Atan2,
                                                    llvm::Intrinsic::atan2);
                    break;
                case TokenType::COPYSIGN: {
                    applyIntrinsic.operator()<2>(llvm::Intrinsic::copysign);
                    break;
                }
                case TokenType::MIN: {
                    applyIntrinsic.operator()<2>(llvm::Intrinsic::minnum);
                    break;
                }
                case TokenType::MAX: {
                    applyIntrinsic.operator()<2>(llvm::Intrinsic::maxnum);
                    break;
                }

                // Binary comparisons
                case TokenType::GT: {
                    applyBinaryCmp(llvm::CmpInst::FCMP_OGT);
                    break;
                }
                case TokenType::LT: {
                    applyBinaryCmp(llvm::CmpInst::FCMP_OLT);
                    break;
                }
                case TokenType::GE: {
                    applyBinaryCmp(llvm::CmpInst::FCMP_OGE);
                    break;
                }
                case TokenType::LE: {
                    applyBinaryCmp(llvm::CmpInst::FCMP_OLE);
                    break;
                }
                case TokenType::EQ: {
                    applyBinaryCmp(llvm::CmpInst::FCMP_OEQ);
                    break;
                }

                // Logical ops on booleanized floats
                case TokenType::AND: {
                    applyLogicalOp(BoolBinOp::And);
                    break;
                }
                case TokenType::OR: {
                    applyLogicalOp(BoolBinOp::Or);
                    break;
                }
                case TokenType::XOR: {
                    applyLogicalOp(BoolBinOp::Xor);
                    break;
                }

                // Bitwise ops on converted ints
                case TokenType::BITAND: {
                    applyBitwiseOp(IntBinOp::And);
                    break;
                }
                case TokenType::BITOR: {
                    applyBitwiseOp(IntBinOp::Or);
                    break;
                }
                case TokenType::BITXOR: {
                    applyBitwiseOp(IntBinOp::Xor);
                    break;
                }

                // Unary Operators
                case TokenType::SQRT: {
                    // Akarin.Expr and JITASM std.Expr both apply max(0, x)
                    // before sqrt, so we do the same for compatibility.
                    // See:
                    // https://github.com/vapoursynth/vapoursynth/issues/1112
                    auto a = rpn_stack.back();
                    rpn_stack.pop_back();
                    auto zero = llvm::ConstantFP::get(float_ty, 0.0);
                    auto max_val =
                        createIntrinsicCall(llvm::Intrinsic::maxnum, a, zero);
                    rpn_stack.push_back(
                        createIntrinsicCall(llvm::Intrinsic::sqrt, max_val));
                    break;
                }
                case TokenType::EXP:
                    applyApproxMathOp.operator()<1>(MathOp::Exp,
                                                    llvm::Intrinsic::exp);
                    break;
                case TokenType::LOG:
                    applyApproxMathOp.operator()<1>(MathOp::Log,
                                                    llvm::Intrinsic::log);
                    break;
                case TokenType::ABS: {
                    applyIntrinsic.operator()<1>(llvm::Intrinsic::fabs);
                    break;
                }
                case TokenType::FLOOR: {
                    applyIntrinsic.operator()<1>(llvm::Intrinsic::floor);
                    break;
                }
                case TokenType::CEIL: {
                    applyIntrinsic.operator()<1>(llvm::Intrinsic::ceil);
                    break;
                }
                case TokenType::TRUNC: {
                    applyIntrinsic.operator()<1>(llvm::Intrinsic::trunc);
                    break;
                }
                case TokenType::ROUND: {
                    applyIntrinsic.operator()<1>(llvm::Intrinsic::round);
                    break;
                }
                case TokenType::SIN:
                    applyApproxMathOp.operator()<1>(MathOp::Sin,
                                                    llvm::Intrinsic::sin);
                    break;
                case TokenType::COS:
                    applyApproxMathOp.operator()<1>(MathOp::Cos,
                                                    llvm::Intrinsic::cos);
                    break;
                case TokenType::TAN:
                    applyApproxMathOp.operator()<1>(MathOp::Tan,
                                                    llvm::Intrinsic::tan);
                    break;
                case TokenType::ASIN:
                    applyApproxMathOp.operator()<1>(MathOp::Asin,
                                                    llvm::Intrinsic::asin);
                    break;
                case TokenType::ACOS:
                    applyApproxMathOp.operator()<1>(MathOp::Acos,
                                                    llvm::Intrinsic::acos);
                    break;
                case TokenType::ATAN:
                    applyApproxMathOp.operator()<1>(MathOp::Atan,
                                                    llvm::Intrinsic::atan);
                    break;
                case TokenType::EXP2: {
                    applyIntrinsic.operator()<1>(llvm::Intrinsic::exp2);
                    break;
                }
                case TokenType::LOG10: {
                    applyIntrinsic.operator()<1>(llvm::Intrinsic::log10);
                    break;
                }
                case TokenType::LOG2: {
                    applyIntrinsic.operator()<1>(llvm::Intrinsic::log2);
                    break;
                }
                case TokenType::SINH: {
                    applyIntrinsic.operator()<1>(llvm::Intrinsic::sinh);
                    break;
                }
                case TokenType::COSH: {
                    applyIntrinsic.operator()<1>(llvm::Intrinsic::cosh);
                    break;
                }
                case TokenType::TANH: {
                    applyIntrinsic.operator()<1>(llvm::Intrinsic::tanh);
                    break;
                }
                case TokenType::SGN: {
                    auto x = rpn_stack.back();
                    rpn_stack.pop_back();
                    auto zero = llvm::ConstantFP::get(float_ty, 0.0);
                    auto one = llvm::ConstantFP::get(float_ty, 1.0);
                    auto nonzero = builder.CreateFCmpONE(x, zero);
                    auto sign = builder.CreateCall(
                        llvm::Intrinsic::getOrInsertDeclaration(
                            module.get(), llvm::Intrinsic::copysign,
                            {float_ty}),
                        {one, x});
                    rpn_stack.push_back(
                        builder.CreateSelect(nonzero, sign, zero));
                    break;
                }
                case TokenType::NEG: {
                    auto a = rpn_stack.back();
                    rpn_stack.pop_back();
                    rpn_stack.push_back(builder.CreateFNeg(a));
                    break;
                }

                case TokenType::NOT: {
                    auto a = rpn_stack.back();
                    rpn_stack.pop_back();
                    rpn_stack.push_back(builder.CreateSelect(
                        builder.CreateFCmpOLE(
                            a, llvm::ConstantFP::get(float_ty, 0.0)),
                        llvm::ConstantFP::get(float_ty, 1.0),
                        llvm::ConstantFP::get(float_ty, 0.0)));
                    break;
                }
                case TokenType::BITNOT: {
                    auto a = rpn_stack.back();
                    rpn_stack.pop_back();
                    rpn_stack.push_back(builder.CreateSIToFP(
                        builder.CreateNot(builder.CreateFPToSI(a, i32_ty)),
                        float_ty));
                    break;
                }

                // Ternary and other multi-arg
                case TokenType::TERNARY: {
                    auto c = rpn_stack.back();
                    rpn_stack.pop_back();
                    auto b = rpn_stack.back();
                    rpn_stack.pop_back();
                    auto a = rpn_stack.back();
                    rpn_stack.pop_back();
                    rpn_stack.push_back(builder.CreateSelect(
                        builder.CreateFCmpOGT(
                            a, llvm::ConstantFP::get(float_ty, 0.0)),
                        b, c));
                    break;
                }
                case TokenType::CLIP:
                case TokenType::CLAMP: {
                    auto max_val = rpn_stack.back();
                    rpn_stack.pop_back();
                    auto min_val = rpn_stack.back();
                    rpn_stack.pop_back();
                    auto val = rpn_stack.back();
                    rpn_stack.pop_back();
                    auto temp = createIntrinsicCall(llvm::Intrinsic::maxnum,
                                                    val, min_val);
                    auto clamped = createIntrinsicCall(llvm::Intrinsic::minnum,
                                                       temp, max_val);
                    rpn_stack.push_back(clamped);
                    break;
                }
                case TokenType::FMA: {
                    auto c = rpn_stack.back();
                    rpn_stack.pop_back();
                    auto b = rpn_stack.back();
                    rpn_stack.pop_back();
                    auto a = rpn_stack.back();
                    rpn_stack.pop_back();
                    rpn_stack.push_back(builder.CreateCall(
                        llvm::Intrinsic::getOrInsertDeclaration(
                            module.get(), llvm::Intrinsic::fma,
                            {builder.getFloatTy()}),
                        {a, b, c}));
                    break;
                }

                // Custom output control
                case TokenType::STORE_ABS: {
                    llvm::Value* coord_y_f = rpn_stack.back();
                    rpn_stack.pop_back();
                    llvm::Value* coord_x_f = rpn_stack.back();
                    rpn_stack.pop_back();
                    llvm::Value* val_to_store = rpn_stack.back();
                    rpn_stack.pop_back();
                    llvm::Value* coord_y =
                        builder.CreateFPToSI(coord_y_f, i32_ty);
                    llvm::Value* coord_x =
                        builder.CreateFPToSI(coord_x_f, i32_ty);
                    generate_pixel_store(val_to_store, coord_x, coord_y);
                    break;
                }
                case TokenType::EXIT_NO_WRITE: {
                    rpn_stack.push_back(llvm::ConstantFP::get(
                        float_ty,
                        std::bit_cast<float>(
                            EXIT_NAN_PAYLOAD))); // Use special NaN for `^exit^`
                    break;
                }

                // Stack manipulation
                case TokenType::DUP: {
                    const auto& payload =
                        std::get<TokenPayload_StackOp>(token.payload);
                    rpn_stack.push_back(
                        rpn_stack[rpn_stack.size() - 1 - payload.n]);
                    break;
                }
                case TokenType::DROP: {
                    const auto& payload =
                        std::get<TokenPayload_StackOp>(token.payload);
                    if (payload.n > 0) {
                        rpn_stack.resize(rpn_stack.size() - payload.n);
                    }
                    break;
                }
                case TokenType::SWAP: {
                    const auto& payload =
                        std::get<TokenPayload_StackOp>(token.payload);
                    std::swap(rpn_stack.back(),
                              rpn_stack[rpn_stack.size() - 1 - payload.n]);
                    break;
                }
                case TokenType::SORTN: {
                    const auto& payload =
                        std::get<TokenPayload_StackOp>(token.payload);
                    int n = payload.n;
                    if (n < 2)
                        break;

                    std::vector<llvm::Value*> values;
                    values.reserve(n);
                    for (int k = 0; k < n; ++k) {
                        values.push_back(rpn_stack.back());
                        rpn_stack.pop_back();
                    }
                    std::reverse(values.begin(), values.end());

                    auto compare_swap = [&](int i_idx, int j_idx) {
                        llvm::Value* val_i = values[i_idx];
                        llvm::Value* val_j = values[j_idx];
                        llvm::Value* cond = builder.CreateFCmpOGT(val_i, val_j);
                        values[i_idx] =
                            builder.CreateSelect(cond, val_j, val_i); // min
                        values[j_idx] =
                            builder.CreateSelect(cond, val_i, val_j); // max
                    };

                    const auto network = get_optimal_sorting_network(n);
                    if (!network.empty()) {
                        for (const auto& pair : network) {
                            if (pair.second < n) { // Bounds check
                                compare_swap(pair.first, pair.second);
                            }
                        }
                    } else {
                        std::vector<std::pair<int, int>> pairs;
                        int p = 1;
                        while (p < n)
                            p <<= 1;
                        generate_oem_sort_pairs(pairs, 0, p);
                        for (const auto& pair : pairs) {
                            if (pair.second < n) {
                                compare_swap(pair.first, pair.second);
                            }
                        }
                    }

                    for (int k = n - 1; k >= 0; --k) {
                        rpn_stack.push_back(values[k]);
                    }
                    break;
                }

                // Control Flow (no-op during this pass)
                case TokenType::LABEL_DEF:
                case TokenType::JUMP:
                    break;
                }
            }

            // Create Terminator
            if (block_info.successors.empty()) {
                builder.CreateBr(exit_bb);
            } else if (block_info.successors.size() == 1) {
                builder.CreateBr(llvm_blocks[block_info.successors[0]]);
            } else { // size is 2, from a JUMP
                llvm::Value* cond_val = rpn_stack.back();
                llvm::Value* cond = builder.CreateFCmpOGT(
                    cond_val, llvm::ConstantFP::get(float_ty, 0.0));
                builder.CreateCondBr(cond,
                                     llvm_blocks[block_info.successors[0]],
                                     llvm_blocks[block_info.successors[1]]);

                // Pop the condition value from stack after creating the
                // terminator
                rpn_stack.pop_back();
            }

            // Save the final stack state (without condition for JUMP blocks)
            block_final_stacks[i] = rpn_stack;
        }

        // Populate PHI nodes
        for (size_t i = 0; i < cfg_blocks.size(); ++i) {
            if (cfg_blocks[i].predecessors.size() > 1) {
                auto& phis = block_initial_stacks.at(i);
                for (int pred_idx : cfg_blocks[i].predecessors) {
                    auto& incoming_stack = block_final_stacks.at(pred_idx);
                    auto* incoming_block = llvm_blocks.at(pred_idx);
                    for (size_t j = 0; j < phis.size(); ++j) {
                        // All predecessor blocks now provide stacks of the
                        // correct depth
                        if (j < incoming_stack.size()) {
                            static_cast<llvm::PHINode*>(phis[j])->addIncoming(
                                incoming_stack[j], incoming_block);
                        }
                    }
                }
            }
        }

        // Final Result PHI
        builder.SetInsertPoint(exit_bb);
        std::vector<std::pair<llvm::Value*, llvm::BasicBlock*>> final_values;
        for (size_t i = 0; i < cfg_blocks.size(); ++i) {
            if (cfg_blocks[i].successors.empty()) { // Terminal block
                final_values.push_back(
                    {block_final_stacks.at(i).back(), llvm_blocks.at(i)});
            }
        }

        llvm::Value* result_val;
        if (final_values.empty()) {
            result_val = llvm::UndefValue::get(float_ty);
        } else if (final_values.size() == 1) {
            result_val = final_values[0].first;
        } else {
            llvm::PHINode* phi =
                builder.CreatePHI(float_ty, final_values.size(), "result_phi");
            for (const auto& pair : final_values) {
                phi->addIncoming(pair.first, pair.second);
            }
            result_val = phi;
        }

        bool has_exit = false;
        for (const auto& token : tokens) {
            if (token.type == TokenType::EXIT_NO_WRITE) {
                has_exit = true;
                break;
            }
        }

        if (has_exit) {
            llvm::Value* result_int =
                builder.CreateBitCast(result_val, builder.getInt32Ty());
            llvm::Value* exit_nan_int = builder.getInt32(EXIT_NAN_PAYLOAD);
            llvm::Value* is_exit_val =
                builder.CreateICmpEQ(result_int, exit_nan_int);

            llvm::BasicBlock* store_block = llvm::BasicBlock::Create(
                *context, "do_default_store", parent_func);
            llvm::BasicBlock* after_store_block = llvm::BasicBlock::Create(
                *context, "after_default_store", parent_func);

            builder.CreateCondBr(is_exit_val, after_store_block, store_block);

            builder.SetInsertPoint(store_block);
            generate_pixel_store(result_val, x, y);
            builder.CreateBr(after_store_block);

            builder.SetInsertPoint(after_store_block);
        } else {
            generate_pixel_store(result_val, x, y);
        }
    }

    llvm::Value* generate_pixel_load(int clip_idx, llvm::Value* x,
                                     llvm::Value* y, bool mirror) {
        llvm::Value* final_x =
            get_final_coord(x, builder.getInt32(width), mirror);
        llvm::Value* final_y =
            get_final_coord(y, builder.getInt32(height), mirror);

        int vs_clip_idx = clip_idx + 1; // 0 is dst
        llvm::Value* base_ptr = preloaded_base_ptrs[vs_clip_idx];
        llvm::Value* stride = preloaded_strides[vs_clip_idx];

        llvm::Value* y_offset = builder.CreateMul(final_y, stride);
        llvm::Value* row_ptr =
            builder.CreateGEP(builder.getInt8Ty(), base_ptr, y_offset);

        return generate_load_from_row_ptr(row_ptr, clip_idx, final_x, 0, mirror,
                                          true);
    }

    void generate_pixel_store(llvm::Value* value_to_store, llvm::Value* x,
                              llvm::Value* y) {
        const VSFormat* format = vo->format;
        int bpp = format->bytesPerSample;
        int dst_idx = 0;

        llvm::Value* base_ptr = preloaded_base_ptrs[dst_idx];
        llvm::Value* stride = preloaded_strides[dst_idx];

        llvm::Value* y_offset = builder.CreateMul(y, stride);
        llvm::Value* x_offset = builder.CreateMul(x, builder.getInt32(bpp));
        llvm::Value* total_offset = builder.CreateAdd(y_offset, x_offset);
        llvm::Value* pixel_addr =
            builder.CreateGEP(builder.getInt8Ty(), base_ptr, total_offset);

        int pixel_align = std::gcd(ALIGNMENT, bpp);
        assumeAligned(pixel_addr, static_cast<unsigned>(pixel_align));

        llvm::Value* final_val;
        if (format->sampleType == stInteger) {
            int max_val = (1 << format->bitsPerSample) - 1;
            llvm::Value* zero_f =
                llvm::ConstantFP::get(builder.getFloatTy(), 0.0);
            llvm::Value* max_f = llvm::ConstantFP::get(
                builder.getFloatTy(), static_cast<double>(max_val));

            llvm::Value* temp = createIntrinsicCall(llvm::Intrinsic::maxnum,
                                                    value_to_store, zero_f);
            llvm::Value* clamped_f =
                createIntrinsicCall(llvm::Intrinsic::minnum, temp, max_f);

            llvm::Value* rounded_f =
                createIntrinsicCall(llvm::Intrinsic::roundeven, clamped_f);

            llvm::Type* store_type =
                bpp == 1
                    ? builder.getInt8Ty()
                    : (bpp == 2 ? builder.getInt16Ty() : builder.getInt32Ty());
            final_val = builder.CreateFPToUI(rounded_f, store_type);
            llvm::StoreInst* si = builder.CreateStore(final_val, pixel_addr);
            setMemoryInstAttrs(si, static_cast<unsigned>(pixel_align), dst_idx);
        } else { // stFloat
            if (bpp == 4) {
                llvm::StoreInst* si =
                    builder.CreateStore(value_to_store, pixel_addr);
                setMemoryInstAttrs(si, static_cast<unsigned>(pixel_align),
                                   dst_idx);
            } else if (bpp == 2) {
                llvm::Value* truncated_val =
                    builder.CreateFPTrunc(value_to_store, builder.getHalfTy());
                llvm::StoreInst* si =
                    builder.CreateStore(truncated_val, pixel_addr);
                setMemoryInstAttrs(si, static_cast<unsigned>(pixel_align),
                                   dst_idx);
            } else {
                throw std::runtime_error("Unsupported float sample size.");
            }
        }
    }
};

enum PlaneOp { PO_PROCESS, PO_COPY };

struct ExprData {
    std::vector<VSNodeRef*> nodes;
    VSVideoInfo vi = {};
    int num_inputs;

    PlaneOp plane_op[3] = {};
    std::string expr_strs[3];
    CompiledFunction compiled[3];
    bool mirror_boundary;
    std::string dump_ir_path;
    int opt_level;
    int approx_math;

    std::vector<std::pair<int, std::string>> required_props;
    std::map<std::pair<int, std::string>, int> prop_map;
};

std::string
generate_cache_key(const std::string& expr, const VSVideoInfo* vo,
                   const std::vector<const VSVideoInfo*>& vi, bool mirror,
                   const std::map<std::pair<int, std::string>, int>& prop_map,
                   int plane_width, int plane_height) {
    std::string result =
        std::format("expr={}|mirror={}|out={}|w={}|h={}", expr, mirror,
                    vo->format->id, plane_width, plane_height);

    for (size_t i = 0; i < vi.size(); ++i) {
        result += std::format("|in{}={}", i, vi[i]->format->id);
    }

    for (const auto& [key, val] : prop_map) {
        result += std::format("|prop{}={}.{}", val, key.first, key.second);
    }

    return result;
}

void VS_CC exprInit([[maybe_unused]] VSMap* in, [[maybe_unused]] VSMap* out,
                    void** instanceData, [[maybe_unused]] VSNode* node,
                    [[maybe_unused]] VSCore* core, const VSAPI* vsapi) {
    ExprData* d = static_cast<ExprData*>(*instanceData);
    vsapi->setVideoInfo(&d->vi, 1, node);
}

const VSFrameRef* VS_CC exprGetFrame(int n, int activationReason,
                                     void** instanceData,
                                     [[maybe_unused]] void** frameData,
                                     VSFrameContext* frameCtx, VSCore* core,
                                     const VSAPI* vsapi) {
    ExprData* d = static_cast<ExprData*>(*instanceData);

    if (activationReason == arInitial) {
        for (int i = 0; i < d->num_inputs; ++i) {
            vsapi->requestFrameFilter(n, d->nodes[i], frameCtx);
        }
    } else if (activationReason == arAllFramesReady) {
        std::vector<const VSFrameRef*> src_frames(d->num_inputs);
        for (int i = 0; i < d->num_inputs; ++i) {
            src_frames[i] = vsapi->getFrameFilter(n, d->nodes[i], frameCtx);
        }

        const VSFrameRef* plane_src[3] = {
            d->plane_op[0] == PO_COPY ? src_frames[0] : nullptr,
            d->plane_op[1] == PO_COPY ? src_frames[0] : nullptr,
            d->plane_op[2] == PO_COPY ? src_frames[0] : nullptr};
        int planes[3] = {0, 1, 2};
        VSFrameRef* dst_frame =
            vsapi->newVideoFrame2(d->vi.format, d->vi.width, d->vi.height,
                                  plane_src, planes, src_frames[0], core);

        std::vector<uint8_t*> rwptrs(d->num_inputs + 1);
        std::vector<int> strides(d->num_inputs + 1);
        std::vector<float> props(1 + d->required_props.size());
        props[0] = static_cast<float>(n);

        for (size_t i = 0; i < d->required_props.size(); ++i) {
            const auto& prop_info = d->required_props[i];
            int clip_idx = prop_info.first;
            const std::string& prop_name = prop_info.second;
            int prop_array_idx = i + 1;

            const VSMap* props_map =
                vsapi->getFramePropsRO(src_frames[clip_idx]);
            int err = 0;
            int type = vsapi->propGetType(props_map, prop_name.c_str());

            if (type == ptInt) {
                props[prop_array_idx] = static_cast<float>(
                    vsapi->propGetInt(props_map, prop_name.c_str(), 0, &err));
            } else if (type == ptFloat) {
                props[prop_array_idx] = static_cast<float>(
                    vsapi->propGetFloat(props_map, prop_name.c_str(), 0, &err));
            } else if (type == ptData) {
                if (vsapi->propGetDataSize(props_map, prop_name.c_str(), 0,
                                           &err) > 0 &&
                    !err)
                    props[prop_array_idx] =
                        static_cast<float>(vsapi->propGetData(
                            props_map, prop_name.c_str(), 0, &err)[0]);
                else
                    err = 1;
            } else {
                err = 1;
            }

            if (err) {
                props[prop_array_idx] = std::bit_cast<float>(PROP_NAN_PAYLOAD);
            }
        }

        for (int plane = 0; plane < d->vi.format->numPlanes; ++plane) {
            if (d->plane_op[plane] == PO_PROCESS) {
                rwptrs[0] = vsapi->getWritePtr(dst_frame, plane);
                strides[0] = vsapi->getStride(dst_frame, plane);
                for (int i = 0; i < d->num_inputs; ++i) {
                    rwptrs[i + 1] = const_cast<uint8_t*>(
                        vsapi->getReadPtr(src_frames[i], plane));
                    strides[i + 1] = vsapi->getStride(src_frames[i], plane);
                }

                if (!d->compiled[plane].func_ptr) {
                    int width = vsapi->getFrameWidth(dst_frame, plane);
                    int height = vsapi->getFrameHeight(dst_frame, plane);

                    std::vector<const VSVideoInfo*> vi(d->num_inputs);
                    for (int i = 0; i < d->num_inputs; ++i) {
                        vi[i] = vsapi->getVideoInfo(d->nodes[i]);
                    }

                    const std::string key = generate_cache_key(
                        d->expr_strs[plane], &d->vi, vi, d->mirror_boundary,
                        d->prop_map, width, height);

                    std::lock_guard<std::mutex> lock(cache_mutex);
                    if (!jit_cache.count(key)) {
                        vsapi->logMessage(
                            mtDebug,
                            std::format("JIT compiling expression for plane {} "
                                        "({}x{}): {}",
                                        plane, width, height,
                                        d->expr_strs[plane])
                                .c_str());
                        size_t key_hash = std::hash<std::string>{}(key);
                        std::string func_name =
                            std::format("process_plane_{}_{}", plane, key_hash);

                        try {
                            Compiler compiler(
                                tokenize(d->expr_strs[plane], d->num_inputs),
                                &d->vi, vi, width, height, d->mirror_boundary,
                                d->dump_ir_path, d->prop_map, func_name,
                                d->opt_level, d->approx_math);
                            jit_cache[key] = compiler.compile();
                        } catch (const std::exception& e) {
                            // This should not happen since validation was done in exprCreate
                            // But we catch it just in case
                            std::string error_msg = std::format(
                                "Compilation error for plane {}: {}", plane,
                                e.what());
                            vsapi->logMessage(mtFatal, error_msg.c_str());
                            // Return an empty frame or handle error appropriately
                            for (const auto& frame : src_frames) {
                                vsapi->freeFrame(frame);
                            }
                            vsapi->freeFrame(dst_frame);
                            return nullptr;
                        }
                    }
                    d->compiled[plane] = jit_cache.at(key);
                }

                d->compiled[plane].func_ptr(rwptrs.data(), strides.data(),
                                            props.data());
            }
        }

        for (const auto& frame : src_frames) {
            vsapi->freeFrame(frame);
        }
        return dst_frame;
    }

    return nullptr;
}

void VS_CC exprFree(void* instanceData, [[maybe_unused]] VSCore* core,
                    const VSAPI* vsapi) {
    ExprData* d = static_cast<ExprData*>(instanceData);
    for (auto* node : d->nodes) {
        vsapi->freeNode(node);
    }
    delete d;
}

void VS_CC exprCreate(const VSMap* in, VSMap* out,
                      [[maybe_unused]] void* userData, VSCore* core,
                      const VSAPI* vsapi) {
    auto d = std::make_unique<ExprData>();
    int err = 0;

    try {
        d->num_inputs = vsapi->propNumElements(in, "clips");
        if (d->num_inputs == 0)
            throw std::runtime_error("At least one clip must be provided.");
        d->nodes.resize(d->num_inputs);
        for (int i = 0; i < d->num_inputs; ++i) {
            d->nodes[i] = vsapi->propGetNode(in, "clips", i, &err);
        }

        std::vector<const VSVideoInfo*> vi(d->num_inputs);
        for (int i = 0; i < d->num_inputs; ++i) {
            vi[i] = vsapi->getVideoInfo(d->nodes[i]);
            if (!isConstantFormat(vi[i]))
                throw std::runtime_error(
                    "Only constant format clips are supported.");
        }
        for (int i = 1; i < d->num_inputs; ++i) {
            if (vi[i]->width != vi[0]->width ||
                vi[i]->height != vi[0]->height) {
                throw std::runtime_error(
                    "All clips must have the same dimensions.");
            }
        }

        d->vi = *vi[0];
        const int format_id =
            static_cast<int>(vsapi->propGetInt(in, "format", 0, &err));
        if (!err) {
            const VSFormat* f = vsapi->getFormatPreset(format_id, core);
            if (f) {
                if (d->vi.format->colorFamily == cmCompat) {
                    throw std::runtime_error(
                        "Compat formats are not supported.");
                }
                if (d->vi.format->numPlanes != f->numPlanes) {
                    throw std::runtime_error("The number of planes in the "
                                             "inputs and output must match.");
                }
                d->vi.format = vsapi->registerFormat(
                    d->vi.format->colorFamily, f->sampleType, f->bitsPerSample,
                    d->vi.format->subSamplingW, d->vi.format->subSamplingH,
                    core);
                if (!d->vi.format) {
                    throw std::runtime_error("Failed to register new format.");
                }
            }
        }

        const int nexpr = vsapi->propNumElements(in, "expr");
        if (nexpr == 0)
            throw std::runtime_error(
                "At least one expression must be provided.");

        std::string expr_strs[3];
        for (int i = 0; i < nexpr; ++i) {
            expr_strs[i] = vsapi->propGetData(in, "expr", i, &err);
        }
        for (int i = nexpr; i < d->vi.format->numPlanes; ++i) {
            expr_strs[i] = expr_strs[nexpr - 1];
        }

        // Tokenize, validate, and extract properties
        for (int i = 0; i < d->vi.format->numPlanes; ++i) {
            if (expr_strs[i].empty()) {
                d->plane_op[i] = PO_COPY;
                continue;
            }
            d->plane_op[i] = PO_PROCESS;
            d->expr_strs[i] = expr_strs[i];
            auto tokens = tokenize(d->expr_strs[i], d->num_inputs);

            for (const auto& token : tokens) {
                if (token.type == TokenType::PROP_ACCESS) {
                    const auto& payload =
                        std::get<TokenPayload_PropAccess>(token.payload);
                    auto key =
                        std::make_pair(payload.clip_idx, payload.prop_name);
                    if (d->prop_map.find(key) == d->prop_map.end()) {
                        d->prop_map[key] =
                            1 +
                            d->required_props.size(); // 0 is for frame number N
                        d->required_props.push_back(key);
                    }
                }
            }

            const int w =
                vi[0]->width >> (i > 0 ? d->vi.format->subSamplingW : 0);
            const int h =
                vi[0]->height >> (i > 0 ? d->vi.format->subSamplingH : 0);

            Compiler validator(std::move(tokens), &d->vi, vi, w, h,
                               d->mirror_boundary, d->prop_map);
            validator.validate();
        }

        d->mirror_boundary = vsapi->propGetInt(in, "boundary", 0, &err) != 0;

        const char* dump_path = vsapi->propGetData(in, "dump_ir", 0, &err);
        if (!err && dump_path) {
            d->dump_ir_path = dump_path;
        }

        d->opt_level = vsapi->propGetInt(in, "opt_level", 0, &err);
        if (err) {
            d->opt_level = 5;
        }
        if (d->opt_level <= 0) {
            throw std::runtime_error("opt_level must be greater than 0.");
        }

        d->approx_math = vsapi->propGetInt(in, "approx_math", 0, &err);
        if (err) {
            d->approx_math = 2; // Default to auto mode
        }
        if (d->approx_math < 0 || d->approx_math > 2) {
            throw std::runtime_error(
                "approx_math must be 0 (disabled), 1 (enabled), or 2 (auto).");
        }
        // TODO: should we enable approx math only for NEON and x86_64?

    } catch (const std::exception& e) {
        for (auto* node : d->nodes) {
            if (node)
                vsapi->freeNode(node);
        }
        vsapi->setError(out, std::format("Expr: {}", e.what()).c_str());
        return;
    }

    vsapi->createFilter(in, out, "Expr", exprInit, exprGetFrame, exprFree,
                        fmParallel, 0, d.release(), core);
}

} // anonymous namespace

VS_EXTERNAL_API(void)
VapourSynthPluginInit(VSConfigPlugin configFunc,
                      VSRegisterFunction registerFunc, VSPlugin* plugin) {
    configFunc("com.yuygfgg.llvmexpr", "llvmexpr",
               "LLVM JIT RPN Expression Filter", VAPOURSYNTH_API_VERSION, 1,
               plugin);
    registerFunc("Expr",
                 "clips:clip[];expr:data[];format:int:opt;boundary:int:opt;"
                 "dump_ir:data:opt;opt_level:int:opt;approx_math:int:opt;",
                 exprCreate, nullptr, plugin);
}