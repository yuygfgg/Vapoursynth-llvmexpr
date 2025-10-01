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

#ifndef LLVMEXPR_TOKENIZER_HPP
#define LLVMEXPR_TOKENIZER_HPP

#include <string>
#include <variant>
#include <vector>

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

// Public interface
std::vector<Token> tokenize(const std::string& expr, int num_inputs);
TokenBehavior get_token_behavior(const Token& token);

#endif // LLVMEXPR_TOKENIZER_HPP


