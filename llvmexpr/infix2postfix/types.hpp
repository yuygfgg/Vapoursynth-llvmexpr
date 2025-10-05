#ifndef LLVMEXPR_INFIX2POSTFIX_TYPES_HPP
#define LLVMEXPR_INFIX2POSTFIX_TYPES_HPP

#include <set>
#include <string>
#include <vector>

namespace infix2postfix {

enum class TokenType {
    // Keywords
    If,
    Else,
    While,
    Goto,
    Function,
    Return,

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    StarStar,   // **
    LogicalAnd, // &&
    LogicalOr,  // ||
    BitAnd,
    BitOr,
    BitXor,
    BitNot, // &, |, ^, ~
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,     // ==, !=, <, <=, >, >=
    Assign, // =
    Question,
    Colon, // ? :
    Not,   // !

    // Punctuation
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Dot,

    // Literals
    Identifier,
    Number,

    // Special
    Global, // <global...>
    EndOfFile,
    Invalid,
};

struct Token {
    TokenType type;
    std::string value;
    int line;
};

enum class Mode { Expr, Single };

enum class GlobalMode { NONE, ALL, SPECIFIC };

struct FunctionSignature {
    std::string name;
    std::vector<std::string> params;
    bool has_return;
    int line;
    GlobalMode global_mode = GlobalMode::NONE;
    std::set<std::string> specific_globals;
};

} // namespace infix2postfix

#endif