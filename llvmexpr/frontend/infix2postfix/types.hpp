#ifndef LLVMEXPR_INFIX2POSTFIX_TYPES_HPP
#define LLVMEXPR_INFIX2POSTFIX_TYPES_HPP

#include <set>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace infix2postfix {

enum class Type {
    VALUE,
    CLIP,
    LITERAL,
    LITERAL_STRING,
};

inline std::string to_string(Type t) {
    switch (t) {
    case Type::VALUE:
        return "Value";
    case Type::CLIP:
        return "Clip";
    case Type::LITERAL:
        return "Literal";
    case Type::LITERAL_STRING:
        return "LiteralString";
    }
}

enum class TokenType {
    // Keywords
    If,       // if
    Else,     // else
    While,    // while
    Goto,     // goto
    Function, // function
    Return,   // return

    // Operators
    Plus,       // +
    Minus,      // -
    Star,       // *
    Slash,      // /
    Percent,    // %
    StarStar,   // **
    LogicalAnd, // &&
    LogicalOr,  // ||
    BitAnd,     // &
    BitOr,      // |
    BitXor,     // ^
    BitNot,     // ~
    Eq,         // ==
    Ne,         // !=
    Lt,         // <
    Le,         // <=
    Gt,         // >
    Ge,         // >=
    Assign,     // =
    Question,   // ?
    Colon,      // :
    Not,        // !

    // Punctuation
    LParen,    // (
    RParen,    // )
    LBrace,    // {
    RBrace,    // }
    LBracket,  // [
    RBracket,  // ]
    Comma,     // ,
    Dot,       // .
    Semicolon, // ;

    // Literals
    Identifier,
    Number,

    // Special
    Global, // <global...>
    Newline,
    EndOfFile,
    Invalid,
};

struct TokenMapping {
    TokenType type;
    std::string_view str;
};

constexpr TokenMapping token_mappings[] = {
    // Keywords
    {TokenType::If, "if"},
    {TokenType::Else, "else"},
    {TokenType::While, "while"},
    {TokenType::Goto, "goto"},
    {TokenType::Function, "function"},
    {TokenType::Return, "return"},

    // Operators
    {TokenType::Plus, "+"},
    {TokenType::Minus, "-"},
    {TokenType::Star, "*"},
    {TokenType::Slash, "/"},
    {TokenType::Percent, "%"},
    {TokenType::StarStar, "**"},
    {TokenType::LogicalAnd, "&&"},
    {TokenType::LogicalOr, "||"},
    {TokenType::BitAnd, "&"},
    {TokenType::BitOr, "|"},
    {TokenType::BitXor, "^"},
    {TokenType::BitNot, "~"},
    {TokenType::Eq, "=="},
    {TokenType::Ne, "!="},
    {TokenType::Lt, "<"},
    {TokenType::Le, "<="},
    {TokenType::Gt, ">"},
    {TokenType::Ge, ">="},
    {TokenType::Assign, "="},
    {TokenType::Question, "?"},
    {TokenType::Colon, ":"},
    {TokenType::Not, "!"},

    // Punctuation
    {TokenType::LParen, "("},
    {TokenType::RParen, ")"},
    {TokenType::LBrace, "{"},
    {TokenType::RBrace, "}"},
    {TokenType::LBracket, "["},
    {TokenType::RBracket, "]"},
    {TokenType::Comma, ","},
    {TokenType::Dot, "."},
    {TokenType::Semicolon, ";"},
};

inline std::string token_type_to_string(TokenType type) {
    for (const auto& mapping : token_mappings) {
        if (mapping.type == type) {
            return std::string(mapping.str);
        }
    }
    // Handle special cases not in the table
    switch (type) {
    case TokenType::Identifier:
        return "identifier";
    case TokenType::Number:
        return "number";
    case TokenType::Global:
        return "global declaration";
    case TokenType::Newline:
        return "newline";
    case TokenType::Semicolon:
        return "semicolon";
    case TokenType::EndOfFile:
        return "end of file";
    case TokenType::Invalid:
        return "invalid token";
    default:
        std::unreachable();
    }
}

struct Token {
    TokenType type;
    std::string value;
    int line;
};

enum class Mode { Expr, Single };

enum class GlobalMode { NONE, ALL, SPECIFIC };

struct ParameterInfo {
    std::string name;
    Type type;
};

struct FunctionSignature {
    std::string name;
    std::vector<ParameterInfo> params;
    bool has_return;
    int line;
    GlobalMode global_mode = GlobalMode::NONE;
    std::set<std::string> specific_globals;
};

} // namespace infix2postfix

#endif