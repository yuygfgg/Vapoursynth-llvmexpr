#ifndef LLVMEXPR_INFIX2POSTFIX_TYPES_HPP
#define LLVMEXPR_INFIX2POSTFIX_TYPES_HPP

#include <format>
#include <set>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace infix2postfix {

struct SourceLocation {
    int line = 0;
    int column = 0;
};

struct Range {
    SourceLocation start;
    SourceLocation end;

    std::string to_string() const {
        return std::format("{}:{} - {}:{}", start.line, start.column, end.line,
                           end.column);
    }
};

enum class Type {
    VALUE,
    CLIP,
    LITERAL,
    LITERAL_STRING,
    ARRAY,
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
    case Type::ARRAY:
        return "Array";
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
    Range range;
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
    bool returns_value;
    Range range;
    GlobalMode global_mode = GlobalMode::NONE;
    std::set<std::string> specific_globals;

    // For <global.all>, track which global variables are actually used in the function body
    std::set<std::string> used_globals;
};

inline int get_clip_index(const std::string& s) {
    if (s.length() == 1 && s[0] >= 'a' && s[0] <= 'z')
        return s[0] - 'a';
    if (s.rfind("src", 0) == 0) {
        for (size_t i = 3; i < s.length(); ++i) {
            if (!std::isdigit(s[i]))
                return -1;
        }
        return std::stoi(s.substr(3));
    }
    return -1;
}

inline bool is_clip_name(const std::string& s) {
    return get_clip_index(s) != -1;
}

} // namespace infix2postfix

#endif