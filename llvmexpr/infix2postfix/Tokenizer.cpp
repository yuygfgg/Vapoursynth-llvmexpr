#include "Tokenizer.hpp"
#include <cctype>

namespace infix2postfix {

const std::map<std::string, TokenType> Tokenizer::keywords = {
    {"if", TokenType::If},
    {"else", TokenType::Else},
    {"while", TokenType::While},
    {"goto", TokenType::Goto},
    {"function", TokenType::Function},
    {"return", TokenType::Return},
};

Tokenizer::Tokenizer(const std::string& source) : source(source) {}

std::vector<Token> Tokenizer::tokenize() {
    std::vector<Token> tokens;
    while (peek() != '\0') {
        tokens.push_back(nextToken());
    }
    tokens.push_back(makeToken(TokenType::EndOfFile));
    return tokens;
}

Token Tokenizer::nextToken() {
    skipWhitespaceAndComments();
    start = current;
    if (peek() == '\0')
        return makeToken(TokenType::EndOfFile);

    char c = advance();

    if (std::isalpha(c) || c == '_')
        return identifier();
    if (std::isdigit(c))
        return number();
    if (c == '$') {
        if (std::isalpha(peek()) || peek() == '_') {
            advance();
            return identifier();
        }
    }

    switch (c) {
    case '(':
        return makeToken(TokenType::LParen);
    case ')':
        return makeToken(TokenType::RParen);
    case '{':
        return makeToken(TokenType::LBrace);
    case '}':
        return makeToken(TokenType::RBrace);
    case '[':
        return makeToken(TokenType::LBracket);
    case ']':
        return makeToken(TokenType::RBracket);
    case ',':
        return makeToken(TokenType::Comma);
    case '.':
        return makeToken(TokenType::Dot);
    case '-':
        return makeToken(TokenType::Minus);
    case '+':
        return makeToken(TokenType::Plus);
    case '/':
        return makeToken(TokenType::Slash);
    case '%':
        return makeToken(TokenType::Percent);
    case '?':
        return makeToken(TokenType::Question);
    case ':':
        return makeToken(TokenType::Colon);
    case '~':
        return makeToken(TokenType::BitNot);

    case '*':
        return makeToken(peek() == '*' ? (advance(), TokenType::StarStar)
                                       : TokenType::Star);
    case '!':
        return makeToken(peek() == '=' ? (advance(), TokenType::Ne)
                                       : TokenType::Not);
    case '=':
        return makeToken(peek() == '=' ? (advance(), TokenType::Eq)
                                       : TokenType::Assign);
    case '<':
        if (peek() == '=')
            return (advance(), makeToken(TokenType::Le));
        if (peek() == 'g')
            return globalDeclaration(); // <global...>
        return makeToken(TokenType::Lt);
    case '>':
        return makeToken(peek() == '=' ? (advance(), TokenType::Ge)
                                       : TokenType::Gt);
    case '&':
        return makeToken(peek() == '&' ? (advance(), TokenType::LogicalAnd)
                                       : TokenType::BitAnd);
    case '|':
        return makeToken(peek() == '|' ? (advance(), TokenType::LogicalOr)
                                       : TokenType::BitOr);
    case '^':
        return makeToken(TokenType::BitXor);
    }

    return makeToken(TokenType::Invalid, std::string(1, c));
}

void Tokenizer::skipWhitespaceAndComments() {
    while (true) {
        char c = peek();
        switch (c) {
        case ' ':
        case '\r':
        case '\t':
            advance();
            break;
        case '\n':
            line++;
            advance();
            break;
        case '#':
            while (peek() != '\n' && peek() != '\0')
                advance();
            break;
        default:
            return;
        }
    }
}

char Tokenizer::peek(int offset) const {
    if (current + offset >= source.length())
        return '\0';
    return source[current + offset];
}

char Tokenizer::advance() {
    if (current < source.length())
        current++;
    return source[current - 1];
}

Token Tokenizer::makeToken(TokenType type, const std::string& value) const {
    return {type, value.empty() ? source.substr(start, current - start) : value,
            line};
}

Token Tokenizer::identifier() {
    while (std::isalnum(peek()) || peek() == '_')
        advance();
    std::string text = source.substr(start, current - start);
    auto it = keywords.find(text);
    if (it != keywords.end()) {
        return makeToken(it->second);
    }
    return makeToken(TokenType::Identifier);
}

Token Tokenizer::number() {
    bool is_hex = false;
    if (peek(-1) == '0' && (peek() == 'x' || peek() == 'X')) {
        is_hex = true;
        advance(); // 'x'
    }

    while (std::isdigit(peek()) || (is_hex && std::isxdigit(peek())))
        advance();

    if (peek() == '.' && std::isdigit(peek(1))) {
        advance(); // '.'
        while (std::isdigit(peek()))
            advance();
    }

    if (is_hex && (peek() == 'p' || peek() == 'P')) {
        if (peek(1) == '+' || peek(1) == '-')
            advance(); // sign
        advance();     // 'p'
        while (std::isdigit(peek()))
            advance();
    } else if (!is_hex && (peek() == 'e' || peek() == 'E')) {
        if (peek(1) == '+' || peek(1) == '-')
            advance(); // sign
        advance();     // 'e'
        while (std::isdigit(peek()))
            advance();
    }

    return makeToken(TokenType::Number);
}

Token Tokenizer::globalDeclaration() {
    int depth = 1; // We've already seen the opening '<'
    while (peek() != '\0' && depth > 0) {
        char c = advance();
        if (c == '<') {
            depth++;
        } else if (c == '>') {
            depth--;
        }
    }
    if (depth == 0) {
        return makeToken(TokenType::Global);
    }
    return makeToken(TokenType::Invalid, "<");
}

} // namespace infix2postfix
