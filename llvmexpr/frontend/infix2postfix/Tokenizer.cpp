#include "Tokenizer.hpp"
#include <algorithm>
#include <cctype>

namespace infix2postfix {

namespace {

std::map<std::string, TokenType> build_keywords_map() {
    std::map<std::string, TokenType> map;
    for (const auto& mapping : token_mappings) {
        if (mapping.str.length() > 1 && std::isalpha(mapping.str[0])) {
            map[std::string(mapping.str)] = mapping.type;
        }
    }
    return map;
}

using OpMap = std::map<char, std::vector<TokenMapping>>;

OpMap build_operator_map() {
    OpMap map;
    for (const auto& mapping : token_mappings) {
        if (!mapping.str.empty() && !std::isalpha(mapping.str[0])) {
            map[mapping.str[0]].push_back(mapping);
        }
    }

    // Sort by length descending for greedy matching
    for (auto& pair : map) {
        std::sort(pair.second.begin(), pair.second.end(),
                  [](const TokenMapping& a, const TokenMapping& b) {
                      return a.str.length() > b.str.length();
                  });
    }
    return map;
}

} // namespace

const std::map<std::string, TokenType> Tokenizer::keywords =
    build_keywords_map();
static const OpMap operator_map = build_operator_map();

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

    char c = peek();

    if (c == '\n') {
        line++;
        advance();
        return makeToken(TokenType::Newline);
    }

    if (std::isalpha(c) || c == '_')
        return identifier();
    if (std::isdigit(c))
        return number();
    if (c == '$') {
        if (std::isalpha(peek(1)) || peek(1) == '_') {
            advance(); // '$'
            while (std::isalnum(peek()) || peek() == '_') {
                advance();
            }
            return makeToken(TokenType::Identifier);
        }
    }

    if (c == '<') {
        if (source.substr(current, 7) == "<global") {
            return globalDeclaration();
        }
    }

    if (operator_map.count(c)) {
        const auto& possible_tokens = operator_map.at(c);
        for (const auto& mapping : possible_tokens) {
            if (source.substr(current, mapping.str.length()) == mapping.str) {
                current += mapping.str.length();
                return makeToken(mapping.type);
            }
        }
    }

    advance();
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
    if (peek() == '0' && (peek(1) == 'x' || peek(1) == 'X')) {
        is_hex = true;
        advance(); // 0
        advance(); // x
    }

    while (std::isdigit(peek()) || (is_hex && std::isxdigit(peek())))
        advance();

    if (peek() == '.' && std::isdigit(peek(1))) {
        advance(); // '.'
        while (std::isdigit(peek()))
            advance();
    }

    if (is_hex && (peek() == 'p' || peek() == 'P')) {
        advance(); // 'p'
        if (peek() == '+' || peek() == '-')
            advance(); // sign
        while (std::isdigit(peek()))
            advance();
    } else if (!is_hex && (peek() == 'e' || peek() == 'E')) {
        advance(); // 'e'
        if (peek() == '+' || peek() == '-')
            advance(); // sign
        while (std::isdigit(peek()))
            advance();
    }

    return makeToken(TokenType::Number);
}

Token Tokenizer::globalDeclaration() {
    advance();     // Consume initial '<'
    int depth = 1; // We've already seen the opening '<'
    while (depth > 0 && peek() != '\0') {
        char c = peek();
        if (c == '<') {
            depth++;
        } else if (c == '>') {
            depth--;
        }
        advance();
    }

    if (depth == 0) {
        return makeToken(TokenType::Global);
    }
    return makeToken(TokenType::Invalid, source.substr(start, current - start));
}

} // namespace infix2postfix