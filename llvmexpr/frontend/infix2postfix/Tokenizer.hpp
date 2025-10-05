#ifndef LLVMEXPR_INFIX2POSTFIX_TOKENIZER_HPP
#define LLVMEXPR_INFIX2POSTFIX_TOKENIZER_HPP

#include "types.hpp"
#include <map>
#include <string>
#include <vector>

namespace infix2postfix {

class Tokenizer {
  public:
    explicit Tokenizer(const std::string& source);
    std::vector<Token> tokenize();

  private:
    Token nextToken();
    char peek(int offset = 0) const;
    char advance();
    void skipWhitespaceAndComments();
    Token makeToken(TokenType type, const std::string& value = "") const;
    Token identifier();
    Token number();
    Token globalDeclaration();

    std::string source;
    size_t current = 0;
    size_t start = 0;
    int line = 1;

    static const std::map<std::string, TokenType> keywords;
};

} // namespace infix2postfix

#endif