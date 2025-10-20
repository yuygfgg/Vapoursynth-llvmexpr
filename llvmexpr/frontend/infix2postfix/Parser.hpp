#ifndef LLVMEXPR_INFIX2POSTFIX_PARSER_HPP
#define LLVMEXPR_INFIX2POSTFIX_PARSER_HPP

#include "AST.hpp"
#include <set>
#include <vector>

namespace infix2postfix {

struct ErrorInfo {
    std::string message;
    int line;
};

struct ParseResult {
    std::unique_ptr<Program> ast;
    std::vector<ErrorInfo> errors;
};

class Parser {
  public:
    explicit Parser(const std::vector<Token>& tokens);
    ParseResult parse();

  private:
    std::unique_ptr<Stmt> parseDeclaration();
    std::unique_ptr<Stmt> parseStatement();
    std::unique_ptr<Stmt> parseIfStatement();
    std::unique_ptr<Stmt> parseWhileStatement();
    std::unique_ptr<Stmt> parseGotoStatement();
    std::unique_ptr<Stmt> parseLabelStatement();
    std::unique_ptr<Stmt> parseReturnStatement();
    std::unique_ptr<BlockStmt> parseBlock();
    std::unique_ptr<Stmt> parseExprStatement();
    std::unique_ptr<FunctionDef> parseFunctionDef();
    std::unique_ptr<GlobalDecl> parseGlobalDecl();

    std::unique_ptr<Expr> parseTernary();

    template <typename NextLevel, typename... TokenTypes>
    std::unique_ptr<Expr> parseBinary(NextLevel next_level,
                                      TokenTypes... token_types);

    std::unique_ptr<Expr> parseLogicalOr();
    std::unique_ptr<Expr> parseLogicalAnd();
    std::unique_ptr<Expr> parseBitwiseOr();
    std::unique_ptr<Expr> parseBitwiseXor();
    std::unique_ptr<Expr> parseBitwiseAnd();
    std::unique_ptr<Expr> parseEquality();
    std::unique_ptr<Expr> parseComparison();
    std::unique_ptr<Expr> parseTerm();
    std::unique_ptr<Expr> parseFactor();
    std::unique_ptr<Expr> parseExponent();
    std::unique_ptr<Expr> parseUnary();
    std::unique_ptr<Expr> parsePostfix();
    std::unique_ptr<Expr> parsePrimary();
    std::unique_ptr<Expr> finishCall(std::unique_ptr<Expr> callee);

    bool match(const std::vector<TokenType>& types);
    Token consume(TokenType type, const std::string& message);
    Token advance();
    Token peek() const;
    Token peek(int offset) const;
    Token previous() const;
    bool isAtEnd() const;
    void error(const Token& token, const std::string& message);
    void synchronize();
    void report_error(const Token& token, const std::string& message);

    std::vector<Token> tokens;
    int current = 0;
    std::set<std::string> defined_functions;
    std::vector<ErrorInfo> errors;
    bool panic_mode = false;
};

} // namespace infix2postfix

#endif