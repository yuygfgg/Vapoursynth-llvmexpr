#include "Parser.hpp"
#include <cctype>
#include <format>
#include <utility>

namespace infix2postfix {

Parser::Parser(const std::vector<Token>& tokens) : tokens(tokens) {}

std::unique_ptr<Program> Parser::parse() {
    auto program = std::make_unique<Program>();
    while (!isAtEnd()) {
        try {
            program->statements.push_back(parseDeclaration());
        } catch (const ParserError& e) {
            // TODO: Report the error and continue to find more errors.
            throw;
        }
    }
    return program;
}

std::unique_ptr<Stmt> Parser::parseDeclaration() {
    if (peek().type == TokenType::Global) {
        auto globalDecl = parseGlobalDecl();
        // Global declaration must be followed by a function definition
        if (peek().type != TokenType::Function) {
            error(peek(), "Global declaration must be followed by a function "
                          "definition.");
        }
        auto funcDef = parseFunctionDef();
        // Attach the global declaration to the function
        funcDef->global_decl = std::move(globalDecl);
        return make_node<Stmt, FunctionDef>(std::move(*funcDef));
    }
    if (peek().type == TokenType::Function) {
        auto funcDef = parseFunctionDef();
        return std::make_unique<Stmt>(FunctionDef(std::move(*funcDef)));
    }
    return parseStatement();
}

std::unique_ptr<Stmt> Parser::parseStatement() {
    if (peek().type == TokenType::If)
        return parseIfStatement();
    if (peek().type == TokenType::While)
        return parseWhileStatement();
    if (peek().type == TokenType::LBrace) {
        auto block = parseBlock();
        return std::make_unique<Stmt>(BlockStmt(std::move(*block)));
    }
    if (peek().type == TokenType::Goto)
        return parseGotoStatement();
    if (peek().type == TokenType::Return)
        return parseReturnStatement();
    if (peek().type == TokenType::Identifier &&
        peek(1).type == TokenType::Colon) {
        return parseLabelStatement();
    }
    return parseExprStatement();
}

std::unique_ptr<Stmt> Parser::parseIfStatement() {
    Token keyword = consume(TokenType::If, "Expect 'if'.");
    consume(TokenType::LParen, "Expect '(' after 'if'.");
    auto condition = parseTernary();
    consume(TokenType::RParen, "Expect ')' after if condition.");
    auto thenBranch = parseStatement();
    std::unique_ptr<Stmt> elseBranch = nullptr;
    if (match({TokenType::Else})) {
        elseBranch = parseStatement();
    }
    return make_node<Stmt, IfStmt>(std::move(condition), std::move(thenBranch),
                                   std::move(elseBranch));
}

std::unique_ptr<Stmt> Parser::parseWhileStatement() {
    consume(TokenType::While, "Expect 'while'.");
    consume(TokenType::LParen, "Expect '(' after 'while'.");
    auto condition = parseTernary();
    consume(TokenType::RParen, "Expect ')' after while condition.");
    auto body = parseStatement();
    return make_node<Stmt, WhileStmt>(std::move(condition), std::move(body));
}

std::unique_ptr<Stmt> Parser::parseGotoStatement() {
    Token keyword = consume(TokenType::Goto, "Expect 'goto'.");
    Token label =
        consume(TokenType::Identifier, "Expect label name after 'goto'.");
    return make_node<Stmt, GotoStmt>(keyword, label, nullptr);
}

std::unique_ptr<Stmt> Parser::parseLabelStatement() {
    Token name = consume(TokenType::Identifier, "Expect label name.");
    consume(TokenType::Colon, "Expect ':' after label name.");
    return make_node<Stmt, LabelStmt>(name);
}

std::unique_ptr<Stmt> Parser::parseReturnStatement() {
    Token keyword = consume(TokenType::Return, "Expect 'return'.");
    std::unique_ptr<Expr> value = nullptr;
    if (peek().type != TokenType::EndOfFile &&
        peek().type != TokenType::RBrace) {
        value = parseTernary();
    }
    return make_node<Stmt, ReturnStmt>(keyword, std::move(value));
}

std::unique_ptr<BlockStmt> Parser::parseBlock() {
    consume(TokenType::LBrace, "Expect '{' to start a block.");
    std::vector<std::unique_ptr<Stmt>> statements;
    while (peek().type != TokenType::RBrace && !isAtEnd()) {
        statements.push_back(parseDeclaration());
    }
    consume(TokenType::RBrace, "Expect '}' to end a block.");
    return std::make_unique<BlockStmt>(BlockStmt(std::move(statements)));
}

std::unique_ptr<Stmt> Parser::parseExprStatement() {
    // Check if this is an assignment statement
    if (peek().type == TokenType::Identifier) {
        Token name = peek();
        if (peek(1).type == TokenType::Assign) {
            advance(); // identifier
            advance(); // '='
            auto value = parseTernary();
            return make_node<Stmt, AssignStmt>(name, std::move(value));
        }
    }
    auto expr = parseTernary();
    return make_node<Stmt, ExprStmt>(std::move(expr));
}

std::unique_ptr<FunctionDef> Parser::parseFunctionDef() {
    consume(TokenType::Function, "Expect 'function'.");
    Token name = consume(TokenType::Identifier, "Expect function name.");

    if (defined_functions.count(name.value)) {
        error(name,
              std::format("Function '{}' is already defined.", name.value));
    }
    defined_functions.insert(name.value);

    consume(TokenType::LParen, "Expect '(' after function name.");
    std::vector<Token> params;
    if (peek().type != TokenType::RParen) {
        do {
            params.push_back(
                consume(TokenType::Identifier, "Expect parameter name."));
        } while (match({TokenType::Comma}));
    }
    consume(TokenType::RParen, "Expect ')' after parameters.");
    auto body = parseBlock();
    return std::make_unique<FunctionDef>(
        FunctionDef(name, params, std::move(body), nullptr));
}

std::unique_ptr<GlobalDecl> Parser::parseGlobalDecl() {
    Token keyword = consume(TokenType::Global, "Expect '<global...>'.");
    std::string content = keyword.value.substr(1, keyword.value.length() - 2);
    if (content == "global.all") {
        return std::make_unique<GlobalDecl>(
            GlobalDecl(keyword, GlobalMode::ALL));
    } else if (content == "global.none") {
        return std::make_unique<GlobalDecl>(
            GlobalDecl(keyword, GlobalMode::NONE));
    } else {
        // <global<var1><var2>...>
        std::vector<Token> globals;

        // Skip "global" prefix
        size_t pos = 0;
        if (content.substr(0, 6) == "global") {
            pos = 6;
        } else {
            error(keyword, "Invalid global declaration format.");
        }

        // Parse each <varname>
        while (pos < content.length()) {
            if (content[pos] != '<') {
                error(keyword, "Expected '<' in global variable list.");
            }
            pos++; // '<'

            size_t start = pos;
            while (pos < content.length() && content[pos] != '>') {
                pos++;
            }

            if (pos >= content.length()) {
                error(keyword, "Unclosed '<' in global variable list.");
            }

            std::string var_name = content.substr(start, pos - start);

            if (var_name.empty()) {
                error(keyword, "Empty variable name in global declaration.");
            }

            if (!std::isalpha(var_name[0]) && var_name[0] != '_') {
                error(keyword, std::format("Invalid identifier '{}': must "
                                           "start with letter or underscore.",
                                           var_name));
            }

            for (size_t i = 1; i < var_name.length(); i++) {
                if (!std::isalnum(var_name[i]) && var_name[i] != '_') {
                    error(keyword, std::format("Invalid identifier '{}': "
                                               "contains invalid character.",
                                               var_name));
                }
            }

            globals.push_back({TokenType::Identifier, var_name, keyword.line});
            pos++; // '>'
        }

        if (globals.empty()) {
            error(keyword,
                  "Global declaration must specify at least one variable.");
        }

        return std::make_unique<GlobalDecl>(
            GlobalDecl(keyword, GlobalMode::SPECIFIC, globals));
    }
}

std::unique_ptr<Expr> Parser::parseTernary() {
    auto expr = parseLogicalOr();
    if (match({TokenType::Question})) {
        auto thenBranch = parseTernary();
        consume(TokenType::Colon, "Expect ':' for ternary operator.");
        auto elseBranch = parseTernary();
        expr = make_node<Expr, TernaryExpr>(
            std::move(expr), std::move(thenBranch), std::move(elseBranch));
    }
    return expr;
}

template <typename NextLevel, typename... TokenTypes>
std::unique_ptr<Expr> Parser::parseBinary(NextLevel next_level,
                                          TokenTypes... token_types) {
    auto expr = (this->*next_level)();
    while (match({token_types...})) {
        Token op = previous();
        auto right = (this->*next_level)();
        expr =
            make_node<Expr, BinaryExpr>(std::move(expr), op, std::move(right));
    }
    return expr;
}

std::unique_ptr<Expr> Parser::parseLogicalOr() {
    return parseBinary(&Parser::parseLogicalAnd, TokenType::LogicalOr);
}

std::unique_ptr<Expr> Parser::parseLogicalAnd() {
    return parseBinary(&Parser::parseBitwiseOr, TokenType::LogicalAnd);
}

std::unique_ptr<Expr> Parser::parseBitwiseOr() {
    return parseBinary(&Parser::parseBitwiseXor, TokenType::BitOr);
}

std::unique_ptr<Expr> Parser::parseBitwiseXor() {
    return parseBinary(&Parser::parseBitwiseAnd, TokenType::BitXor);
}

std::unique_ptr<Expr> Parser::parseBitwiseAnd() {
    return parseBinary(&Parser::parseEquality, TokenType::BitAnd);
}

std::unique_ptr<Expr> Parser::parseEquality() {
    return parseBinary(&Parser::parseComparison, TokenType::Eq, TokenType::Ne);
}

std::unique_ptr<Expr> Parser::parseComparison() {
    return parseBinary(&Parser::parseTerm, TokenType::Gt, TokenType::Ge,
                       TokenType::Lt, TokenType::Le);
}

std::unique_ptr<Expr> Parser::parseTerm() {
    return parseBinary(&Parser::parseFactor, TokenType::Plus, TokenType::Minus);
}

std::unique_ptr<Expr> Parser::parseFactor() {
    return parseBinary(&Parser::parseExponent, TokenType::Star,
                       TokenType::Slash, TokenType::Percent);
}

std::unique_ptr<Expr> Parser::parseExponent() {
    return parseBinary(&Parser::parseUnary, TokenType::StarStar);
}

std::unique_ptr<Expr> Parser::parseUnary() {
    if (match({TokenType::Not, TokenType::Minus, TokenType::BitNot})) {
        Token op = previous();
        auto right = parseUnary();
        return make_node<Expr, UnaryExpr>(op, std::move(right));
    }
    return parsePostfix();
}

std::unique_ptr<Expr> Parser::parsePostfix() {
    auto expr = parsePrimary();
    while (true) {
        if (match({TokenType::LParen})) {
            expr = finishCall(std::move(expr));
        } else if (match({TokenType::LBracket})) {
            auto index1 = parseTernary();
            consume(TokenType::Comma, "Expect ',' in index operator");
            auto index2 = parseTernary();
            consume(TokenType::RBracket, "Expect ']' after indices");
            std::string suffix;
            if (match({TokenType::Colon})) {
                Token s =
                    consume(TokenType::Identifier, "Expect boundary suffix");
                suffix = std::format(":{}", s.value);
            }
            if (auto* var = get_if<VariableExpr>(expr.get())) {
                auto get_constant_token = [](Expr* e) -> Token* {
                    if (auto* num = get_if<NumberExpr>(e)) {
                        return &num->value;
                    }
                    if (auto* unary = get_if<UnaryExpr>(e)) {
                        if (unary->op.type == TokenType::Minus) {
                            if (auto* num =
                                    get_if<NumberExpr>(unary->right.get())) {
                                static Token neg_token;
                                neg_token = num->value;
                                neg_token.value =
                                    std::format("-{}", neg_token.value);
                                return &neg_token;
                            }
                        }
                    }
                    return nullptr;
                };

                Token* x_tok = get_constant_token(index1.get());
                Token* y_tok = get_constant_token(index2.get());

                if (x_tok && y_tok) {
                    return make_node<Expr, StaticRelPixelAccessExpr>(
                        var->name, *x_tok, *y_tok, suffix);
                }
            }
            error(peek(), "Dynamic pixel access should use dyn().");
        } else if (match({TokenType::Dot})) {
            Token prop = consume(TokenType::Identifier,
                                 "Expect property name after '.'");
            if (auto* var = get_if<VariableExpr>(expr.get())) {
                // Check if this is frame.width[N] or frame.height[N]
                if (var->name.value == "frame" &&
                    (prop.value == "width" || prop.value == "height")) {
                    if (match({TokenType::LBracket})) {
                        Token plane_index = consume(
                            TokenType::Number,
                            std::format(
                                "Expect plane index (integer) in frame.{}[N]",
                                prop.value));
                        consume(TokenType::RBracket,
                                "Expect ']' after plane index");
                        return make_node<Expr, FrameDimensionExpr>(prop,
                                                                   plane_index);
                    }
                }
                return make_node<Expr, PropAccessExpr>(var->name, prop);
            }
            error(prop, "Invalid property access target.");
        } else {
            break;
        }
    }
    return expr;
}

std::unique_ptr<Expr> Parser::finishCall(std::unique_ptr<Expr> callee) {
    if (auto* var = get_if<VariableExpr>(callee.get())) {
        std::vector<std::unique_ptr<Expr>> args;
        if (peek().type != TokenType::RParen) {
            do {
                args.push_back(parseTernary());
            } while (match({TokenType::Comma}));
        }
        consume(TokenType::RParen, "Expect ')' after arguments.");
        std::string suffix;
        if (match({TokenType::Colon})) {
            Token s = consume(TokenType::Identifier, "Expect boundary suffix");
            suffix = std::format(":{}", s.value);
        }
        return make_node<Expr, CallExpr>(var->name, std::move(args), suffix);
    }
    error(peek(), "Invalid call target.");
    std::unreachable();
}

std::unique_ptr<Expr> Parser::parsePrimary() {
    if (match({TokenType::Number}))
        return make_node<Expr, NumberExpr>(previous());
    if (match({TokenType::Identifier}))
        return make_node<Expr, VariableExpr>(previous());
    if (match({TokenType::LParen})) {
        auto expr = parseTernary();
        consume(TokenType::RParen, "Expect ')' after expression.");
        return expr;
    }
    error(peek(), "Expect expression.");
    std::unreachable();
}

bool Parser::match(const std::vector<TokenType>& types) {
    for (TokenType type : types) {
        if (peek().type == type) {
            advance();
            return true;
        }
    }
    return false;
}

Token Parser::consume(TokenType type, const std::string& message) {
    if (peek().type == type)
        return advance();
    error(peek(), message);
    std::unreachable();
}

Token Parser::advance() {
    if (!isAtEnd())
        current++;
    return previous();
}

Token Parser::peek() const { return tokens[current]; }
Token Parser::peek(int offset) const {
    if (current + offset >= static_cast<int>(tokens.size())) {
        return tokens.back(); // Return EOF token
    }
    return tokens[current + offset];
}
Token Parser::previous() const { return tokens[current - 1]; }
bool Parser::isAtEnd() const { return peek().type == TokenType::EndOfFile; }

void Parser::error(const Token& token, const std::string& message) {
    if (token.type == TokenType::EndOfFile) {
        throw ParserError(std::format("at end: {}", message), token.line);
    } else {
        throw ParserError(std::format("at '{}': {}", token.value, message),
                          token.line);
    }
}

} // namespace infix2postfix
