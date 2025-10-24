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

#include "Preprocessor.hpp"
#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdint>
#include <format>
#include <ranges>
#include <sstream>
#include <stdexcept>
#include <string_view>
#include <utility>
#include <variant>

namespace infix2postfix {

namespace {
std::string trim(const std::string& str) {
    auto start = str.find_first_not_of(" \t\r\n");
    if (start == std::string::npos) {
        return "";
    }
    auto end = str.find_last_not_of(" \t\r\n");
    return str.substr(start, end - start + 1);
}

bool isIdentifierChar(char c) { return (std::isalnum(c) != 0) || c == '_'; }

bool isValidIdentifierStart(char c) {
    return (std::isalpha(c) != 0) || c == '_';
}

bool isValidIdentifier(const std::string& name) {
    return !name.empty() && isValidIdentifierStart(name[0]);
}

std::pair<std::string, size_t> extractIdentifier(std::string_view str,
                                                 size_t pos) {
    size_t start = pos;
    while (pos < str.length() && isIdentifierChar(str[pos])) {
        pos++;
    }
    return {std::string(str.substr(start, pos - start)), pos};
}

bool isWordBoundary(const std::string& str, size_t pos, size_t word_len) {
    bool is_start = (pos == 0) || !isIdentifierChar(str[pos - 1]);
    bool is_end = (pos + word_len >= str.length()) ||
                  !isIdentifierChar(str[pos + word_len]);
    return is_start && is_end;
}

} // namespace

class Preprocessor::ExpressionEvaluator {
  public:
    ExpressionEvaluator(std::string expression_in,
                        Preprocessor* preprocessor_in,
                        int recursion_depth_in = 0)
        : expression(std::move(expression_in)), preprocessor(preprocessor_in),
          recursion_depth(recursion_depth_in) {}

    std::variant<int64_t, double> evaluate() {
        expression = preprocessor->expandDefinedOperator(expression);
        tokenize();
        pos = 0;
        if (tokens.size() == 1 && tokens[0].type == TokenType::Eof) {
            throw std::runtime_error("Cannot evaluate an empty expression");
        }
        Value result = parse_conditional();
        if (peek().type != TokenType::Eof) {
            throw std::runtime_error("Unexpected tokens at end of expression");
        }
        return result.val;
    }

    std::optional<std::variant<int64_t, double>> try_evaluate_constant() {
        try {
            return evaluate();
        } catch (const PreprocessorError&) {
            throw;
        } catch (const std::runtime_error&) {
            return std::nullopt;
        }
    }

    static bool is_truthy(const std::variant<int64_t, double>& val) {
        return Value(val).is_truthy();
    }

    static std::string toString(const std::variant<int64_t, double>& val) {
        return Value(val).to_string();
    }

  private:
    static constexpr int MAX_EVAL_RECURSION = 1000;

    struct Value {
        std::variant<int64_t, double> val;

        explicit Value(std::variant<int64_t, double> v = (int64_t)0) : val(v) {}
        Value(int64_t v) : val(v) {}
        Value(double v) : val(v) {}

        [[nodiscard]] bool is_double() const {
            return std::holds_alternative<double>(val);
        }
        [[nodiscard]] double to_double() const {
            if (is_double()) {
                return std::get<double>(val);
            }
            return static_cast<double>(std::get<int64_t>(val));
        }
        [[nodiscard]] bool is_truthy() const { return to_double() != 0.0; }
        [[nodiscard]] std::string to_string() const {
            if (is_double()) {
                return std::to_string(std::get<double>(val));
            }
            return std::to_string(std::get<int64_t>(val));
        }
    };

    enum class TokenType : std::uint8_t {
        Number,
        Identifier,
        Plus,
        Minus,
        Multiply,
        Divide,
        Modulo,
        Power,
        LParen,
        RParen,
        Comma,
        Question,
        Colon,
        LogicalAnd,
        LogicalOr,
        LogicalNot,
        Equal,
        NotEqual,
        Greater,
        GreaterEqual,
        Less,
        LessEqual,
        Eof
    };

    struct Token {
        TokenType type;
        std::string text;
        Value value{}; // NOLINT(readability-redundant-member-init)
    };

    void tokenize() {
        size_t i = 0;
        while (i < expression.length()) {
            if (std::isspace(expression[i]) != 0) {
                i++;
                continue;
            }

            if ((std::isdigit(expression[i]) != 0) ||
                (expression[i] == '.' && i + 1 < expression.length() &&
                 (std::isdigit(expression[i + 1]) != 0))) {
                size_t start = i;
                while (i < expression.length() &&
                       ((std::isalnum(expression[i]) != 0) ||
                        expression[i] == '.' ||
                        ((expression[i] == '+' || expression[i] == '-') &&
                         (std::tolower(expression[i - 1]) == 'e' ||
                          std::tolower(expression[i - 1]) == 'p')))) {
                    i++;
                }
                std::string num_str = expression.substr(start, i - start);
                try {
                    if (num_str.contains('.') || num_str.contains('e') ||
                        num_str.contains('E')) {
                        tokens.push_back({TokenType::Number, num_str,
                                          Value(std::stod(num_str))});
                    } else {
                        tokens.push_back({TokenType::Number, num_str,
                                          Value(static_cast<int64_t>(std::stoll(
                                              num_str, nullptr, 0)))});
                    }
                } catch (const std::invalid_argument&) {
                    throw std::runtime_error("Invalid number format: " +
                                             num_str);
                }
                continue;
            }

            if ((std::isalpha(expression[i]) != 0) || expression[i] == '_') {
                size_t start = i;
                while (i < expression.length() &&
                       ((std::isalnum(expression[i]) != 0) ||
                        expression[i] == '_')) {
                    i++;
                }
                std::string text = expression.substr(start, i - start);
                tokens.push_back({TokenType::Identifier, text, Value{}});
                continue;
            }

            switch (expression[i]) {
            case '+':
                tokens.push_back({TokenType::Plus, "+"});
                break;
            case '-':
                tokens.push_back({TokenType::Minus, "-"});
                break;
            case '*':
                if (i + 1 < expression.length() && expression[i + 1] == '*') {
                    tokens.push_back({TokenType::Power, "**"});
                    i++;
                } else {
                    tokens.push_back({TokenType::Multiply, "*"});
                }
                break;
            case '/':
                tokens.push_back({TokenType::Divide, "/"});
                break;
            case '%':
                tokens.push_back({TokenType::Modulo, "%"});
                break;
            case '(':
                tokens.push_back({TokenType::LParen, "("});
                break;
            case ')':
                tokens.push_back({TokenType::RParen, ")"});
                break;
            case ',':
                tokens.push_back({TokenType::Comma, ","});
                break;
            case '?':
                tokens.push_back({TokenType::Question, "?"});
                break;
            case ':':
                tokens.push_back({TokenType::Colon, ":"});
                break;
            case '!':
                if (i + 1 < expression.length() && expression[i + 1] == '=') {
                    tokens.push_back({TokenType::NotEqual, "!="});
                    i++;
                } else {
                    tokens.push_back({TokenType::LogicalNot, "!"});
                }
                break;
            case '=':
                if (i + 1 < expression.length() && expression[i + 1] == '=') {
                    tokens.push_back({TokenType::Equal, "=="});
                    i++;
                } else {
                    throw std::runtime_error("Unexpected token '='");
                }
                break;
            case '&':
                if (i + 1 < expression.length() && expression[i + 1] == '&') {
                    tokens.push_back({TokenType::LogicalAnd, "&&"});
                    i++;
                } else {
                    throw std::runtime_error("Unexpected token '&'");
                }
                break;
            case '|':
                if (i + 1 < expression.length() && expression[i + 1] == '|') {
                    tokens.push_back({TokenType::LogicalOr, "||"});
                    i++;
                } else {
                    throw std::runtime_error("Unexpected token '|'");
                }
                break;
            case '>':
                if (i + 1 < expression.length() && expression[i + 1] == '=') {
                    tokens.push_back({TokenType::GreaterEqual, ">="});
                    i++;
                } else {
                    tokens.push_back({TokenType::Greater, ">"});
                }
                break;
            case '<':
                if (i + 1 < expression.length() && expression[i + 1] == '=') {
                    tokens.push_back({TokenType::LessEqual, "<="});
                    i++;
                } else {
                    tokens.push_back({TokenType::Less, "<"});
                }
                break;
            default:
                throw std::runtime_error(std::format(
                    "Unexpected character in expression: {}", expression[i]));
            }
            i++;
        }
        tokens.push_back({TokenType::Eof, ""});
    }

    Token& peek() { return tokens[pos]; }
    Token& consume() { return tokens[pos++]; }
    bool match(TokenType type) {
        if (peek().type == type) {
            consume();
            return true;
        }
        return false;
    }

    Value parse_primary() {
        if (match(TokenType::Number)) {
            return tokens[pos - 1].value;
        }
        if (match(TokenType::Identifier)) {
            std::string name = tokens[pos - 1].text;
            auto it = preprocessor->macros.find(name);
            if (it == preprocessor->macros.end()) {
                throw std::runtime_error("Unknown identifier in expression: " +
                                         name);
            }

            const MacroDefinition& macro = it->second;

            if (macro.is_function_like) {
                if (!match(TokenType::LParen)) {
                    throw std::runtime_error(
                        "Function-like macro used without arguments: " + name);
                }

                std::vector<Value> arg_values;
                if (!match(TokenType::RParen)) {
                    while (true) {
                        Value v = parse_conditional();
                        arg_values.push_back(v);
                        if (match(TokenType::RParen)) {
                            break;
                        }
                        if (!match(TokenType::Comma)) {
                            throw std::runtime_error(
                                "Expected ',' or ')' in macro call arguments");
                        }
                    }
                }

                if (arg_values.size() != macro.parameters.size()) {
                    throw std::runtime_error(std::format(
                        "Macro '{}' expects {} arguments, but {} were provided",
                        name, macro.parameters.size(), arg_values.size()));
                }

                std::string replacement = macro.body;
                for (size_t param_idx = 0; param_idx < macro.parameters.size();
                     ++param_idx) {
                    const std::string& param = macro.parameters[param_idx];
                    const std::string arg =
                        Value(arg_values[param_idx]).to_string();

                    size_t rp = 0;
                    while ((rp = replacement.find(param, rp)) !=
                           std::string::npos) {
                        if (isWordBoundary(replacement, rp, param.length())) {
                            replacement.replace(rp, param.length(), arg);
                            rp += arg.length();
                        } else {
                            rp++;
                        }
                    }
                }

                if (recursion_depth > MAX_EVAL_RECURSION) {
                    throw PreprocessorError("Expression evaluator recursion "
                                            "limit reached, possibly due to "
                                            "infinite recursion");
                }
                ExpressionEvaluator nested(replacement, preprocessor,
                                           recursion_depth + 1);
                auto maybe = nested.try_evaluate_constant();
                if (!maybe) {
                    throw std::runtime_error(
                        "Non-constant macro expansion for '" + name + "'");
                }
                return Value(*maybe);
            }
            if (recursion_depth > MAX_EVAL_RECURSION) {
                throw PreprocessorError("Expression evaluator recursion "
                                        "limit reached, possibly due to "
                                        "infinite recursion");
            }
            ExpressionEvaluator nested(macro.body, preprocessor,
                                       recursion_depth + 1);
            auto maybe = nested.try_evaluate_constant();
            if (!maybe) {
                throw std::runtime_error("Non-constant object-like macro '" +
                                         name + "'");
            }
            return Value(*maybe);
        }
        if (match(TokenType::LParen)) {
            Value val = parse_conditional();
            if (!match(TokenType::RParen)) {
                throw std::runtime_error("Expected ')'");
            }
            return val;
        }
        throw std::runtime_error("Unexpected token in expression, expected "
                                 "number, identifier or '('");
    }

    Value parse_unary() {
        if (match(TokenType::Minus)) {
            Value val = parse_unary();
            if (val.is_double()) {
                return {-val.to_double()};
            }
            return {-std::get<int64_t>(val.val)};
        }
        if (match(TokenType::LogicalNot)) {
            return {(int64_t)!parse_unary().is_truthy()};
        }
        if (match(TokenType::Plus)) {
            return parse_unary();
        }
        return parse_primary();
    }

    Value parse_power() {
        Value left = parse_unary();
        // Power is right-associative
        if (match(TokenType::Power)) {
            Value right = parse_power();
            return {std::pow(left.to_double(), right.to_double())};
        }
        return left;
    }

    using SubParser = Value (ExpressionEvaluator::*)();

    Value parse_left_associative_binary_op(
        SubParser next_level, const std::initializer_list<TokenType>& ops) {
        Value left = (this->*next_level)();

        while (std::ranges::contains(ops, peek().type)) {
            Token op = consume();
            Value right = (this->*next_level)();

            if (left.is_double() || right.is_double()) {
                double l = left.to_double();
                double r = right.to_double();
                switch (op.type) {
                case TokenType::Multiply:
                    left = Value(l * r);
                    break;
                case TokenType::Divide:
                    left = Value(l / r);
                    break;
                case TokenType::Plus:
                    left = Value(l + r);
                    break;
                case TokenType::Minus:
                    left = Value(l - r);
                    break;
                case TokenType::Greater:
                    left = Value((int64_t)(l > r));
                    break;
                case TokenType::GreaterEqual:
                    left = Value((int64_t)(l >= r));
                    break;
                case TokenType::Less:
                    left = Value((int64_t)(l < r));
                    break;
                case TokenType::LessEqual:
                    left = Value((int64_t)(l <= r));
                    break;
                case TokenType::Equal:
                    left = Value((int64_t)(l == r));
                    break;
                case TokenType::NotEqual:
                    left = Value((int64_t)(l != r));
                    break;
                case TokenType::Modulo:
                    throw std::runtime_error(
                        "Modulo requires integer operands");
                default:
                    std::unreachable();
                }
            } else {
                int64_t l = std::get<int64_t>(left.val);
                int64_t r = std::get<int64_t>(right.val);
                switch (op.type) {
                case TokenType::Multiply:
                    left = Value(l * r);
                    break;
                case TokenType::Divide:
                    left = Value(l / r);
                    break;
                case TokenType::Modulo:
                    left = Value(l % r);
                    break;
                case TokenType::Plus:
                    left = Value(l + r);
                    break;
                case TokenType::Minus:
                    left = Value(l - r);
                    break;
                case TokenType::Greater:
                    left = Value((int64_t)(l > r));
                    break;
                case TokenType::GreaterEqual:
                    left = Value((int64_t)(l >= r));
                    break;
                case TokenType::Less:
                    left = Value((int64_t)(l < r));
                    break;
                case TokenType::LessEqual:
                    left = Value((int64_t)(l <= r));
                    break;
                case TokenType::Equal:
                    left = Value((int64_t)(l == r));
                    break;
                case TokenType::NotEqual:
                    left = Value((int64_t)(l != r));
                    break;
                default:
                    std::unreachable();
                }
            }
        }
        return left;
    }

    Value parse_factor() {
        return parse_left_associative_binary_op(
            &ExpressionEvaluator::parse_power,
            {TokenType::Multiply, TokenType::Divide, TokenType::Modulo});
    }

    Value parse_term() {
        return parse_left_associative_binary_op(
            &ExpressionEvaluator::parse_factor,
            {TokenType::Plus, TokenType::Minus});
    }

    Value parse_comparison() {
        return parse_left_associative_binary_op(
            &ExpressionEvaluator::parse_term,
            {TokenType::Greater, TokenType::GreaterEqual, TokenType::Less,
             TokenType::LessEqual});
    }

    Value parse_equality() {
        return parse_left_associative_binary_op(
            &ExpressionEvaluator::parse_comparison,
            {TokenType::Equal, TokenType::NotEqual});
    }

    Value parse_logical_and() {
        Value left = parse_equality();
        while (match(TokenType::LogicalAnd)) {
            Value right = parse_equality();
            left = Value((int64_t)(left.is_truthy() && right.is_truthy()));
        }
        return left;
    }

    Value parse_logical_or() {
        Value left = parse_logical_and();
        while (match(TokenType::LogicalOr)) {
            Value right = parse_logical_and();
            left = Value((int64_t)(left.is_truthy() || right.is_truthy()));
        }
        return left;
    }

    // Parse conditional operator with short-circuit semantics
    Value parse_conditional() {
        Value condition = parse_logical_or();
        if (!match(TokenType::Question)) {
            return condition;
        }

        bool cond_truthy = condition.is_truthy();

        if (cond_truthy) {
            Value then_val = parse_conditional();
            if (!match(TokenType::Colon)) {
                throw std::runtime_error(
                    "Expected ':' in conditional expression");
            }
            skip_else_branch();
            return then_val;
        }
        skip_then_branch_to_colon();
        if (!match(TokenType::Colon)) {
            throw std::runtime_error("Expected ':' in conditional expression");
        }
        Value else_val = parse_conditional();
        return else_val;
    }

    void skip_then_branch_to_colon() {
        int nested = 0;
        while (true) {
            TokenType t = peek().type;
            if (t == TokenType::Eof) {
                break;
            }
            if (t == TokenType::Question) {
                consume();
                nested++;
                continue;
            }
            if (t == TokenType::Colon) {
                if (nested == 0) {
                    break; // do not consume ':' here
                }
                nested--;
                consume();
                continue;
            }
            consume();
        }
    }

    void skip_else_branch() {
        int nested = 0;
        int paren_depth = 0;
        while (true) {
            TokenType t = peek().type;
            if (t == TokenType::Eof) {
                break;
            }
            if (t == TokenType::LParen) {
                paren_depth++;
                consume();
                continue;
            }
            if (t == TokenType::RParen) {
                if (paren_depth == 0) {
                    break; // let caller handle ')'
                }
                paren_depth--;
                consume();
                continue;
            }
            if (t == TokenType::Comma && paren_depth == 0) {
                break; // end of this branch in argument list
            }
            if (t == TokenType::Question) {
                nested++;
                consume();
                continue;
            }
            if (t == TokenType::Colon) {
                if (nested == 0) {
                    break; // end of current else-branch at outer ':'
                }
                nested--;
                consume();
                continue;
            }
            consume();
        }
    }

    std::string expression;
    std::vector<Token> tokens;
    size_t pos = 0;
    Preprocessor* preprocessor;
    int recursion_depth;
};

std::string Preprocessor::evaluateIfPossible(const std::string& text) {
    ExpressionEvaluator evaluator(text, this);
    auto evaluated = evaluator.try_evaluate_constant();
    return evaluated ? ExpressionEvaluator::toString(*evaluated) : text;
}

void Preprocessor::LineParser::skipWhitespace() {
    while (!eof() && (std::isspace(peek()) != 0)) {
        consume();
    }
}

std::string_view Preprocessor::LineParser::extractIdentifier() {
    size_t start = pos;
    pos = ::infix2postfix::extractIdentifier(str, pos).second;
    return std::string_view(str).substr(start, pos - start);
}

Preprocessor::Preprocessor(std::string source) : source(std::move(source)) {}

void Preprocessor::addPredefinedMacro(std::string name, std::string value) {
    MacroDefinition macro;
    macro.name = std::move(name);
    macro.body = std::move(value);
    macro.is_function_like = false;
    macros[macro.name] = macro;
}

PreprocessResult Preprocessor::process() {
    output_lines.clear();
    line_mappings.clear();
    errors.clear();
    conditional_stack.clear();
    current_output_line = 1;

    std::istringstream stream(source);
    std::string line;
    int line_number = 1;

    while (std::getline(stream, line)) {
        processLine(line, line_number);
        line_number++;
    }

    if (!conditional_stack.empty()) {
        addError(
            std::format("Unclosed @ifdef/@ifndef directive started at line {}",
                        conditional_stack.back().start_line),
            line_number - 1);
    }

    PreprocessResult result;
    result.success = errors.empty();
    result.errors = errors;
    result.line_map = line_mappings;

    std::ostringstream oss;
    for (size_t i = 0; i < output_lines.size(); ++i) {
        oss << output_lines[i];
        if (i < output_lines.size() - 1) {
            oss << '\n';
        }
    }
    result.source = oss.str();

    return result;
}

void Preprocessor::processLine(const std::string& line, int line_number) {
    if (isDirective(line)) {
        handleDirective(line, line_number);
        addOutputLine("", line_number);
    } else if (!isCurrentBlockActive()) {
        addOutputLine("", line_number);
    } else {
        addOutputLine("", line_number);
        std::string expanded = expandMacros(line, line_number);
        output_lines.back() = expanded;
    }
}

std::string
Preprocessor::expandMacrosImpl(const std::string& line, int line_number,
                               std::vector<MacroExpansion>* expansions_out) {
    std::string current_line = line;
    std::string result;
    bool changed = false;
    constexpr int RECURSION_LIMIT = 1000;
    int count = 0;

    do {
        changed = false;
        result.clear();
        std::vector<MacroExpansion> expansions_this_pass;
        LineParser parser(current_line);
        int column = 1;

        while (!parser.eof()) {
            char c = parser.peek();

            if ((std::isalpha(c) != 0) || c == '_') {
                int token_column = column;
                size_t start_pos = parser.pos;

                std::string_view token_sv = parser.extractIdentifier();
                std::string token(token_sv);
                column += static_cast<int>(token.length());

                auto it = macros.find(token);
                if (it != macros.end()) {
                    const MacroDefinition& macro = it->second;
                    std::string replacement;
                    size_t expansion_end_pos = parser.pos;

                    if (macro.is_function_like) {
                        LineParser arg_parser_state = parser;
                        arg_parser_state.skipWhitespace();

                        if (!arg_parser_state.eof() &&
                            arg_parser_state.peek() == '(') {
                            parser.pos = arg_parser_state.pos;
                            auto arguments =
                                parseMacroArguments(parser, token, line_number);

                            if (arguments) {
                                // Try to evaluate each argument as a constant expression
                                std::vector<std::string> evaluated_arguments;
                                for (const auto& arg : *arguments) {
                                    evaluated_arguments.push_back(
                                        evaluateIfPossible(arg));
                                }

                                replacement = macro.body;
                                for (size_t param_idx = 0;
                                     param_idx < macro.parameters.size();
                                     param_idx++) {
                                    const std::string& param =
                                        macro.parameters[param_idx];
                                    const std::string& arg =
                                        evaluated_arguments[param_idx];

                                    size_t pos = 0;
                                    while (
                                        (pos = replacement.find(param, pos)) !=
                                        std::string::npos) {
                                        if (isWordBoundary(replacement, pos,
                                                           param.length())) {
                                            replacement.replace(
                                                pos, param.length(), arg);
                                            pos += arg.length();
                                        } else {
                                            pos++;
                                        }
                                    }
                                }

                                try {
                                    replacement =
                                        evaluateIfPossible(replacement);
                                } catch (const PreprocessorError& e) {
                                    addError(e.what(), line_number);
                                    replacement = token;
                                    changed = false;
                                }

                                expansion_end_pos = parser.pos;
                                changed = true;
                            } else { // Argument parsing failed
                                replacement = token;
                            }
                        } else {
                            // Function-like macro not followed by '('
                            replacement = token;
                        }
                    } else {
                        // Object-like macro
                        replacement = macro.body;
                        expansion_end_pos = parser.pos;
                        changed = true;
                    }

                    int preprocessed_start_col =
                        static_cast<int>(result.length()) + 1;
                    result += replacement;
                    int preprocessed_end_col =
                        static_cast<int>(result.length());

                    if (changed && (expansions_out != nullptr) &&
                        (expansion_end_pos > start_pos ||
                         !macro.is_function_like)) {
                        MacroExpansion expansion;
                        expansion.macro_name = token;
                        expansion.original_line = line_number;
                        expansion.original_column = token_column;
                        expansion.replacement_text = replacement;
                        expansion.preprocessed_start_column =
                            preprocessed_start_col;
                        expansion.preprocessed_end_column =
                            preprocessed_end_col;
                        expansions_this_pass.push_back(expansion);
                    }
                } else {
                    result += token;
                }
            } else {
                result += parser.consume_one();
                column++;
            }
        }

        current_line = result;
        if ((expansions_out != nullptr) && !expansions_this_pass.empty()) {
            expansions_out->insert(expansions_out->end(),
                                   expansions_this_pass.begin(),
                                   expansions_this_pass.end());
        }

        if (++count > RECURSION_LIMIT) {
            addError("Macro expansion limit reached, possibly due to infinite "
                     "recursion",
                     line_number);
            break;
        }
    } while (changed);

    return current_line;
}

bool Preprocessor::isDirective(const std::string& line) const {
    size_t pos = line.find_first_not_of(" \t");
    if (pos == std::string::npos) {
        return false;
    }
    return line[pos] == '@';
}

void Preprocessor::handleDirective(const std::string& line, int line_number) {
    size_t start = line.find_first_not_of(" \t");
    if (start == std::string::npos || line[start] != '@') {
        return;
    }

    start++; // Skip '@'
    size_t end = start;
    while (end < line.length() && (std::isalpha(line[end]) != 0)) {
        end++;
    }

    std::string directive = line.substr(start, end - start);

    if (directive == "define") {
        if (isCurrentBlockActive()) {
            handleDefine(line, line_number);
        }
    } else if (directive == "undef") {
        if (isCurrentBlockActive()) {
            handleUndef(line, line_number);
        }
    } else if (directive == "ifdef") {
        handleIfdef(line, line_number);
    } else if (directive == "ifndef") {
        handleIfndef(line, line_number);
    } else if (directive == "if") {
        handleIf(line, line_number);
    } else if (directive == "else") {
        handleElse(line, line_number);
    } else if (directive == "endif") {
        handleEndif(line, line_number);
    } else if (directive == "error") {
        if (isCurrentBlockActive()) {
            handleError(line, line_number);
        }
    } else {
        if (isCurrentBlockActive()) {
            addError(std::format("Unknown directive '@{}'", directive),
                     line_number);
        }
    }
}

void Preprocessor::handleDefine(const std::string& line, int line_number) {
    // @define NAME [value...] or @define NAME(param1, param2, ...) body
    LineParser parser(line);
    size_t define_pos = line.find("@define");
    if (define_pos == std::string::npos) {
        return;
    }
    parser.pos = define_pos + std::string("@define").length();
    parser.skipWhitespace();

    if (parser.eof()) {
        addError("@define requires a macro name", line_number);
        return;
    }

    auto name_sv = parser.extractIdentifier();
    std::string name(name_sv);

    if (!isValidIdentifier(name)) {
        addError(name.empty() ? "@define requires a macro name"
                              : std::format("Invalid macro name '{}': must "
                                            "start with letter or underscore",
                                            name),
                 line_number);
        return;
    }

    MacroDefinition macro;
    macro.name = name;
    macro.is_function_like = false;

    // Function-like macro: no space between name and '('
    if (!parser.eof() && parser.peek() == '(') {
        macro.is_function_like = true;
        parser.consume(); // Skip '('

        // Parse parameter list
        while (!parser.eof() && parser.peek() != ')') {
            parser.skipWhitespace();

            if (parser.eof()) {
                addError("Unterminated parameter list in macro definition",
                         line_number);
                return;
            }

            if (parser.peek() == ')') {
                break;
            }

            auto param_sv = parser.extractIdentifier();
            std::string param(param_sv);

            if (!isValidIdentifier(param)) {
                addError(param.empty()
                             ? "Expected parameter name in macro definition"
                             : std::format("Invalid parameter name '{}': must "
                                           "start with letter or underscore",
                                           param),
                         line_number);
                return;
            }

            if (std::ranges::contains(macro.parameters, param)) {
                addError(
                    std::format(
                        "Duplicate parameter name '{}' in macro definition",
                        param),
                    line_number);
                return;
            }

            macro.parameters.push_back(param);
            parser.skipWhitespace();

            if (parser.eof()) {
                addError("Unterminated parameter list in macro definition",
                         line_number);
                return;
            }

            if (parser.peek() == ',') {
                parser.consume(); // Skip comma
            } else if (parser.peek() != ')') {
                addError("Expected ',' or ')' in parameter list", line_number);
                return;
            }
        }

        if (parser.eof() || parser.peek() != ')') {
            addError("Unterminated parameter list in macro definition",
                     line_number);
            return;
        }

        parser.consume(); // Skip ')'
    }

    // Extract and trim body
    parser.skipWhitespace();
    std::string body = !parser.eof() ? trim(line.substr(parser.pos)) : "";

    if (macros.contains(name)) {
        // TODO: handle redefinition of macro
    }

    // Object-like macro: try to evaluate as constant expression
    if (!macro.is_function_like && !body.empty()) {
        body = evaluateIfPossible(body);
    }

    macro.body = body;
    macros[name] = macro;
}

void Preprocessor::handleUndef(const std::string& line, int line_number) {
    // @undef NAME
    LineParser parser(line);
    size_t undef_pos = line.find("@undef");
    if (undef_pos == std::string::npos) {
        return;
    }
    parser.pos = undef_pos + std::string("@undef").length();
    parser.skipWhitespace();

    if (parser.eof()) {
        addError("@undef requires a macro name", line_number);
        return;
    }

    auto name_sv = parser.extractIdentifier();
    std::string name(name_sv);

    if (!isValidIdentifier(name)) {
        addError(name.empty() ? "@undef requires a macro name"
                              : std::format("Invalid macro name '{}': must "
                                            "start with letter or underscore",
                                            name),
                 line_number);
        return;
    }

    macros.erase(name);
}

void Preprocessor::handleIfdef(const std::string& line, int line_number) {
    handleIfdefCommon(line, line_number, true);
}

void Preprocessor::handleIfndef(const std::string& line, int line_number) {
    handleIfdefCommon(line, line_number, false);
}

void Preprocessor::handleIfdefCommon(const std::string& line, int line_number,
                                     bool check_defined) {
    const char* directive = check_defined ? "@ifdef" : "@ifndef";
    size_t pos = line.find(directive);
    if (pos == std::string::npos) {
        return;
    }

    pos += check_defined ? std::string("@ifdef").length()
                         : std::string("@ifndef").length();
    pos = line.find_first_not_of(" \t", pos);

    if (pos == std::string::npos) {
        addError(std::format("{} requires a macro name", directive),
                 line_number);
        conditional_stack.push_back({line_number, false, true});
        return;
    }

    auto [name, end_pos] = extractIdentifier(line, pos);
    if (name.empty()) {
        addError(std::format("{} requires a macro name", directive),
                 line_number);
        conditional_stack.push_back({line_number, false, true});
        return;
    }

    bool parent_active = isCurrentBlockActive();
    bool macro_defined = macros.contains(name);
    bool condition_met = check_defined ? macro_defined : !macro_defined;
    bool is_active = parent_active && condition_met;

    conditional_stack.push_back({line_number, is_active, condition_met});
}

void Preprocessor::handleIf(const std::string& line, int line_number) {
    // @if expression
    size_t pos = line.find("@if");
    if (pos == std::string::npos) {
        return;
    }

    pos += std::string("@if").length();
    pos = line.find_first_not_of(" \t", pos);

    if (pos == std::string::npos) {
        addError("@if requires an expression", line_number);
        conditional_stack.push_back({line_number, false, false});
        return;
    }

    std::string expression = line.substr(pos);

    bool parent_active = isCurrentBlockActive();
    bool condition_met = false;

    if (parent_active) {
        try {
            // Expand defined() operator before evaluating
            std::string expanded_expr = expandDefinedOperator(expression);
            ExpressionEvaluator evaluator(expanded_expr, this);
            auto result = evaluator.evaluate();
            condition_met = ExpressionEvaluator::is_truthy(result);
        } catch (const PreprocessorError& e) {
            addError(e.what(), line_number);
        } catch (const std::runtime_error& e) {
            addError(
                std::format("Failed to evaluate @if expression: {}", e.what()),
                line_number);
        }
    }

    bool is_active = parent_active && condition_met;
    conditional_stack.push_back({line_number, is_active, condition_met});
}

void Preprocessor::handleElse([[maybe_unused]] const std::string& line,
                              int line_number) {
    if (conditional_stack.empty()) {
        addError("@else without matching @ifdef/@ifndef", line_number);
        return;
    }

    auto& block = conditional_stack.back();

    bool parent_active = true;
    if (conditional_stack.size() > 1) {
        parent_active =
            conditional_stack[conditional_stack.size() - 2].is_active;
    }

    block.is_active = parent_active && !block.had_true_branch;
}

void Preprocessor::handleEndif([[maybe_unused]] const std::string& line,
                               int line_number) {
    if (conditional_stack.empty()) {
        addError("@endif without matching @ifdef/@ifndef", line_number);
        return;
    }

    conditional_stack.pop_back();
}

void Preprocessor::handleError(const std::string& line, int line_number) {
    // @error [message]
    size_t pos = line.find("@error");
    if (pos == std::string::npos) {
        return;
    }

    pos += std::string("@error").length();
    pos = line.find_first_not_of(" \t", pos);
    std::string message =
        (pos != std::string::npos) ? trim(line.substr(pos)) : "";

    addError(message.empty() ? "@error directive encountered"
                             : std::format("@error: {}", message),
             line_number);
}

std::string Preprocessor::expandDefinedOperator(const std::string& text) {
    std::string processed = text;
    size_t pos = 0;
    while ((pos = processed.find("defined", pos)) != std::string::npos) {
        // Check if "defined" is a standalone word (not part of another identifier)
        if (!isWordBoundary(processed, pos, std::string("defined").length())) {
            pos++;
            continue;
        }

        size_t start = processed.find_first_not_of(
            " \t", pos + std::string("defined").length());
        bool has_paren = false;
        if (start != std::string::npos && processed[start] == '(') {
            has_paren = true;
            start = processed.find_first_not_of(" \t", start + 1);
        }

        if (start == std::string::npos) {
            pos++;
            continue;
        }

        auto [macro_name, end] = extractIdentifier(processed, start);

        if (!macro_name.empty()) {
            size_t final_end = end;
            if (has_paren) {
                size_t paren_end =
                    processed.find_first_not_of(" \t", final_end);
                if (paren_end != std::string::npos &&
                    processed[paren_end] == ')') {
                    final_end = paren_end + 1;
                }
            }
            processed.replace(pos, final_end - pos,
                              macros.contains(macro_name) ? "1" : "0");
        }
        pos++;
    }
    return processed;
}

std::string Preprocessor::expandMacros(const std::string& line,
                                       int line_number) {
    // First expand defined() operator
    std::string processed_line = expandDefinedOperator(line);

    if (macros.empty()) {
        return processed_line;
    }

    std::vector<MacroExpansion> expansions;
    std::string result =
        expandMacrosImpl(processed_line, line_number, &expansions);

    if (!expansions.empty() && !line_mappings.empty()) {
        line_mappings.back().expansions.insert(
            line_mappings.back().expansions.end(), expansions.begin(),
            expansions.end());
    }

    return result;
}

bool Preprocessor::isCurrentBlockActive() const {
    return std::ranges::all_of(
        conditional_stack, [](const auto& block) { return block.is_active; });
}

void Preprocessor::addError(const std::string& message, int line) {
    errors.push_back(std::format("Line {}: {}", line, message));
}

void Preprocessor::addOutputLine(
    const std::string& line, int original_line,
    const std::vector<MacroExpansion>& expansions) {
    output_lines.push_back(line);

    LineMapping mapping;
    mapping.preprocessed_line = current_output_line;
    mapping.original_line = original_line;
    mapping.expansions = expansions;

    line_mappings.push_back(mapping);
    current_output_line++;
}

std::optional<std::vector<std::string>> Preprocessor::parseMacroArguments(
    LineParser& parser, const std::string& macro_name, int line_number) {
    parser.consume(); // Skip '('
    std::vector<std::string> arguments;
    std::string current_arg;
    int paren_depth = 0;

    while (!parser.eof()) {
        char ch = parser.peek();

        if (ch == '(') {
            paren_depth++;
            current_arg += ch;
            parser.consume();
        } else if (ch == ')') {
            if (paren_depth == 0) {
                // End of argument list
                arguments.push_back(trim(current_arg));
                parser.consume(); // Skip ')'
                if (arguments.size() != macros[macro_name].parameters.size()) {
                    if (!macros[macro_name].parameters.empty() ||
                        arguments.size() != 1 || !arguments[0].empty()) {
                        addError(
                            std::format("Macro '{}' expects {} "
                                        "arguments, but {} were provided",
                                        macro_name,
                                        macros[macro_name].parameters.size(),
                                        arguments.size()),
                            line_number);
                        return std::nullopt;
                    }
                    // Macro with no params called with empty args
                    arguments.clear();
                }
                return arguments;
            }
            paren_depth--;
            current_arg += ch;
            parser.consume();
        } else if (ch == ',' && paren_depth == 0) {
            arguments.push_back(trim(current_arg));
            current_arg.clear();
            parser.consume();
        } else {
            current_arg += ch;
            parser.consume();
        }
    }

    addError(
        std::format("Unterminated argument list for macro '{}'", macro_name),
        line_number);
    return std::nullopt;
}

std::string Preprocessor::formatDiagnosticWithExpansion(
    const std::string& message, int line,
    const std::vector<LineMapping>& line_map) {

    const LineMapping* mapping = nullptr;
    for (const auto& m : line_map) {
        if (m.preprocessed_line == line) {
            mapping = &m;
            break;
        }
    }

    if ((mapping == nullptr) || mapping->expansions.empty()) {
        return message;
    }

    std::string result = message;
    result += "\n  Macro expansion trace:";

    for (const auto& expansion : mapping->expansions) {
        result += std::format("\n    {}:{}: in expansion of macro '{}'",
                              expansion.original_line,
                              expansion.original_column, expansion.macro_name);
        if (!expansion.replacement_text.empty()) {
            result += std::format(" -> '{}'", expansion.replacement_text);
        }
    }

    return result;
}

std::string
Preprocessor::formatMacroExpansions(const std::vector<LineMapping>& line_map) {
    std::string result;

    for (const auto& mapping : line_map) {
        if (!mapping.expansions.empty()) {
            for (const auto& expansion : mapping.expansions) {
                result += std::format(
                    "Line {} (original line {}:{}): macro '{}' expanded to "
                    "'{}'\n",
                    mapping.preprocessed_line, expansion.original_line,
                    expansion.original_column, expansion.macro_name,
                    expansion.replacement_text.empty()
                        ? "(empty)"
                        : expansion.replacement_text);
            }
        }
    }

    return result;
}

} // namespace infix2postfix
