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
    if (start == std::string::npos)
        return "";
    auto end = str.find_last_not_of(" \t\r\n");
    return str.substr(start, end - start + 1);
}

bool isIdentifierChar(char c) { return std::isalnum(c) || c == '_'; }

bool isValidIdentifierStart(char c) { return std::isalpha(c) || c == '_'; }

bool isValidIdentifier(const std::string& name) {
    return !name.empty() && isValidIdentifierStart(name[0]);
}

std::pair<std::string, size_t> extractIdentifier(const std::string& str,
                                                 size_t pos) {
    size_t start = pos;
    while (pos < str.length() && isIdentifierChar(str[pos])) {
        pos++;
    }
    return {str.substr(start, pos - start), pos};
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
                        Preprocessor* preprocessor_in)
        : expression(std::move(expression_in)), preprocessor(preprocessor_in) {}

    std::variant<int64_t, double> evaluate() {
        expression = preprocessor->expandMacrosImpl(expression, -1, nullptr);
        tokenize();
        pos = 0;
        if (tokens.size() == 1 && tokens[0].type == TokenType::Eof) {
            throw std::runtime_error("Cannot evaluate an empty expression");
        }
        Value result = parse_logical_or();
        if (peek().type != TokenType::Eof) {
            throw std::runtime_error("Unexpected tokens at end of expression");
        }
        return result.val;
    }

    std::optional<std::variant<int64_t, double>> try_evaluate_constant() {
        try {
            return evaluate();
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
    struct Value {
        std::variant<int64_t, double> val;

        explicit Value(std::variant<int64_t, double> v = (int64_t)0)
            : val(std::move(v)) {}
        Value(int64_t v) : val(v) {}
        Value(double v) : val(v) {}

        bool is_double() const { return std::holds_alternative<double>(val); }
        double to_double() const {
            if (is_double())
                return std::get<double>(val);
            return static_cast<double>(std::get<int64_t>(val));
        }
        bool is_truthy() const { return to_double() != 0.0; }
        std::string to_string() const {
            if (is_double())
                return std::to_string(std::get<double>(val));
            return std::to_string(std::get<int64_t>(val));
        }
    };

    enum class TokenType {
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
        Value value{};
    };

    void tokenize() {
        size_t i = 0;
        while (i < expression.length()) {
            if (std::isspace(expression[i])) {
                i++;
                continue;
            }

            if (std::isdigit(expression[i]) ||
                (expression[i] == '.' && i + 1 < expression.length() &&
                 std::isdigit(expression[i + 1]))) {
                size_t start = i;
                while (i < expression.length() &&
                       (std::isalnum(expression[i]) || expression[i] == '.' ||
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

            if (std::isalpha(expression[i]) || expression[i] == '_') {
                size_t start = i;
                while (i < expression.length() &&
                       (std::isalnum(expression[i]) || expression[i] == '_')) {
                    i++;
                }
                std::string text = expression.substr(start, i - start);
                throw std::runtime_error("Unknown identifier in expression: " +
                                         text);
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
        if (match(TokenType::LParen)) {
            Value val = parse_logical_or();
            if (!match(TokenType::RParen)) {
                throw std::runtime_error("Expected ')'");
            }
            return val;
        }
        throw std::runtime_error(
            "Unexpected token in expression, expected number or '('");
    }

    Value parse_unary() {
        if (match(TokenType::Minus)) {
            Value val = parse_unary();
            if (val.is_double())
                return Value(-val.to_double());
            return Value(-std::get<int64_t>(val.val));
        }
        if (match(TokenType::LogicalNot)) {
            return Value((int64_t)!parse_unary().is_truthy());
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
            return Value(std::pow(left.to_double(), right.to_double()));
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

    std::string expression;
    std::vector<Token> tokens;
    size_t pos = 0;
    Preprocessor* preprocessor;
};

std::string Preprocessor::evaluateIfPossible(const std::string& text) {
    ExpressionEvaluator evaluator(text, this);
    auto evaluated = evaluator.try_evaluate_constant();
    return evaluated ? ExpressionEvaluator::toString(*evaluated) : text;
}

void Preprocessor::LineParser::skipWhitespace() {
    while (!eof() && std::isspace(peek())) {
        consume();
    }
}

std::string_view Preprocessor::LineParser::extractIdentifier() {
    size_t start = pos;
    pos = ::infix2postfix::extractIdentifier(str, pos).second;
    return std::string_view(str).substr(start, pos - start);
}

Preprocessor::Preprocessor(const std::string& source) : source(source) {}

void Preprocessor::addPredefinedMacro(const std::string& name,
                                      const std::string& value) {
    MacroDefinition macro;
    macro.name = name;
    macro.body = value;
    macro.is_function_like = false;
    macros[name] = macro;
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
    bool changed;
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

            if (std::isalpha(c) || c == '_') {
                int token_column = column;
                size_t start_pos = parser.pos;

                std::string_view token_sv = parser.extractIdentifier();
                std::string token(token_sv);
                column += token.length();

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

                                // Recursively expand and evaluate if possible
                                std::string recursively_expanded =
                                    expandMacrosImpl(replacement, line_number,
                                                     nullptr);
                                replacement =
                                    evaluateIfPossible(recursively_expanded);

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

                    int preprocessed_start_col = result.length() + 1;
                    result += replacement;
                    int preprocessed_end_col = result.length();

                    if (changed && expansions_out &&
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
        if (expansions_out && !expansions_this_pass.empty()) {
            expansions_out->insert(expansions_out->end(),
                                   expansions_this_pass.begin(),
                                   expansions_this_pass.end());
        }

        if (++count > RECURSION_LIMIT) {
            addError("Macro expansion limit reached, possible recursive macro?",
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
    while (end < line.length() && std::isalpha(line[end])) {
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
    parser.pos = define_pos + 7; // Skip "@define"
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

            if (parser.peek() == ')')
                break;

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
    if (undef_pos == std::string::npos)
        return;
    parser.pos = undef_pos + 6;
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

    pos += check_defined ? 6 : 7; // Skip directive
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

    pos += 3; // Skip "@if"
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

    pos += 6; // Skip "@error"
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
        if (!isWordBoundary(processed, pos, 7)) {
            pos++;
            continue;
        }

        size_t start = processed.find_first_not_of(" \t", pos + 7);
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
                    if (!(macros[macro_name].parameters.empty() &&
                          arguments.size() == 1 && arguments[0].empty())) {
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

    if (!mapping || mapping->expansions.empty()) {
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
