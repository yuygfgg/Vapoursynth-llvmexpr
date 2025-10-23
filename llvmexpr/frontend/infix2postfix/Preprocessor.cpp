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
#include <cctype>
#include <cmath>
#include <cstdint>
#include <format>
#include <sstream>
#include <stdexcept>
#include <variant>

namespace infix2postfix {

class Preprocessor::ExpressionEvaluator {
  public:
    using Value = std::variant<int64_t, double>;

    ExpressionEvaluator(std::string expression_in,
                        const std::map<std::string, MacroDefinition>& macros_in,
                        Preprocessor* preprocessor_in = nullptr)
        : expression(std::move(expression_in)), macros(macros_in),
          preprocessor(preprocessor_in) {}

    Value evaluate() {
        full_macro_expansion();
        tokenize();
        pos_ = 0;
        if (tokens_.size() == 1 && tokens_[0].type == TokenType::Eof) {
            throw std::runtime_error("Cannot evaluate an empty expression");
        }
        Value result = parse_logical_or();
        if (peek().type != TokenType::Eof) {
            throw std::runtime_error("Unexpected tokens at end of expression");
        }
        return result;
    }

    std::optional<Value> try_evaluate_constant() {
        try {
            return evaluate();
        } catch (const std::runtime_error&) {
            return std::nullopt;
        }
    }

    static bool is_truthy(const Value& val) {
        if (std::holds_alternative<int64_t>(val)) {
            return std::get<int64_t>(val) != 0;
        }
        if (std::holds_alternative<double>(val)) {
            return std::get<double>(val) != 0.0;
        }
        return false;
    }

  private:
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

    void full_macro_expansion() {
        if (!preprocessor) {
            // Fallback for old behavior if preprocessor is not provided
            // This can be removed if all call sites are updated
            std::string current_expr = expression;
            std::string result;
            bool changed;
            do {
                changed = false;
                result.clear();
                size_t i = 0;
                while (i < current_expr.length()) {
                    if (std::isalpha(current_expr[i]) ||
                        current_expr[i] == '_') {
                        size_t start = i;
                        while (i < current_expr.length() &&
                               (std::isalnum(current_expr[i]) ||
                                current_expr[i] == '_')) {
                            i++;
                        }
                        std::string identifier =
                            current_expr.substr(start, i - start);
                        auto it = macros.find(identifier);
                        if (it != macros.end()) {
                            result += it->second.body;
                            changed = true;
                        } else {
                            result += identifier;
                        }
                    } else {
                        result += current_expr[i];
                        i++;
                    }
                }
                current_expr = result;
            } while (changed);
            expression = result;
            return;
        }
        expression = preprocessor->expandMacrosImpl(expression, -1, nullptr);
    }

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
                    if (num_str.find('.') != std::string::npos ||
                        num_str.find('e') != std::string::npos ||
                        num_str.find('E') != std::string::npos) {
                        tokens_.push_back(
                            {TokenType::Number, num_str, std::stod(num_str)});
                    } else {
                        tokens_.push_back({TokenType::Number, num_str,
                                           std::stoll(num_str, nullptr, 0)});
                    }
                } catch (const std::invalid_argument&) {
                    throw std::runtime_error("Invalid number format: " +
                                             num_str);
                }
                continue;
            }

            if (std::isalpha(expression[i]) || expression[i] == '_') {
                size_t start = i;
                while (
                    i < expression.length() &&
                    (std::isalnum(expression[i]) || expression[i] == '_')) {
                    i++;
                }
                std::string text = expression.substr(start, i - start);
                throw std::runtime_error("Unknown identifier in expression: " +
                                         text);
            }

            switch (expression[i]) {
            case '+':
                tokens_.push_back({TokenType::Plus, "+"});
                break;
            case '-':
                tokens_.push_back({TokenType::Minus, "-"});
                break;
            case '*':
                if (i + 1 < expression.length() && expression[i + 1] == '*') {
                    tokens_.push_back({TokenType::Power, "**"});
                    i++;
                } else {
                    tokens_.push_back({TokenType::Multiply, "*"});
                }
                break;
            case '/':
                tokens_.push_back({TokenType::Divide, "/"});
                break;
            case '%':
                tokens_.push_back({TokenType::Modulo, "%"});
                break;
            case '(':
                tokens_.push_back({TokenType::LParen, "("});
                break;
            case ')':
                tokens_.push_back({TokenType::RParen, ")"});
                break;
            case '!':
                if (i + 1 < expression.length() && expression[i + 1] == '=') {
                    tokens_.push_back({TokenType::NotEqual, "!="});
                    i++;
                } else {
                    tokens_.push_back({TokenType::LogicalNot, "!"});
                }
                break;
            case '=':
                if (i + 1 < expression.length() && expression[i + 1] == '=') {
                    tokens_.push_back({TokenType::Equal, "=="});
                    i++;
                } else {
                    throw std::runtime_error("Unexpected token '='");
                }
                break;
            case '&':
                if (i + 1 < expression.length() && expression[i + 1] == '&') {
                    tokens_.push_back({TokenType::LogicalAnd, "&&"});
                    i++;
                } else {
                    throw std::runtime_error("Unexpected token '&'");
                }
                break;
            case '|':
                if (i + 1 < expression.length() && expression[i + 1] == '|') {
                    tokens_.push_back({TokenType::LogicalOr, "||"});
                    i++;
                } else {
                    throw std::runtime_error("Unexpected token '|'");
                }
                break;
            case '>':
                if (i + 1 < expression.length() && expression[i + 1] == '=') {
                    tokens_.push_back({TokenType::GreaterEqual, ">="});
                    i++;
                } else {
                    tokens_.push_back({TokenType::Greater, ">"});
                }
                break;
            case '<':
                if (i + 1 < expression.length() && expression[i + 1] == '=') {
                    tokens_.push_back({TokenType::LessEqual, "<="});
                    i++;
                } else {
                    tokens_.push_back({TokenType::Less, "<"});
                }
                break;
            default:
                throw std::runtime_error(
                    std::string("Unexpected character in expression: ") +
                    expression[i]);
            }
            i++;
        }
        tokens_.push_back({TokenType::Eof, ""});
    }

    Token& peek() { return tokens_[pos_]; }
    Token& consume() { return tokens_[pos_++]; }
    bool match(TokenType type) {
        if (peek().type == type) {
            consume();
            return true;
        }
        return false;
    }

    Value parse_primary() {
        if (match(TokenType::Number)) {
            return tokens_[pos_ - 1].value;
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
            if (std::holds_alternative<int64_t>(val))
                return -std::get<int64_t>(val);
            return -std::get<double>(val);
        }
        if (match(TokenType::LogicalNot)) {
            return (int64_t)!is_truthy(parse_unary());
        }
        if (match(TokenType::Plus)) {
            return parse_unary();
        }
        return parse_primary();
    }

    Value parse_power() {
        Value left = parse_unary();
        // Power is right-associative, so we use recursion instead of iteration
        if (match(TokenType::Power)) {
            Value right = parse_power(); // Right-associative: recurse
            double l = std::holds_alternative<double>(left)
                           ? std::get<double>(left)
                           : static_cast<double>(std::get<int64_t>(left));
            double r = std::holds_alternative<double>(right)
                           ? std::get<double>(right)
                           : static_cast<double>(std::get<int64_t>(right));
            return std::pow(l, r);
        }
        return left;
    }

    Value parse_factor() {
        Value left = parse_power();
        while (peek().type == TokenType::Multiply ||
               peek().type == TokenType::Divide ||
               peek().type == TokenType::Modulo) {
            Token op = consume();
            Value right = parse_power();
            if (std::holds_alternative<double>(left) ||
                std::holds_alternative<double>(right)) {
                double l = std::holds_alternative<double>(left)
                               ? std::get<double>(left)
                               : static_cast<double>(std::get<int64_t>(left));
                double r = std::holds_alternative<double>(right)
                               ? std::get<double>(right)
                               : static_cast<double>(std::get<int64_t>(right));
                if (op.type == TokenType::Multiply)
                    left = l * r;
                else if (op.type == TokenType::Divide)
                    left = l / r;
                else
                    throw std::runtime_error(
                        "Modulo requires integer operands");
            } else {
                int64_t l = std::get<int64_t>(left);
                int64_t r = std::get<int64_t>(right);
                if (op.type == TokenType::Multiply)
                    left = l * r;
                else if (op.type == TokenType::Divide)
                    left = l / r;
                else if (op.type == TokenType::Modulo)
                    left = l % r;
            }
        }
        return left;
    }

    Value parse_term() {
        Value left = parse_factor();
        while (peek().type == TokenType::Plus ||
               peek().type == TokenType::Minus) {
            Token op = consume();
            Value right = parse_factor();
            if (std::holds_alternative<double>(left) ||
                std::holds_alternative<double>(right)) {
                double l = std::holds_alternative<double>(left)
                               ? std::get<double>(left)
                               : static_cast<double>(std::get<int64_t>(left));
                double r = std::holds_alternative<double>(right)
                               ? std::get<double>(right)
                               : static_cast<double>(std::get<int64_t>(right));
                if (op.type == TokenType::Plus)
                    left = l + r;
                else if (op.type == TokenType::Minus)
                    left = l - r;
            } else {
                int64_t l = std::get<int64_t>(left);
                int64_t r = std::get<int64_t>(right);
                if (op.type == TokenType::Plus)
                    left = l + r;
                else if (op.type == TokenType::Minus)
                    left = l - r;
            }
        }
        return left;
    }

    Value parse_comparison() {
        Value left = parse_term();
        while (peek().type == TokenType::Greater ||
               peek().type == TokenType::GreaterEqual ||
               peek().type == TokenType::Less ||
               peek().type == TokenType::LessEqual) {
            Token op = consume();
            Value right = parse_term();
            double l = std::holds_alternative<double>(left)
                           ? std::get<double>(left)
                           : static_cast<double>(std::get<int64_t>(left));
            double r = std::holds_alternative<double>(right)
                           ? std::get<double>(right)
                           : static_cast<double>(std::get<int64_t>(right));
            bool res = false;
            if (op.type == TokenType::Greater)
                res = l > r;
            else if (op.type == TokenType::GreaterEqual)
                res = l >= r;
            else if (op.type == TokenType::Less)
                res = l < r;
            else if (op.type == TokenType::LessEqual)
                res = l <= r;
            left = (int64_t)res;
        }
        return left;
    }

    Value parse_equality() {
        Value left = parse_comparison();
        while (peek().type == TokenType::Equal ||
               peek().type == TokenType::NotEqual) {
            Token op = consume();
            Value right = parse_comparison();
            double l = std::holds_alternative<double>(left)
                           ? std::get<double>(left)
                           : static_cast<double>(std::get<int64_t>(left));
            double r = std::holds_alternative<double>(right)
                           ? std::get<double>(right)
                           : static_cast<double>(std::get<int64_t>(right));
            bool res = false;
            if (op.type == TokenType::Equal)
                res = l == r;
            else if (op.type == TokenType::NotEqual)
                res = l != r;
            left = (int64_t)res;
        }
        return left;
    }

    Value parse_logical_and() {
        Value left = parse_equality();
        while (match(TokenType::LogicalAnd)) {
            Value right = parse_equality();
            left = (int64_t)(is_truthy(left) && is_truthy(right));
        }
        return left;
    }

    Value parse_logical_or() {
        Value left = parse_logical_and();
        while (match(TokenType::LogicalOr)) {
            Value right = parse_logical_and();
            left = (int64_t)(is_truthy(left) || is_truthy(right));
        }
        return left;
    }

    std::string expression;
    const std::map<std::string, MacroDefinition>& macros;
    std::vector<Token> tokens_;
    size_t pos_ = 0;
    Preprocessor* preprocessor;
};

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

    for (size_t i = 0; i < output_lines.size(); ++i) {
        result.source += output_lines[i];
        if (i < output_lines.size() - 1) {
            result.source += '\n';
        }
    }

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

        size_t i = 0;
        int column = 1;

        while (i < current_line.length()) {
            char c = current_line[i];

            if (std::isalpha(c) || c == '_') {
                size_t start = i;
                int token_column = column;

                while (i < current_line.length() &&
                       isIdentifierChar(current_line[i])) {
                    i++;
                    column++;
                }

                std::string token = current_line.substr(start, i - start);

                auto it = macros.find(token);
                if (it != macros.end()) {
                    const MacroDefinition& macro = it->second;
                    std::string replacement;
                    size_t expansion_end = i;

                    if (macro.is_function_like) {
                        size_t temp_i = i;
                        while (temp_i < current_line.length() &&
                               std::isspace(current_line[temp_i])) {
                            temp_i++;
                        }

                        if (temp_i < current_line.length() &&
                            current_line[temp_i] == '(') {
                            // Parse arguments
                            temp_i++; // Skip '('
                            std::vector<std::string> arguments;
                            std::string current_arg;
                            int paren_depth = 0;
                            bool error = false;

                            while (temp_i < current_line.length()) {
                                char ch = current_line[temp_i];

                                if (ch == '(') {
                                    paren_depth++;
                                    current_arg += ch;
                                    temp_i++;
                                } else if (ch == ')') {
                                    if (paren_depth == 0) {
                                        // End of argument list
                                        size_t arg_start =
                                            current_arg.find_first_not_of(
                                                " \t");
                                        size_t arg_end =
                                            current_arg.find_last_not_of(" \t");
                                        if (arg_start != std::string::npos &&
                                            arg_end != std::string::npos) {
                                            arguments.push_back(
                                                current_arg.substr(
                                                    arg_start,
                                                    arg_end - arg_start + 1));
                                        } else if (!current_arg.empty() ||
                                                   !arguments.empty()) {
                                            arguments.push_back("");
                                        }
                                        temp_i++; // Skip ')'
                                        break;
                                    } else {
                                        paren_depth--;
                                        current_arg += ch;
                                        temp_i++;
                                    }
                                } else if (ch == ',' && paren_depth == 0) {
                                    size_t arg_start =
                                        current_arg.find_first_not_of(" \t");
                                    size_t arg_end =
                                        current_arg.find_last_not_of(" \t");
                                    if (arg_start != std::string::npos &&
                                        arg_end != std::string::npos) {
                                        arguments.push_back(current_arg.substr(
                                            arg_start,
                                            arg_end - arg_start + 1));
                                    } else {
                                        arguments.push_back("");
                                    }
                                    current_arg.clear();
                                    temp_i++;
                                } else {
                                    current_arg += ch;
                                    temp_i++;
                                }
                            }

                            if (paren_depth != 0 ||
                                (temp_i > i &&
                                 current_line[temp_i - 1] != ')')) {
                                addError(std::format("Unterminated argument "
                                                     "list for macro '{}'",
                                                     token),
                                         line_number);
                                error = true;
                            }

                            if (!error) {
                                if (arguments.size() !=
                                    macro.parameters.size()) {
                                    addError(
                                        std::format(
                                            "Macro '{}' expects {} "
                                            "arguments, but {} were provided",
                                            token, macro.parameters.size(),
                                            arguments.size()),
                                        line_number);
                                    error = true;
                                }
                            }

                            if (!error) {
                                // Try to evaluate each argument as a constant expression
                                std::vector<std::string> evaluated_arguments;
                                for (const auto& arg : arguments) {
                                    ExpressionEvaluator evaluator(arg, macros,
                                                                  this);
                                    auto evaluated =
                                        evaluator.try_evaluate_constant();
                                    if (evaluated) {
                                        // Successfully evaluated, use the result
                                        if (std::holds_alternative<int64_t>(
                                                *evaluated)) {
                                            evaluated_arguments.push_back(
                                                std::to_string(
                                                    std::get<int64_t>(
                                                        *evaluated)));
                                        } else {
                                            evaluated_arguments.push_back(
                                                std::to_string(std::get<double>(
                                                    *evaluated)));
                                        }
                                    } else {
                                        // Cannot evaluate, keep original argument
                                        evaluated_arguments.push_back(arg);
                                    }
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
                                        bool is_start_boundary =
                                            (pos == 0) ||
                                            !isIdentifierChar(
                                                replacement[pos - 1]);
                                        bool is_end_boundary =
                                            (pos + param.length() >=
                                             replacement.length()) ||
                                            !isIdentifierChar(
                                                replacement[pos +
                                                            param.length()]);

                                        if (is_start_boundary &&
                                            is_end_boundary) {
                                            replacement.replace(
                                                pos, param.length(), arg);
                                            pos += arg.length();
                                        } else {
                                            pos++;
                                        }
                                    }
                                }

                                // Recursively expand the result of the macro expansion
                                std::string recursively_expanded =
                                    expandMacrosImpl(replacement, line_number,
                                                     nullptr);

                                // Try to evaluate the expanded macro as a constant expression
                                ExpressionEvaluator evaluator(
                                    recursively_expanded, macros, this);
                                auto evaluated =
                                    evaluator.try_evaluate_constant();
                                if (evaluated) {
                                    if (std::holds_alternative<int64_t>(
                                            *evaluated)) {
                                        replacement = std::to_string(
                                            std::get<int64_t>(*evaluated));
                                    } else {
                                        replacement = std::to_string(
                                            std::get<double>(*evaluated));
                                    }
                                } else {
                                    replacement = recursively_expanded;
                                }

                                expansion_end = temp_i;
                                changed = true;
                            } else {
                                replacement = token;
                            }
                        } else {
                            // Function-like macro not followed by '('
                            replacement = token;
                        }
                    } else {
                        // Object-like macro
                        replacement = macro.body;
                        expansion_end = i;
                        changed = true;
                    }

                    int preprocessed_start_col = result.length() + 1;
                    result += replacement;
                    int preprocessed_end_col = result.length();

                    if (changed && expansions_out &&
                        (expansion_end > start || !macro.is_function_like)) {
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

                    i = expansion_end;
                    column += (expansion_end - start);
                } else {
                    result += token;
                }
            } else {
                result += c;
                i++;
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
    while (end < line.length() && isalpha(line[end])) {
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
    size_t pos = line.find("@define");
    if (pos == std::string::npos) {
        return;
    }

    pos += 7; // Skip "@define"

    while (pos < line.length() && std::isspace(line[pos])) {
        pos++;
    }

    if (pos >= line.length()) {
        addError("@define requires a macro name", line_number);
        return;
    }

    size_t name_start = pos;
    while (pos < line.length() && isIdentifierChar(line[pos])) {
        pos++;
    }

    std::string name = line.substr(name_start, pos - name_start);
    if (name.empty()) {
        addError("@define requires a macro name", line_number);
        return;
    }

    if (!std::isalpha(name[0]) && name[0] != '_') {
        addError(
            std::format(
                "Invalid macro name '{}': must start with letter or underscore",
                name),
            line_number);
        return;
    }

    MacroDefinition macro;
    macro.name = name;
    macro.is_function_like = false;

    // Function-like macro: no space between name and '('
    if (pos < line.length() && line[pos] == '(') {
        macro.is_function_like = true;
        pos++; // Skip '('

        // Parse parameter list
        while (pos < line.length() && line[pos] != ')') {
            while (pos < line.length() && std::isspace(line[pos])) {
                pos++;
            }

            if (pos >= line.length()) {
                addError("Unterminated parameter list in macro definition",
                         line_number);
                return;
            }

            if (line[pos] == ')') {
                break;
            }

            size_t param_start = pos;
            while (pos < line.length() && isIdentifierChar(line[pos])) {
                pos++;
            }

            if (pos == param_start) {
                addError("Expected parameter name in macro definition",
                         line_number);
                return;
            }

            std::string param = line.substr(param_start, pos - param_start);
            if (!std::isalpha(param[0]) && param[0] != '_') {
                addError(std::format("Invalid parameter name '{}': must start "
                                     "with letter or underscore",
                                     param),
                         line_number);
                return;
            }

            for (const auto& existing_param : macro.parameters) {
                if (existing_param == param) {
                    addError(
                        std::format("Duplicate parameter name '{}' in macro "
                                    "definition",
                                    param),
                        line_number);
                    return;
                }
            }

            macro.parameters.push_back(param);

            while (pos < line.length() && std::isspace(line[pos])) {
                pos++;
            }

            if (pos >= line.length()) {
                addError("Unterminated parameter list in macro definition",
                         line_number);
                return;
            }

            if (line[pos] == ',') {
                pos++; // Skip comma
            } else if (line[pos] != ')') {
                addError("Expected ',' or ')' in parameter list", line_number);
                return;
            }
        }

        if (pos >= line.length() || line[pos] != ')') {
            addError("Unterminated parameter list in macro definition",
                     line_number);
            return;
        }

        pos++; // Skip ')'
    }

    // Skip whitespace before body
    while (pos < line.length() && std::isspace(line[pos])) {
        pos++;
    }

    std::string body;
    if (pos < line.length()) {
        body = line.substr(pos);
        size_t end = body.find_last_not_of(" \t\r\n");
        if (end != std::string::npos) {
            body = body.substr(0, end + 1);
        }
    }

    if (macros.count(name) > 0) {
        // TODO: handle redefinition of macro
    }

    // Object-like macro: try to evaluate as constant expression
    if (!macro.is_function_like && !body.empty()) {
        ExpressionEvaluator evaluator(body, macros, this);
        auto evaluated = evaluator.try_evaluate_constant();
        if (evaluated) {
            if (std::holds_alternative<int64_t>(*evaluated)) {
                body = std::to_string(std::get<int64_t>(*evaluated));
            } else {
                body = std::to_string(std::get<double>(*evaluated));
            }
        }
    }

    macro.body = body;
    macros[name] = macro;
}

void Preprocessor::handleUndef(const std::string& line, int line_number) {
    // @undef NAME
    size_t pos = line.find("@undef");
    if (pos == std::string::npos) {
        return;
    }

    pos += 6; // Skip "@undef"

    while (pos < line.length() && std::isspace(line[pos])) {
        pos++;
    }

    if (pos >= line.length()) {
        addError("@undef requires a macro name", line_number);
        return;
    }

    size_t name_start = pos;
    while (pos < line.length() && isIdentifierChar(line[pos])) {
        pos++;
    }

    std::string name = line.substr(name_start, pos - name_start);
    if (name.empty()) {
        addError("@undef requires a macro name", line_number);
        return;
    }

    if (!std::isalpha(name[0]) && name[0] != '_') {
        addError(
            std::format(
                "Invalid macro name '{}': must start with letter or underscore",
                name),
            line_number);
        return;
    }

    macros.erase(name);
}

void Preprocessor::handleIfdef(const std::string& line, int line_number) {
    // @ifdef NAME
    size_t pos = line.find("@ifdef");
    if (pos == std::string::npos) {
        return;
    }

    pos += 6; // Skip "@ifdef"

    while (pos < line.length() && std::isspace(line[pos])) {
        pos++;
    }

    if (pos >= line.length()) {
        addError("@ifdef requires a macro name", line_number);
        conditional_stack.push_back({line_number, false, true});
        return;
    }

    size_t name_start = pos;
    while (pos < line.length() && isIdentifierChar(line[pos])) {
        pos++;
    }

    std::string name = line.substr(name_start, pos - name_start);
    if (name.empty()) {
        addError("@ifdef requires a macro name", line_number);
        conditional_stack.push_back({line_number, false, true});
        return;
    }

    bool parent_active = isCurrentBlockActive();
    bool macro_defined = macros.count(name) > 0;
    bool is_active = parent_active && macro_defined;

    conditional_stack.push_back({line_number, is_active, macro_defined});
}

void Preprocessor::handleIfndef(const std::string& line, int line_number) {
    // @ifndef NAME
    size_t pos = line.find("@ifndef");
    if (pos == std::string::npos) {
        return;
    }

    pos += 7; // Skip "@ifndef"

    while (pos < line.length() && std::isspace(line[pos])) {
        pos++;
    }

    if (pos >= line.length()) {
        addError("@ifndef requires a macro name", line_number);
        conditional_stack.push_back({line_number, false, true});
        return;
    }

    size_t name_start = pos;
    while (pos < line.length() && isIdentifierChar(line[pos])) {
        pos++;
    }

    std::string name = line.substr(name_start, pos - name_start);
    if (name.empty()) {
        addError("@ifndef requires a macro name", line_number);
        conditional_stack.push_back({line_number, false, true});
        return;
    }

    bool parent_active = isCurrentBlockActive();
    bool macro_not_defined = macros.count(name) == 0;
    bool is_active = parent_active && macro_not_defined;

    conditional_stack.push_back({line_number, is_active, macro_not_defined});
}

void Preprocessor::handleIf(const std::string& line, int line_number) {
    // @if expression
    size_t pos = line.find("@if");
    if (pos == std::string::npos) {
        return;
    }

    pos += 3; // Skip "@if"
    while (pos < line.length() && std::isspace(line[pos])) {
        pos++;
    }

    std::string expression = line.substr(pos);
    if (expression.empty()) {
        addError("@if requires an expression", line_number);
        conditional_stack.push_back({line_number, false, false});
        return;
    }

    bool parent_active = isCurrentBlockActive();
    bool condition_met = false;

    if (parent_active) {
        try {
            // Expand defined() operator before evaluating
            std::string expanded_expr = expandDefinedOperator(expression);
            ExpressionEvaluator evaluator(expanded_expr, macros, this);
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

    while (pos < line.length() && std::isspace(line[pos])) {
        pos++;
    }

    std::string message;
    if (pos < line.length()) {
        message = line.substr(pos);
        size_t end = message.find_last_not_of(" \t\r\n");
        if (end != std::string::npos) {
            message = message.substr(0, end + 1);
        }
    }

    if (message.empty()) {
        addError("@error directive encountered", line_number);
    } else {
        addError(std::format("@error: {}", message), line_number);
    }
}

std::string Preprocessor::expandDefinedOperator(const std::string& text) {
    std::string processed = text;
    size_t pos = 0;
    while ((pos = processed.find("defined", pos)) != std::string::npos) {
        // Check if "defined" is a standalone word (not part of another identifier)
        bool is_start_boundary =
            (pos == 0) ||
            (!std::isalnum(processed[pos - 1]) && processed[pos - 1] != '_');
        bool is_end_boundary =
            (pos + 7 >= processed.length()) ||
            (!std::isalnum(processed[pos + 7]) && processed[pos + 7] != '_');

        if (!is_start_boundary || !is_end_boundary) {
            pos++;
            continue;
        }

        size_t start = pos + 7;
        bool has_paren = false;
        while (start < processed.length() && std::isspace(processed[start]))
            start++;
        if (start < processed.length() && processed[start] == '(') {
            has_paren = true;
            start++;
            while (start < processed.length() && std::isspace(processed[start]))
                start++;
        }

        size_t end = start;
        while (end < processed.length() &&
               (std::isalnum(processed[end]) || processed[end] == '_')) {
            end++;
        }

        if (end > start) {
            std::string macro_name = processed.substr(start, end - start);
            size_t final_end = end;
            if (has_paren) {
                size_t paren_end = final_end;
                while (paren_end < processed.length() &&
                       std::isspace(processed[paren_end]))
                    paren_end++;
                if (paren_end < processed.length() &&
                    processed[paren_end] == ')') {
                    final_end = paren_end + 1;
                }
            }
            processed.replace(pos, final_end - pos,
                              macros.count(macro_name) ? "1" : "0");
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

bool Preprocessor::isIdentifierChar(char c) const {
    return std::isalnum(c) || c == '_';
}

bool Preprocessor::isCurrentBlockActive() const {
    if (conditional_stack.empty()) {
        return true;
    }

    for (const auto& block : conditional_stack) {
        if (!block.is_active) {
            return false;
        }
    }

    return true;
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
