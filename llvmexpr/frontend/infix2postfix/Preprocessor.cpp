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
#include <cstdint>
#include <format>
#include <sstream>
#include <stdexcept>
#include <variant>

namespace infix2postfix {

class Preprocessor::ExpressionEvaluator {
  public:
    using Value = std::variant<int64_t, double>;

    ExpressionEvaluator(std::string expression,
                        const std::map<std::string, std::string>& macros)
        : expression_(std::move(expression)), macros_(macros) {}

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
        std::string current_expr = expression_;
        std::string result;
        bool changed;
        do {
            changed = false;
            result.clear();
            size_t i = 0;
            while (i < current_expr.length()) {
                if (std::isalpha(current_expr[i]) || current_expr[i] == '_') {
                    size_t start = i;
                    while (i < current_expr.length() &&
                           (std::isalnum(current_expr[i]) ||
                            current_expr[i] == '_')) {
                        i++;
                    }
                    std::string identifier =
                        current_expr.substr(start, i - start);
                    auto it = macros_.find(identifier);
                    if (it != macros_.end()) {
                        result += it->second;
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
        expression_ = result;
    }

    void tokenize() {
        size_t i = 0;
        while (i < expression_.length()) {
            if (std::isspace(expression_[i])) {
                i++;
                continue;
            }

            if (std::isdigit(expression_[i]) ||
                (expression_[i] == '.' && i + 1 < expression_.length() &&
                 std::isdigit(expression_[i + 1]))) {
                size_t start = i;
                while (i < expression_.length() &&
                       (std::isalnum(expression_[i]) || expression_[i] == '.' ||
                        ((expression_[i] == '+' || expression_[i] == '-') &&
                         (std::tolower(expression_[i - 1]) == 'e' ||
                          std::tolower(expression_[i - 1]) == 'p')))) {
                    i++;
                }
                std::string num_str = expression_.substr(start, i - start);
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

            if (std::isalpha(expression_[i]) || expression_[i] == '_') {
                size_t start = i;
                while (
                    i < expression_.length() &&
                    (std::isalnum(expression_[i]) || expression_[i] == '_')) {
                    i++;
                }
                std::string text = expression_.substr(start, i - start);
                throw std::runtime_error("Unknown identifier in expression: " +
                                         text);
            }

            switch (expression_[i]) {
            case '+':
                tokens_.push_back({TokenType::Plus, "+"});
                break;
            case '-':
                tokens_.push_back({TokenType::Minus, "-"});
                break;
            case '*':
                tokens_.push_back({TokenType::Multiply, "*"});
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
                if (i + 1 < expression_.length() && expression_[i + 1] == '=') {
                    tokens_.push_back({TokenType::NotEqual, "!="});
                    i++;
                } else {
                    tokens_.push_back({TokenType::LogicalNot, "!"});
                }
                break;
            case '=':
                if (i + 1 < expression_.length() && expression_[i + 1] == '=') {
                    tokens_.push_back({TokenType::Equal, "=="});
                    i++;
                } else {
                    throw std::runtime_error("Unexpected token '='");
                }
                break;
            case '&':
                if (i + 1 < expression_.length() && expression_[i + 1] == '&') {
                    tokens_.push_back({TokenType::LogicalAnd, "&&"});
                    i++;
                } else {
                    throw std::runtime_error("Unexpected token '&'");
                }
                break;
            case '|':
                if (i + 1 < expression_.length() && expression_[i + 1] == '|') {
                    tokens_.push_back({TokenType::LogicalOr, "||"});
                    i++;
                } else {
                    throw std::runtime_error("Unexpected token '|'");
                }
                break;
            case '>':
                if (i + 1 < expression_.length() && expression_[i + 1] == '=') {
                    tokens_.push_back({TokenType::GreaterEqual, ">="});
                    i++;
                } else {
                    tokens_.push_back({TokenType::Greater, ">"});
                }
                break;
            case '<':
                if (i + 1 < expression_.length() && expression_[i + 1] == '=') {
                    tokens_.push_back({TokenType::LessEqual, "<="});
                    i++;
                } else {
                    tokens_.push_back({TokenType::Less, "<"});
                }
                break;
            default:
                throw std::runtime_error(
                    std::string("Unexpected character in expression: ") +
                    expression_[i]);
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

    Value parse_factor() {
        Value left = parse_unary();
        while (peek().type == TokenType::Multiply ||
               peek().type == TokenType::Divide ||
               peek().type == TokenType::Modulo) {
            Token op = consume();
            Value right = parse_unary();
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

    std::string expression_;
    const std::map<std::string, std::string>& macros_;
    std::vector<Token> tokens_;
    size_t pos_ = 0;
};

Preprocessor::Preprocessor(const std::string& source) : source(source) {}

void Preprocessor::addPredefinedMacro(const std::string& name,
                                      const std::string& value) {
    macros[name] = value;
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
    // @define NAME [value...]
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

    while (pos < line.length() && std::isspace(line[pos])) {
        pos++;
    }

    std::string value;
    if (pos < line.length()) {
        value = line.substr(pos);
        size_t end = value.find_last_not_of(" \t\r\n");
        if (end != std::string::npos) {
            value = value.substr(0, end + 1);
        }
    }

    if (macros.count(name) > 0) {
        // TODO: handle redefinition of macro
    }

    ExpressionEvaluator evaluator(value, macros);
    auto evaluated = evaluator.try_evaluate_constant();
    if (evaluated) {
        if (std::holds_alternative<int64_t>(*evaluated)) {
            macros[name] = std::to_string(std::get<int64_t>(*evaluated));
        } else {
            macros[name] = std::to_string(std::get<double>(*evaluated));
        }
    } else {
        macros[name] = value;
    }
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
            ExpressionEvaluator evaluator(expression, macros);
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

std::string Preprocessor::expandMacros(const std::string& line,
                                       int line_number) {
    std::string processed_line = line;
    size_t pos = 0;
    while ((pos = processed_line.find("defined", pos)) != std::string::npos) {
        size_t start = pos + 7;
        bool has_paren = false;
        while (start < processed_line.length() &&
               std::isspace(processed_line[start]))
            start++;
        if (start < processed_line.length() && processed_line[start] == '(') {
            has_paren = true;
            start++;
            while (start < processed_line.length() &&
                   std::isspace(processed_line[start]))
                start++;
        }

        size_t end = start;
        while (
            end < processed_line.length() &&
            (std::isalnum(processed_line[end]) || processed_line[end] == '_')) {
            end++;
        }

        if (end > start) {
            std::string macro_name = processed_line.substr(start, end - start);
            size_t final_end = end;
            if (has_paren) {
                size_t paren_end = final_end;
                while (paren_end < processed_line.length() &&
                       std::isspace(processed_line[paren_end]))
                    paren_end++;
                if (paren_end < processed_line.length() &&
                    processed_line[paren_end] == ')') {
                    final_end = paren_end + 1;
                }
            }
            processed_line.replace(pos, final_end - pos,
                                   macros.count(macro_name) ? "1" : "0");
        }
        pos++;
    }

    if (macros.empty()) {
        return processed_line;
    }

    std::string current_line = processed_line;
    std::string result;
    bool changed;
    int recursion_limit = 1000;
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
                    int preprocessed_start_col = result.length() + 1;
                    result += it->second;
                    int preprocessed_end_col = result.length();

                    MacroExpansion expansion;
                    expansion.macro_name = token;
                    expansion.original_line = line_number;
                    expansion.original_column = token_column;
                    expansion.replacement_text = it->second;
                    expansion.preprocessed_start_column =
                        preprocessed_start_col;
                    expansion.preprocessed_end_column = preprocessed_end_col;
                    expansions_this_pass.push_back(expansion);
                    changed = true;
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
        if (!expansions_this_pass.empty() && !line_mappings.empty()) {
            line_mappings.back().expansions.insert(
                line_mappings.back().expansions.end(),
                expansions_this_pass.begin(), expansions_this_pass.end());
        }

        if (++count > recursion_limit) {
            addError("Macro expansion limit reached, possible recursive macro?",
                     line_number);
            break;
        }
    } while (changed);

    return current_line;
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
