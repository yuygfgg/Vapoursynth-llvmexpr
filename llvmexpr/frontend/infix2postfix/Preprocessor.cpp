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
#include <format>
#include <sstream>

namespace infix2postfix {

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
    macros[name] = value;
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
    if (macros.empty()) {
        return line;
    }

    std::string result;
    result.reserve(line.length());
    std::vector<MacroExpansion> expansions;

    size_t pos = 0;
    int column = 1;

    while (pos < line.length()) {
        char c = line[pos];

        if (std::isalpha(c) || c == '_') {
            size_t start = pos;
            int token_column = column;

            while (pos < line.length() && isIdentifierChar(line[pos])) {
                pos++;
                column++;
            }

            std::string token = line.substr(start, pos - start);

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
                expansion.preprocessed_start_column = preprocessed_start_col;
                expansion.preprocessed_end_column = preprocessed_end_col;
                expansions.push_back(expansion);
            } else {
                result += token;
            }
        } else {
            result += c;
            pos++;
            column++;
        }
    }

    if (!expansions.empty() && !line_mappings.empty()) {
        line_mappings.back().expansions = expansions;
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
