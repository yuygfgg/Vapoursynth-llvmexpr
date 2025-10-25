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

#ifndef LLVMEXPR_INFIX2POSTFIX_PREPROCESSOR_HPP
#define LLVMEXPR_INFIX2POSTFIX_PREPROCESSOR_HPP

#include <map>
#include <optional>
#include <stdexcept>
#include <string>
#include <vector>

namespace infix2postfix {

class PreprocessorError : public std::runtime_error {
  public:
    using std::runtime_error::runtime_error;
};

struct MacroExpansion {
    std::string macro_name;
    int original_line;
    int original_column;
    std::string replacement_text;
    int preprocessed_start_column;
    int preprocessed_end_column;
};

struct LineMapping {
    int preprocessed_line;
    int original_line;
    std::vector<MacroExpansion> expansions;
};

struct MacroDefinition {
    std::string name;
    std::string body;
    bool is_function_like = false;
    std::vector<std::string> parameters;
};

struct PreprocessResult {
    std::string source;
    std::vector<LineMapping> line_map;
    std::vector<std::string> errors;
    bool success;
};

class Preprocessor {
  public:
    explicit Preprocessor(std::string source);

    void addPredefinedMacro(std::string name, std::string value = "");

    PreprocessResult process();

    static std::string
    formatDiagnosticWithExpansion(const std::string& message, int line,
                                  const std::vector<LineMapping>& line_map);

    static std::string
    formatMacroExpansions(const std::vector<LineMapping>& line_map);

  private:
    class ExpressionEvaluator;

    struct LineParser {
        const std::string& str;
        size_t pos = 0;

        explicit LineParser(const std::string& s) : str(s) {}

        [[nodiscard]] bool eof() const { return pos >= str.length(); }
        [[nodiscard]] char peek() const { return eof() ? '\0' : str[pos]; }
        void consume() {
            if (!eof()) {
                pos++;
            }
        }
        char consume_one() { return eof() ? '\0' : str[pos++]; }
        void skipWhitespace();
        std::string_view extractIdentifier();
    };

    struct ConditionalBlock {
        int start_line;
        bool is_active;
        bool had_true_branch;
    };

    void processLine(const std::string& line, int line_number);
    [[nodiscard]] bool isDirective(const std::string& line) const;
    void handleDirective(const std::string& line, int line_number);
    void handleDefine(const std::string& line, int line_number);
    void handleUndef(const std::string& line, int line_number);
    void handleIfdef(const std::string& line, int line_number);
    void handleIfndef(const std::string& line, int line_number);
    void handleIfdefCommon(const std::string& line, int line_number,
                           bool check_defined);
    void handleIf(const std::string& line, int line_number);
    void handleElse(const std::string& line, int line_number);
    void handleEndif(const std::string& line, int line_number);
    void handleError(const std::string& line, int line_number);

    std::optional<std::vector<std::string>>
    parseMacroArguments(LineParser& parser, const std::string& macro_name,
                        int line_number);

    std::string expandMacros(const std::string& line, int line_number);
    std::string expandMacrosImpl(const std::string& line, int line_number,
                                 std::vector<MacroExpansion>* expansions_out);
    std::string expandDefinedOperator(const std::string& text);
    std::string expandConstEvalOperators(const std::string& text);
    std::string evaluateIfPossible(const std::string& text);
    [[nodiscard]] bool isCurrentBlockActive() const;

    void addError(const std::string& message, int line);
    void addOutputLine(const std::string& line, int original_line,
                       const std::vector<MacroExpansion>& expansions = {});

    std::string source;
    std::string filename;

    std::map<std::string, MacroDefinition> macros;

    std::vector<ConditionalBlock> conditional_stack;

    std::vector<std::string> output_lines;
    std::vector<LineMapping> line_mappings;
    std::vector<std::string> errors;

    int current_output_line = 1;
};

} // namespace infix2postfix

#endif // LLVMEXPR_INFIX2POSTFIX_PREPROCESSOR_HPP
