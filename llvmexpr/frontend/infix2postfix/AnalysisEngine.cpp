#include "AnalysisEngine.hpp"
#include "CodeGenerator.hpp"
#include "Parser.hpp"
#include "SemanticAnalyzer.hpp"
#include <format>

namespace infix2postfix {

AnalysisEngine::AnalysisEngine(const std::vector<Token>& tokens, Mode mode,
                               int num_inputs)
    : tokens(tokens), mode(mode), num_inputs(num_inputs) {}

AnalysisEngine::~AnalysisEngine() = default;

bool AnalysisEngine::runAnalysis() {
    diagnostics.clear();

    Parser parser(tokens);
    auto parse_result = parser.parse();
    ast = std::move(parse_result.ast);

    for (const auto& error : parse_result.errors) {
        diagnostics.emplace_back(DiagnosticSeverity::ERROR, error.message,
                                 error.line);
    }

    if (!ast || hasErrors()) {
        return false;
    }

    semantic_analyzer = std::make_unique<SemanticAnalyzer>(mode, num_inputs);
    semantic_analyzer->analyze(ast.get());

    const auto& semantic_diagnostics = semantic_analyzer->getDiagnostics();
    diagnostics.insert(diagnostics.end(), semantic_diagnostics.begin(),
                       semantic_diagnostics.end());

    return !hasErrors();
}

std::string AnalysisEngine::generateCode() {
    if (!ast || !semantic_analyzer) {
        throw std::runtime_error(
            "Cannot generate code: analysis not run or failed");
    }

    if (hasErrors()) {
        throw std::runtime_error(
            "Cannot generate code: semantic analysis had errors");
    }

    CodeGenerator code_generator(mode, num_inputs);

    return code_generator.generate(ast.get());
}

bool AnalysisEngine::hasErrors() const {
    for (const auto& diag : diagnostics) {
        if (diag.severity == DiagnosticSeverity::ERROR) {
            return true;
        }
    }
    return false;
}

std::string AnalysisEngine::formatDiagnostics() const {
    if (diagnostics.empty()) {
        return "";
    }

    std::string result;
    int error_count = 0;
    int warning_count = 0;

    for (const auto& diag : diagnostics) {
        if (diag.severity == DiagnosticSeverity::ERROR) {
            error_count++;
        } else {
            warning_count++;
        }

        std::string severity_str =
            (diag.severity == DiagnosticSeverity::ERROR) ? "Error" : "Warning";

        if (!result.empty()) {
            result += "\n";
        }
        result += std::format("Line {}: {} - {}", diag.line, severity_str,
                              diag.message);
    }

    std::string summary;
    if (error_count > 0 && warning_count > 0) {
        summary = std::format("\nFound {} error(s) and {} warning(s).",
                              error_count, warning_count);
    } else if (error_count > 0) {
        summary = std::format("\nFound {} error(s).", error_count);
    } else if (warning_count > 0) {
        summary = std::format("\nFound {} warning(s).", warning_count);
    }

    result += summary;
    return result;
}

} // namespace infix2postfix
