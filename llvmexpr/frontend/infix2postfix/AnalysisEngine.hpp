#ifndef LLVMEXPR_INFIX2POSTFIX_ANALYSISENGINE_HPP
#define LLVMEXPR_INFIX2POSTFIX_ANALYSISENGINE_HPP

#include "AST.hpp"
#include "Preprocessor.hpp"
#include "types.hpp"
#include <cstdint>
#include <memory>
#include <string>
#include <vector>

namespace infix2postfix {

enum class DiagnosticSeverity : std::uint8_t {
    ERROR,
    WARNING,
};

struct Diagnostic {
    DiagnosticSeverity severity;
    std::string message;
    Range range;

    Diagnostic(DiagnosticSeverity sev, std::string msg, Range r)
        : severity(sev), message(std::move(msg)), range(r) {}
};

class SemanticAnalyzer; // Forward declaration

class AnalysisEngine {
  public:
    AnalysisEngine(const std::vector<Token>& tokens, Mode mode, int num_inputs,
                   const std::vector<LineMapping>& line_map);
    ~AnalysisEngine();

    AnalysisEngine(const AnalysisEngine&) = delete;
    AnalysisEngine& operator=(const AnalysisEngine&) = delete;
    AnalysisEngine(AnalysisEngine&&) = delete;
    AnalysisEngine& operator=(AnalysisEngine&&) = delete;

    bool runAnalysis();

    std::string generateCode();

    [[nodiscard]] const Program* getAST() const { return ast.get(); }
    Program* getAST() { return ast.get(); }

    [[nodiscard]] const std::vector<Diagnostic>& getDiagnostics() const {
        return diagnostics;
    }

    [[nodiscard]] bool hasErrors() const;

    [[nodiscard]] std::string formatDiagnostics() const;

  private:
    std::vector<Token> tokens;
    Mode mode;
    int num_inputs;
    std::vector<LineMapping> line_map;

    std::unique_ptr<Program> ast;
    std::unique_ptr<SemanticAnalyzer> semantic_analyzer;
    std::vector<Diagnostic> diagnostics;
};

} // namespace infix2postfix

#endif
