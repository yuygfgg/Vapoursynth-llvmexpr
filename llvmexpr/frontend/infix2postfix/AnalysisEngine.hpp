#ifndef LLVMEXPR_INFIX2POSTFIX_ANALYSISENGINE_HPP
#define LLVMEXPR_INFIX2POSTFIX_ANALYSISENGINE_HPP

#include "AST.hpp"
#include "Preprocessor.hpp"
#include "types.hpp"
#include <memory>
#include <string>
#include <vector>

namespace infix2postfix {

enum class DiagnosticSeverity {
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

    bool runAnalysis();

    std::string generateCode();

    const Program* getAST() const { return ast.get(); }
    Program* getAST() { return ast.get(); }

    const std::vector<Diagnostic>& getDiagnostics() const {
        return diagnostics;
    }

    bool hasErrors() const;

    std::string formatDiagnostics() const;

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
