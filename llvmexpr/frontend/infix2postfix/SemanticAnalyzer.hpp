#ifndef LLVMEXPR_INFIX2POSTFIX_SEMANTICANALYZER_HPP
#define LLVMEXPR_INFIX2POSTFIX_SEMANTICANALYZER_HPP

#include "AST.hpp"
#include "AnalysisEngine.hpp"
#include "SymbolTable.hpp"
#include "types.hpp"
#include <map>
#include <memory>
#include <set>
#include <string>
#include <vector>

namespace infix2postfix {

class SemanticAnalyzer {
  public:
    SemanticAnalyzer(Mode mode, int num_inputs);

    bool analyze(Program* program);

    const std::vector<Diagnostic>& getDiagnostics() const {
        return diagnostics;
    }

    bool hasErrors() const;

    const std::map<std::string, std::vector<FunctionSignature>>&
    getFunctionSignatures() const {
        return function_signatures;
    }

    const std::map<std::string, std::vector<FunctionDef*>>&
    getFunctionDefs() const {
        return function_defs;
    }

  private:
    // Expression analysis
    Type analyzeExpr(Expr* expr);
    Type analyze(VariableExpr& expr);
    Type analyze(const NumberExpr& expr);
    Type analyze(UnaryExpr& expr);
    Type analyze(BinaryExpr& expr);
    Type analyze(TernaryExpr& expr);
    Type analyze(CallExpr& expr);
    Type analyze(const PropAccessExpr& expr);
    Type analyze(const StaticRelPixelAccessExpr& expr);
    Type analyze(FrameDimensionExpr& expr);
    Type analyze(ArrayAccessExpr& expr);

    // Statement analysis
    void analyzeStmt(Stmt* stmt);
    void analyze(const ExprStmt& stmt);
    void analyze(AssignStmt& stmt);
    void analyze(BlockStmt& stmt);
    void analyze(IfStmt& stmt);
    void analyze(WhileStmt& stmt);
    void analyze(const ReturnStmt& stmt);
    void analyze(LabelStmt& stmt);
    void analyze(GotoStmt& stmt);
    void analyze(FunctionDef& stmt);
    void analyze(const GlobalDecl& stmt);
    void analyze(ArrayAssignStmt& stmt);

    void enterScope();
    void exitScope();
    std::shared_ptr<Symbol>
    defineSymbol(SymbolKind kind, const std::string& name, Type type, int line);
    std::shared_ptr<Symbol> resolveSymbol(const std::string& name, int line);

    void reportError(const std::string& message, int line);
    void reportWarning(const std::string& message, int line);
    bool isClipName(const std::string& s);
    bool isConvertible(Type from, Type to);
    bool builtinParamTypeIsEvaluatable(
        const std::vector<struct BuiltinFunction>& overloads, size_t param_idx);

    const FunctionSignature*
    resolveOverload(const std::string& name,
                    const std::vector<std::unique_ptr<Expr>>& args,
                    const std::string& boundary_suffix, int line);

    void collectLabels(Stmt* stmt, std::set<std::string>& labels,
                       const std::string& context, int context_line);

    std::string generateMangledName(const std::string& name);

    void validateGlobalDependencies(Stmt* stmt);
    void validateFunctionCall(const CallExpr& expr);

    void collectUsedGlobals(Expr* expr, std::set<std::string>& used_globals);
    void collectUsedGlobalsInStmt(Stmt* stmt,
                                  std::set<std::string>& used_globals);

    Mode mode;
    [[maybe_unused]] int num_inputs;
    bool has_result = false;
    bool result_defined_in_global_scope = false;

    std::unique_ptr<SymbolTable> global_scope;
    SymbolTable* current_scope;

    std::vector<std::unique_ptr<SymbolTable>> scope_stack;

    std::set<std::string> defined_global_vars;

    std::map<std::string, std::vector<FunctionSignature>> function_signatures;
    std::map<std::string, std::vector<FunctionDef*>> function_defs;

    std::set<std::string> global_labels;
    std::set<std::string> current_function_labels;

    std::vector<Diagnostic> diagnostics;

    int mangle_counter = 0;

    const FunctionSignature* current_function = nullptr;
};

} // namespace infix2postfix

#endif
