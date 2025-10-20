#ifndef LLVMEXPR_INFIX2POSTFIX_SEMANTICANALYZER_HPP
#define LLVMEXPR_INFIX2POSTFIX_SEMANTICANALYZER_HPP

#include "AST.hpp"
#include "AnalysisEngine.hpp"
#include "types.hpp"
#include <map>
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

  private:
    // Expression analysis
    Type analyzeExpr(Expr* expr);
    Type analyze(const NumberExpr& expr);
    Type analyze(const VariableExpr& expr);
    Type analyze(const UnaryExpr& expr);
    Type analyze(const BinaryExpr& expr);
    Type analyze(const TernaryExpr& expr);
    Type analyze(const CallExpr& expr);
    Type analyze(const PropAccessExpr& expr);
    Type analyze(const StaticRelPixelAccessExpr& expr);
    Type analyze(const FrameDimensionExpr& expr);
    Type analyze(const ArrayAccessExpr& expr);

    // Statement analysis
    void analyzeStmt(Stmt* stmt);
    void analyze(const ExprStmt& stmt);
    void analyze(const AssignStmt& stmt);
    void analyze(const BlockStmt& stmt);
    void analyze(const IfStmt& stmt);
    void analyze(const WhileStmt& stmt);
    void analyze(const ReturnStmt& stmt);
    void analyze(const LabelStmt& stmt);
    void analyze(const GotoStmt& stmt);
    void analyze(const FunctionDef& stmt);
    void analyze(const GlobalDecl& stmt);
    void analyze(const ArrayAssignStmt& stmt);

    void reportError(const std::string& message, int line);
    void reportWarning(const std::string& message, int line);

    void checkVariableDefined(const std::string& var_name, int line);
    void enterScope();
    void exitScope();
    void defineVariableInCurrentScope(const std::string& var_name,
                                      Type type = Type::VALUE);
    std::string renameVariable(const std::string& var_name);

    bool isClipName(const std::string& s);
    bool isConvertible(Type from, Type to);
    bool builtinParamTypeIsEvaluatable(
        const std::vector<struct BuiltinFunction>& overloads, size_t param_idx);

    Mode mode;
    [[maybe_unused]] int num_inputs;
    bool has_result = false;
    bool result_defined_in_global_scope = false;

    std::map<std::string, std::vector<FunctionSignature>> functions;
    std::map<std::string, std::vector<FunctionDef*>> function_defs;

    std::set<std::string> defined_globals;
    std::set<std::string> local_scope_vars;
    std::vector<std::set<std::string>> scope_stack;
    std::set<std::string> all_defined_vars_in_scope;

    std::map<std::string, Type> variable_types;

    std::map<std::string, std::string> var_rename_map;
    std::map<std::string, Expr*> param_substitutions;
    const FunctionSignature* current_function = nullptr;

    std::set<std::string> global_labels;
    std::set<std::string> current_function_labels;

    std::vector<Diagnostic> diagnostics;
};

} // namespace infix2postfix

#endif
