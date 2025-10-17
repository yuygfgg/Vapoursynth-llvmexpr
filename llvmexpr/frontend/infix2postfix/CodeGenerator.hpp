#ifndef LLVMEXPR_INFIX2POSTFIX_CODEGENERATOR_HPP
#define LLVMEXPR_INFIX2POSTFIX_CODEGENERATOR_HPP

#include "AST.hpp"
#include "PostfixBuilder.hpp"
#include "types.hpp"
#include <format>
#include <map>
#include <set>
#include <stdexcept>
#include <string>
#include <vector>

namespace infix2postfix {

struct BuiltinFunction; // Forward declaration

class CodeGenError : public std::runtime_error {
  public:
    int line;
    CodeGenError(const std::string& message, int l)
        : std::runtime_error(std::format("Line {}: {}", l, message)), line(l) {}
};

class CodeGenerator {
  public:
    struct ExprResult {
        PostfixBuilder postfix;
        Type type;
    };

    CodeGenerator(Mode mode, int num_inputs);
    std::string generate(Program* program);

    Mode get_mode() const { return mode; }
    ExprResult generate_expr(Expr* expr) { return generate(expr); }
    static bool is_constant_infix(const std::string& name);

  private:
    ExprResult generate(Expr* expr);
    PostfixBuilder generate(Stmt* stmt);

    // Expression handlers
    ExprResult handle(const NumberExpr& expr);
    ExprResult handle(const VariableExpr& expr);
    ExprResult handle(const UnaryExpr& expr);
    ExprResult handle(const BinaryExpr& expr);
    ExprResult handle(const TernaryExpr& expr);
    ExprResult handle(const CallExpr& expr);
    ExprResult handle(const PropAccessExpr& expr);
    ExprResult handle(const StaticRelPixelAccessExpr& expr);
    ExprResult handle(const FrameDimensionExpr& expr);

    // Statement handlers
    PostfixBuilder handle(const ExprStmt& stmt);
    PostfixBuilder handle(const AssignStmt& stmt);
    PostfixBuilder handle(const BlockStmt& stmt);
    PostfixBuilder handle(const IfStmt& stmt);
    PostfixBuilder handle(const WhileStmt& stmt);
    PostfixBuilder handle(const ReturnStmt& stmt);
    PostfixBuilder handle(const LabelStmt& stmt);
    PostfixBuilder handle(const GotoStmt& stmt);
    PostfixBuilder handle(const FunctionDef& stmt);
    PostfixBuilder handle(const GlobalDecl& stmt);

    void check_stack_effect(const std::string& s, int expected, int line);
    int compute_stack_effect(const std::string& s, int line);

    PostfixBuilder
    inline_function_call(const FunctionSignature& sig, FunctionDef* func_def,
                         const std::vector<std::unique_ptr<Expr>>& args,
                         int call_line);

    // Variable scoping
    void check_variable_defined(const std::string& var_name, int line);
    void enter_scope();
    void exit_scope();
    void define_variable_in_current_scope(const std::string& var_name);
    std::string rename_variable(const std::string& var_name);

    bool is_clip_name(const std::string& s);
    bool is_convertible(Type from, Type to);
    bool builtin_param_type_is_evaluatable(
        const std::vector<BuiltinFunction>& overloads, size_t param_idx);

    Mode mode;
    int num_inputs;
    bool has_result = false;
    bool result_defined_in_global_scope = false;
    int label_counter = 0;

    std::map<std::string, std::vector<FunctionSignature>> functions;
    std::map<std::string, std::vector<FunctionDef*>> function_defs;

    // Scope management
    std::set<std::string> defined_globals;
    std::set<std::string> local_scope_vars;
    std::vector<std::set<std::string>> scope_stack;
    std::set<std::string> all_defined_vars_in_scope;

    // Function inlining context
    std::map<std::string, std::string> var_rename_map;
    std::map<std::string, Expr*> param_substitutions;
    const FunctionSignature* current_function = nullptr;

    // Label checking
    std::set<std::string> global_labels;
    std::set<std::string> current_function_labels;
};

} // namespace infix2postfix

#endif
