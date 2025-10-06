#ifndef LLVMEXPR_INFIX2POSTFIX_CODEGENERATOR_HPP
#define LLVMEXPR_INFIX2POSTFIX_CODEGENERATOR_HPP

#include "AST.hpp"
#include <map>
#include <set>
#include <stdexcept>

namespace infix2postfix {

class CodeGenError : public std::runtime_error {
  public:
    CodeGenError(const std::string& message, int line)
        : std::runtime_error("Line " + std::to_string(line) + ": " + message),
          line(line) {}
    int line;
};

class CodeGenerator : public ExprVisitor, public StmtVisitor {
  public:
    CodeGenerator(Mode mode, int num_inputs);
    std::string generate(Program* program);

    // Expr visitors
    std::string visit(NumberExpr& expr) override;
    std::string visit(VariableExpr& expr) override;
    std::string visit(UnaryExpr& expr) override;
    std::string visit(BinaryExpr& expr) override;
    std::string visit(TernaryExpr& expr) override;
    std::string visit(CallExpr& expr) override;
    std::string visit(PropAccessExpr& expr) override;
    std::string visit(StaticRelPixelAccessExpr& expr) override;
    std::string visit(FrameDimensionExpr& expr) override;

    // Stmt visitors
    std::string visit(ExprStmt& stmt) override;
    std::string visit(AssignStmt& stmt) override;
    std::string visit(BlockStmt& stmt) override;
    std::string visit(IfStmt& stmt) override;
    std::string visit(WhileStmt& stmt) override;
    std::string visit(ReturnStmt& stmt) override;
    std::string visit(LabelStmt& stmt) override;
    std::string visit(GotoStmt& stmt) override;
    std::string visit(FunctionDef& stmt) override;
    std::string visit(GlobalDecl& stmt) override;

  private:
    std::string generate(Node* node);
    std::string get_op_postfix(TokenType type);
    std::string get_unary_op_postfix(TokenType type);
    void check_stack_effect(const std::string& s, int expected, int line);
    int compute_stack_effect(const std::string& s, int line);
    std::string
    inline_function_call(const std::string& func_name,
                         const std::vector<std::unique_ptr<Expr>>& args,
                         int call_line);
    std::string rename_variable(const std::string& var_name);
    bool is_constant_infix(const std::string& name);

    void check_variable_defined(const std::string& var_name, int line);
    void enter_scope();
    void exit_scope();
    void define_variable_in_current_scope(const std::string& var_name);

    Mode mode;
    int num_inputs;
    int label_counter = 0;
    std::map<std::string, FunctionSignature> functions;
    std::map<std::string, FunctionDef*> function_defs;
    const FunctionSignature* current_function = nullptr;
    std::set<std::string> defined_globals;
    std::set<std::string> local_scope_vars;
    std::map<std::string, std::string> var_rename_map;
    std::map<std::string, Expr*> param_substitutions;
    std::set<std::string> literals_in_scope;
    bool has_result = false;

    // Scope tracking
    std::vector<std::set<std::string>> scope_stack;
    std::set<std::string> all_defined_vars_in_scope;
};

} // namespace infix2postfix

#endif