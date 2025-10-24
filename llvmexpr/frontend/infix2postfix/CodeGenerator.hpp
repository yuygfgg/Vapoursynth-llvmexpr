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
    Range range;
    CodeGenError(const std::string& message, const Range& r)
        : std::runtime_error(std::format("Line {}: {}", r.start.line, message)),
          range(r) {}
};

class CodeGenerator {
  public:
    struct ExprResult {
        PostfixBuilder postfix;
        Type type;
    };

    CodeGenerator(Mode mode, int num_inputs);

    std::string generate(Program* program);

    [[nodiscard]] Mode get_mode() const { return mode; }
    ExprResult generate_expr(Expr* expr) { return generate(expr); }

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
    ExprResult handle(const ArrayAccessExpr& expr);

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
    PostfixBuilder handle(const ArrayAssignStmt& stmt);

    void check_stack_effect(const std::string& s, int expected,
                            const Range& range);
    int compute_stack_effect(const std::string& s, const Range& range);

    PostfixBuilder
    inline_function_call(const FunctionSignature& sig, FunctionDef* func_def,
                         const std::vector<std::unique_ptr<Expr>>& args,
                         const Range& call_range);

    Mode mode;
    int num_inputs;
    int label_counter = 0;
    int call_site_counter = 0;

    std::map<std::string, Expr*> param_substitutions;
    std::map<std::string, std::string> var_rename_map;
    const FunctionSignature* current_function = nullptr;
    std::vector<int> call_site_id_stack;

    std::set<std::string> current_function_labels;
};

} // namespace infix2postfix

#endif
