#ifndef LLVMEXPR_INFIX2POSTFIX_AST_HPP
#define LLVMEXPR_INFIX2POSTFIX_AST_HPP

#include "types.hpp"
#include <memory>
#include <string>
#include <variant>
#include <vector>

namespace infix2postfix {

// Forward declarations
struct Expr;
struct Stmt;
struct BlockStmt;
struct GlobalDecl;

// Expression node types
struct NumberExpr {
    Token value;
    int line = 0;

    explicit NumberExpr(Token val) : value(std::move(val)) {
        line = value.line;
    }
};

struct VariableExpr {
    Token name;
    int line = 0;

    explicit VariableExpr(Token n) : name(std::move(n)) { line = name.line; }
};

struct UnaryExpr {
    Token op;
    std::unique_ptr<Expr> right;
    int line = 0;

    UnaryExpr(Token o, std::unique_ptr<Expr> r);
};

struct BinaryExpr {
    std::unique_ptr<Expr> left;
    Token op;
    std::unique_ptr<Expr> right;
    int line = 0;

    BinaryExpr(std::unique_ptr<Expr> l, Token o, std::unique_ptr<Expr> r);
};

struct TernaryExpr {
    std::unique_ptr<Expr> cond;
    std::unique_ptr<Expr> true_expr;
    std::unique_ptr<Expr> false_expr;
    int line = 0;

    TernaryExpr(std::unique_ptr<Expr> c, std::unique_ptr<Expr> t,
                std::unique_ptr<Expr> f);
};

struct CallExpr {
    std::string callee;
    std::vector<std::unique_ptr<Expr>> args;
    std::string boundary_suffix;
    int line = 0;

    CallExpr(Token callee_token, std::vector<std::unique_ptr<Expr>> a,
             std::string suffix = "");
};

struct PropAccessExpr {
    Token clip;
    Token prop;
    int line = 0;

    PropAccessExpr(Token c, Token p) : clip(std::move(c)), prop(std::move(p)) {
        line = clip.line;
    }
};

struct StaticRelPixelAccessExpr {
    Token clip;
    Token offsetX;
    Token offsetY;
    std::string boundary_suffix;
    int line = 0;

    StaticRelPixelAccessExpr(Token c, Token x, Token y, std::string suffix)
        : clip(std::move(c)), offsetX(std::move(x)), offsetY(std::move(y)),
          boundary_suffix(std::move(suffix)) {
        line = clip.line;
    }
};

struct FrameDimensionExpr {
    std::string dimension_name; // "width" or "height"
    std::unique_ptr<Expr> plane_index_expr;
    int line = 0;

    FrameDimensionExpr(Token keyword, std::unique_ptr<Expr> plane);
};

struct ArrayAccessExpr {
    std::unique_ptr<Expr> array;
    std::unique_ptr<Expr> index;
    int line = 0;

    ArrayAccessExpr(std::unique_ptr<Expr> arr, std::unique_ptr<Expr> idx);
};

// Statement node types
struct ExprStmt {
    std::unique_ptr<Expr> expr;
    int line = 0;

    explicit ExprStmt(std::unique_ptr<Expr> e);
};

struct AssignStmt {
    Token name;
    std::unique_ptr<Expr> value;
    int line = 0;

    AssignStmt(Token n, std::unique_ptr<Expr> v);
};

struct ArrayAssignStmt {
    std::unique_ptr<Expr>
        target; // Should be an ArrayAccessExpr
    std::unique_ptr<Expr> value;
    int line = 0;

    ArrayAssignStmt(std::unique_ptr<Expr> t, std::unique_ptr<Expr> v);
};

struct BlockStmt {
    std::vector<std::unique_ptr<Stmt>> statements;
    int line = 0;

    explicit BlockStmt(std::vector<std::unique_ptr<Stmt>> s);
};

struct IfStmt {
    std::unique_ptr<Expr> condition;
    std::unique_ptr<Stmt> then_branch;
    std::unique_ptr<Stmt> else_branch;
    int line = 0;

    IfStmt(std::unique_ptr<Expr> c, std::unique_ptr<Stmt> t,
           std::unique_ptr<Stmt> e);
};

struct WhileStmt {
    std::unique_ptr<Expr> condition;
    std::unique_ptr<Stmt> body;
    int line = 0;

    WhileStmt(std::unique_ptr<Expr> c, std::unique_ptr<Stmt> b);
};

struct ReturnStmt {
    Token keyword;
    std::unique_ptr<Expr> value;
    int line = 0;

    ReturnStmt(Token kw, std::unique_ptr<Expr> v);
};

struct LabelStmt {
    Token name;
    int line = 0;

    explicit LabelStmt(Token n) : name(std::move(n)) { line = name.line; }
};

struct GotoStmt {
    Token keyword;
    Token label;
    std::unique_ptr<Expr> condition;
    int line = 0;

    GotoStmt(Token kw, Token l, std::unique_ptr<Expr> c);
};

struct GlobalDecl {
    Token keyword;
    GlobalMode mode;
    std::vector<Token> globals;
    int line = 0;

    GlobalDecl(Token t, GlobalMode m, std::vector<Token> g = {})
        : keyword(std::move(t)), mode(m), globals(std::move(g)) {
        line = keyword.line;
    }
};

struct Parameter {
    Token type_name;
    Token name;
    Type type;
};

struct FunctionDef {
    Token name;
    std::vector<Parameter> params;
    std::unique_ptr<BlockStmt> body;
    std::unique_ptr<GlobalDecl> global_decl;
    int line = 0;

    FunctionDef(Token n, std::vector<Parameter> p, std::unique_ptr<BlockStmt> b,
                std::unique_ptr<GlobalDecl> g);
};

struct Expr {
    using ExprVariant = std::variant<NumberExpr, VariableExpr, UnaryExpr,
                                     BinaryExpr, TernaryExpr, CallExpr,
                                     PropAccessExpr, StaticRelPixelAccessExpr,
                                     FrameDimensionExpr, ArrayAccessExpr>;

    ExprVariant value;

    template <typename T> explicit Expr(T&& v) : value(std::forward<T>(v)) {}

    Expr(const Expr&) = delete;
    Expr& operator=(const Expr&) = delete;
    Expr(Expr&&) = default;
    Expr& operator=(Expr&&) = default;

    int line() const {
        return std::visit([](const auto& e) { return e.line; }, value);
    }
};

struct Stmt {
    using StmtVariant = std::variant<ExprStmt, AssignStmt, BlockStmt, IfStmt,
                                     WhileStmt, ReturnStmt, LabelStmt, GotoStmt,
                                     FunctionDef, GlobalDecl, ArrayAssignStmt>;

    StmtVariant value;

    template <typename T> explicit Stmt(T&& v) : value(std::forward<T>(v)) {}

    Stmt(const Stmt&) = delete;
    Stmt& operator=(const Stmt&) = delete;
    Stmt(Stmt&&) = default;
    Stmt& operator=(Stmt&&) = default;

    int line() const {
        return std::visit([](const auto& s) { return s.line; }, value);
    }
};

inline FrameDimensionExpr::FrameDimensionExpr(Token keyword,
                                              std::unique_ptr<Expr> plane)
    : dimension_name(std::move(keyword.value)),
      plane_index_expr(std::move(plane)) {
    line = keyword.line;
}

inline UnaryExpr::UnaryExpr(Token o, std::unique_ptr<Expr> r)
    : op(std::move(o)), right(std::move(r)) {
    line = op.line;
}

inline BinaryExpr::BinaryExpr(std::unique_ptr<Expr> l, Token o,
                              std::unique_ptr<Expr> r)
    : left(std::move(l)), op(std::move(o)), right(std::move(r)) {
    line = op.line;
}

inline TernaryExpr::TernaryExpr(std::unique_ptr<Expr> c,
                                std::unique_ptr<Expr> t,
                                std::unique_ptr<Expr> f)
    : cond(std::move(c)), true_expr(std::move(t)), false_expr(std::move(f)) {
    if (cond)
        line = cond->line();
}

inline CallExpr::CallExpr(Token callee_token,
                          std::vector<std::unique_ptr<Expr>> a,
                          std::string suffix)
    : callee(std::move(callee_token.value)), args(std::move(a)),
      boundary_suffix(std::move(suffix)) {
    line = callee_token.line;
}

inline ExprStmt::ExprStmt(std::unique_ptr<Expr> e) : expr(std::move(e)) {
    if (expr)
        line = expr->line();
}

inline AssignStmt::AssignStmt(Token n, std::unique_ptr<Expr> v)
    : name(std::move(n)), value(std::move(v)) {
    line = name.line;
}

inline BlockStmt::BlockStmt(std::vector<std::unique_ptr<Stmt>> s)
    : statements(std::move(s)) {
    if (!statements.empty())
        line = statements.front()->line();
}

inline IfStmt::IfStmt(std::unique_ptr<Expr> c, std::unique_ptr<Stmt> t,
                      std::unique_ptr<Stmt> e)
    : condition(std::move(c)), then_branch(std::move(t)),
      else_branch(std::move(e)) {
    if (condition)
        line = condition->line();
}

inline WhileStmt::WhileStmt(std::unique_ptr<Expr> c, std::unique_ptr<Stmt> b)
    : condition(std::move(c)), body(std::move(b)) {
    if (condition)
        line = condition->line();
}

inline ReturnStmt::ReturnStmt(Token kw, std::unique_ptr<Expr> v)
    : keyword(std::move(kw)), value(std::move(v)) {
    line = keyword.line;
}

inline GotoStmt::GotoStmt(Token kw, Token l, std::unique_ptr<Expr> c)
    : keyword(std::move(kw)), label(std::move(l)), condition(std::move(c)) {
    line = keyword.line;
}

inline FunctionDef::FunctionDef(Token n, std::vector<Parameter> p,
                                std::unique_ptr<BlockStmt> b,
                                std::unique_ptr<GlobalDecl> g)
    : name(std::move(n)), params(std::move(p)), body(std::move(b)),
      global_decl(std::move(g)) {
    line = name.line;
}

inline ArrayAccessExpr::ArrayAccessExpr(std::unique_ptr<Expr> arr,
                                        std::unique_ptr<Expr> idx)
    : array(std::move(arr)), index(std::move(idx)) {
    if (array)
        line = array->line();
}

inline ArrayAssignStmt::ArrayAssignStmt(std::unique_ptr<Expr> t,
                                        std::unique_ptr<Expr> v)
    : target(std::move(t)), value(std::move(v)) {
    if (target)
        line = target->line();
}

struct Program {
    std::vector<std::unique_ptr<Stmt>> statements;
};

template <typename Wrapper, typename T, typename... Args>
auto make_node(Args&&... args) {
    static_assert(std::is_same_v<Wrapper, Expr> ||
                      std::is_same_v<Wrapper, Stmt>,
                  "Wrapper must be Expr or Stmt");
    return std::make_unique<Wrapper>(T(std::forward<Args>(args)...));
}

template <typename T, typename Wrapper>
auto get_if(Wrapper* wrapper) -> decltype(std::get_if<T>(&wrapper->value)) {
    if (!wrapper)
        return nullptr;
    return std::get_if<T>(&wrapper->value);
}

} // namespace infix2postfix

#endif
