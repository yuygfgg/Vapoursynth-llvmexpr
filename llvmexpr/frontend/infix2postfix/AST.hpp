#ifndef LLVMEXPR_INFIX2POSTFIX_AST_HPP
#define LLVMEXPR_INFIX2POSTFIX_AST_HPP

#include "types.hpp"
#include <memory>
#include <string>
#include <vector>

namespace infix2postfix {

struct NumberExpr;
struct VariableExpr;
struct UnaryExpr;
struct BinaryExpr;
struct TernaryExpr;
struct CallExpr;
struct PropAccessExpr;
struct StaticRelPixelAccessExpr;
struct FrameDimensionExpr;
struct ExprStmt;
struct AssignStmt;
struct BlockStmt;
struct IfStmt;
struct WhileStmt;
struct ReturnStmt;
struct LabelStmt;
struct GotoStmt;
struct FunctionDef;
struct GlobalDecl;

struct ExprVisitor {
    virtual ~ExprVisitor() = default;
    virtual std::string visit(NumberExpr& expr) = 0;
    virtual std::string visit(VariableExpr& expr) = 0;
    virtual std::string visit(UnaryExpr& expr) = 0;
    virtual std::string visit(BinaryExpr& expr) = 0;
    virtual std::string visit(TernaryExpr& expr) = 0;
    virtual std::string visit(CallExpr& expr) = 0;
    virtual std::string visit(PropAccessExpr& expr) = 0;
    virtual std::string visit(StaticRelPixelAccessExpr& expr) = 0;
    virtual std::string visit(FrameDimensionExpr& expr) = 0;
};

struct StmtVisitor {
    virtual ~StmtVisitor() = default;
    virtual std::string visit(ExprStmt& stmt) = 0;
    virtual std::string visit(AssignStmt& stmt) = 0;
    virtual std::string visit(BlockStmt& stmt) = 0;
    virtual std::string visit(IfStmt& stmt) = 0;
    virtual std::string visit(WhileStmt& stmt) = 0;
    virtual std::string visit(ReturnStmt& stmt) = 0;
    virtual std::string visit(LabelStmt& stmt) = 0;
    virtual std::string visit(GotoStmt& stmt) = 0;
    virtual std::string visit(FunctionDef& stmt) = 0;
    virtual std::string visit(GlobalDecl& stmt) = 0;
};

struct Node {
    virtual ~Node() = default;
    int line = 0;
};

struct Expr : public Node {
    virtual std::string accept(ExprVisitor& visitor) = 0;
};

struct Stmt : public Node {
    virtual std::string accept(StmtVisitor& visitor) = 0;
};

struct NumberExpr : public Expr {
    Token value;
    explicit NumberExpr(Token val) : value(std::move(val)) {
        line = value.line;
    }
    std::string accept(ExprVisitor& visitor) override {
        return visitor.visit(*this);
    }
};

struct VariableExpr : public Expr {
    Token name;
    explicit VariableExpr(Token n) : name(std::move(n)) { line = name.line; }
    std::string accept(ExprVisitor& visitor) override {
        return visitor.visit(*this);
    }
};

struct UnaryExpr : public Expr {
    Token op;
    std::unique_ptr<Expr> right;
    UnaryExpr(Token o, std::unique_ptr<Expr> r)
        : op(std::move(o)), right(std::move(r)) {
        line = op.line;
    }
    std::string accept(ExprVisitor& visitor) override {
        return visitor.visit(*this);
    }
};

struct BinaryExpr : public Expr {
    std::unique_ptr<Expr> left;
    Token op;
    std::unique_ptr<Expr> right;
    BinaryExpr(std::unique_ptr<Expr> l, Token o, std::unique_ptr<Expr> r)
        : left(std::move(l)), op(std::move(o)), right(std::move(r)) {
        line = op.line;
    }
    std::string accept(ExprVisitor& visitor) override {
        return visitor.visit(*this);
    }
};

struct TernaryExpr : public Expr {
    std::unique_ptr<Expr> cond;
    std::unique_ptr<Expr> true_expr;
    std::unique_ptr<Expr> false_expr;
    TernaryExpr(std::unique_ptr<Expr> c, std::unique_ptr<Expr> t,
                std::unique_ptr<Expr> f)
        : cond(std::move(c)), true_expr(std::move(t)),
          false_expr(std::move(f)) {
        if (cond)
            line = cond->line;
    }
    std::string accept(ExprVisitor& visitor) override {
        return visitor.visit(*this);
    }
};

struct CallExpr : public Expr {
    std::string callee;
    std::vector<std::unique_ptr<Expr>> args;
    std::string boundary_suffix;
    CallExpr(Token callee_token, std::vector<std::unique_ptr<Expr>> a,
             std::string suffix = "")
        : callee(std::move(callee_token.value)), args(std::move(a)),
          boundary_suffix(std::move(suffix)) {
        line = callee_token.line;
    }
    std::string accept(ExprVisitor& visitor) override {
        return visitor.visit(*this);
    }
};

struct PropAccessExpr : public Expr {
    Token clip;
    Token prop;
    PropAccessExpr(Token c, Token p) : clip(std::move(c)), prop(std::move(p)) {
        line = clip.line;
    }
    std::string accept(ExprVisitor& visitor) override {
        return visitor.visit(*this);
    }
};

struct StaticRelPixelAccessExpr : public Expr {
    Token clip;
    Token offsetX;
    Token offsetY;
    std::string boundary_suffix;
    StaticRelPixelAccessExpr(Token c, Token x, Token y, std::string suffix)
        : clip(std::move(c)), offsetX(std::move(x)), offsetY(std::move(y)),
          boundary_suffix(std::move(suffix)) {
        line = clip.line;
    }
    std::string accept(ExprVisitor& visitor) override {
        return visitor.visit(*this);
    }
};

struct FrameDimensionExpr : public Expr {
    std::string dimension_name; // "width" or "height"
    Token plane_index;
    FrameDimensionExpr(Token keyword, Token plane)
        : dimension_name(std::move(keyword.value)),
          plane_index(std::move(plane)) {
        line = keyword.line;
    }
    std::string accept(ExprVisitor& visitor) override {
        return visitor.visit(*this);
    }
};

struct ExprStmt : public Stmt {
    std::unique_ptr<Expr> expr;
    explicit ExprStmt(std::unique_ptr<Expr> e) : expr(std::move(e)) {
        if (expr)
            line = expr->line;
    }
    std::string accept(StmtVisitor& visitor) override {
        return visitor.visit(*this);
    }
};

struct AssignStmt : public Stmt {
    Token name;
    std::unique_ptr<Expr> value;
    AssignStmt(Token n, std::unique_ptr<Expr> v)
        : name(std::move(n)), value(std::move(v)) {
        line = name.line;
    }
    std::string accept(StmtVisitor& visitor) override {
        return visitor.visit(*this);
    }
};

struct BlockStmt : public Stmt {
    std::vector<std::unique_ptr<Stmt>> statements;
    explicit BlockStmt(std::vector<std::unique_ptr<Stmt>> s)
        : statements(std::move(s)) {
        if (!statements.empty())
            line = statements.front()->line;
    }
    std::string accept(StmtVisitor& visitor) override {
        return visitor.visit(*this);
    }
};

struct IfStmt : public Stmt {
    std::unique_ptr<Expr> condition;
    std::unique_ptr<Stmt> then_branch;
    std::unique_ptr<Stmt> else_branch;
    IfStmt(std::unique_ptr<Expr> c, std::unique_ptr<Stmt> t,
           std::unique_ptr<Stmt> e)
        : condition(std::move(c)), then_branch(std::move(t)),
          else_branch(std::move(e)) {
        if (condition)
            line = condition->line;
    }
    std::string accept(StmtVisitor& visitor) override {
        return visitor.visit(*this);
    }
};

struct WhileStmt : public Stmt {
    std::unique_ptr<Expr> condition;
    std::unique_ptr<Stmt> body;
    WhileStmt(std::unique_ptr<Expr> c, std::unique_ptr<Stmt> b)
        : condition(std::move(c)), body(std::move(b)) {
        if (condition)
            line = condition->line;
    }
    std::string accept(StmtVisitor& visitor) override {
        return visitor.visit(*this);
    }
};

struct ReturnStmt : public Stmt {
    Token keyword;
    std::unique_ptr<Expr> value;
    ReturnStmt(Token kw, std::unique_ptr<Expr> v)
        : keyword(std::move(kw)), value(std::move(v)) {
        line = keyword.line;
    }
    std::string accept(StmtVisitor& visitor) override {
        return visitor.visit(*this);
    }
};

struct LabelStmt : public Stmt {
    Token name;
    explicit LabelStmt(Token n) : name(std::move(n)) { line = name.line; }
    std::string accept(StmtVisitor& visitor) override {
        return visitor.visit(*this);
    }
};

struct GotoStmt : public Stmt {
    Token keyword;
    Token label;
    std::unique_ptr<Expr> condition;
    GotoStmt(Token kw, Token l, std::unique_ptr<Expr> c)
        : keyword(std::move(kw)), label(std::move(l)), condition(std::move(c)) {
        line = keyword.line;
    }
    std::string accept(StmtVisitor& visitor) override {
        return visitor.visit(*this);
    }
};

struct GlobalDecl : public Stmt {
    Token keyword;
    GlobalMode mode;
    std::vector<Token> globals;
    GlobalDecl(Token t, GlobalMode m, std::vector<Token> g = {})
        : keyword(std::move(t)), mode(m), globals(std::move(g)) {
        line = keyword.line;
    }
    std::string accept(StmtVisitor& visitor) override {
        return visitor.visit(*this);
    }
};

struct FunctionDef : public Stmt {
    Token name;
    std::vector<Token> params;
    std::unique_ptr<BlockStmt> body;
    std::unique_ptr<GlobalDecl> global_decl;
    FunctionDef(Token n, std::vector<Token> p, std::unique_ptr<BlockStmt> b,
                std::unique_ptr<GlobalDecl> g)
        : name(std::move(n)), params(std::move(p)), body(std::move(b)),
          global_decl(std::move(g)) {
        line = name.line;
    }
    std::string accept(StmtVisitor& visitor) override {
        return visitor.visit(*this);
    }
};

struct Program : public Node {
    std::vector<std::unique_ptr<Stmt>> statements;
};

} // namespace infix2postfix

#endif