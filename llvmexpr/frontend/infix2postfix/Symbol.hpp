#ifndef LLVMEXPR_INFIX2POSTFIX_SYMBOL_HPP
#define LLVMEXPR_INFIX2POSTFIX_SYMBOL_HPP

#include "types.hpp"
#include <string>

namespace infix2postfix {

struct Stmt;
struct Expr;

enum class SymbolKind {
    VARIABLE,
    FUNCTION,
    PARAMETER,
    LABEL
};

struct Symbol {
    SymbolKind kind;
    std::string name;
    Type type;
    int definition_line;

    const FunctionSignature* signature = nullptr;
};

} // namespace infix2postfix

#endif

