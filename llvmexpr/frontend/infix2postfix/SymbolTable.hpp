#ifndef LLVMEXPR_INFIX2POSTFIX_SYMBOLTABLE_HPP
#define LLVMEXPR_INFIX2POSTFIX_SYMBOLTABLE_HPP

#include "Symbol.hpp"
#include <map>
#include <memory>
#include <string>

namespace infix2postfix {

class SymbolTable {
  public:
    explicit SymbolTable(SymbolTable* parent = nullptr);

    bool define(std::shared_ptr<Symbol> symbol);

    std::shared_ptr<Symbol> resolve(const std::string& name) const;

    SymbolTable* get_parent() const { return parent; }

  private:
    SymbolTable* parent;
    std::map<std::string, std::shared_ptr<Symbol>> symbols;
};

} // namespace infix2postfix

#endif
