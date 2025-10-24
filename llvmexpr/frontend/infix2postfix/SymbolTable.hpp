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

    bool define(const std::shared_ptr<Symbol>& symbol);

    [[nodiscard]] std::shared_ptr<Symbol>
    resolve(const std::string& name) const;

    [[nodiscard]] SymbolTable* get_parent() const { return parent; }

    [[nodiscard]] const std::map<std::string, std::shared_ptr<Symbol>>&
    get_symbols() const {
        return symbols;
    }

  private:
    SymbolTable* parent;
    std::map<std::string, std::shared_ptr<Symbol>> symbols;
};

} // namespace infix2postfix

#endif
