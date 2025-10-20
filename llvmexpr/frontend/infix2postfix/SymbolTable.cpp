#include "SymbolTable.hpp"

namespace infix2postfix {

SymbolTable::SymbolTable(SymbolTable* parent) : parent(parent) {}

bool SymbolTable::define(std::shared_ptr<Symbol> symbol) {
    if (symbols.count(symbol->name)) {
        return false;
    }
    symbols[symbol->name] = symbol;
    return true;
}

std::shared_ptr<Symbol> SymbolTable::resolve(const std::string& name) const {
    auto it = symbols.find(name);
    if (it != symbols.end()) {
        return it->second;
    }
    if (parent) {
        return parent->resolve(name);
    }
    return nullptr;
}

} // namespace infix2postfix
