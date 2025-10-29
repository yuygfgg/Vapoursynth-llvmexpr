/**
 * Copyright (C) 2025 yuygfgg
 * 
 * This file is part of Vapoursynth-llvmexpr.
 * 
 * Vapoursynth-llvmexpr is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Vapoursynth-llvmexpr is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with Vapoursynth-llvmexpr.  If not, see <https://www.gnu.org/licenses/>.
 */

#include "SymbolTable.hpp"

namespace infix2postfix {

SymbolTable::SymbolTable(SymbolTable* parent) : parent(parent) {}

bool SymbolTable::define(const std::shared_ptr<Symbol>& symbol) {
    if (symbols.contains(symbol->name)) {
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
    if (parent != nullptr) {
        return parent->resolve(name);
    }
    return nullptr;
}

} // namespace infix2postfix
