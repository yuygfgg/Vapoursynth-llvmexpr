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

#include "InfixConverter.hpp"
#include <format>
#include <stdexcept>

#include "infix2postfix/CodeGenerator.hpp"
#include "infix2postfix/Parser.hpp"
#include "infix2postfix/Tokenizer.hpp"

std::string convertInfixToPostfixExpr(const std::string& infix_expr) {
    try {
        infix2postfix::Tokenizer tokenizer(infix_expr);
        auto tokens = tokenizer.tokenize();

        infix2postfix::Parser parser(tokens);
        auto ast = parser.parse();

        infix2postfix::CodeGenerator generator(infix2postfix::Mode::Expr);
        return generator.generate(ast.get());
    } catch (const std::exception& e) {
        throw std::runtime_error(
            std::format("Infix to postfix conversion error: {}", e.what()));
    }
}

std::string convertInfixToPostfixSingle(const std::string& infix_expr) {
    try {
        infix2postfix::Tokenizer tokenizer(infix_expr);
        auto tokens = tokenizer.tokenize();

        infix2postfix::Parser parser(tokens);
        auto ast = parser.parse();

        infix2postfix::CodeGenerator generator(infix2postfix::Mode::Single);
        return generator.generate(ast.get());
    } catch (const std::exception& e) {
        throw std::runtime_error(
            std::format("Infix to postfix conversion error: {}", e.what()));
    }
}
