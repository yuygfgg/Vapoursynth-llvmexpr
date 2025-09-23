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

#ifndef LLVMEXPR_MATH_LIBRARY_HPP
#define LLVMEXPR_MATH_LIBRARY_HPP

#include "math_compiler.hpp"
#include "math_config.hpp"
#include <map>
#include <string>

class MathLibraryManager {
  public:
    MathLibraryManager(llvm::Module* module, llvm::LLVMContext& context)
        : module_(module), context_(context) {}

    // The only, concise interface for Compiler
    llvm::Function* getScalarFunction(MathOp op) {
        if (auto it = scalarFuncCache_.find(op); it != scalarFuncCache_.end()) {
            return it->second;
        }
        return generateAndCache(op);
    }

  private:
    llvm::Module* module_;
    llvm::LLVMContext& context_;
    std::map<MathOp, llvm::Function*> scalarFuncCache_;

    // Template dispatch: Map MathOp enum to concrete generator methods
    template <MathOp op, int VectorWidth> llvm::Function* dispatchGeneration() {
        MathFunctionGenerator<VectorWidth> generator(module_, context_);
        if constexpr (op == MathOp::Exp)
            return generator.getOrCreateExp();
        if constexpr (op == MathOp::Log)
            return generator.getOrCreateLog();
        // if constexpr (op == MathOp::Pow)
            // return generator.getOrCreatePow();
        if constexpr (op == MathOp::Sin)
            return generator.getOrCreateSin();
        if constexpr (op == MathOp::Cos)
            return generator.getOrCreateCos();
        if constexpr (op == MathOp::Tan)
            return generator.getOrCreateTan();
        return nullptr;
    }

    // Template implementation: Core of automated generation and linking
    template <MathOp op> llvm::Function* generateAndCacheImpl() {
        llvm::Function* scalarFunc = dispatchGeneration<op, 1>();
        if (!scalarFunc) {
            return nullptr;
        }

        auto link_vectors = [&]<int... Widths>(
                                std::integer_sequence<int, Widths...>) {
            (
                [&] {
                    // Generate vector function for each width and link it to scalar version
                    llvm::Function* vecFunc = dispatchGeneration<op, Widths>();
                    if (vecFunc) {
                        // Vector function ABI variant attribute
                        std::string abi_string = "_ZGV";

                        // ISA identifier based on vector width
                        if constexpr (Widths == 4) {
                            #ifdef __SSE__
                            abi_string += "b"; // SSE
                            #elif defined(__ARM_NEON__)
                            abi_string += "n"; // NEON
                            #endif
                        } else if constexpr (Widths == 8) {
                            #ifdef __AVX2__
                            abi_string += "d"; // AVX2
                            #endif
                        } else if constexpr (Widths == 16) {
                            #ifdef __AVX512F__
                            abi_string += "e"; // AVX-512
                            #endif
                        }

                        abi_string += "N"; // No mask

                        abi_string += std::to_string(Widths);
                        
                        // Parameters
                        // if constexpr (op == MathOp::Pow) {
                            // abi_string += "vv";
                        // } else {
                            abi_string += "v";
                        // }

                        abi_string += "_";
                        abi_string += scalarFunc->getName().str();
                        abi_string += "(";
                        abi_string += vecFunc->getName().str();
                        abi_string += ")";


                        // Add the vector function ABI variant attribute to scalar function
                        scalarFunc->addFnAttr(llvm::Attribute::get(
                            context_, "vector-function-abi-variant",
                            abi_string));
                    }
                }(),
                ...);
        };

        // Start iteration
        link_vectors(SupportedVectorWidths{});

        scalarFuncCache_[op] = scalarFunc;
        return scalarFunc;
    }

    llvm::Function* generateAndCache(MathOp op) {
        switch (op) {
        case MathOp::Exp:
            return generateAndCacheImpl<MathOp::Exp>();
        case MathOp::Log:
            return generateAndCacheImpl<MathOp::Log>();
        case MathOp::Sin:
            return generateAndCacheImpl<MathOp::Sin>();
        case MathOp::Cos:
            return generateAndCacheImpl<MathOp::Cos>();
        case MathOp::Tan:
            return generateAndCacheImpl<MathOp::Tan>();
        }
        return nullptr;
    }
};

#endif // LLVMEXPR_MATH_LIBRARY_HPP
