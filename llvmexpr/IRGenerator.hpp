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

#ifndef LLVMEXPR_IRGENERATOR_HPP
#define LLVMEXPR_IRGENERATOR_HPP

#include <map>
#include <string>
#include <vector>

#include "VapourSynth4.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Module.h"

#include "Analysis.hpp"
#include "Tokenizer.hpp"
#include "utils/Math.hpp"

class IRGenerator {
  public:
    IRGenerator(const std::vector<Token>& tokens_in, const VSVideoInfo* out_vi,
                const std::vector<const VSVideoInfo*>& in_vi, int width_in,
                int height_in, bool mirror,
                const std::map<std::pair<int, std::string>, int>& p_map,
                const std::vector<CFGBlock>& cfg_blocks_in,
                const std::map<std::string, int>& label_to_block_idx_in,
                const std::vector<int>& stack_depth_in_in,
                llvm::LLVMContext& context_ref, llvm::Module& module_ref,
                llvm::IRBuilder<>& builder_ref, MathLibraryManager& math_mgr,
                std::string func_name_in, int approx_math_in);

    // Main generation method
    void generate();

  private:
    // Input parameters
    const std::vector<Token>& tokens;
    const VSVideoInfo* vo;
    const std::vector<const VSVideoInfo*>& vi;
    int num_inputs;
    int width;
    int height;
    bool mirror_boundary;
    const std::map<std::pair<int, std::string>, int>& prop_map;
    const std::vector<CFGBlock>& cfg_blocks;
    const std::map<std::string, int>& label_to_block_idx;
    const std::vector<int>& stack_depth_in;
    std::string func_name;
    int approx_math;

    // LLVM objects (references)
    llvm::LLVMContext& context;
    llvm::Module& module;
    llvm::IRBuilder<>& builder;
    MathLibraryManager& math_manager;

    // Function and arguments
    llvm::Function* func;
    llvm::Value* rwptrs_arg;
    llvm::Value* strides_arg;
    llvm::Value* props_arg;

    // Loop-invariant caches
    std::vector<llvm::Value*> preloaded_base_ptrs;
    std::vector<llvm::Value*> preloaded_strides;

    // Alias scope metadata
    llvm::MDNode* alias_scope_domain;
    std::vector<llvm::MDNode*> alias_scopes;
    std::vector<llvm::MDNode*> alias_scope_lists;
    std::vector<llvm::MDNode*> noalias_scope_lists;

    // Row ptr cache
    std::vector<RelYAccess> unique_rel_y_accesses;
    std::map<RelYAccess, llvm::Value*> row_ptr_cache;
    int min_rel_x;
    int max_rel_x;

    // Flags
    bool uses_x;
    bool uses_y;

    // Helper methods
    llvm::AllocaInst* createAllocaInEntry(llvm::Type* type,
                                          const std::string& name);

    template <typename... Args>
    llvm::Value* createIntrinsicCall(llvm::Intrinsic::ID intrinsic_id,
                                     Args... args);

    void assumeAligned(llvm::Value* ptrValue, unsigned alignment);

    template <typename MemInstT>
    void setMemoryInstAttrs(MemInstT* inst, unsigned alignment,
                            int rwptr_index);

    void define_function_signature();
    void collect_rel_y_accesses();
    void collect_rel_x_accesses();
    llvm::Value* get_final_coord(llvm::Value* coord, llvm::Value* max_dim,
                                 bool use_mirror);
    llvm::Value* generate_load_from_row_ptr(llvm::Value* row_ptr, int clip_idx,
                                            llvm::Value* x, int rel_x,
                                            bool use_mirror,
                                            bool no_x_bounds_check);
    void add_loop_metadata(llvm::BranchInst* loop_br);
    void generate_loops();
    void generate_x_loop_body(llvm::Value* x_var, llvm::Value* x_fp_var,
                              llvm::Value* y_var, llvm::Value* y_fp_var,
                              bool no_x_bounds_check);
    void generate_ir_from_tokens(llvm::Value* x, llvm::Value* y,
                                 llvm::Value* x_fp, llvm::Value* y_fp,
                                 bool no_x_bounds_check);
    llvm::Value* generate_pixel_load(int clip_idx, llvm::Value* x,
                                     llvm::Value* y, bool mirror);
    void generate_pixel_store(llvm::Value* value_to_store, llvm::Value* x,
                              llvm::Value* y);
};

template <typename... Args>
llvm::Value* IRGenerator::createIntrinsicCall(llvm::Intrinsic::ID intrinsic_id,
                                              Args... args) {
    static_assert(sizeof...(Args) >= 1, "At least one argument required");
    llvm::SmallVector<llvm::Value*, 4> arg_vec{args...};
    auto* callee = llvm::Intrinsic::getOrInsertDeclaration(
        &module, intrinsic_id, {arg_vec[0]->getType()});
    auto* call = builder.CreateCall(callee, arg_vec);
    call->setFastMathFlags(builder.getFastMathFlags());
    return call;
}

template <typename MemInstT>
void IRGenerator::setMemoryInstAttrs(MemInstT* inst, unsigned alignment,
                                     int rwptr_index) {
    inst->setAlignment(llvm::Align(alignment));
    inst->setMetadata(llvm::LLVMContext::MD_alias_scope,
                      alias_scope_lists[rwptr_index]);
    inst->setMetadata(llvm::LLVMContext::MD_noalias,
                      noalias_scope_lists[rwptr_index]);
}

#endif // LLVMEXPR_IRGENERATOR_HPP
