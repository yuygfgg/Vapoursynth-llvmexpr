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

#include "IRGenerator.hpp"

#include <algorithm>
#include <array>
#include <bit>
#include <format>
#include <numbers>
#include <numeric>
#include <set>
#include <stdexcept>
#include <unordered_map>

#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Value.h"
#include "llvm/TargetParser/Host.h"

#include "utils/Sorting.hpp"

constexpr unsigned ALIGNMENT = 32; // Vapoursynth should guarantee this
constexpr uint32_t EXIT_NAN_PAYLOAD = 0x7FC0E71F; // qNaN with payload 0xE71F

IRGenerator::IRGenerator(
    const std::vector<Token>& tokens_in, const VSVideoInfo* out_vi,
    const std::vector<const VSVideoInfo*>& in_vi, int width_in, int height_in,
    bool mirror, const std::map<std::pair<int, std::string>, int>& p_map,
    const ExpressionAnalysisResults& analysis_results_in,
    llvm::LLVMContext& context_ref, llvm::Module& module_ref,
    llvm::IRBuilder<>& builder_ref, MathLibraryManager& math_mgr,
    std::string func_name_in, int approx_math_in)
    : tokens(tokens_in), vo(out_vi), vi(in_vi), num_inputs(in_vi.size()),
      width(width_in), height(height_in), mirror_boundary(mirror),
      prop_map(p_map), analysis_results(analysis_results_in),
      func_name(std::move(func_name_in)), approx_math(approx_math_in),
      context(context_ref), module(module_ref), builder(builder_ref),
      math_manager(math_mgr), func(nullptr), rwptrs_arg(nullptr),
      strides_arg(nullptr), props_arg(nullptr), alias_scope_domain(nullptr),
      min_rel_x(0), max_rel_x(0), uses_x(false), uses_y(false) {

    for (const auto& token : tokens) {
        if (token.type == TokenType::CONSTANT_X)
            uses_x = true;
        if (token.type == TokenType::CONSTANT_Y)
            uses_y = true;
        if (uses_x && uses_y)
            break;
    }
}

void IRGenerator::generate() {
    collect_rel_y_accesses();
    collect_rel_x_accesses();
    define_function_signature();
    generate_loops();
}

llvm::AllocaInst* IRGenerator::createAllocaInEntry(llvm::Type* type,
                                                   const std::string& name) {
    llvm::IRBuilder<> entryBuilder(&func->getEntryBlock(),
                                   func->getEntryBlock().begin());
    return entryBuilder.CreateAlloca(type, nullptr, name);
}

void IRGenerator::assumeAligned(llvm::Value* ptrValue, unsigned alignment) {
    llvm::Function* assumeFn = llvm::Intrinsic::getOrInsertDeclaration(
        &module, llvm::Intrinsic::assume);
    llvm::Value* cond = builder.getInt1(true);
    llvm::SmallVector<llvm::Value*, 2> args;
    args.push_back(ptrValue);
    args.push_back(builder.getInt64(static_cast<uint64_t>(alignment)));
    llvm::OperandBundleDefT<llvm::Value*> alignBundle("align", args);
    builder.CreateCall(assumeFn, {cond}, {alignBundle});
}

void IRGenerator::define_function_signature() {
    llvm::Type* void_ty = llvm::Type::getVoidTy(context);
    llvm::Type* ptr_ty = llvm::PointerType::get(context, 0);
    llvm::Type* i8_ptr_ptr_ty = ptr_ty; // opaque pointer (represents uint8_t**)
    llvm::Type* i32_ptr_ty = ptr_ty;    // opaque pointer (represents int32_t*)
    llvm::Type* float_ptr_ty = ptr_ty;  // opaque pointer (represents float*)

    llvm::FunctionType* func_ty = llvm::FunctionType::get(
        void_ty, {i8_ptr_ptr_ty, i32_ptr_ty, float_ptr_ty}, false);

    func = llvm::Function::Create(func_ty, llvm::Function::ExternalLinkage,
                                  func_name, &module);
    func->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::None);

    auto args = func->arg_begin();
    rwptrs_arg = &*args++;
    rwptrs_arg->setName("rwptrs");
    strides_arg = &*args++;
    strides_arg->setName("strides");
    props_arg = &*args++;
    props_arg->setName("props");

    func->addParamAttr(1, llvm::Attribute::ReadOnly); // strides (int32_t*)
    func->addParamAttr(2, llvm::Attribute::ReadOnly); // props (float*)
}

void IRGenerator::collect_rel_y_accesses() {
    std::set<RelYAccess> seen;
    for (const auto& token : tokens) {
        if (token.type == TokenType::CLIP_REL) {
            const auto& payload =
                std::get<TokenPayload_ClipAccess>(token.payload);
            bool use_mirror =
                payload.has_mode ? payload.use_mirror : mirror_boundary;
            RelYAccess access{payload.clip_idx, payload.rel_y, use_mirror};
            if (seen.find(access) == seen.end()) {
                seen.insert(access);
                unique_rel_y_accesses.push_back(access);
            }
        } else if (token.type == TokenType::CLIP_CUR) {
            const auto& payload =
                std::get<TokenPayload_ClipAccess>(token.payload);
            RelYAccess access{payload.clip_idx, 0, mirror_boundary};
            if (seen.find(access) == seen.end()) {
                seen.insert(access);
                unique_rel_y_accesses.push_back(access);
            }
        }
    }
}

void IRGenerator::collect_rel_x_accesses() {
    for (const auto& token : tokens) {
        if (token.type == TokenType::CLIP_REL) {
            const auto& payload =
                std::get<TokenPayload_ClipAccess>(token.payload);
            min_rel_x = std::min(min_rel_x, payload.rel_x);
            max_rel_x = std::max(max_rel_x, payload.rel_x);
        }
    }
}

llvm::Value* IRGenerator::get_final_coord(llvm::Value* coord,
                                          llvm::Value* max_dim,
                                          bool use_mirror) {
    llvm::Value* zero = builder.getInt32(0);
    llvm::Value* one = builder.getInt32(1);

    llvm::Value* result;
    if (use_mirror) {
        // idx = abs(idx); N = 2*(len-1); if (N==0) N=1; r = idx % N;
        // result = (len-1) - abs(r - (len-1))
        llvm::Function* abs_func = llvm::Intrinsic::getOrInsertDeclaration(
            &module, llvm::Intrinsic::abs, {builder.getInt32Ty()});
        auto abs_coord =
            builder.CreateCall(abs_func, {coord, builder.getInt1(false)});

        auto dim_minus_1 = builder.CreateSub(max_dim, one);
        auto twice_dim_minus_1 =
            builder.CreateMul(dim_minus_1, builder.getInt32(2));

        // base = base | (base==0 ? 1 : 0)
        auto base_is_zero = builder.CreateICmpEQ(twice_dim_minus_1, zero);
        auto one_if_zero =
            builder.CreateZExt(base_is_zero, builder.getInt32Ty());
        auto safe_mod_base = builder.CreateOr(twice_dim_minus_1, one_if_zero);

        auto rem = builder.CreateURem(abs_coord, safe_mod_base);

        // result = (len-1) - abs(rem - (len-1))
        auto diff = builder.CreateSub(rem, dim_minus_1);
        auto abs_diff =
            builder.CreateCall(abs_func, {diff, builder.getInt1(false)});
        result = builder.CreateSub(dim_minus_1, abs_diff);
    } else { // Clamping
        // clamp(coord, 0, max_dim - 1)
        auto dim_minus_1 = builder.CreateSub(max_dim, one);

        llvm::Function* smax_func = llvm::Intrinsic::getOrInsertDeclaration(
            &module, llvm::Intrinsic::smax, {builder.getInt32Ty()});
        llvm::Function* smin_func = llvm::Intrinsic::getOrInsertDeclaration(
            &module, llvm::Intrinsic::smin, {builder.getInt32Ty()});

        auto clamped_at_zero = builder.CreateCall(smax_func, {coord, zero});
        result = builder.CreateCall(smin_func, {clamped_at_zero, dim_minus_1});
    }

    return result;
}

llvm::Value* IRGenerator::generate_load_from_row_ptr(llvm::Value* row_ptr,
                                                     int clip_idx,
                                                     llvm::Value* x, int rel_x,
                                                     bool use_mirror,
                                                     bool no_x_bounds_check) {
    const VSVideoInfo* vinfo = vi[clip_idx];
    llvm::Value* coord_x = builder.CreateAdd(x, builder.getInt32(rel_x));
    llvm::Value* final_x;
    if (no_x_bounds_check) {
        final_x = coord_x;
    } else {
        final_x = get_final_coord(coord_x, builder.getInt32(width), use_mirror);
    }

    const VSVideoFormat& format = vinfo->format;
    int bpp = format.bytesPerSample;
    int vs_clip_idx = clip_idx + 1;

    llvm::Value* x_offset = builder.CreateMul(final_x, builder.getInt32(bpp));
    llvm::Value* pixel_addr =
        builder.CreateGEP(builder.getInt8Ty(), row_ptr, x_offset);

    int pixel_align = std::gcd(ALIGNMENT, bpp);
    assumeAligned(pixel_addr, static_cast<unsigned>(pixel_align));

    llvm::Value* loaded_val;
    if (format.sampleType == stInteger) {
        llvm::Type* load_type =
            bpp == 1 ? builder.getInt8Ty()
                     : (bpp == 2 ? builder.getInt16Ty() : builder.getInt32Ty());
        llvm::LoadInst* li = builder.CreateLoad(load_type, pixel_addr);
        setMemoryInstAttrs(li, static_cast<unsigned>(pixel_align), vs_clip_idx);
        loaded_val = builder.CreateZExtOrBitCast(li, builder.getInt32Ty());
        return builder.CreateUIToFP(loaded_val, builder.getFloatTy());
    } else { // stFloat
        if (bpp == 4) {
            llvm::LoadInst* li =
                builder.CreateLoad(builder.getFloatTy(), pixel_addr);
            setMemoryInstAttrs(li, static_cast<unsigned>(pixel_align),
                               vs_clip_idx);
            return li;
        } else if (bpp == 2) {
            llvm::LoadInst* li =
                builder.CreateLoad(builder.getHalfTy(), pixel_addr);
            setMemoryInstAttrs(li, static_cast<unsigned>(pixel_align),
                               vs_clip_idx);
            return builder.CreateFPExt(li, builder.getFloatTy());
        } else {
            throw std::runtime_error("Unsupported float sample size.");
        }
    }
}

void IRGenerator::add_loop_metadata(llvm::BranchInst* loop_br) {
    llvm::StringMap<bool> host_features = llvm::sys::getHostCPUFeatures();
    unsigned simd_width = 4;
    if (!host_features.empty()) {
        if (host_features["avx512f"]) {
            simd_width = 16;
        } else if (host_features["avx2"]) {
            simd_width = 8;
        }
    }

    auto create_md_node = [this](const char* name, llvm::Type* type,
                                 uint64_t value) -> llvm::MDNode* {
        llvm::Metadata* md[] = {
            llvm::MDString::get(context, name),
            llvm::ConstantAsMetadata::get(llvm::ConstantInt::get(type, value))};
        return llvm::MDNode::get(context, md);
    };

    llvm::MDNode* vec_width_node =
        create_md_node("llvm.loop.vectorize.width",
                       llvm::Type::getInt32Ty(context), simd_width);

    llvm::MDNode* enable_vec_node = create_md_node(
        "llvm.loop.vectorize.enable", llvm::Type::getInt1Ty(context), 1);

    llvm::MDNode* interleave_node = create_md_node(
        "llvm.loop.interleave.count", llvm::Type::getInt32Ty(context), 4);

    llvm::SmallVector<llvm::Metadata*, 5> loop_md_elems;
    loop_md_elems.push_back(nullptr); // to be replaced with self reference
    loop_md_elems.push_back(enable_vec_node);
    loop_md_elems.push_back(vec_width_node);
    loop_md_elems.push_back(interleave_node);
    llvm::MDNode* loop_id = llvm::MDNode::getDistinct(context, loop_md_elems);
    loop_id->replaceOperandWith(0, loop_id);

    loop_br->setMetadata(llvm::LLVMContext::MD_loop, loop_id);
}

void IRGenerator::generate_loops() {
    llvm::BasicBlock* entry_bb =
        llvm::BasicBlock::Create(context, "entry", func);
    builder.SetInsertPoint(entry_bb);

    llvm::Function* parent_func = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock* loop_y_header =
        llvm::BasicBlock::Create(context, "loop_y_header", parent_func);
    llvm::BasicBlock* loop_y_body =
        llvm::BasicBlock::Create(context, "loop_y_body", parent_func);
    llvm::BasicBlock* loop_y_exit =
        llvm::BasicBlock::Create(context, "loop_y_exit", parent_func);

    llvm::Value* y_var =
        builder.CreateAlloca(builder.getInt32Ty(), nullptr, "y.var");
    llvm::Value* x_var =
        builder.CreateAlloca(builder.getInt32Ty(), nullptr, "x.var");
    builder.CreateStore(builder.getInt32(0), y_var);

    llvm::Value* x_fp_var = nullptr;
    if (uses_x) {
        x_fp_var = createAllocaInEntry(builder.getFloatTy(), "x_fp.var");
    }
    llvm::Value* y_fp_var = nullptr;
    if (uses_y) {
        y_fp_var = createAllocaInEntry(builder.getFloatTy(), "y_fp.var");
        builder.CreateStore(llvm::ConstantFP::get(builder.getFloatTy(), 0.0),
                            y_fp_var);
    }

    // Index 0 = dst, 1..num_inputs = sources
    preloaded_base_ptrs.resize(num_inputs + 1);
    preloaded_strides.resize(num_inputs + 1);
    for (int i = 0; i <= num_inputs; ++i) {
        llvm::Value* base_ptr_i = builder.CreateLoad(
            llvm::PointerType::get(context, 0),
            builder.CreateGEP(llvm::PointerType::get(context, 0), rwptrs_arg,
                              builder.getInt32(i)));
        llvm::Value* stride_i = builder.CreateLoad(
            builder.getInt32Ty(),
            builder.CreateGEP(builder.getInt32Ty(), strides_arg,
                              builder.getInt32(i)));
        preloaded_base_ptrs[i] = base_ptr_i;
        preloaded_strides[i] = stride_i;

        assumeAligned(base_ptr_i, ALIGNMENT);
    }

    // Build alias scopes
    alias_scope_domain = llvm::MDNode::getDistinct(context, {});
    alias_scopes.resize(num_inputs + 1);
    for (int i = 0; i <= num_inputs; ++i) {
        llvm::SmallVector<llvm::Metadata*, 2> elems;
        elems.push_back(nullptr);
        llvm::Metadata* name_node = llvm::MDNode::get(
            context, {llvm::MDString::get(
                         context, std::format("rwptrs_{}", i).c_str())});
        elems.push_back(name_node);
        alias_scopes[i] = llvm::MDNode::getDistinct(context, elems);
        alias_scopes[i]->replaceOperandWith(0, alias_scopes[i]);
    }
    alias_scope_lists.resize(num_inputs + 1);
    noalias_scope_lists.resize(num_inputs + 1);
    for (int i = 0; i <= num_inputs; ++i) {
        std::vector<llvm::Metadata*> self_list = {alias_scopes[i]};
        alias_scope_lists[i] = llvm::MDNode::get(context, self_list);
        std::vector<llvm::Metadata*> others;
        for (int j = 0; j <= num_inputs; ++j) {
            if (j == i)
                continue;
            others.push_back(alias_scopes[j]);
        }
        noalias_scope_lists[i] = llvm::MDNode::get(context, others);
    }

    builder.CreateBr(loop_y_header);

    builder.SetInsertPoint(loop_y_header);
    llvm::Value* y_val = builder.CreateLoad(builder.getInt32Ty(), y_var, "y");
    llvm::Value* y_cond =
        builder.CreateICmpSLT(y_val, builder.getInt32(height), "y.cond");
    builder.CreateCondBr(y_cond, loop_y_body, loop_y_exit);

    builder.SetInsertPoint(loop_y_body);

    // Pre-calculate and cache row pointers
    row_ptr_cache.clear();
    for (const auto& access : unique_rel_y_accesses) {
        const VSVideoInfo* vinfo = vi[access.clip_idx];
        llvm::Value* clip_height = builder.getInt32(vinfo->height);
        llvm::Value* coord_y =
            builder.CreateAdd(y_val, builder.getInt32(access.rel_y));
        llvm::Value* final_y =
            get_final_coord(coord_y, clip_height, access.use_mirror);

        int vs_clip_idx = access.clip_idx + 1;
        llvm::Value* base_ptr = preloaded_base_ptrs[vs_clip_idx];
        llvm::Value* stride = preloaded_strides[vs_clip_idx];

        llvm::Value* y_offset = builder.CreateMul(final_y, stride);
        llvm::Value* row_ptr = builder.CreateGEP(builder.getInt8Ty(), base_ptr,
                                                 y_offset, "row_ptr");
        row_ptr_cache[access] = row_ptr;
    }

    llvm::Value* width_val = builder.getInt32(width);
    llvm::Value* start_main_x = builder.getInt32(-min_rel_x);
    llvm::Value* end_main_x = builder.getInt32(width - max_rel_x);

    bool has_left_peel = min_rel_x < 0;
    bool has_right_peel = max_rel_x > 0;

    llvm::BasicBlock* loop_x_start_bb =
        llvm::BasicBlock::Create(context, "loop_x_start", parent_func);
    llvm::BasicBlock* loop_x_exit_bb =
        llvm::BasicBlock::Create(context, "loop_x_exit", parent_func);

    builder.CreateBr(loop_x_start_bb);
    builder.SetInsertPoint(loop_x_start_bb);

    builder.CreateStore(builder.getInt32(0), x_var);
    if (uses_x) {
        builder.CreateStore(llvm::ConstantFP::get(builder.getFloatTy(), 0.0),
                            x_fp_var);
    }

    if (has_left_peel) {
        llvm::BasicBlock* left_peel_header =
            llvm::BasicBlock::Create(context, "left_peel_header", parent_func);
        llvm::BasicBlock* left_peel_body =
            llvm::BasicBlock::Create(context, "left_peel_body", parent_func);
        llvm::BasicBlock* after_left_peel =
            llvm::BasicBlock::Create(context, "after_left_peel", parent_func);

        builder.CreateBr(left_peel_header);
        builder.SetInsertPoint(left_peel_header);
        llvm::Value* x_val =
            builder.CreateLoad(builder.getInt32Ty(), x_var, "x_peel_l");
        llvm::Value* cond = builder.CreateICmpSLT(x_val, start_main_x);
        llvm::BranchInst* left_peel_br =
            builder.CreateCondBr(cond, left_peel_body, after_left_peel);
        add_loop_metadata(left_peel_br);

        builder.SetInsertPoint(left_peel_body);
        generate_x_loop_body(x_var, x_fp_var, y_var, y_fp_var, false);
        builder.CreateBr(left_peel_header);

        builder.SetInsertPoint(after_left_peel);
    }

    llvm::BasicBlock* main_loop_header =
        llvm::BasicBlock::Create(context, "main_loop_header", parent_func);
    llvm::BasicBlock* main_loop_body =
        llvm::BasicBlock::Create(context, "main_loop_body", parent_func);
    llvm::BasicBlock* after_main_loop =
        llvm::BasicBlock::Create(context, "after_main_loop", parent_func);

    builder.CreateBr(main_loop_header);
    builder.SetInsertPoint(main_loop_header);
    llvm::Value* x_val_main =
        builder.CreateLoad(builder.getInt32Ty(), x_var, "x_main");
    llvm::Value* main_cond = builder.CreateICmpSLT(x_val_main, end_main_x);

    llvm::BranchInst* loop_br =
        builder.CreateCondBr(main_cond, main_loop_body, after_main_loop);
    add_loop_metadata(loop_br);

    builder.SetInsertPoint(main_loop_body);
    generate_x_loop_body(x_var, x_fp_var, y_var, y_fp_var, true);
    builder.CreateBr(main_loop_header);

    builder.SetInsertPoint(after_main_loop);

    if (has_right_peel) {
        llvm::BasicBlock* right_peel_header =
            llvm::BasicBlock::Create(context, "right_peel_header", parent_func);
        llvm::BasicBlock* right_peel_body =
            llvm::BasicBlock::Create(context, "right_peel_body", parent_func);

        builder.CreateBr(right_peel_header);
        builder.SetInsertPoint(right_peel_header);
        llvm::Value* x_val =
            builder.CreateLoad(builder.getInt32Ty(), x_var, "x_peel_r");
        llvm::Value* cond = builder.CreateICmpSLT(x_val, width_val);
        llvm::BranchInst* right_peel_br =
            builder.CreateCondBr(cond, right_peel_body, loop_x_exit_bb);
        add_loop_metadata(right_peel_br);

        builder.SetInsertPoint(right_peel_body);
        generate_x_loop_body(x_var, x_fp_var, y_var, y_fp_var, false);
        builder.CreateBr(right_peel_header);
    } else {
        builder.CreateBr(loop_x_exit_bb);
    }

    builder.SetInsertPoint(loop_x_exit_bb);
    llvm::Value* y_next = builder.CreateAdd(y_val, builder.getInt32(1));
    builder.CreateStore(y_next, y_var);
    if (uses_y) {
        llvm::Value* y_fp_val =
            builder.CreateLoad(builder.getFloatTy(), y_fp_var);
        llvm::Value* y_fp_next = builder.CreateFAdd(
            y_fp_val, llvm::ConstantFP::get(builder.getFloatTy(), 1.0));
        builder.CreateStore(y_fp_next, y_fp_var);
    }
    builder.CreateBr(loop_y_header);

    builder.SetInsertPoint(loop_y_exit);
    builder.CreateRetVoid();
}

void IRGenerator::generate_x_loop_body(llvm::Value* x_var,
                                       llvm::Value* x_fp_var,
                                       llvm::Value* y_var,
                                       llvm::Value* y_fp_var,
                                       bool no_x_bounds_check) {
    llvm::Value* x_val = builder.CreateLoad(builder.getInt32Ty(), x_var, "x");
    llvm::Value* y_val =
        builder.CreateLoad(builder.getInt32Ty(), y_var, "y_in_x_loop");

    llvm::Value* x_fp = nullptr;
    if (uses_x) {
        x_fp = builder.CreateLoad(builder.getFloatTy(), x_fp_var, "x_fp");
    }
    llvm::Value* y_fp = nullptr;
    if (uses_y) {
        y_fp = builder.CreateLoad(builder.getFloatTy(), y_fp_var, "y_fp");
    }

    generate_ir_from_tokens(x_val, y_val, x_fp, y_fp, no_x_bounds_check);

    llvm::Value* x_next = builder.CreateAdd(x_val, builder.getInt32(1));
    builder.CreateStore(x_next, x_var);
    if (uses_x) {
        llvm::Value* x_fp_next = builder.CreateFAdd(
            x_fp, llvm::ConstantFP::get(builder.getFloatTy(), 1.0));
        builder.CreateStore(x_fp_next, x_fp_var);
    }
}

llvm::Value* IRGenerator::generate_pixel_load(int clip_idx, llvm::Value* x,
                                              llvm::Value* y, bool mirror) {
    llvm::Value* final_x = get_final_coord(x, builder.getInt32(width), mirror);
    llvm::Value* final_y = get_final_coord(y, builder.getInt32(height), mirror);

    int vs_clip_idx = clip_idx + 1;
    llvm::Value* base_ptr = preloaded_base_ptrs[vs_clip_idx];
    llvm::Value* stride = preloaded_strides[vs_clip_idx];

    llvm::Value* y_offset = builder.CreateMul(final_y, stride);
    llvm::Value* row_ptr =
        builder.CreateGEP(builder.getInt8Ty(), base_ptr, y_offset);

    return generate_load_from_row_ptr(row_ptr, clip_idx, final_x, 0, mirror,
                                      true);
}

void IRGenerator::generate_pixel_store(llvm::Value* value_to_store,
                                       llvm::Value* x, llvm::Value* y) {
    const VSVideoFormat& format = vo->format;
    int bpp = format.bytesPerSample;
    int dst_idx = 0;

    llvm::Value* base_ptr = preloaded_base_ptrs[dst_idx];
    llvm::Value* stride = preloaded_strides[dst_idx];

    llvm::Value* y_offset = builder.CreateMul(y, stride);
    llvm::Value* x_offset = builder.CreateMul(x, builder.getInt32(bpp));
    llvm::Value* total_offset = builder.CreateAdd(y_offset, x_offset);
    llvm::Value* pixel_addr =
        builder.CreateGEP(builder.getInt8Ty(), base_ptr, total_offset);

    int pixel_align = std::gcd(ALIGNMENT, bpp);
    assumeAligned(pixel_addr, static_cast<unsigned>(pixel_align));

    llvm::Value* final_val;
    if (format.sampleType == stInteger) {
        int max_val = (1 << format.bitsPerSample) - 1;
        llvm::Value* zero_f = llvm::ConstantFP::get(builder.getFloatTy(), 0.0);
        llvm::Value* max_f = llvm::ConstantFP::get(
            builder.getFloatTy(), static_cast<double>(max_val));

        llvm::Value* temp = createIntrinsicCall(llvm::Intrinsic::maxnum,
                                                value_to_store, zero_f);
        llvm::Value* clamped_f =
            createIntrinsicCall(llvm::Intrinsic::minnum, temp, max_f);

        llvm::Value* rounded_f =
            createIntrinsicCall(llvm::Intrinsic::roundeven, clamped_f);

        llvm::Type* store_type =
            bpp == 1 ? builder.getInt8Ty()
                     : (bpp == 2 ? builder.getInt16Ty() : builder.getInt32Ty());
        final_val = builder.CreateFPToUI(rounded_f, store_type);
        llvm::StoreInst* si = builder.CreateStore(final_val, pixel_addr);
        setMemoryInstAttrs(si, static_cast<unsigned>(pixel_align), dst_idx);
    } else {
        if (bpp == 4) {
            llvm::StoreInst* si =
                builder.CreateStore(value_to_store, pixel_addr);
            setMemoryInstAttrs(si, static_cast<unsigned>(pixel_align), dst_idx);
        } else if (bpp == 2) {
            llvm::Value* truncated_val =
                builder.CreateFPTrunc(value_to_store, builder.getHalfTy());
            llvm::StoreInst* si =
                builder.CreateStore(truncated_val, pixel_addr);
            setMemoryInstAttrs(si, static_cast<unsigned>(pixel_align), dst_idx);
        } else {
            throw std::runtime_error("Unsupported float sample size.");
        }
    }
}

void IRGenerator::generate_ir_from_tokens(llvm::Value* x, llvm::Value* y,
                                          llvm::Value* x_fp, llvm::Value* y_fp,
                                          bool no_x_bounds_check) {
    llvm::Type* float_ty = builder.getFloatTy();
    llvm::Type* i32_ty = builder.getInt32Ty();
    llvm::Function* parent_func = builder.GetInsertBlock()->getParent();

    bool use_approx_math = false;
    if (approx_math == 1) {
        use_approx_math = true;
    } else if (approx_math == 2) {
        // In auto mode, always try approx math first
        use_approx_math = true;
    }

    if (tokens.empty()) {
        generate_pixel_store(llvm::ConstantFP::get(float_ty, 0.0), x, y);
        return;
    }

    // Pre-scan all variables and allocate them in the entry block
    std::unordered_map<std::string, llvm::Value*> named_vars;
    std::set<std::string> all_vars;

    for (const auto& token : tokens) {
        if (token.type == TokenType::VAR_STORE ||
            token.type == TokenType::VAR_LOAD) {
            const auto& payload = std::get<TokenPayload_Var>(token.payload);
            all_vars.insert(payload.name);
        }
    }

    // Allocate all variables in the entry block
    for (const std::string& var_name : all_vars) {
        named_vars[var_name] = createAllocaInEntry(float_ty, var_name);
    }

    // Create all Basic Blocks
    std::map<int, llvm::BasicBlock*> llvm_blocks;
    for (size_t i = 0; i < analysis_results.cfg_blocks.size(); ++i) {
        std::string name = std::format("b{}", i);
        for (const auto& [label_name, block_idx] :
             analysis_results.label_to_block_idx) {
            if (block_idx == static_cast<int>(i)) {
                name = label_name;
                break;
            }
        }
        llvm_blocks[i] = llvm::BasicBlock::Create(context, name, parent_func);
    }
    llvm::BasicBlock* exit_bb =
        llvm::BasicBlock::Create(context, "exit", parent_func);

    // Branch from current block to the first CFG block
    builder.CreateBr(llvm_blocks[0]);

    // Initial PHI generation for merge blocks
    std::map<int, std::vector<llvm::Value*>> block_initial_stacks;
    for (size_t i = 0; i < analysis_results.cfg_blocks.size(); ++i) {
        if (analysis_results.cfg_blocks[i].predecessors.size() > 1) {
            builder.SetInsertPoint(llvm_blocks[i]);
            std::vector<llvm::Value*> initial_stack;
            int depth = analysis_results.stack_depth_in[i];
            for (int j = 0; j < depth; ++j) {
                initial_stack.push_back(builder.CreatePHI(
                    float_ty,
                    analysis_results.cfg_blocks[i].predecessors.size()));
            }
            block_initial_stacks[i] = initial_stack;
        }
    }

    // Process blocks
    std::map<int, std::vector<llvm::Value*>> block_final_stacks;

    for (size_t i = 0; i < analysis_results.cfg_blocks.size(); ++i) {
        const auto& block_info = analysis_results.cfg_blocks[i];
        builder.SetInsertPoint(llvm_blocks[i]);

        std::vector<llvm::Value*> rpn_stack;
        if (block_info.predecessors.empty()) {
            // Entry block, empty stack
        } else if (block_info.predecessors.size() == 1) {
            int pred_idx = block_info.predecessors[0];
            // This can fail on first pass of a loop, so we use
            // get-or-create
            if (block_final_stacks.count(pred_idx)) {
                rpn_stack = block_final_stacks.at(pred_idx);
            }
        } else {
            rpn_stack = block_initial_stacks.at(i);
        }

        for (int j = block_info.start_token_idx; j < block_info.end_token_idx;
             ++j) {
            const auto& token = tokens[j];

            auto applyStackOp = [&]<size_t ARITY>(auto&& op) {
                std::array<llvm::Value*, ARITY> args;
                for (size_t i = ARITY; i > 0; --i) {
                    args[i - 1] = rpn_stack.back();
                    rpn_stack.pop_back();
                }

                rpn_stack.push_back(std::apply(op, args));
            };

            auto applyIntrinsic = [&]<size_t ARITY>(llvm::Intrinsic::ID id) {
                applyStackOp.operator()<ARITY>([&](auto... args) {
                    return createIntrinsicCall(id, args...);
                });
            };

            auto applyBinaryOp = [&](auto opCallable) {
                applyStackOp.operator()<2>(
                    [&](auto a, auto b) { return opCallable(a, b); });
            };

            auto applyBinaryCmp = [&](llvm::CmpInst::Predicate pred) {
                applyStackOp.operator()<2>([&](auto a, auto b) {
                    auto cmp = builder.CreateFCmp(pred, a, b);
                    return builder.CreateSelect(
                        cmp, llvm::ConstantFP::get(float_ty, 1.0),
                        llvm::ConstantFP::get(float_ty, 0.0));
                });
            };

            enum class BoolBinOp { And, Or, Xor };
            auto applyLogicalOp = [&](BoolBinOp which) {
                applyStackOp.operator()<2>([&](auto a_val, auto b_val) {
                    auto a_bool = builder.CreateFCmpOGT(
                        a_val, llvm::ConstantFP::get(float_ty, 0.0));
                    auto b_bool = builder.CreateFCmpOGT(
                        b_val, llvm::ConstantFP::get(float_ty, 0.0));
                    llvm::Value* logic_res = nullptr;
                    switch (which) {
                    case BoolBinOp::And:
                        logic_res = builder.CreateAnd(a_bool, b_bool);
                        break;
                    case BoolBinOp::Or:
                        logic_res = builder.CreateOr(a_bool, b_bool);
                        break;
                    case BoolBinOp::Xor:
                        logic_res = builder.CreateXor(a_bool, b_bool);
                        break;
                    }
                    return builder.CreateSelect(
                        logic_res, llvm::ConstantFP::get(float_ty, 1.0),
                        llvm::ConstantFP::get(float_ty, 0.0));
                });
            };

            enum class IntBinOp { And, Or, Xor };
            auto applyBitwiseOp = [&](IntBinOp which) {
                applyStackOp.operator()<2>([&](auto a, auto b) {
                    auto ai = builder.CreateFPToSI(a, i32_ty);
                    auto bi = builder.CreateFPToSI(b, i32_ty);
                    llvm::Value* resi = nullptr;
                    switch (which) {
                    case IntBinOp::And:
                        resi = builder.CreateAnd(ai, bi);
                        break;
                    case IntBinOp::Or:
                        resi = builder.CreateOr(ai, bi);
                        break;
                    case IntBinOp::Xor:
                        resi = builder.CreateXor(ai, bi);
                        break;
                    }
                    return builder.CreateSIToFP(resi, float_ty);
                });
            };

            auto applyApproxMathOp = [&]<size_t ARITY>(
                                         MathOp math_op,
                                         llvm::Intrinsic::ID intrinsic_id) {
                static_assert(ARITY == 1 || ARITY == 2,
                              "Only unary or binary operations supported");

                std::array<llvm::Value*, ARITY> args;
                for (size_t i = ARITY; i > 0; --i) {
                    args[i - 1] = rpn_stack.back();
                    rpn_stack.pop_back();
                }

                if (use_approx_math) {
                    auto* callee = math_manager.getFunction(math_op);
                    llvm::SmallVector<llvm::Value*, 2> call_args(args.begin(),
                                                                 args.end());
                    auto* call = builder.CreateCall(callee, call_args);
                    call->setFastMathFlags(builder.getFastMathFlags());
                    rpn_stack.push_back(call);
                } else {
                    rpn_stack.push_back(std::apply(
                        [&](auto... args) {
                            return createIntrinsicCall(intrinsic_id, args...);
                        },
                        args));
                }
            };

            switch (token.type) {
            // Literals & Constants
            case TokenType::NUMBER: {
                const auto& payload =
                    std::get<TokenPayload_Number>(token.payload);
                rpn_stack.push_back(
                    llvm::ConstantFP::get(float_ty, payload.value));
                break;
            }
            case TokenType::CONSTANT_X:
                rpn_stack.push_back(x_fp);
                break;
            case TokenType::CONSTANT_Y:
                rpn_stack.push_back(y_fp);
                break;
            case TokenType::CONSTANT_WIDTH:
                rpn_stack.push_back(
                    builder.CreateSIToFP(builder.getInt32(width), float_ty));
                break;
            case TokenType::CONSTANT_HEIGHT:
                rpn_stack.push_back(
                    builder.CreateSIToFP(builder.getInt32(height), float_ty));
                break;
            case TokenType::CONSTANT_N:
                rpn_stack.push_back(builder.CreateLoad(
                    float_ty, builder.CreateGEP(float_ty, props_arg,
                                                builder.getInt32(0))));
                break;
            case TokenType::CONSTANT_PI:
                rpn_stack.push_back(
                    llvm::ConstantFP::get(float_ty, std::numbers::pi));
                break;

            // Variable Ops
            case TokenType::VAR_STORE: {
                const auto& payload = std::get<TokenPayload_Var>(token.payload);
                llvm::Value* val_to_store = rpn_stack.back();
                rpn_stack.pop_back();
                llvm::Value* var_ptr = named_vars[payload.name];
                builder.CreateStore(val_to_store, var_ptr);
                break;
            }
            case TokenType::VAR_LOAD: {
                const auto& payload = std::get<TokenPayload_Var>(token.payload);
                llvm::Value* var_ptr = named_vars[payload.name];
                rpn_stack.push_back(builder.CreateLoad(float_ty, var_ptr));
                break;
            }

            // Data Access
            case TokenType::CLIP_REL: {
                const auto& payload =
                    std::get<TokenPayload_ClipAccess>(token.payload);
                bool use_mirror =
                    payload.has_mode ? payload.use_mirror : mirror_boundary;
                RelYAccess access{payload.clip_idx, payload.rel_y, use_mirror};
                llvm::Value* row_ptr = row_ptr_cache.at(access);
                rpn_stack.push_back(generate_load_from_row_ptr(
                    row_ptr, payload.clip_idx, x, payload.rel_x, use_mirror,
                    no_x_bounds_check));
                break;
            }
            case TokenType::CLIP_ABS: {
                const auto& payload =
                    std::get<TokenPayload_ClipAccess>(token.payload);
                llvm::Value* coord_y_f = rpn_stack.back();
                rpn_stack.pop_back();
                llvm::Value* coord_x_f = rpn_stack.back();
                rpn_stack.pop_back();

                llvm::Value* coord_y = builder.CreateCall(
                    llvm::Intrinsic::getOrInsertDeclaration(
                        &module, llvm::Intrinsic::rint, {float_ty}),
                    {coord_y_f});
                coord_y = builder.CreateFPToSI(coord_y, i32_ty);

                llvm::Value* coord_x = builder.CreateCall(
                    llvm::Intrinsic::getOrInsertDeclaration(
                        &module, llvm::Intrinsic::rint, {float_ty}),
                    {coord_x_f});
                coord_x = builder.CreateFPToSI(coord_x, i32_ty);

                bool use_mirror_final;
                if (payload.has_mode) {
                    use_mirror_final = payload.use_mirror;
                } else {
                    use_mirror_final = mirror_boundary;
                }

                rpn_stack.push_back(generate_pixel_load(
                    payload.clip_idx, coord_x, coord_y, use_mirror_final));
                break;
            }
            case TokenType::CLIP_CUR: {
                const auto& payload =
                    std::get<TokenPayload_ClipAccess>(token.payload);
                RelYAccess access{payload.clip_idx, 0, mirror_boundary};
                llvm::Value* row_ptr = row_ptr_cache.at(access);
                rpn_stack.push_back(generate_load_from_row_ptr(
                    row_ptr, payload.clip_idx, x, 0, mirror_boundary,
                    no_x_bounds_check));
                break;
            }
            case TokenType::PROP_ACCESS: {
                const auto& payload =
                    std::get<TokenPayload_PropAccess>(token.payload);
                auto key = std::make_pair(payload.clip_idx, payload.prop_name);
                int prop_idx = prop_map.at(key);
                llvm::Value* prop_val = builder.CreateLoad(
                    float_ty, builder.CreateGEP(float_ty, props_arg,
                                                builder.getInt32(prop_idx)));
                rpn_stack.push_back(prop_val);
                break;
            }

            // Binary Operators
            case TokenType::ADD: {
                applyBinaryOp([&](llvm::Value* a, llvm::Value* b) {
                    return builder.CreateFAdd(a, b);
                });
                break;
            }
            case TokenType::SUB: {
                applyBinaryOp([&](llvm::Value* a, llvm::Value* b) {
                    return builder.CreateFSub(a, b);
                });
                break;
            }
            case TokenType::MUL: {
                applyBinaryOp([&](llvm::Value* a, llvm::Value* b) {
                    return builder.CreateFMul(a, b);
                });
                break;
            }
            case TokenType::DIV: {
                applyBinaryOp([&](llvm::Value* a, llvm::Value* b) {
                    return builder.CreateFDiv(a, b);
                });
                break;
            }
            case TokenType::MOD: {
                applyBinaryOp([&](llvm::Value* a, llvm::Value* b) {
                    return builder.CreateFRem(a, b);
                });
                break;
            }
            case TokenType::POW:
                applyIntrinsic.operator()<2>(llvm::Intrinsic::pow);
                break;
            case TokenType::ATAN2:
                applyApproxMathOp.operator()<2>(MathOp::Atan2,
                                                llvm::Intrinsic::atan2);
                break;
            case TokenType::COPYSIGN: {
                applyIntrinsic.operator()<2>(llvm::Intrinsic::copysign);
                break;
            }
            case TokenType::MIN: {
                applyIntrinsic.operator()<2>(llvm::Intrinsic::minnum);
                break;
            }
            case TokenType::MAX: {
                applyIntrinsic.operator()<2>(llvm::Intrinsic::maxnum);
                break;
            }

            // Binary comparisons
            case TokenType::GT: {
                applyBinaryCmp(llvm::CmpInst::FCMP_OGT);
                break;
            }
            case TokenType::LT: {
                applyBinaryCmp(llvm::CmpInst::FCMP_OLT);
                break;
            }
            case TokenType::GE: {
                applyBinaryCmp(llvm::CmpInst::FCMP_OGE);
                break;
            }
            case TokenType::LE: {
                applyBinaryCmp(llvm::CmpInst::FCMP_OLE);
                break;
            }
            case TokenType::EQ: {
                applyBinaryCmp(llvm::CmpInst::FCMP_OEQ);
                break;
            }

            // Logical ops on booleanized floats
            case TokenType::AND: {
                applyLogicalOp(BoolBinOp::And);
                break;
            }
            case TokenType::OR: {
                applyLogicalOp(BoolBinOp::Or);
                break;
            }
            case TokenType::XOR: {
                applyLogicalOp(BoolBinOp::Xor);
                break;
            }

            // Bitwise ops on converted ints
            case TokenType::BITAND: {
                applyBitwiseOp(IntBinOp::And);
                break;
            }
            case TokenType::BITOR: {
                applyBitwiseOp(IntBinOp::Or);
                break;
            }
            case TokenType::BITXOR: {
                applyBitwiseOp(IntBinOp::Xor);
                break;
            }

            // Unary Operators
            case TokenType::SQRT: {
                // Akarin.Expr and JITASM std.Expr both apply max(0, x)
                // before sqrt, so we do the same for compatibility.
                // See:
                // https://github.com/vapoursynth/vapoursynth/issues/1112
                auto a = rpn_stack.back();
                rpn_stack.pop_back();
                auto zero = llvm::ConstantFP::get(float_ty, 0.0);
                auto max_val =
                    createIntrinsicCall(llvm::Intrinsic::maxnum, a, zero);
                rpn_stack.push_back(
                    createIntrinsicCall(llvm::Intrinsic::sqrt, max_val));
                break;
            }
            case TokenType::EXP:
                applyApproxMathOp.operator()<1>(MathOp::Exp,
                                                llvm::Intrinsic::exp);
                break;
            case TokenType::LOG:
                applyApproxMathOp.operator()<1>(MathOp::Log,
                                                llvm::Intrinsic::log);
                break;
            case TokenType::ABS: {
                applyIntrinsic.operator()<1>(llvm::Intrinsic::fabs);
                break;
            }
            case TokenType::FLOOR: {
                applyIntrinsic.operator()<1>(llvm::Intrinsic::floor);
                break;
            }
            case TokenType::CEIL: {
                applyIntrinsic.operator()<1>(llvm::Intrinsic::ceil);
                break;
            }
            case TokenType::TRUNC: {
                applyIntrinsic.operator()<1>(llvm::Intrinsic::trunc);
                break;
            }
            case TokenType::ROUND: {
                applyIntrinsic.operator()<1>(llvm::Intrinsic::round);
                break;
            }
            case TokenType::SIN:
                applyApproxMathOp.operator()<1>(MathOp::Sin,
                                                llvm::Intrinsic::sin);
                break;
            case TokenType::COS:
                applyApproxMathOp.operator()<1>(MathOp::Cos,
                                                llvm::Intrinsic::cos);
                break;
            case TokenType::TAN:
                applyApproxMathOp.operator()<1>(MathOp::Tan,
                                                llvm::Intrinsic::tan);
                break;
            case TokenType::ASIN:
                applyApproxMathOp.operator()<1>(MathOp::Asin,
                                                llvm::Intrinsic::asin);
                break;
            case TokenType::ACOS:
                applyApproxMathOp.operator()<1>(MathOp::Acos,
                                                llvm::Intrinsic::acos);
                break;
            case TokenType::ATAN:
                applyApproxMathOp.operator()<1>(MathOp::Atan,
                                                llvm::Intrinsic::atan);
                break;
            case TokenType::EXP2: {
                applyIntrinsic.operator()<1>(llvm::Intrinsic::exp2);
                break;
            }
            case TokenType::LOG10: {
                applyIntrinsic.operator()<1>(llvm::Intrinsic::log10);
                break;
            }
            case TokenType::LOG2: {
                applyIntrinsic.operator()<1>(llvm::Intrinsic::log2);
                break;
            }
            case TokenType::SINH: {
                applyIntrinsic.operator()<1>(llvm::Intrinsic::sinh);
                break;
            }
            case TokenType::COSH: {
                applyIntrinsic.operator()<1>(llvm::Intrinsic::cosh);
                break;
            }
            case TokenType::TANH: {
                applyIntrinsic.operator()<1>(llvm::Intrinsic::tanh);
                break;
            }
            case TokenType::SGN: {
                auto x = rpn_stack.back();
                rpn_stack.pop_back();
                auto zero = llvm::ConstantFP::get(float_ty, 0.0);
                auto one = llvm::ConstantFP::get(float_ty, 1.0);
                auto nonzero = builder.CreateFCmpONE(x, zero);
                auto sign = builder.CreateCall(
                    llvm::Intrinsic::getOrInsertDeclaration(
                        &module, llvm::Intrinsic::copysign, {float_ty}),
                    {one, x});
                rpn_stack.push_back(builder.CreateSelect(nonzero, sign, zero));
                break;
            }
            case TokenType::NEG: {
                auto a = rpn_stack.back();
                rpn_stack.pop_back();
                rpn_stack.push_back(builder.CreateFNeg(a));
                break;
            }

            case TokenType::NOT: {
                auto a = rpn_stack.back();
                rpn_stack.pop_back();
                rpn_stack.push_back(builder.CreateSelect(
                    builder.CreateFCmpOLE(a,
                                          llvm::ConstantFP::get(float_ty, 0.0)),
                    llvm::ConstantFP::get(float_ty, 1.0),
                    llvm::ConstantFP::get(float_ty, 0.0)));
                break;
            }
            case TokenType::BITNOT: {
                auto a = rpn_stack.back();
                rpn_stack.pop_back();
                rpn_stack.push_back(builder.CreateSIToFP(
                    builder.CreateNot(builder.CreateFPToSI(a, i32_ty)),
                    float_ty));
                break;
            }

            // Ternary and other multi-arg
            case TokenType::TERNARY: {
                auto c = rpn_stack.back();
                rpn_stack.pop_back();
                auto b = rpn_stack.back();
                rpn_stack.pop_back();
                auto a = rpn_stack.back();
                rpn_stack.pop_back();
                rpn_stack.push_back(builder.CreateSelect(
                    builder.CreateFCmpOGT(a,
                                          llvm::ConstantFP::get(float_ty, 0.0)),
                    b, c));
                break;
            }
            case TokenType::CLIP:
            case TokenType::CLAMP: {
                auto max_val = rpn_stack.back();
                rpn_stack.pop_back();
                auto min_val = rpn_stack.back();
                rpn_stack.pop_back();
                auto val = rpn_stack.back();
                rpn_stack.pop_back();
                auto temp =
                    createIntrinsicCall(llvm::Intrinsic::maxnum, val, min_val);
                auto clamped =
                    createIntrinsicCall(llvm::Intrinsic::minnum, temp, max_val);
                rpn_stack.push_back(clamped);
                break;
            }
            case TokenType::FMA: {
                auto c = rpn_stack.back();
                rpn_stack.pop_back();
                auto b = rpn_stack.back();
                rpn_stack.pop_back();
                auto a = rpn_stack.back();
                rpn_stack.pop_back();
                rpn_stack.push_back(builder.CreateCall(
                    llvm::Intrinsic::getOrInsertDeclaration(
                        &module, llvm::Intrinsic::fma, {builder.getFloatTy()}),
                    {a, b, c}));
                break;
            }

            // Custom output control
            case TokenType::STORE_ABS: {
                llvm::Value* coord_y_f = rpn_stack.back();
                rpn_stack.pop_back();
                llvm::Value* coord_x_f = rpn_stack.back();
                rpn_stack.pop_back();
                llvm::Value* val_to_store = rpn_stack.back();
                rpn_stack.pop_back();
                llvm::Value* coord_y = builder.CreateFPToSI(coord_y_f, i32_ty);
                llvm::Value* coord_x = builder.CreateFPToSI(coord_x_f, i32_ty);
                generate_pixel_store(val_to_store, coord_x, coord_y);
                break;
            }
            case TokenType::EXIT_NO_WRITE: {
                rpn_stack.push_back(llvm::ConstantFP::get(
                    float_ty,
                    std::bit_cast<float>(
                        EXIT_NAN_PAYLOAD))); // Use special NaN for `^exit^`
                break;
            }

            // Stack manipulation
            case TokenType::DUP: {
                const auto& payload =
                    std::get<TokenPayload_StackOp>(token.payload);
                rpn_stack.push_back(
                    rpn_stack[rpn_stack.size() - 1 - payload.n]);
                break;
            }
            case TokenType::DROP: {
                const auto& payload =
                    std::get<TokenPayload_StackOp>(token.payload);
                if (payload.n > 0) {
                    rpn_stack.resize(rpn_stack.size() - payload.n);
                }
                break;
            }
            case TokenType::SWAP: {
                const auto& payload =
                    std::get<TokenPayload_StackOp>(token.payload);
                std::swap(rpn_stack.back(),
                          rpn_stack[rpn_stack.size() - 1 - payload.n]);
                break;
            }
            case TokenType::SORTN: {
                const auto& payload =
                    std::get<TokenPayload_StackOp>(token.payload);
                int n = payload.n;
                if (n < 2)
                    break;

                std::vector<llvm::Value*> values;
                values.reserve(n);
                for (int k = 0; k < n; ++k) {
                    values.push_back(rpn_stack.back());
                    rpn_stack.pop_back();
                }
                std::reverse(values.begin(), values.end());

                auto compare_swap = [&](int i_idx, int j_idx) {
                    llvm::Value* val_i = values[i_idx];
                    llvm::Value* val_j = values[j_idx];
                    llvm::Value* cond = builder.CreateFCmpOGT(val_i, val_j);
                    values[i_idx] =
                        builder.CreateSelect(cond, val_j, val_i); // min
                    values[j_idx] =
                        builder.CreateSelect(cond, val_i, val_j); // max
                };

                std::vector<std::pair<int, int>> network;
                get_sorting_network(n, network);
                for (const auto& pair : network) {
                    compare_swap(pair.first, pair.second);
                }

                for (int k = n - 1; k >= 0; --k) {
                    rpn_stack.push_back(values[k]);
                }
                break;
            }

            // Control Flow (no-op during this pass)
            case TokenType::LABEL_DEF:
            case TokenType::JUMP:
                break;
            }
        }

        // Create Terminator
        if (block_info.successors.empty()) {
            builder.CreateBr(exit_bb);
        } else if (block_info.successors.size() == 1) {
            builder.CreateBr(llvm_blocks[block_info.successors[0]]);
        } else { // size is 2, from a JUMP
            llvm::Value* cond_val = rpn_stack.back();
            llvm::Value* cond = builder.CreateFCmpOGT(
                cond_val, llvm::ConstantFP::get(float_ty, 0.0));
            builder.CreateCondBr(cond, llvm_blocks[block_info.successors[0]],
                                 llvm_blocks[block_info.successors[1]]);

            // Pop the condition value from stack after creating the
            // terminator
            rpn_stack.pop_back();
        }

        // Save the final stack state (without condition for JUMP blocks)
        block_final_stacks[i] = rpn_stack;
    }

    // Populate PHI nodes
    for (size_t i = 0; i < analysis_results.cfg_blocks.size(); ++i) {
        if (analysis_results.cfg_blocks[i].predecessors.size() > 1) {
            auto& phis = block_initial_stacks.at(i);
            for (int pred_idx : analysis_results.cfg_blocks[i].predecessors) {
                auto& incoming_stack = block_final_stacks.at(pred_idx);
                auto* incoming_block = llvm_blocks.at(pred_idx);
                for (size_t j = 0; j < phis.size(); ++j) {
                    // All predecessor blocks now provide stacks of the
                    // correct depth
                    if (j < incoming_stack.size()) {
                        static_cast<llvm::PHINode*>(phis[j])->addIncoming(
                            incoming_stack[j], incoming_block);
                    }
                }
            }
        }
    }

    // Final Result PHI
    builder.SetInsertPoint(exit_bb);
    std::vector<std::pair<llvm::Value*, llvm::BasicBlock*>> final_values;
    for (size_t i = 0; i < analysis_results.cfg_blocks.size(); ++i) {
        if (analysis_results.cfg_blocks[i]
                .successors.empty()) { // Terminal block
            final_values.push_back(
                {block_final_stacks.at(i).back(), llvm_blocks.at(i)});
        }
    }

    llvm::Value* result_val;
    if (final_values.empty()) {
        result_val = llvm::UndefValue::get(float_ty);
    } else if (final_values.size() == 1) {
        result_val = final_values[0].first;
    } else {
        llvm::PHINode* phi =
            builder.CreatePHI(float_ty, final_values.size(), "result_phi");
        for (const auto& pair : final_values) {
            phi->addIncoming(pair.first, pair.second);
        }
        result_val = phi;
    }

    bool has_exit = false;
    for (const auto& token : tokens) {
        if (token.type == TokenType::EXIT_NO_WRITE) {
            has_exit = true;
            break;
        }
    }

    if (has_exit) {
        llvm::Value* result_int =
            builder.CreateBitCast(result_val, builder.getInt32Ty());
        llvm::Value* exit_nan_int = builder.getInt32(EXIT_NAN_PAYLOAD);
        llvm::Value* is_exit_val =
            builder.CreateICmpEQ(result_int, exit_nan_int);

        llvm::BasicBlock* store_block =
            llvm::BasicBlock::Create(context, "do_default_store", parent_func);
        llvm::BasicBlock* after_store_block = llvm::BasicBlock::Create(
            context, "after_default_store", parent_func);

        builder.CreateCondBr(is_exit_val, after_store_block, store_block);

        builder.SetInsertPoint(store_block);
        generate_pixel_store(result_val, x, y);
        builder.CreateBr(after_store_block);

        builder.SetInsertPoint(after_store_block);
    } else {
        generate_pixel_store(result_val, x, y);
    }
}
