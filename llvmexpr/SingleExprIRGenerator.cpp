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

#include "SingleExprIRGenerator.hpp"

#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Instructions.h"

SingleExprIRGenerator::SingleExprIRGenerator(
    const std::vector<Token>& tokens_in, const VSVideoInfo* out_vi,
    const std::vector<const VSVideoInfo*>& in_vi, bool mirror,
    const std::map<std::pair<int, std::string>, int>& p_map,
    const std::vector<std::string>& output_props,
    const ExpressionAnalysisResults& analysis_results_in,
    llvm::LLVMContext& context_ref, llvm::Module& module_ref,
    llvm::IRBuilder<>& builder_ref, MathLibraryManager& math_mgr,
    std::string func_name_in, int approx_math_in)
    : IRGeneratorBase(tokens_in, out_vi, in_vi, out_vi->width, out_vi->height,
                      mirror, p_map, analysis_results_in, context_ref,
                      module_ref, builder_ref, math_mgr,
                      std::move(func_name_in), approx_math_in),
      output_props_list(output_props) {

    for (size_t i = 0; i < output_props_list.size(); ++i) {
        output_prop_map[output_props_list[i]] = i;
    }
}

void SingleExprIRGenerator::define_function_signature() {
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
}

void SingleExprIRGenerator::
    generate_loops() { //TODO: rename this. Nothing to do with loops here.
    llvm::BasicBlock* entry_bb =
        llvm::BasicBlock::Create(context, "entry", func);
    builder.SetInsertPoint(entry_bb);

    // Pre-load all plane pointers and strides
    plane_base_ptrs.resize(num_inputs + 1);
    plane_strides.resize(num_inputs + 1);
    int num_planes = vo->format.numPlanes;
    for (int i = 0; i <= num_inputs; ++i) {
        plane_base_ptrs[i].resize(num_planes);
        plane_strides[i].resize(num_planes);
        for (int p = 0; p < num_planes; ++p) {
            int flat_idx = i * num_planes + p;
            llvm::Value* base_ptr_i = builder.CreateLoad(
                llvm::PointerType::get(context, 0),
                builder.CreateGEP(llvm::PointerType::get(context, 0),
                                  rwptrs_arg, builder.getInt32(flat_idx)));
            llvm::Value* stride_i = builder.CreateLoad(
                builder.getInt32Ty(),
                builder.CreateGEP(builder.getInt32Ty(), strides_arg,
                                  builder.getInt32(flat_idx)));
            plane_base_ptrs[i][p] = base_ptr_i;
            plane_strides[i][p] = stride_i;
        }
    }

    // Aligned loads for properties
    for (const auto& [key, idx] : prop_map) {
        llvm::Value* prop_val = builder.CreateLoad(
            builder.getFloatTy(),
            builder.CreateGEP(builder.getFloatTy(), props_arg,
                              builder.getInt32(idx)));
        llvm::Value* alloca =
            createAllocaInEntry(builder.getFloatTy(), key.second);
        builder.CreateStore(prop_val, alloca);
        prop_allocas[key.second] = alloca;
    }
    for (const auto& prop_name : output_props_list) {
        if (prop_allocas.find(prop_name) == prop_allocas.end()) {
            prop_allocas[prop_name] =
                createAllocaInEntry(builder.getFloatTy(), prop_name);
        }
    }

    // Only generate IR if there are tokens to process
    if (!tokens.empty()) {
        generate_ir_from_tokens(nullptr, nullptr, nullptr, nullptr, true);
    }

    // Store output properties back
    for (const auto& [name, idx] : output_prop_map) {
        llvm::Value* val =
            builder.CreateLoad(builder.getFloatTy(), prop_allocas.at(name));
        builder.CreateStore(
            val,
            builder.CreateGEP(builder.getFloatTy(), props_arg,
                              builder.getInt32(prop_map.size() + 1 + idx)));
    }

    builder.CreateRetVoid();
}

llvm::Value* SingleExprIRGenerator::generate_pixel_load_plane(int clip_idx,
                                                              int plane_idx,
                                                              llvm::Value* x,
                                                              llvm::Value* y) {
    const VSVideoInfo* vinfo = vi[clip_idx];
    int plane_w = vinfo->width >> vinfo->format.subSamplingW;
    int plane_h = vinfo->height >> vinfo->format.subSamplingH;

    llvm::Value* final_x =
        get_final_coord(x, builder.getInt32(plane_w), mirror_boundary);
    llvm::Value* final_y =
        get_final_coord(y, builder.getInt32(plane_h), mirror_boundary);

    llvm::Value* base_ptr = plane_base_ptrs[clip_idx + 1][plane_idx];
    llvm::Value* stride = plane_strides[clip_idx + 1][plane_idx];

    llvm::Value* y_offset = builder.CreateMul(final_y, stride);
    llvm::Value* row_ptr =
        builder.CreateGEP(builder.getInt8Ty(), base_ptr, y_offset);

    const VSVideoFormat& format = vinfo->format;
    int bpp = format.bytesPerSample;

    llvm::Value* x_offset = builder.CreateMul(final_x, builder.getInt32(bpp));
    llvm::Value* pixel_addr =
        builder.CreateGEP(builder.getInt8Ty(), row_ptr, x_offset);

    llvm::Value* loaded_val;
    if (format.sampleType == stInteger) {
        llvm::Type* load_type =
            bpp == 1 ? builder.getInt8Ty()
                     : (bpp == 2 ? builder.getInt16Ty() : builder.getInt32Ty());
        llvm::LoadInst* li = builder.CreateLoad(load_type, pixel_addr);
        loaded_val = builder.CreateZExtOrBitCast(li, builder.getInt32Ty());
        return builder.CreateUIToFP(loaded_val, builder.getFloatTy());
    } else { // stFloat
        if (bpp == 4) {
            return builder.CreateLoad(builder.getFloatTy(), pixel_addr);
        } else if (bpp == 2) {
            llvm::Value* half_val =
                builder.CreateLoad(builder.getHalfTy(), pixel_addr);
            return builder.CreateFPExt(half_val, builder.getFloatTy());
        } else {
            throw std::runtime_error("Unsupported float sample size.");
        }
    }
}

void SingleExprIRGenerator::generate_pixel_store_plane(
    llvm::Value* value_to_store, int plane_idx, llvm::Value* x,
    llvm::Value* y) {
    const VSVideoFormat& format = vo->format;
    int bpp = format.bytesPerSample;

    llvm::Value* base_ptr = plane_base_ptrs[0][plane_idx];
    llvm::Value* stride = plane_strides[0][plane_idx];

    llvm::Value* y_offset = builder.CreateMul(y, stride);
    llvm::Value* x_offset = builder.CreateMul(x, builder.getInt32(bpp));
    llvm::Value* total_offset = builder.CreateAdd(y_offset, x_offset);
    llvm::Value* pixel_addr =
        builder.CreateGEP(builder.getInt8Ty(), base_ptr, total_offset);

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
        builder.CreateStore(final_val, pixel_addr);
    } else {
        if (bpp == 4) {
            builder.CreateStore(value_to_store, pixel_addr);
        } else if (bpp == 2) {
            llvm::Value* truncated_val =
                builder.CreateFPTrunc(value_to_store, builder.getHalfTy());
            builder.CreateStore(truncated_val, pixel_addr);
        } else {
            throw std::runtime_error("Unsupported float sample size.");
        }
    }
}

bool SingleExprIRGenerator::process_mode_specific_token(
    const Token& token, std::vector<llvm::Value*>& rpn_stack,
    [[maybe_unused]] llvm::Value* x, [[maybe_unused]] llvm::Value* y,
    [[maybe_unused]] llvm::Value* x_fp, [[maybe_unused]] llvm::Value* y_fp,
    [[maybe_unused]] bool no_x_bounds_check) {
    llvm::Type* float_ty = builder.getFloatTy();
    llvm::Type* i32_ty = builder.getInt32Ty();

    switch (token.type) {
    case TokenType::CLIP_ABS_PLANE: {
        const auto& payload =
            std::get<TokenPayload_ClipAccessPlane>(token.payload);
        llvm::Value* coord_y_f = rpn_stack.back();
        rpn_stack.pop_back();
        llvm::Value* coord_x_f = rpn_stack.back();
        rpn_stack.pop_back();

        llvm::Value* coord_y =
            builder.CreateCall(llvm::Intrinsic::getOrInsertDeclaration(
                                   &module, llvm::Intrinsic::rint, {float_ty}),
                               {coord_y_f});
        coord_y = builder.CreateFPToSI(coord_y, i32_ty);

        llvm::Value* coord_x =
            builder.CreateCall(llvm::Intrinsic::getOrInsertDeclaration(
                                   &module, llvm::Intrinsic::rint, {float_ty}),
                               {coord_x_f});
        coord_x = builder.CreateFPToSI(coord_x, i32_ty);

        rpn_stack.push_back(generate_pixel_load_plane(
            payload.clip_idx, payload.plane_idx, coord_x, coord_y));
        return true;
    }
    case TokenType::STORE_ABS_PLANE: {
        const auto& payload =
            std::get<TokenPayload_StoreAbsPlane>(token.payload);
        llvm::Value* coord_y_f = rpn_stack.back();
        rpn_stack.pop_back();
        llvm::Value* coord_x_f = rpn_stack.back();
        rpn_stack.pop_back();
        llvm::Value* val_to_store = rpn_stack.back();
        rpn_stack.pop_back();

        llvm::Value* coord_y =
            builder.CreateCall(llvm::Intrinsic::getOrInsertDeclaration(
                                   &module, llvm::Intrinsic::rint, {float_ty}),
                               {coord_y_f});
        coord_y = builder.CreateFPToSI(coord_y, i32_ty);

        llvm::Value* coord_x =
            builder.CreateCall(llvm::Intrinsic::getOrInsertDeclaration(
                                   &module, llvm::Intrinsic::rint, {float_ty}),
                               {coord_x_f});
        coord_x = builder.CreateFPToSI(coord_x, i32_ty);

        generate_pixel_store_plane(val_to_store, payload.plane_idx, coord_x,
                                   coord_y);
        return true;
    }
    case TokenType::PROP_STORE: {
        const auto& payload = std::get<TokenPayload_PropStore>(token.payload);
        llvm::Value* val_to_store = rpn_stack.back();
        rpn_stack.pop_back();
        builder.CreateStore(val_to_store, prop_allocas.at(payload.prop_name));
        return true;
    }
    case TokenType::PROP_ACCESS: {
        const auto& payload = std::get<TokenPayload_PropAccess>(token.payload);
        rpn_stack.push_back(
            builder.CreateLoad(float_ty, prop_allocas.at(payload.prop_name)));
        return true;
    }
    default:
        return false;
    }
}

void SingleExprIRGenerator::finalize_and_store_result(
    [[maybe_unused]] llvm::Value* result_val, [[maybe_unused]] llvm::Value* x,
    [[maybe_unused]] llvm::Value* y) {
    // No-op for SingleExpr, all stores are explicit
}
