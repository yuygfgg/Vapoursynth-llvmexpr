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

#ifndef LLVMEXPR_MATH_FUNCTIONS_HPP
#define LLVMEXPR_MATH_FUNCTIONS_HPP

#include <format>
#include <functional>
#include <map>
#include <string>
#include <tuple>
#include <utility>

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"

enum class MathOp { Exp, Log, Sin, Cos, Tan, Atan, Atan2, Acos };

struct MathOpInfo {
    int arity;
    const char* name;
};

constexpr MathOpInfo getMathOpInfo(MathOp op) {
    switch (op) {
    case MathOp::Exp:
        return {1, "fast_exp"};
    case MathOp::Log:
        return {1, "fast_log"};
    case MathOp::Sin:
        return {1, "fast_sin"};
    case MathOp::Cos:
        return {1, "fast_cos"};
    case MathOp::Tan:
        return {1, "fast_tan"};
    case MathOp::Atan:
        return {1, "fast_atan"};
    case MathOp::Atan2:
        return {2, "fast_atan2"};
    case MathOp::Acos:
        return {1, "fast_acos"};
    }
    std::unreachable();
}

#ifdef __x86_64__
using SupportedVectorWidths = std::integer_sequence<int, 4, 8, 16>;
#elif defined(__ARM_NEON__)
using SupportedVectorWidths = std::integer_sequence<int, 4>;
#else
#error "Unsupported architecture. Only x86_64 and ARM NEON are supported."
#endif
// TODO: Figure out if other architectures have >4 wide vector support.

template <int VectorWidth> class MathFunctionGenerator {
  public:
    MathFunctionGenerator(llvm::Module* module, llvm::LLVMContext& context)
        : module_(module), context_(context), builder_(context) {}

    template <MathOp op> llvm::Function* getOrCreate();

  private:
    llvm::Module* module_;
    llvm::LLVMContext& context_;
    llvm::IRBuilder<> builder_;

    llvm::Type* getFloatType() {
        auto* ty = llvm::Type::getFloatTy(context_);
        if (VectorWidth == 1) {
            return ty;
        } else {
            return llvm::VectorType::get(ty, VectorWidth, false);
        }
    }

    llvm::Type* getInt32Type() {
        auto* ty = llvm::Type::getInt32Ty(context_);
        if (VectorWidth == 1) {
            return ty;
        } else {
            return llvm::VectorType::get(ty, VectorWidth, false);
        }
    }

    llvm::Value* getConstant(double val) {
        auto* scalarConst =
            llvm::ConstantFP::get(llvm::Type::getFloatTy(context_), val);
        return (VectorWidth == 1)
                   ? (llvm::Value*)scalarConst
                   : builder_.CreateVectorSplat(VectorWidth, scalarConst);
    }

    llvm::Value* getInt32Constant(int32_t val) {
        auto* scalarConst =
            llvm::ConstantInt::get(llvm::Type::getInt32Ty(context_), val);
        return (VectorWidth == 1)
                   ? (llvm::Value*)scalarConst
                   : builder_.CreateVectorSplat(VectorWidth, scalarConst);
    }

    std::string getFunctionName(const std::string& base_name) {
        if (VectorWidth == 1) {
            return base_name;
        } else {
            return base_name + "_v" + std::to_string(VectorWidth);
        }
    }

    llvm::Value* createIntrinsicCall(llvm::Intrinsic::ID intrinsic_id,
                                     llvm::ArrayRef<llvm::Value*> args) {
        auto* intrinsic = llvm::Intrinsic::getOrInsertDeclaration(
            module_, intrinsic_id, getFloatType());
        return builder_.CreateCall(intrinsic, args);
    }

    llvm::Function* createFunction(
        const std::string& base_name, int arity,
        const std::function<llvm::Value*(llvm::ArrayRef<llvm::Value*>)>&
            body_generator) {
        std::string func_name = getFunctionName(base_name);
        if (auto* existing_func = module_->getFunction(func_name)) {
            return existing_func;
        }

        auto last_ip = builder_.saveIP();

        auto* float_ty = getFloatType();
        std::vector<llvm::Type*> arg_types(arity, float_ty);
        auto* func_ty = llvm::FunctionType::get(float_ty, arg_types, false);
        auto* func = llvm::Function::Create(
            func_ty, llvm::Function::ExternalLinkage, func_name, module_);

        auto* entry_bb = llvm::BasicBlock::Create(context_, "entry", func);
        builder_.SetInsertPoint(entry_bb);

        std::vector<llvm::Value*> args;
        for (auto& arg : func->args()) {
            args.push_back(&arg);
        }
        if (arity > 0) {
            args[0]->setName("x");
            if (arity > 1)
                args[1]->setName("y");
            if (arity > 2)
                args[2]->setName("z");
        }

        llvm::Value* result = body_generator(args);

        builder_.CreateRet(result);
        builder_.restoreIP(last_ip);

        if (llvm::verifyFunction(*func, &llvm::errs())) {
            func->eraseFromParent();
            return nullptr;
        }

        return func;
    }
};

template <int VectorWidth>
template <MathOp op>
llvm::Function* MathFunctionGenerator<VectorWidth>::getOrCreate() {
    constexpr auto opInfo = getMathOpInfo(op);

    if constexpr (op == MathOp::Exp) {
        // https://github.com/vapoursynth/vapoursynth/blob/2a3d3657320ca505c784b98f10e7cd9649d6169a/src/core/expr/jitcompiler_x86.cpp#L635
        return createFunction(
            opInfo.name, opInfo.arity,
            [this](llvm::ArrayRef<llvm::Value*> args) -> llvm::Value* {
                auto* x = args[0];
                auto* exp_hi = getConstant(88.3762626647949f);
                auto* exp_lo = getConstant(-88.3762626647949f);
                auto* log2e = getConstant(1.44269504088896341f);
                auto* exp_p0 = getConstant(1.9875691500E-4f);
                auto* exp_p1 = getConstant(1.3981999507E-3f);
                auto* exp_p2 = getConstant(8.3334519073E-3f);
                auto* exp_p3 = getConstant(4.1665795894E-2f);
                auto* exp_p4 = getConstant(1.6666665459E-1f);
                auto* exp_p5 = getConstant(5.0000001201E-1f);
                auto* half = getConstant(0.5f);
                auto* one = getConstant(1.0f);
                auto* neg_exp_c1 = getConstant(-0.693359375f);
                auto* neg_exp_c2 = getConstant(2.12194440e-4f);
                auto* const_0x7f = getInt32Constant(0x7f);
                auto* const_23 = getInt32Constant(23);

                x = createIntrinsicCall(llvm::Intrinsic::minnum, {x, exp_hi});
                x = createIntrinsicCall(llvm::Intrinsic::maxnum, {x, exp_lo});
                auto* fx =
                    createIntrinsicCall(llvm::Intrinsic::fma, {log2e, x, half});
                auto* etmp =
                    createIntrinsicCall(llvm::Intrinsic::nearbyint, {fx});
                auto* cmp_gt = builder_.CreateFCmpOGT(etmp, fx);
                auto* ext_cmp = builder_.CreateSExt(cmp_gt, getInt32Type());
                auto* one_int = builder_.CreateBitCast(one, getInt32Type());
                auto* mask_int = builder_.CreateAnd(ext_cmp, one_int);
                auto* mask = builder_.CreateBitCast(mask_int, getFloatType());
                fx = builder_.CreateFSub(etmp, mask);
                x = createIntrinsicCall(llvm::Intrinsic::fma,
                                        {fx, neg_exp_c1, x});
                x = createIntrinsicCall(llvm::Intrinsic::fma,
                                        {fx, neg_exp_c2, x});
                auto* z = builder_.CreateFMul(x, x);
                llvm::Value* y = exp_p0;
                y = createIntrinsicCall(llvm::Intrinsic::fma, {y, x, exp_p1});
                y = createIntrinsicCall(llvm::Intrinsic::fma, {y, x, exp_p2});
                y = createIntrinsicCall(llvm::Intrinsic::fma, {y, x, exp_p3});
                y = createIntrinsicCall(llvm::Intrinsic::fma, {y, x, exp_p4});
                y = createIntrinsicCall(llvm::Intrinsic::fma, {y, x, exp_p5});
                y = createIntrinsicCall(llvm::Intrinsic::fma, {y, z, x});
                y = builder_.CreateFAdd(y, one);
                auto* emm0_float =
                    createIntrinsicCall(llvm::Intrinsic::nearbyint, {fx});
                auto* emm0 = builder_.CreateFPToSI(emm0_float, getInt32Type());
                emm0 = builder_.CreateAdd(emm0, const_0x7f);
                emm0 = builder_.CreateShl(emm0, const_23);
                auto* emm0_as_float =
                    builder_.CreateBitCast(emm0, getFloatType());
                x = builder_.CreateFMul(y, emm0_as_float);
                return x;
            });
    } else if constexpr (op == MathOp::Log) {
        // https://github.com/vapoursynth/vapoursynth/blob/2a3d3657320ca505c784b98f10e7cd9649d6169a/src/core/expr/jitcompiler_x86.cpp#L671
        return createFunction(
            opInfo.name, opInfo.arity,
            [this](llvm::ArrayRef<llvm::Value*> args) -> llvm::Value* {
                auto* x = args[0];
                auto* min_norm_pos = getInt32Constant(0x00800000);
                auto* inv_mant_mask = getInt32Constant(~0x7F800000);
                auto* sqrt_1_2 = getConstant(0.707106781186547524f);
                auto* log_p0 = getConstant(7.0376836292E-2f);
                auto* log_p1 = getConstant(-1.1514610310E-1f);
                auto* log_p2 = getConstant(1.1676998740E-1f);
                auto* log_p3 = getConstant(-1.2420140846E-1f);
                auto* log_p4 = getConstant(1.4249322787E-1f);
                auto* log_p5 = getConstant(-1.6668057665E-1f);
                auto* log_p6 = getConstant(2.0000714765E-1f);
                auto* log_p7 = getConstant(-2.4999993993E-1f);
                auto* log_p8 = getConstant(3.3333331174E-1f);
                auto* log_q2 = getConstant(0.693359375f);
                auto* log_q1 = getConstant(-2.12194440e-4f);
                auto* one = getConstant(1.0f);
                auto* neg_half = getConstant(-0.5f);
                auto* const_0x7f = getInt32Constant(0x7f);
                auto* const_23 = getInt32Constant(23);
                auto* is_one = builder_.CreateFCmpOEQ(x, one);
                auto* min_norm_pos_float =
                    builder_.CreateBitCast(min_norm_pos, getFloatType());
                x = createIntrinsicCall(llvm::Intrinsic::maxnum,
                                        {x, min_norm_pos_float});
                auto* x_as_int = builder_.CreateBitCast(x, getInt32Type());
                auto* emm0i = builder_.CreateLShr(x_as_int, const_23);
                auto* x_masked = builder_.CreateAnd(x_as_int, inv_mant_mask);
                auto* half_as_int =
                    builder_.CreateBitCast(getConstant(0.5f), getInt32Type());
                x_masked = builder_.CreateOr(x_masked, half_as_int);
                x = builder_.CreateBitCast(x_masked, getFloatType());
                emm0i = builder_.CreateSub(emm0i, const_0x7f);
                auto* emm0 = builder_.CreateSIToFP(emm0i, getFloatType());
                emm0 = builder_.CreateFAdd(emm0, one);
                auto* mask = builder_.CreateFCmpOLT(x, sqrt_1_2);
                auto* ext_mask = builder_.CreateSExt(mask, getInt32Type());
                x_as_int = builder_.CreateBitCast(x, getInt32Type());
                auto* etmp_as_int = builder_.CreateAnd(ext_mask, x_as_int);
                auto* etmp =
                    builder_.CreateBitCast(etmp_as_int, getFloatType());
                x = builder_.CreateFSub(x, one);
                auto* one_as_int = builder_.CreateBitCast(one, getInt32Type());
                auto* maskf_as_int = builder_.CreateAnd(ext_mask, one_as_int);
                auto* maskf =
                    builder_.CreateBitCast(maskf_as_int, getFloatType());
                emm0 = builder_.CreateFSub(emm0, maskf);
                x = builder_.CreateFAdd(x, etmp);
                auto* z = builder_.CreateFMul(x, x);
                auto* fma_intrinsic = llvm::Intrinsic::getOrInsertDeclaration(
                    module_, llvm::Intrinsic::fma, getFloatType());
                llvm::Value* y = log_p0;
                y = builder_.CreateCall(fma_intrinsic, {y, x, log_p1});
                y = builder_.CreateCall(fma_intrinsic, {y, x, log_p2});
                y = builder_.CreateCall(fma_intrinsic, {y, x, log_p3});
                y = builder_.CreateCall(fma_intrinsic, {y, x, log_p4});
                y = builder_.CreateCall(fma_intrinsic, {y, x, log_p5});
                y = builder_.CreateCall(fma_intrinsic, {y, x, log_p6});
                y = builder_.CreateCall(fma_intrinsic, {y, x, log_p7});
                y = builder_.CreateCall(fma_intrinsic, {y, x, log_p8});
                y = builder_.CreateFMul(y, x);
                y = builder_.CreateFMul(y, z);
                y = builder_.CreateCall(fma_intrinsic, {emm0, log_q1, y});
                y = builder_.CreateCall(fma_intrinsic, {z, neg_half, y});
                x = builder_.CreateFAdd(x, y);
                x = builder_.CreateCall(fma_intrinsic, {emm0, log_q2, x});
                x_as_int = builder_.CreateBitCast(x, getInt32Type());
                auto* ext_is_one = builder_.CreateSExt(is_one, getInt32Type());
                auto* not_ext_is_one = builder_.CreateNot(ext_is_one);
                auto* result_as_int =
                    builder_.CreateAnd(not_ext_is_one, x_as_int);
                x = builder_.CreateBitCast(result_as_int, getFloatType());
                return x;
            });
    } else if constexpr (op == MathOp::Sin) {
        // https://github.com/vapoursynth/vapoursynth/blob/2a3d3657320ca505c784b98f10e7cd9649d6169a/src/core/expr/jitcompiler_x86.cpp#L813
        return createFunction(
            opInfo.name, opInfo.arity,
            [this](llvm::ArrayRef<llvm::Value*> args) -> llvm::Value* {
                auto* x = args[0];
                auto* float_ty = getFloatType();
                auto* int32_ty = getInt32Type();
                auto* float_invpi = getConstant(0.31830988618f);
                auto* float_pi1 = getConstant(3.140625f);
                auto* float_pi2 = getConstant(0.0009670257568359375f);
                auto* float_pi3 = getConstant(1.984187252998352e-07f);
                auto* float_pi4 = getConstant(1.273533813134432e-11f);
                auto* float_sinC3 = getConstant(-0.1666666567325592f);
                auto* float_sinC5 = getConstant(0.00833307858556509f);
                auto* float_sinC7 = getConstant(-0.00019807418575510383f);
                auto* float_sinC9 = getConstant(2.6019030363451748e-06f);
                auto* signmask = getInt32Constant(0x80000000);
                llvm::Value* sign = builder_.CreateBitCast(x, int32_ty);
                sign = builder_.CreateAnd(sign, signmask);
                llvm::Value* t1 =
                    createIntrinsicCall(llvm::Intrinsic::fabs, {x});
                llvm::Value* t2 = builder_.CreateFMul(t1, float_invpi);
                llvm::Value* t2_rounded =
                    createIntrinsicCall(llvm::Intrinsic::nearbyint, {t2});
                llvm::Value* t2i = builder_.CreateFPToSI(t2_rounded, int32_ty);
                llvm::Value* t4 = builder_.CreateShl(t2i, 31);
                sign = builder_.CreateXor(sign, t4);
                t2 = builder_.CreateSIToFP(t2i, float_ty);
                auto* fma_intrinsic = llvm::Intrinsic::getOrInsertDeclaration(
                    module_, llvm::Intrinsic::fma, {float_ty});
                t1 = builder_.CreateCall(
                    fma_intrinsic, {t2, builder_.CreateFNeg(float_pi1), t1});
                t1 = builder_.CreateCall(
                    fma_intrinsic, {t2, builder_.CreateFNeg(float_pi2), t1});
                t1 = builder_.CreateCall(
                    fma_intrinsic, {t2, builder_.CreateFNeg(float_pi3), t1});
                t1 = builder_.CreateCall(
                    fma_intrinsic, {t2, builder_.CreateFNeg(float_pi4), t1});
                t2 = builder_.CreateFMul(t1, t1);
                llvm::Value* t3 = builder_.CreateCall(
                    fma_intrinsic, {t2, float_sinC9, float_sinC7});
                t3 = builder_.CreateCall(fma_intrinsic, {t3, t2, float_sinC5});
                t3 = builder_.CreateCall(fma_intrinsic, {t3, t2, float_sinC3});
                t3 = builder_.CreateFMul(t3, t2);
                t3 = builder_.CreateFMul(t3, t1);
                t1 = builder_.CreateFAdd(t1, t3);
                llvm::Value* t1_as_int = builder_.CreateBitCast(t1, int32_ty);
                llvm::Value* result_as_int =
                    builder_.CreateXor(sign, t1_as_int);
                return builder_.CreateBitCast(result_as_int, float_ty);
            });
    } else if constexpr (op == MathOp::Cos) {
        // https://github.com/vapoursynth/vapoursynth/blob/2a3d3657320ca505c784b98f10e7cd9649d6169a/src/core/expr/jitcompiler_x86.cpp#L813
        return createFunction(
            opInfo.name, opInfo.arity,
            [this](llvm::ArrayRef<llvm::Value*> args) -> llvm::Value* {
                auto* x = args[0];
                auto* float_ty = getFloatType();
                auto* int32_ty = getInt32Type();
                auto* float_invpi = getConstant(0.31830988618f);
                auto* float_pi1 = getConstant(3.140625f);
                auto* float_pi2 = getConstant(0.0009670257568359375f);
                auto* float_pi3 = getConstant(1.984187252998352e-07f);
                auto* float_pi4 = getConstant(1.273533813134432e-11f);
                auto* float_cosC2 = getConstant(-0.4999999701976776f);
                auto* float_cosC4 = getConstant(0.04166652262210846f);
                auto* float_cosC6 = getConstant(-0.001388676579343155f);
                auto* float_cosC8 = getConstant(2.4390448881604243e-05f);
                auto* one_float = getConstant(1.0f);
                llvm::Value* sign = getInt32Constant(0);
                llvm::Value* t1 =
                    createIntrinsicCall(llvm::Intrinsic::fabs, {x});
                llvm::Value* t2 = builder_.CreateFMul(t1, float_invpi);
                llvm::Value* t2_rounded =
                    createIntrinsicCall(llvm::Intrinsic::nearbyint, {t2});
                llvm::Value* t2i = builder_.CreateFPToSI(t2_rounded, int32_ty);
                llvm::Value* t4 = builder_.CreateShl(t2i, 31);
                sign = builder_.CreateXor(sign, t4);
                t2 = builder_.CreateSIToFP(t2i, float_ty);
                auto* fma_intrinsic = llvm::Intrinsic::getOrInsertDeclaration(
                    module_, llvm::Intrinsic::fma, {float_ty});
                t1 = builder_.CreateCall(
                    fma_intrinsic, {t2, builder_.CreateFNeg(float_pi1), t1});
                t1 = builder_.CreateCall(
                    fma_intrinsic, {t2, builder_.CreateFNeg(float_pi2), t1});
                t1 = builder_.CreateCall(
                    fma_intrinsic, {t2, builder_.CreateFNeg(float_pi3), t1});
                t1 = builder_.CreateCall(
                    fma_intrinsic, {t2, builder_.CreateFNeg(float_pi4), t1});
                t2 = builder_.CreateFMul(t1, t1);
                llvm::Value* t3 = builder_.CreateCall(
                    fma_intrinsic, {t2, float_cosC8, float_cosC6});
                t3 = builder_.CreateCall(fma_intrinsic, {t3, t2, float_cosC4});
                t3 = builder_.CreateCall(fma_intrinsic, {t3, t2, float_cosC2});
                t1 = builder_.CreateCall(fma_intrinsic, {t3, t2, one_float});
                llvm::Value* t1_as_int = builder_.CreateBitCast(t1, int32_ty);
                llvm::Value* result_as_int =
                    builder_.CreateXor(sign, t1_as_int);
                return builder_.CreateBitCast(result_as_int, float_ty);
            });
    } else if constexpr (op == MathOp::Tan) {
        return createFunction(
            opInfo.name, opInfo.arity,
            [this](llvm::ArrayRef<llvm::Value*> args) -> llvm::Value* {
                auto* x = args[0];
                llvm::Function* sinFunc = this->getOrCreate<MathOp::Sin>();
                llvm::Function* cosFunc = this->getOrCreate<MathOp::Cos>();
                llvm::Value* sin_x = builder_.CreateCall(sinFunc, {x});
                llvm::Value* cos_x = builder_.CreateCall(cosFunc, {x});
                return builder_.CreateFDiv(sin_x, cos_x);
            });
    } else if constexpr (op == MathOp::Atan) {
        // https://stackoverflow.com/a/23097989
        return createFunction(
            opInfo.name, opInfo.arity,
            [this](llvm::ArrayRef<llvm::Value*> args) -> llvm::Value* {
                auto* var = args[0];
                auto* one = getConstant(1.0f);
                auto* pi_div_2 = getConstant(1.5707963267948966f);
                auto* z = createIntrinsicCall(llvm::Intrinsic::fabs, {var});
                auto* z_gt_1 = builder_.CreateFCmpOGT(z, one);
                auto* one_div_zz = builder_.CreateFDiv(one, z);
                auto* a = builder_.CreateSelect(z_gt_1, one_div_zz, z);
                auto* s = builder_.CreateFMul(a, a);
                auto* q = builder_.CreateFMul(s, s);
                llvm::Value* p = getConstant(-2.0258553044340116e-5f);
                llvm::Value* t = getConstant(2.2302240345710764e-4f);
                p = createIntrinsicCall(
                    llvm::Intrinsic::fma,
                    {p, q, getConstant(-1.1640717779912220e-3f)});
                t = createIntrinsicCall(
                    llvm::Intrinsic::fma,
                    {t, q, getConstant(3.8559749383656407e-3f)});
                p = createIntrinsicCall(
                    llvm::Intrinsic::fma,
                    {p, q, getConstant(-9.1845592187222193e-3f)});
                t = createIntrinsicCall(
                    llvm::Intrinsic::fma,
                    {t, q, getConstant(1.6978035834594660e-2f)});
                p = createIntrinsicCall(
                    llvm::Intrinsic::fma,
                    {p, q, getConstant(-2.5826796814492296e-2f)});
                t = createIntrinsicCall(
                    llvm::Intrinsic::fma,
                    {t, q, getConstant(3.4067811082715810e-2f)});
                p = createIntrinsicCall(
                    llvm::Intrinsic::fma,
                    {p, q, getConstant(-4.0926382420509999e-2f)});
                t = createIntrinsicCall(
                    llvm::Intrinsic::fma,
                    {t, q, getConstant(4.6739496199158334e-2f)});
                p = createIntrinsicCall(
                    llvm::Intrinsic::fma,
                    {p, q, getConstant(-5.2392330054601366e-2f)});
                t = createIntrinsicCall(
                    llvm::Intrinsic::fma,
                    {t, q, getConstant(5.8773077721790683e-2f)});
                p = createIntrinsicCall(
                    llvm::Intrinsic::fma,
                    {p, q, getConstant(-6.6658603633512892e-2f)});
                t = createIntrinsicCall(
                    llvm::Intrinsic::fma,
                    {t, q, getConstant(7.6922129305867892e-2f)});
                p = createIntrinsicCall(llvm::Intrinsic::fma, {p, s, t});
                p = createIntrinsicCall(
                    llvm::Intrinsic::fma,
                    {p, s, getConstant(-9.0909012354005267e-2f)});
                p = createIntrinsicCall(
                    llvm::Intrinsic::fma,
                    {p, s, getConstant(1.1111110678749421e-1f)});
                p = createIntrinsicCall(
                    llvm::Intrinsic::fma,
                    {p, s, getConstant(-1.4285714271334810e-1f)});
                p = createIntrinsicCall(
                    llvm::Intrinsic::fma,
                    {p, s, getConstant(1.9999999999755005e-1f)});
                p = createIntrinsicCall(
                    llvm::Intrinsic::fma,
                    {p, s, getConstant(-3.3333333333331838e-1f)});
                auto* pp_mul_ss = builder_.CreateFMul(p, s);
                p = createIntrinsicCall(llvm::Intrinsic::fma,
                                        {pp_mul_ss, a, a});
                auto* rr_if_gt_1 = builder_.CreateFSub(pi_div_2, p);
                auto* rr = builder_.CreateSelect(z_gt_1, rr_if_gt_1, p);
                return createIntrinsicCall(llvm::Intrinsic::copysign,
                                           {rr, var});
            });
    } else if constexpr (op == MathOp::Atan2) {
        return createFunction(
            opInfo.name, opInfo.arity,
            [this](llvm::ArrayRef<llvm::Value*> args) -> llvm::Value* {
                auto* var_y = args[0];
                auto* var_x = args[1];
                auto* atan_func = this->getOrCreate<MathOp::Atan>();
                auto* zero = getConstant(0.0f);
                auto* pi = getConstant(3.141592653589793f);
                auto* pi_div_2 = getConstant(1.5707963267948966f);
                auto* y_div_x = builder_.CreateFDiv(var_y, var_x);
                auto* atan_y_div_x = builder_.CreateCall(atan_func, {y_div_x});
                auto* res_x_gt_0 = atan_y_div_x;
                auto* signed_pi =
                    createIntrinsicCall(llvm::Intrinsic::copysign, {pi, var_y});
                auto* res_x_lt_0 = builder_.CreateFAdd(atan_y_div_x, signed_pi);
                auto* res_x_eq_0 = createIntrinsicCall(
                    llvm::Intrinsic::copysign, {pi_div_2, var_y});
                auto* x_gt_0 = builder_.CreateFCmpOGT(var_x, zero);
                auto* x_lt_0 = builder_.CreateFCmpOLT(var_x, zero);
                auto* result =
                    builder_.CreateSelect(x_gt_0, res_x_gt_0, res_x_lt_0);
                result = builder_.CreateSelect(
                    x_lt_0, res_x_lt_0,
                    builder_.CreateSelect(x_gt_0, res_x_gt_0, res_x_eq_0));
                auto* x_is_zero = builder_.CreateFCmpOEQ(var_x, zero);
                auto* y_is_zero = builder_.CreateFCmpOEQ(var_y, zero);
                auto* both_zero = builder_.CreateAnd(x_is_zero, y_is_zero);
                result = builder_.CreateSelect(both_zero, zero, result);
                return result;
            });
    } else if constexpr (op == MathOp::Acos) {
        // https://forwardscattering.org/post/66
        return createFunction(
            opInfo.name, opInfo.arity,
            [this](llvm::ArrayRef<llvm::Value*> args) -> llvm::Value* {
                auto* x = args[0];
                auto* pi = getConstant(3.14159265f);
                auto* ax = createIntrinsicCall(llvm::Intrinsic::fabs, {x});

                auto* term1_mul = getConstant(-0.124605335f);
                auto* term1_add = getConstant(0.1570634f);
                auto* term1 = createIntrinsicCall(llvm::Intrinsic::fma,
                                                  {ax, term1_mul, term1_add});

                auto* term2_sub = getConstant(0.99418175f);
                auto* term2 = builder_.CreateFSub(term2_sub, ax);

                auto* poly_part = builder_.CreateFMul(term1, term2);

                auto* two = getConstant(2.0f);
                auto* neg_two = getConstant(-2.0f);
                auto* sqrt_arg = createIntrinsicCall(llvm::Intrinsic::fma,
                                                     {ax, neg_two, two});
                auto* sqrt_part =
                    createIntrinsicCall(llvm::Intrinsic::sqrt, {sqrt_arg});

                auto* res_pos = builder_.CreateFAdd(poly_part, sqrt_part);

                auto* zero = getConstant(0.0f);
                auto* is_neg = builder_.CreateFCmpOLT(x, zero);

                auto* res_neg = builder_.CreateFSub(pi, res_pos);

                return builder_.CreateSelect(is_neg, res_neg, res_pos);
            });
    }
    return nullptr;
}

using SupportedMathOpsTuple =
    std::tuple<std::integral_constant<MathOp, MathOp::Exp>,
               std::integral_constant<MathOp, MathOp::Log>,
               std::integral_constant<MathOp, MathOp::Sin>,
               std::integral_constant<MathOp, MathOp::Cos>,
               std::integral_constant<MathOp, MathOp::Tan>,
               std::integral_constant<MathOp, MathOp::Atan>,
               std::integral_constant<MathOp, MathOp::Atan2>,
               std::integral_constant<MathOp, MathOp::Acos>>;

class MathLibraryManager {
  public:
    MathLibraryManager(llvm::Module* module, llvm::LLVMContext& context)
        : module_(module), context_(context) {}

    llvm::Function* getFunction(MathOp op) {
        if (auto it = funcCache_.find(op); it != funcCache_.end()) {
            return it->second;
        }
        return generateAndCache(op);
    }

  private:
    llvm::Module* module_;
    llvm::LLVMContext& context_;
    std::map<MathOp, llvm::Function*> funcCache_;

    template <MathOp op, int VectorWidth> llvm::Function* dispatch() {
        MathFunctionGenerator<VectorWidth> generator(module_, context_);
        return generator.template getOrCreate<op>();
    }

    template <MathOp op> llvm::Function* generateAndCacheImpl() {
        llvm::Function* scalarFunc = dispatch<op, 1>();

        if (!scalarFunc) {
            return nullptr;
        }

        // https://llvm.org/docs/LangRef.html#id1998
        auto link_vectors = [&]<int... vlen>(
                                std::integer_sequence<int, vlen...>) {
            (
                [&] {
                    llvm::Function* vecFunc = dispatch<op, vlen>();

                    if (vecFunc) {
                        std::string isa;
#ifdef __x86_64__
                        if constexpr (vlen == 4) {
                            isa = "b"; // SSE
                        } else if constexpr (vlen == 8) {
                            isa = "d"; // AVX2
                        } else if constexpr (vlen == 16) {
                            isa = "e"; // AVX512
                        }
#elif defined(__ARM_NEON__)
                        if constexpr (vlen == 4) {
                            isa = "n"; // Armv8 Advanced SIMD
                        } else {
                            isa = "s"; // SVE
                        }
#else
#error "Unsupported architecture. Only x86_64 and ARM NEON are supported."
#endif

                        constexpr auto opInfo = getMathOpInfo(op);
                        std::string parameters(opInfo.arity, 'v');
                        std::string mask = "N";
                        std::string abi_string =
                            std::format("_ZGV{}{}{}{}_{}({})", isa, mask, vlen,
                                        parameters, scalarFunc->getName().str(),
                                        vecFunc->getName().str());

                        scalarFunc->addFnAttr(llvm::Attribute::get(
                            context_, "vector-function-abi-variant",
                            abi_string));
                    }
                }(),
                ...);
        };

        link_vectors(SupportedVectorWidths{});

        funcCache_[op] = scalarFunc;
        return scalarFunc;
    }

    llvm::Function* generateAndCache(MathOp op) {
        llvm::Function* result = nullptr;
        std::apply(
            [&](auto... op_constant) {
                auto dispatcher = [&](auto op_c) {
                    if (op_c.value == op) {
                        result = generateAndCacheImpl<op_c.value>();
                    }
                };
                (dispatcher(op_constant), ...);
            },
            SupportedMathOpsTuple{});
        return result;
    }
};

#endif // LLVMEXPR_MATH_FUNCTIONS_HPP
