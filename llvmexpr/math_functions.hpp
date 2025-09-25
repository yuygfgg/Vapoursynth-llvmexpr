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

#include <functional>
#include <map>
#include <string>
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

enum class MathOp { Exp, Log, Sin, Cos, Tan, Atan };

enum class BinaryMathOp { Atan2 };

#ifdef __x86_64__
using SupportedVectorWidths = std::integer_sequence<int, 4, 8, 16>;
#else
using SupportedVectorWidths = std::integer_sequence<int, 4>;
#endif
// TODO: Figure out if other architectures have >4 wide vector support.

template <int VectorWidth> class MathFunctionGenerator {
  public:
    MathFunctionGenerator(llvm::Module* module, llvm::LLVMContext& context)
        : module_(module), context_(context), builder_(context) {}

    llvm::Function* getOrCreateExp();
    llvm::Function* getOrCreateLog();
    llvm::Function* getOrCreateSin();
    llvm::Function* getOrCreateCos();
    llvm::Function* getOrCreateTan();
    llvm::Function* getOrCreateAtan();
    llvm::Function* getOrCreateAtan2();

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

    llvm::Function* createUnaryFunction(
        const std::string& base_name,
        const std::function<llvm::Value*(llvm::Value*)>& body_generator) {
        std::string func_name = getFunctionName(base_name);
        if (auto* existing_func = module_->getFunction(func_name)) {
            return existing_func;
        }

        auto last_ip = builder_.saveIP();

        auto* float_ty = getFloatType();
        auto* func_ty = llvm::FunctionType::get(float_ty, {float_ty}, false);
        auto* func = llvm::Function::Create(
            func_ty, llvm::Function::ExternalLinkage, func_name, module_);

        auto* entry_bb = llvm::BasicBlock::Create(context_, "entry", func);
        builder_.SetInsertPoint(entry_bb);

        auto* arg = func->getArg(0);
        arg->setName("x");

        llvm::Value* result = body_generator(arg);

        builder_.CreateRet(result);
        builder_.restoreIP(last_ip);

        if (llvm::verifyFunction(*func, &llvm::errs())) {
            func->eraseFromParent();
            return nullptr;
        }

        return func;
    }

    llvm::Function* createBinaryFunction(
        const std::string& base_name,
        const std::function<llvm::Value*(llvm::Value*, llvm::Value*)>&
            body_generator) {
        std::string func_name = getFunctionName(base_name);
        if (auto* existing_func = module_->getFunction(func_name)) {
            return existing_func;
        }

        auto last_ip = builder_.saveIP();

        auto* float_ty = getFloatType();
        auto* func_ty =
            llvm::FunctionType::get(float_ty, {float_ty, float_ty}, false);
        auto* func = llvm::Function::Create(
            func_ty, llvm::Function::ExternalLinkage, func_name, module_);

        auto* entry_bb = llvm::BasicBlock::Create(context_, "entry", func);
        builder_.SetInsertPoint(entry_bb);

        auto* arg1 = func->getArg(0);
        arg1->setName("y");
        auto* arg2 = func->getArg(1);
        arg2->setName("x");

        llvm::Value* result = body_generator(arg1, arg2);

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
llvm::Function* MathFunctionGenerator<VectorWidth>::getOrCreateExp() {
    return createUnaryFunction(
        "fast_exp", [this](llvm::Value* x) -> llvm::Value* {
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

            // x = min(x, exp_hi)
            x = createIntrinsicCall(llvm::Intrinsic::minnum, {x, exp_hi});

            // x = max(x, exp_lo)
            x = createIntrinsicCall(llvm::Intrinsic::maxnum, {x, exp_lo});

            // fx = log2e * x + 0.5
            auto* fx =
                createIntrinsicCall(llvm::Intrinsic::fma, {log2e, x, half});

            // emm0 = round(fx)
            auto* etmp = createIntrinsicCall(llvm::Intrinsic::nearbyint, {fx});

            // mask = (etmp > fx) ? 1.0f : 0.0f (as float bits)
            auto* cmp_gt = builder_.CreateFCmpOGT(etmp, fx);
            auto* ext_cmp = builder_.CreateSExt(cmp_gt, getInt32Type());
            auto* one_int = builder_.CreateBitCast(one, getInt32Type());
            auto* mask_int = builder_.CreateAnd(ext_cmp, one_int);
            auto* mask = builder_.CreateBitCast(mask_int, getFloatType());

            // fx = etmp - mask
            fx = builder_.CreateFSub(etmp, mask);

            // x = x + fx * (-exp_c1)
            auto* temp1 = builder_.CreateFMul(fx, neg_exp_c1);
            x = builder_.CreateFAdd(x, temp1);

            // x = x + fx * (-exp_c2)
            auto* temp2 = builder_.CreateFMul(fx, neg_exp_c2);
            x = builder_.CreateFAdd(x, temp2);

            // z = x * x
            auto* z = builder_.CreateFMul(x, x);

            // y = exp_p0
            llvm::Value* y = exp_p0;

            // Polynomial evaluation: y = y * x + exp_p1, etc.
            y = createIntrinsicCall(llvm::Intrinsic::fma, {y, x, exp_p1});
            y = createIntrinsicCall(llvm::Intrinsic::fma, {y, x, exp_p2});
            y = createIntrinsicCall(llvm::Intrinsic::fma, {y, x, exp_p3});
            y = createIntrinsicCall(llvm::Intrinsic::fma, {y, x, exp_p4});
            y = createIntrinsicCall(llvm::Intrinsic::fma, {y, x, exp_p5});

            // y = y * z + x
            y = createIntrinsicCall(llvm::Intrinsic::fma, {y, z, x});

            // y = y + 1.0
            y = builder_.CreateFAdd(y, one);

            // emm0 = round(fx) as int
            auto* emm0_float =
                createIntrinsicCall(llvm::Intrinsic::nearbyint, {fx});
            auto* emm0 = builder_.CreateFPToSI(emm0_float, getInt32Type());

            // emm0 = emm0 + 0x7f
            emm0 = builder_.CreateAdd(emm0, const_0x7f);

            // emm0 = emm0 << 23
            emm0 = builder_.CreateShl(emm0, const_23);

            // x = y * (emm0 as float)
            auto* emm0_as_float = builder_.CreateBitCast(emm0, getFloatType());
            x = builder_.CreateFMul(y, emm0_as_float);

            return x;
        });
}

template <int VectorWidth>
llvm::Function* MathFunctionGenerator<VectorWidth>::getOrCreateLog() {
    return createUnaryFunction(
        "fast_log", [this](llvm::Value* x) -> llvm::Value* {
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

            // Special case masks
            auto* is_one = builder_.CreateFCmpOEQ(x, one);

            // x = max(x, min_norm_pos as float)
            auto* min_norm_pos_float =
                builder_.CreateBitCast(min_norm_pos, getFloatType());
            x = createIntrinsicCall(llvm::Intrinsic::maxnum,
                                    {x, min_norm_pos_float});

            // emm0i = (x as int) >> 23
            auto* x_as_int = builder_.CreateBitCast(x, getInt32Type());
            auto* emm0i = builder_.CreateLShr(x_as_int, const_23);

            // x = (x as int) & inv_mant_mask
            auto* x_masked = builder_.CreateAnd(x_as_int, inv_mant_mask);
            // x = x | (0.5f as int)
            auto* half_as_int =
                builder_.CreateBitCast(getConstant(0.5f), getInt32Type());
            x_masked = builder_.CreateOr(x_masked, half_as_int);
            x = builder_.CreateBitCast(x_masked, getFloatType());

            // emm0i = emm0i - 0x7f
            emm0i = builder_.CreateSub(emm0i, const_0x7f);
            auto* emm0 = builder_.CreateSIToFP(emm0i, getFloatType());

            // emm0 = emm0 + 1.0f
            emm0 = builder_.CreateFAdd(emm0, one);

            // mask = (x < sqrt_1_2)
            auto* mask = builder_.CreateFCmpOLT(x, sqrt_1_2);

            // etmp = mask ? x : 0.0f
            auto* ext_mask = builder_.CreateSExt(mask, getInt32Type());
            x_as_int = builder_.CreateBitCast(x, getInt32Type());
            auto* etmp_as_int = builder_.CreateAnd(ext_mask, x_as_int);
            auto* etmp = builder_.CreateBitCast(etmp_as_int, getFloatType());

            // x = x - 1.0f
            x = builder_.CreateFSub(x, one);

            // maskf = mask ? 1.0f : 0.0f
            auto* one_as_int = builder_.CreateBitCast(one, getInt32Type());
            auto* maskf_as_int = builder_.CreateAnd(ext_mask, one_as_int);
            auto* maskf = builder_.CreateBitCast(maskf_as_int, getFloatType());

            // emm0 = emm0 - maskf
            emm0 = builder_.CreateFSub(emm0, maskf);

            // x = x + etmp
            x = builder_.CreateFAdd(x, etmp);

            // z = x * x
            auto* z = builder_.CreateFMul(x, x);

            // Polynomial approximation
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

            // Handle special cases
            x_as_int = builder_.CreateBitCast(x, getInt32Type());
            auto* ext_is_one = builder_.CreateSExt(is_one, getInt32Type());
            auto* not_ext_is_one = builder_.CreateNot(ext_is_one);
            auto* result_as_int = builder_.CreateAnd(not_ext_is_one, x_as_int);
            x = builder_.CreateBitCast(result_as_int, getFloatType());

            return x;
        });
}

template <int VectorWidth>
llvm::Function* MathFunctionGenerator<VectorWidth>::getOrCreateSin() {
    return createUnaryFunction(
        "fast_sin", [this](llvm::Value* x) -> llvm::Value* {
            auto* float_ty = getFloatType();
            auto* int32_ty = getInt32Type();

            auto* float_invpi = getConstant(0.31830988618f); // 1.0f / pi
            auto* float_pi1 = getConstant(3.140625f);
            auto* float_pi2 = getConstant(0.0009670257568359375f);
            auto* float_pi3 = getConstant(1.984187252998352e-07f);
            auto* float_pi4 = getConstant(1.273533813134432e-11f);
            auto* float_sinC3 = getConstant(-0.1666666567325592f);
            auto* float_sinC5 = getConstant(0.00833307858556509f);
            auto* float_sinC7 = getConstant(-0.00019807418575510383f);
            auto* float_sinC9 = getConstant(2.6019030363451748e-06f);

            auto* signmask = getInt32Constant(0x80000000);

            // sign = (x as int) & 0x80000000;
            llvm::Value* sign = builder_.CreateBitCast(x, int32_ty);
            sign = builder_.CreateAnd(sign, signmask);

            // t1 = Abs(x)
            llvm::Value* t1 = createIntrinsicCall(llvm::Intrinsic::fabs, {x});

            // Range reduction to [-pi/2, pi/2]
            llvm::Value* t2 = builder_.CreateFMul(t1, float_invpi);

            llvm::Value* t2_rounded =
                createIntrinsicCall(llvm::Intrinsic::nearbyint, {t2});
            llvm::Value* t2i = builder_.CreateFPToSI(t2_rounded, int32_ty);

            // sign ^= (t2i << 31); // Flip sign based on quadrant
            llvm::Value* t4 = builder_.CreateShl(t2i, 31);
            sign = builder_.CreateXor(sign, t4);

            t2 = builder_.CreateSIToFP(t2i, float_ty);

            // Reconstruct the value in the [-pi/2, pi/2] range using
            // extended precision pi
            auto* fma_intrinsic = llvm::Intrinsic::getOrInsertDeclaration(
                module_, llvm::Intrinsic::fma, {float_ty});

            t1 = builder_.CreateCall(fma_intrinsic,
                                     {t2, builder_.CreateFNeg(float_pi1), t1});
            t1 = builder_.CreateCall(fma_intrinsic,
                                     {t2, builder_.CreateFNeg(float_pi2), t1});
            t1 = builder_.CreateCall(fma_intrinsic,
                                     {t2, builder_.CreateFNeg(float_pi3), t1});
            t1 = builder_.CreateCall(fma_intrinsic,
                                     {t2, builder_.CreateFNeg(float_pi4), t1});

            // Minimax polynomial for sin(x)
            t2 = builder_.CreateFMul(t1, t1); // x^2
            llvm::Value* t3 = builder_.CreateCall(
                fma_intrinsic, {t2, float_sinC9, float_sinC7});
            t3 = builder_.CreateCall(fma_intrinsic, {t3, t2, float_sinC5});
            t3 = builder_.CreateCall(fma_intrinsic, {t3, t2, float_sinC3});
            t3 = builder_.CreateFMul(t3, t2);
            t3 = builder_.CreateFMul(t3, t1);
            t1 = builder_.CreateFAdd(t1, t3);

            // Apply final sign
            llvm::Value* t1_as_int = builder_.CreateBitCast(t1, int32_ty);
            llvm::Value* result_as_int = builder_.CreateXor(sign, t1_as_int);
            return builder_.CreateBitCast(result_as_int, float_ty);
        });
}

template <int VectorWidth>
llvm::Function* MathFunctionGenerator<VectorWidth>::getOrCreateCos() {
    return createUnaryFunction(
        "fast_cos", [this](llvm::Value* x) -> llvm::Value* {
            auto* float_ty = getFloatType();
            auto* int32_ty = getInt32Type();

            auto* float_invpi = getConstant(0.31830988618f); // 1.0f / pi
            auto* float_pi1 = getConstant(3.140625f);
            auto* float_pi2 = getConstant(0.0009670257568359375f);
            auto* float_pi3 = getConstant(1.984187252998352e-07f);
            auto* float_pi4 = getConstant(1.273533813134432e-11f);
            auto* float_cosC2 = getConstant(-0.4999999701976776f);
            auto* float_cosC4 = getConstant(0.04166652262210846f);
            auto* float_cosC6 = getConstant(-0.001388676579343155f);
            auto* float_cosC8 = getConstant(2.4390448881604243e-05f);
            auto* one_float = getConstant(1.0f);

            // sign = 0;
            llvm::Value* sign = getInt32Constant(0);

            // t1 = Abs(x)
            llvm::Value* t1 = createIntrinsicCall(llvm::Intrinsic::fabs, {x});

            // Range reduction to [-pi/2, pi/2]
            llvm::Value* t2 = builder_.CreateFMul(t1, float_invpi);

            llvm::Value* t2_rounded =
                createIntrinsicCall(llvm::Intrinsic::nearbyint, {t2});
            llvm::Value* t2i = builder_.CreateFPToSI(t2_rounded, int32_ty);

            // sign ^= (t2i << 31); // Flip sign based on quadrant
            llvm::Value* t4 = builder_.CreateShl(t2i, 31);
            sign = builder_.CreateXor(sign, t4);

            t2 = builder_.CreateSIToFP(t2i, float_ty);

            // Reconstruct the value in the [-pi/2, pi/2] range using
            // extended precision pi
            auto* fma_intrinsic = llvm::Intrinsic::getOrInsertDeclaration(
                module_, llvm::Intrinsic::fma, {float_ty});

            t1 = builder_.CreateCall(fma_intrinsic,
                                     {t2, builder_.CreateFNeg(float_pi1), t1});
            t1 = builder_.CreateCall(fma_intrinsic,
                                     {t2, builder_.CreateFNeg(float_pi2), t1});
            t1 = builder_.CreateCall(fma_intrinsic,
                                     {t2, builder_.CreateFNeg(float_pi3), t1});
            t1 = builder_.CreateCall(fma_intrinsic,
                                     {t2, builder_.CreateFNeg(float_pi4), t1});

            // Minimax polynomial for cos(x)
            t2 = builder_.CreateFMul(t1, t1); // x^2
            llvm::Value* t3 = builder_.CreateCall(
                fma_intrinsic, {t2, float_cosC8, float_cosC6});
            t3 = builder_.CreateCall(fma_intrinsic, {t3, t2, float_cosC4});
            t3 = builder_.CreateCall(fma_intrinsic, {t3, t2, float_cosC2});
            t1 = builder_.CreateCall(fma_intrinsic, {t3, t2, one_float});

            // Apply final sign
            llvm::Value* t1_as_int = builder_.CreateBitCast(t1, int32_ty);
            llvm::Value* result_as_int = builder_.CreateXor(sign, t1_as_int);
            return builder_.CreateBitCast(result_as_int, float_ty);
        });
}

template <int VectorWidth>
llvm::Function* MathFunctionGenerator<VectorWidth>::getOrCreateTan() {
    return createUnaryFunction(
        "fast_tan", [this](llvm::Value* x) -> llvm::Value* {
            // tan(x) = sin(x) / cos(x)
            llvm::Function* sinFunc = this->getOrCreateSin();
            llvm::Function* cosFunc = this->getOrCreateCos();

            llvm::Value* sin_x = builder_.CreateCall(sinFunc, {x});
            llvm::Value* cos_x = builder_.CreateCall(cosFunc, {x});
            return builder_.CreateFDiv(sin_x, cos_x);
        });
}

template <int VectorWidth>
llvm::Function* MathFunctionGenerator<VectorWidth>::getOrCreateAtan() {
    return createUnaryFunction(
        "fast_atan", [this](llvm::Value* var) -> llvm::Value* {
            auto* one = getConstant(1.0f);
            auto* pi_div_2 = getConstant(1.5707963267948966f);

            auto* zz = createIntrinsicCall(llvm::Intrinsic::fabs, {var});
            auto* zz_gt_1 = builder_.CreateFCmpOGT(zz, one);

            auto* one_div_zz = builder_.CreateFDiv(one, zz);
            auto* aa = builder_.CreateSelect(zz_gt_1, one_div_zz, zz);

            auto* ss = builder_.CreateFMul(aa, aa);
            auto* qq = builder_.CreateFMul(ss, ss);

            llvm::Value* pp = getConstant(-2.0258553044340116e-5f);
            llvm::Value* tt = getConstant(2.2302240345710764e-4f);
            pp = createIntrinsicCall(
                llvm::Intrinsic::fma,
                {pp, qq, getConstant(-1.1640717779912220e-3f)});
            tt = createIntrinsicCall(
                llvm::Intrinsic::fma,
                {tt, qq, getConstant(3.8559749383656407e-3f)});
            pp = createIntrinsicCall(
                llvm::Intrinsic::fma,
                {pp, qq, getConstant(-9.1845592187222193e-3f)});
            tt = createIntrinsicCall(
                llvm::Intrinsic::fma,
                {tt, qq, getConstant(1.6978035834594660e-2f)});
            pp = createIntrinsicCall(
                llvm::Intrinsic::fma,
                {pp, qq, getConstant(-2.5826796814492296e-2f)});
            tt = createIntrinsicCall(
                llvm::Intrinsic::fma,
                {tt, qq, getConstant(3.4067811082715810e-2f)});
            pp = createIntrinsicCall(
                llvm::Intrinsic::fma,
                {pp, qq, getConstant(-4.0926382420509999e-2f)});
            tt = createIntrinsicCall(
                llvm::Intrinsic::fma,
                {tt, qq, getConstant(4.6739496199158334e-2f)});
            pp = createIntrinsicCall(
                llvm::Intrinsic::fma,
                {pp, qq, getConstant(-5.2392330054601366e-2f)});
            tt = createIntrinsicCall(
                llvm::Intrinsic::fma,
                {tt, qq, getConstant(5.8773077721790683e-2f)});
            pp = createIntrinsicCall(
                llvm::Intrinsic::fma,
                {pp, qq, getConstant(-6.6658603633512892e-2f)});
            tt = createIntrinsicCall(
                llvm::Intrinsic::fma,
                {tt, qq, getConstant(7.6922129305867892e-2f)});

            pp = createIntrinsicCall(llvm::Intrinsic::fma, {pp, ss, tt});
            pp = createIntrinsicCall(
                llvm::Intrinsic::fma,
                {pp, ss, getConstant(-9.0909012354005267e-2f)});
            pp = createIntrinsicCall(
                llvm::Intrinsic::fma,
                {pp, ss, getConstant(1.1111110678749421e-1f)});
            pp = createIntrinsicCall(
                llvm::Intrinsic::fma,
                {pp, ss, getConstant(-1.4285714271334810e-1f)});
            pp = createIntrinsicCall(
                llvm::Intrinsic::fma,
                {pp, ss, getConstant(1.9999999999755005e-1f)});
            pp = createIntrinsicCall(
                llvm::Intrinsic::fma,
                {pp, ss, getConstant(-3.3333333333331838e-1f)});

            auto* pp_mul_ss = builder_.CreateFMul(pp, ss);
            pp = createIntrinsicCall(llvm::Intrinsic::fma, {pp_mul_ss, aa, aa});

            auto* rr_if_gt_1 = builder_.CreateFSub(pi_div_2, pp);
            auto* rr = builder_.CreateSelect(zz_gt_1, rr_if_gt_1, pp);

            return createIntrinsicCall(llvm::Intrinsic::copysign, {rr, var});
        });
}

template <int VectorWidth>
llvm::Function* MathFunctionGenerator<VectorWidth>::getOrCreateAtan2() {
    return createBinaryFunction(
        "fast_atan2",
        [this](llvm::Value* var_y, llvm::Value* var_x) -> llvm::Value* {
            auto* atan_func = this->getOrCreateAtan();

            auto* zero = getConstant(0.0f);
            auto* pi = getConstant(3.141592653589793f);
            auto* pi_div_2 = getConstant(1.5707963267948966f);

            auto* y_div_x = builder_.CreateFDiv(var_y, var_x);
            auto* atan_y_div_x = builder_.CreateCall(atan_func, {y_div_x});

            // Case 1: x > 0
            auto* res_x_gt_0 = atan_y_div_x;

            // Case 2: x < 0
            auto* signed_pi =
                createIntrinsicCall(llvm::Intrinsic::copysign, {pi, var_y});
            auto* res_x_lt_0 = builder_.CreateFAdd(atan_y_div_x, signed_pi);

            // Case 3: x == 0
            auto* res_x_eq_0 = createIntrinsicCall(llvm::Intrinsic::copysign,
                                                   {pi_div_2, var_y});

            // Select based on x
            auto* x_gt_0 = builder_.CreateFCmpOGT(var_x, zero);
            auto* x_lt_0 = builder_.CreateFCmpOLT(var_x, zero);

            auto* result =
                builder_.CreateSelect(x_gt_0, res_x_gt_0, res_x_lt_0);
            result = builder_.CreateSelect(
                x_lt_0, res_x_lt_0,
                builder_.CreateSelect(x_gt_0, res_x_gt_0, res_x_eq_0));

            // Handle (x=0, y=0) case, which should be 0.
            auto* x_is_zero = builder_.CreateFCmpOEQ(var_x, zero);
            auto* y_is_zero = builder_.CreateFCmpOEQ(var_y, zero);
            auto* both_zero = builder_.CreateAnd(x_is_zero, y_is_zero);

            result = builder_.CreateSelect(both_zero, zero, result);

            return result;
        });
}

class MathLibraryManager {
  public:
    MathLibraryManager(llvm::Module* module, llvm::LLVMContext& context)
        : module_(module), context_(context) {}

    llvm::Function* getScalarFunction(MathOp op) {
        if (auto it = scalarFuncCache_.find(op); it != scalarFuncCache_.end()) {
            return it->second;
        }
        return generateAndCache(op);
    }

    llvm::Function* getScalarBinaryFunction(BinaryMathOp op) {
        if (auto it = scalarBinaryFuncCache_.find(op);
            it != scalarBinaryFuncCache_.end()) {
            return it->second;
        }
        return generateAndCacheBinary(op);
    }

  private:
    llvm::Module* module_;
    llvm::LLVMContext& context_;
    std::map<MathOp, llvm::Function*> scalarFuncCache_;
    std::map<BinaryMathOp, llvm::Function*> scalarBinaryFuncCache_;

    // Template dispatch: Map MathOp enum to concrete generator methods
    template <MathOp op, int VectorWidth> llvm::Function* dispatchGeneration() {
        MathFunctionGenerator<VectorWidth> generator(module_, context_);
        if constexpr (op == MathOp::Exp)
            return generator.getOrCreateExp();
        if constexpr (op == MathOp::Log)
            return generator.getOrCreateLog();
        if constexpr (op == MathOp::Sin)
            return generator.getOrCreateSin();
        if constexpr (op == MathOp::Cos)
            return generator.getOrCreateCos();
        if constexpr (op == MathOp::Tan)
            return generator.getOrCreateTan();
        if constexpr (op == MathOp::Atan)
            return generator.getOrCreateAtan();
        return nullptr;
    }

    template <BinaryMathOp op, int VectorWidth>
    llvm::Function* dispatchBinaryGeneration() {
        MathFunctionGenerator<VectorWidth> generator(module_, context_);
        if constexpr (op == BinaryMathOp::Atan2)
            return generator.getOrCreateAtan2();
        return nullptr;
    }

    template <typename OpType, OpType op>
    llvm::Function* generateAndCacheImpl() {
        llvm::Function* scalarFunc;
        constexpr bool is_unary = std::is_same_v<OpType, MathOp>;

        if constexpr (is_unary) {
            scalarFunc = dispatchGeneration<static_cast<MathOp>(op), 1>();
        } else {
            scalarFunc =
                dispatchBinaryGeneration<static_cast<BinaryMathOp>(op), 1>();
        }

        if (!scalarFunc) {
            return nullptr;
        }

        auto link_vectors = [&]<int... Widths>(
                                std::integer_sequence<int, Widths...>) {
            (
                [&] {
                    llvm::Function* vecFunc;
                    if constexpr (is_unary) {
                        vecFunc = dispatchGeneration<static_cast<MathOp>(op),
                                                     Widths>();
                    } else {
                        vecFunc = dispatchBinaryGeneration<
                            static_cast<BinaryMathOp>(op), Widths>();
                    }

                    if (vecFunc) {
                        std::string abi_string = "_ZGV";

                        if constexpr (Widths == 4) {
#ifdef __SSE__
                            abi_string += "b";
#elif defined(__ARM_NEON__)
                            abi_string += "n";
#endif
                        } else if constexpr (Widths == 8) {
#ifdef __AVX2__
                            abi_string += "d";
#endif
                        } else if constexpr (Widths == 16) {
#ifdef __AVX512F__
                            abi_string += "e";
#endif
                        }

                        abi_string += "N"; // No mask
                        abi_string += std::to_string(Widths);
                        if constexpr (is_unary) {
                            abi_string += "v";
                        } else {
                            abi_string += "vv"; // two vector arguments
                        }
                        abi_string += "_";
                        abi_string += scalarFunc->getName().str();
                        abi_string += "(";
                        abi_string += vecFunc->getName().str();
                        abi_string += ")";

                        scalarFunc->addFnAttr(llvm::Attribute::get(
                            context_, "vector-function-abi-variant",
                            abi_string));
                    }
                }(),
                ...);
        };

        link_vectors(SupportedVectorWidths{});

        if constexpr (is_unary) {
            scalarFuncCache_[static_cast<MathOp>(op)] = scalarFunc;
        } else {
            scalarBinaryFuncCache_[static_cast<BinaryMathOp>(op)] = scalarFunc;
        }
        return scalarFunc;
    }

    llvm::Function* generateAndCache(MathOp op) {
        switch (op) {
        case MathOp::Exp:
            return generateAndCacheImpl<MathOp, MathOp::Exp>();
        case MathOp::Log:
            return generateAndCacheImpl<MathOp, MathOp::Log>();
        case MathOp::Sin:
            return generateAndCacheImpl<MathOp, MathOp::Sin>();
        case MathOp::Cos:
            return generateAndCacheImpl<MathOp, MathOp::Cos>();
        case MathOp::Tan:
            return generateAndCacheImpl<MathOp, MathOp::Tan>();
        case MathOp::Atan:
            return generateAndCacheImpl<MathOp, MathOp::Atan>();
        }
        return nullptr;
    }

    llvm::Function* generateAndCacheBinary(BinaryMathOp op) {
        switch (op) {
        case BinaryMathOp::Atan2:
            return generateAndCacheImpl<BinaryMathOp, BinaryMathOp::Atan2>();
        }
        return nullptr;
    }
};

#endif // LLVMEXPR_MATH_FUNCTIONS_HPP