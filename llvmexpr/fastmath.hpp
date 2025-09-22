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

#ifndef FASTMATH_HPP
#define FASTMATH_HPP

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include <limits>

namespace FastMath {

inline llvm::Value* createFastApproximateExp(llvm::IRBuilder<>& builder,
                                             llvm::LLVMContext& context,
                                             llvm::Value* x_) {
    auto* float_ty = llvm::Type::getFloatTy(context);
    auto* int32_ty = llvm::Type::getInt32Ty(context);

    auto* exp_hi = llvm::ConstantFP::get(float_ty, 88.3762626647949f);
    auto* exp_lo = llvm::ConstantFP::get(float_ty, -88.3762626647949f);
    auto* log2e = llvm::ConstantFP::get(float_ty, 1.44269504088896341f);
    // auto* exp_c1 = llvm::ConstantFP::get(float_ty, 0.693359375f);
    // auto* exp_c2 = llvm::ConstantFP::get(float_ty, -2.12194440e-4f);
    auto* exp_p0 = llvm::ConstantFP::get(float_ty, 1.9875691500E-4f);
    auto* exp_p1 = llvm::ConstantFP::get(float_ty, 1.3981999507E-3f);
    auto* exp_p2 = llvm::ConstantFP::get(float_ty, 8.3334519073E-3f);
    auto* exp_p3 = llvm::ConstantFP::get(float_ty, 4.1665795894E-2f);
    auto* exp_p4 = llvm::ConstantFP::get(float_ty, 1.6666665459E-1f);
    auto* exp_p5 = llvm::ConstantFP::get(float_ty, 5.0000001201E-1f);
    auto* half = llvm::ConstantFP::get(float_ty, 0.5f);
    auto* one = llvm::ConstantFP::get(float_ty, 1.0f);
    auto* neg_exp_c1 = llvm::ConstantFP::get(float_ty, -0.693359375f);
    auto* neg_exp_c2 = llvm::ConstantFP::get(float_ty, 2.12194440e-4f);
    auto* const_0x7f = builder.getInt32(0x7f);
    auto* const_23 = builder.getInt32(23);

    llvm::Value* x = x_;

    // x = min(x, exp_hi)
    auto* minnum_intrinsic = llvm::Intrinsic::getOrInsertDeclaration(
        builder.GetInsertBlock()->getModule(), llvm::Intrinsic::minnum,
        float_ty);
    x = builder.CreateCall(minnum_intrinsic, {x, exp_hi});

    // x = max(x, exp_lo)
    auto* maxnum_intrinsic = llvm::Intrinsic::getOrInsertDeclaration(
        builder.GetInsertBlock()->getModule(), llvm::Intrinsic::maxnum,
        float_ty);
    x = builder.CreateCall(maxnum_intrinsic, {x, exp_lo});

    // fx = log2e * x + 0.5
    auto* fx = builder.CreateFMul(log2e, x);
    fx = builder.CreateFAdd(fx, half);

    // emm0 = round(fx)
    auto* round_intrinsic = llvm::Intrinsic::getOrInsertDeclaration(
        builder.GetInsertBlock()->getModule(), llvm::Intrinsic::round,
        float_ty);
    auto* etmp = builder.CreateCall(round_intrinsic, {fx});

    // mask = (etmp > fx) ? 1.0f : 0.0f (as float bits)
    auto* cmp_gt = builder.CreateFCmpOGT(etmp, fx);
    auto* one_int = builder.CreateBitCast(one, int32_ty);
    auto* zero_int = builder.getInt32(0);
    auto* mask_int = builder.CreateSelect(cmp_gt, one_int, zero_int);
    auto* mask = builder.CreateBitCast(mask_int, float_ty);

    // fx = etmp - mask
    fx = builder.CreateFSub(etmp, mask);

    // x = x + fx * (-exp_c1)
    auto* temp1 = builder.CreateFMul(fx, neg_exp_c1);
    x = builder.CreateFAdd(x, temp1);

    // x = x + fx * (-exp_c2)
    auto* temp2 = builder.CreateFMul(fx, neg_exp_c2);
    x = builder.CreateFAdd(x, temp2);

    // z = x * x
    auto* z = builder.CreateFMul(x, x);

    // y = exp_p0
    llvm::Value* y = exp_p0;

    // y = y * x + exp_p1
    y = builder.CreateFMul(y, x);
    y = builder.CreateFAdd(y, exp_p1);

    // y = y * x + exp_p2
    y = builder.CreateFMul(y, x);
    y = builder.CreateFAdd(y, exp_p2);

    // y = y * x + exp_p3
    y = builder.CreateFMul(y, x);
    y = builder.CreateFAdd(y, exp_p3);

    // y = y * x + exp_p4
    y = builder.CreateFMul(y, x);
    y = builder.CreateFAdd(y, exp_p4);

    // y = y * x + exp_p5
    y = builder.CreateFMul(y, x);
    y = builder.CreateFAdd(y, exp_p5);

    // y = y * z + x
    y = builder.CreateFMul(y, z);
    y = builder.CreateFAdd(y, x);

    // y = y + 1.0
    y = builder.CreateFAdd(y, one);

    // emm0 = round(fx) as int
    auto* emm0_float = builder.CreateCall(round_intrinsic, {fx});
    auto* emm0 = builder.CreateFPToSI(emm0_float, int32_ty);

    // emm0 = emm0 + 0x7f
    emm0 = builder.CreateAdd(emm0, const_0x7f);

    // emm0 = emm0 << 23
    emm0 = builder.CreateShl(emm0, const_23);

    // x = y * (emm0 as float)
    auto* emm0_as_float = builder.CreateBitCast(emm0, float_ty);
    x = builder.CreateFMul(y, emm0_as_float);

    return x;
}

inline llvm::Value* createFastApproximateLog(llvm::IRBuilder<>& builder,
                                             llvm::LLVMContext& context,
                                             llvm::Value* x_) {
    auto* float_ty = llvm::Type::getFloatTy(context);
    auto* int32_ty = llvm::Type::getInt32Ty(context);

    // Constants for log approximation
    auto* min_norm_pos = builder.getInt32(0x00800000);
    auto* inv_mant_mask = builder.getInt32(~0x7F800000);
    auto* float_half = llvm::ConstantFP::get(float_ty, 0.5f);
    auto* sqrt_1_2 = llvm::ConstantFP::get(float_ty, 0.707106781186547524f);
    auto* log_p0 = llvm::ConstantFP::get(float_ty, 7.0376836292E-2f);
    auto* log_p1 = llvm::ConstantFP::get(float_ty, -1.1514610310E-1f);
    auto* log_p2 = llvm::ConstantFP::get(float_ty, 1.1676998740E-1f);
    auto* log_p3 = llvm::ConstantFP::get(float_ty, -1.2420140846E-1f);
    auto* log_p4 = llvm::ConstantFP::get(float_ty, 1.4249322787E-1f);
    auto* log_p5 = llvm::ConstantFP::get(float_ty, -1.6668057665E-1f);
    auto* log_p6 = llvm::ConstantFP::get(float_ty, 2.0000714765E-1f);
    auto* log_p7 = llvm::ConstantFP::get(float_ty, -2.4999993993E-1f);
    auto* log_p8 = llvm::ConstantFP::get(float_ty, 3.3333331174E-1f);
    auto* log_q2 = llvm::ConstantFP::get(float_ty, 0.693359375f);
    auto* log_q1 = llvm::ConstantFP::get(float_ty, -2.12194440e-4f);
    auto* zero = llvm::ConstantFP::get(float_ty, 0.0f);
    auto* one = llvm::ConstantFP::get(float_ty, 1.0f);
    auto* neg_half = llvm::ConstantFP::get(float_ty, -0.5f);
    auto* neg_inf = llvm::ConstantFP::get(
        float_ty, -std::numeric_limits<float>::infinity());
    auto* pos_inf =
        llvm::ConstantFP::get(float_ty, std::numeric_limits<float>::infinity());
    auto* nan_val = llvm::ConstantFP::get(
        float_ty, std::numeric_limits<float>::quiet_NaN());
    auto* const_0x7f = builder.getInt32(0x7f);
    auto* const_23 = builder.getInt32(23);

    llvm::Value* x = x_;

    // Special case masks
    auto* is_zero = builder.CreateFCmpOEQ(x, zero);
    auto* is_negative = builder.CreateFCmpOLT(x, zero);
    auto* is_one = builder.CreateFCmpOEQ(x, one);
    auto* is_inf = builder.CreateFCmpOEQ(x, pos_inf);
    auto* is_nan = builder.CreateFCmpUNO(x, x); // NaN check

    // x = max(x, min_norm_pos as float)
    auto* min_norm_pos_float = builder.CreateBitCast(min_norm_pos, float_ty);
    auto* maxnum_intrinsic = llvm::Intrinsic::getOrInsertDeclaration(
        builder.GetInsertBlock()->getModule(), llvm::Intrinsic::maxnum,
        float_ty);
    x = builder.CreateCall(maxnum_intrinsic, {x, min_norm_pos_float});

    // emm0i = (x as int) >> 23
    auto* x_as_int = builder.CreateBitCast(x, int32_ty);
    auto* emm0i = builder.CreateLShr(x_as_int, const_23);

    // x = (x as int) & inv_mant_mask
    auto* x_masked = builder.CreateAnd(x_as_int, inv_mant_mask);
    // x = x | (0.5f as int)
    auto* half_as_int = builder.CreateBitCast(float_half, int32_ty);
    x_masked = builder.CreateOr(x_masked, half_as_int);
    x = builder.CreateBitCast(x_masked, float_ty);

    // emm0i = emm0i - 0x7f
    emm0i = builder.CreateSub(emm0i, const_0x7f);
    auto* emm0 = builder.CreateSIToFP(emm0i, float_ty);

    // emm0 = emm0 + 1.0f
    emm0 = builder.CreateFAdd(emm0, one);

    // mask = (x < sqrt_1_2)
    auto* mask = builder.CreateFCmpOLT(x, sqrt_1_2);

    // etmp = mask ? x : 0.0f
    auto* etmp = builder.CreateSelect(mask, x, zero);

    // x = x - 1.0f
    x = builder.CreateFSub(x, one);

    // maskf = mask ? 1.0f : 0.0f
    auto* maskf = builder.CreateSelect(mask, one, zero);

    // emm0 = emm0 - maskf
    emm0 = builder.CreateFSub(emm0, maskf);

    // x = x + etmp
    x = builder.CreateFAdd(x, etmp);

    // z = x * x
    auto* z = builder.CreateFMul(x, x);

    // Polynomial approximation using FMA
    auto* fma_intrinsic = llvm::Intrinsic::getOrInsertDeclaration(
        builder.GetInsertBlock()->getModule(), llvm::Intrinsic::fma, float_ty);

    llvm::Value* y = log_p0;
    y = builder.CreateCall(fma_intrinsic, {y, x, log_p1});
    y = builder.CreateCall(fma_intrinsic, {y, x, log_p2});
    y = builder.CreateCall(fma_intrinsic, {y, x, log_p3});
    y = builder.CreateCall(fma_intrinsic, {y, x, log_p4});
    y = builder.CreateCall(fma_intrinsic, {y, x, log_p5});
    y = builder.CreateCall(fma_intrinsic, {y, x, log_p6});
    y = builder.CreateCall(fma_intrinsic, {y, x, log_p7});
    y = builder.CreateCall(fma_intrinsic, {y, x, log_p8});
    y = builder.CreateFMul(y, x);
    y = builder.CreateFMul(y, z);
    y = builder.CreateCall(fma_intrinsic, {emm0, log_q1, y});
    y = builder.CreateCall(fma_intrinsic, {z, neg_half, y});
    x = builder.CreateFAdd(x, y);
    x = builder.CreateCall(fma_intrinsic, {emm0, log_q2, x});

    // log(0) = -inf
    x = builder.CreateSelect(is_zero, neg_inf, x);

    // log(negative) = NaN
    x = builder.CreateSelect(is_negative, nan_val, x);

    // log(1) = 0
    x = builder.CreateSelect(is_one, zero, x);

    // log(+inf) = +inf
    x = builder.CreateSelect(is_inf, pos_inf, x);

    // log(NaN) = NaN
    x = builder.CreateSelect(is_nan, nan_val, x);

    return x;
}

inline llvm::Value* createFastApproximatePow(llvm::IRBuilder<>& builder,
                                             llvm::LLVMContext& context,
                                             llvm::Value* base,
                                             llvm::Value* exponent) {
    auto* float_ty = base->getType();
    auto* one = llvm::ConstantFP::get(float_ty, 1.0);
    auto* zero = llvm::ConstantFP::get(float_ty, 0.0);

    // pow(1, y) = 1
    // pow(x, 0) = 1
    auto* is_base_one = builder.CreateFCmpOEQ(base, one);
    auto* is_exp_zero = builder.CreateFCmpOEQ(exponent, zero);

    // The main calculation logic is inside this block.
    llvm::Value* main_result;
    {
        // base > 0. Use log-exp formula.
        llvm::Value* log_base_pos =
            createFastApproximateLog(builder, context, base);
        llvm::Value* product_pos = builder.CreateFMul(exponent, log_base_pos);
        llvm::Value* result_pos_base =
            createFastApproximateExp(builder, context, product_pos);

        // base < 0.
        auto* int32_ty = llvm::Type::getInt32Ty(context);
        auto* nan_val = llvm::ConstantFP::get(
            float_ty, std::numeric_limits<float>::quiet_NaN());

        // Check if exponent is an integer.
        auto* floor_intrinsic = llvm::Intrinsic::getOrInsertDeclaration(
            builder.GetInsertBlock()->getModule(), llvm::Intrinsic::floor,
            {float_ty});
        auto* exponent_floor = builder.CreateCall(floor_intrinsic, {exponent});
        auto* is_exp_integer = builder.CreateFCmpOEQ(exponent, exponent_floor);

        // If exponent is an integer, calculate pow(abs(base), exponent) and adjust sign.
        auto* fabs_intrinsic = llvm::Intrinsic::getOrInsertDeclaration(
            builder.GetInsertBlock()->getModule(), llvm::Intrinsic::fabs,
            {float_ty});
        auto* abs_base = builder.CreateCall(fabs_intrinsic, {base});

        llvm::Value* log_abs_base =
            createFastApproximateLog(builder, context, abs_base);
        llvm::Value* product_abs = builder.CreateFMul(exponent, log_abs_base);
        llvm::Value* pow_abs_base =
            createFastApproximateExp(builder, context, product_abs);

        // Check if integer exponent is odd.
        auto* exponent_as_int = builder.CreateFPToSI(exponent, int32_ty);
        auto* two_int = llvm::ConstantInt::get(int32_ty, 2);
        auto* remainder_int = builder.CreateSRem(exponent_as_int, two_int);
        auto* zero_int = llvm::ConstantInt::get(int32_ty, 0);
        auto* is_exp_odd = builder.CreateICmpNE(remainder_int, zero_int);

        auto* neg_pow_abs_base = builder.CreateFNeg(pow_abs_base);
        auto* result_neg_base_int_exp =
            builder.CreateSelect(is_exp_odd, neg_pow_abs_base, pow_abs_base);

        // If base < 0 and exponent is not integer, result is NaN.
        auto* result_neg_base = builder.CreateSelect(
            is_exp_integer, result_neg_base_int_exp, nan_val);

        // base == 0.
        auto* pos_inf = llvm::ConstantFP::get(
            float_ty, std::numeric_limits<float>::infinity());
        auto* is_exp_pos = builder.CreateFCmpOGT(exponent, zero);

        auto* result_zero_base =
            builder.CreateSelect(is_exp_pos, zero, pos_inf);

        auto* is_base_pos = builder.CreateFCmpOGT(base, zero);
        auto* is_base_zero = builder.CreateFCmpOEQ(base, zero);

        main_result =
            builder.CreateSelect(is_base_pos, result_pos_base, result_neg_base);
        main_result =
            builder.CreateSelect(is_base_zero, result_zero_base, main_result);
    }

    llvm::Value* final_result =
        builder.CreateSelect(is_base_one, one, main_result);
    final_result = builder.CreateSelect(is_exp_zero, one, final_result);

    return final_result;
}

} // namespace FastMath

#endif // FASTMATH_HPP