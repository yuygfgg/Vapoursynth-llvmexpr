#include "Builtins.hpp"
#include "AST.hpp"
#include "CodeGenerator.hpp"
#include "PostfixBuilder.hpp"
#include <format>
#include <string>

namespace infix2postfix {

namespace {

PostfixBuilder handle_dyn_expr_3args(CodeGenerator* codegen,
                                     const CallExpr& expr) {
    // Signature: dyn($clip, x_expr, y_expr)
    // Default boundary mode is clamped.
    PostfixBuilder b;
    b.append(codegen->generate_expr(expr.args[1].get()).postfix);
    b.append(codegen->generate_expr(expr.args[2].get()).postfix);

    auto clip_res = codegen->generate_expr(expr.args[0].get());
    if (clip_res.type != Type::CLIP) {
        throw CodeGenError("The first argument to dyn() must be a clip.",
                           expr.line);
    }
    std::string clip_name = clip_res.postfix.get_expression();

    b.add_dyn_pixel_access_expr(clip_name, ":c");
    return b;
}

PostfixBuilder handle_dyn_expr_4args(CodeGenerator* codegen,
                                     const CallExpr& expr) {
    // Signature: dyn($clip, x_expr, y_expr, boundary_mode)
    PostfixBuilder b;
    b.append(codegen->generate_expr(expr.args[1].get()).postfix);
    b.append(codegen->generate_expr(expr.args[2].get()).postfix);

    auto clip_res = codegen->generate_expr(expr.args[0].get());
    if (clip_res.type != Type::CLIP) {
        throw CodeGenError("The first argument to dyn() must be a clip.",
                           expr.line);
    }
    std::string clip_name = clip_res.postfix.get_expression();

    auto* boundary_expr = get_if<NumberExpr>(expr.args[3].get());
    int boundary_mode = std::stoi(boundary_expr->value.value);
    std::string suffix;
    switch (boundary_mode) {
    case 0: // global
        suffix = "";
        break;
    case 1: // mirrored
        suffix = ":m";
        break;
    case 2: // clamped
        suffix = ":c";
        break;
    default:
        throw CodeGenError(
            std::format("Invalid boundary mode '{}' for dyn()", boundary_mode),
            expr.line);
    }
    b.add_dyn_pixel_access_expr(clip_name, suffix);
    return b;
}

PostfixBuilder handle_dyn_single(CodeGenerator* codegen, const CallExpr& expr) {
    // Signature: dyn($clip, x, y, plane)
    auto clip_res = codegen->generate_expr(expr.args[0].get());
    if (clip_res.type != Type::CLIP) {
        throw CodeGenError("The first argument to dyn() must be a clip.",
                           expr.line);
    }
    std::string clip_name = clip_res.postfix.get_expression();

    auto* plane_expr = get_if<NumberExpr>(expr.args[3].get());
    std::string plane_idx = plane_expr->value.value;

    PostfixBuilder b;
    b.append(codegen->generate_expr(expr.args[1].get()).postfix);
    b.append(codegen->generate_expr(expr.args[2].get()).postfix);
    b.add_dyn_pixel_access_single(clip_name, plane_idx);
    return b;
}

PostfixBuilder handle_store_expr(CodeGenerator* codegen, const CallExpr& expr) {
    // store(x, y, val)
    PostfixBuilder b;
    b.append(codegen->generate_expr(expr.args[2].get()).postfix);
    b.append(codegen->generate_expr(expr.args[0].get()).postfix);
    b.append(codegen->generate_expr(expr.args[1].get()).postfix);
    b.add_store_expr();
    return b;
}

PostfixBuilder handle_store_single(CodeGenerator* codegen,
                                   const CallExpr& expr) {
    // store(x, y, plane, value)
    auto* plane_expr = get_if<NumberExpr>(expr.args[2].get());
    std::string plane_idx = plane_expr->value.value;

    PostfixBuilder b;
    b.append(codegen->generate_expr(expr.args[3].get()).postfix);
    b.append(codegen->generate_expr(expr.args[0].get()).postfix);
    b.append(codegen->generate_expr(expr.args[1].get()).postfix);
    b.add_store_single(plane_idx);
    return b;
}

PostfixBuilder handle_set_prop(CodeGenerator* codegen, const CallExpr& expr) {
    // set_prop(prop_name, value)
    auto* prop_name_expr = get_if<VariableExpr>(expr.args[0].get());
    if (!prop_name_expr) {
        throw CodeGenError("set_prop() requires a property name identifier as "
                           "the first argument.",
                           expr.line);
    }
    if (prop_name_expr->name.value.starts_with('$')) {
        throw CodeGenError("Property names in set_prop() cannot be $-prefixed "
                           "constants.",
                           expr.line);
    }

    PostfixBuilder b;
    b.append(codegen->generate_expr(expr.args[1].get()).postfix);
    b.add_set_prop(prop_name_expr->name.value);
    return b;
}

PostfixBuilder handle_exit([[maybe_unused]] CodeGenerator* codegen,
                           [[maybe_unused]] const CallExpr& expr) {
    PostfixBuilder b;
    b.add_exit_marker();
    return b;
}

// nth_N functions are not handled here
const std::map<std::string, std::vector<BuiltinFunction>> builtin_functions = {
    // Standard math
    {"sin", {BuiltinFunction{"sin", 1, std::nullopt, {Type::VALUE}}}},
    {"cos", {BuiltinFunction{"cos", 1, std::nullopt, {Type::VALUE}}}},
    {"tan", {BuiltinFunction{"tan", 1, std::nullopt, {Type::VALUE}}}},
    {"asin", {BuiltinFunction{"asin", 1, std::nullopt, {Type::VALUE}}}},
    {"acos", {BuiltinFunction{"acos", 1, std::nullopt, {Type::VALUE}}}},
    {"atan", {BuiltinFunction{"atan", 1, std::nullopt, {Type::VALUE}}}},
    {"atan2",
     {BuiltinFunction{"atan2", 2, std::nullopt, {Type::VALUE, Type::VALUE}}}},
    {"exp", {BuiltinFunction{"exp", 1, std::nullopt, {Type::VALUE}}}},
    {"exp2", {BuiltinFunction{"exp2", 1, std::nullopt, {Type::VALUE}}}},
    {"log", {BuiltinFunction{"log", 1, std::nullopt, {Type::VALUE}}}},
    {"log2", {BuiltinFunction{"log2", 1, std::nullopt, {Type::VALUE}}}},
    {"log10", {BuiltinFunction{"log10", 1, std::nullopt, {Type::VALUE}}}},
    {"sqrt", {BuiltinFunction{"sqrt", 1, std::nullopt, {Type::VALUE}}}},
    {"abs", {BuiltinFunction{"abs", 1, std::nullopt, {Type::VALUE}}}},
    {"sgn", {BuiltinFunction{"sgn", 1, std::nullopt, {Type::VALUE}}}},
    {"floor", {BuiltinFunction{"floor", 1, std::nullopt, {Type::VALUE}}}},
    {"ceil", {BuiltinFunction{"ceil", 1, std::nullopt, {Type::VALUE}}}},
    {"round", {BuiltinFunction{"round", 1, std::nullopt, {Type::VALUE}}}},
    {"trunc", {BuiltinFunction{"trunc", 1, std::nullopt, {Type::VALUE}}}},
    {"min",
     {BuiltinFunction{"min", 2, std::nullopt, {Type::VALUE, Type::VALUE}}}},
    {"max",
     {BuiltinFunction{"max", 2, std::nullopt, {Type::VALUE, Type::VALUE}}}},
    {"copysign",
     {BuiltinFunction{
         "copysign", 2, std::nullopt, {Type::VALUE, Type::VALUE}}}},
    {"clamp",
     {BuiltinFunction{
         "clamp", 3, std::nullopt, {Type::VALUE, Type::VALUE, Type::VALUE}}}},
    {"fma",
     {BuiltinFunction{
         "fma", 3, std::nullopt, {Type::VALUE, Type::VALUE, Type::VALUE}}}},

    // Mode-specific & special handling
    {"set_prop",
     {BuiltinFunction{"set_prop",
                      2,
                      Mode::Single,
                      {Type::LITERAL_STRING, Type::VALUE},
                      &handle_set_prop}}},
    {"dyn",
     {
         BuiltinFunction{"dyn",
                         3,
                         Mode::Expr,
                         {Type::CLIP, Type::VALUE, Type::VALUE},
                         &handle_dyn_expr_3args},
         BuiltinFunction{"dyn",
                         4,
                         Mode::Expr,
                         {Type::CLIP, Type::VALUE, Type::VALUE, Type::LITERAL},
                         &handle_dyn_expr_4args},
         BuiltinFunction{"dyn",
                         4,
                         Mode::Single,
                         {Type::CLIP, Type::VALUE, Type::VALUE, Type::LITERAL},
                         &handle_dyn_single},
     }},
    {"store",
     {
         BuiltinFunction{"store",
                         3,
                         Mode::Expr,
                         {Type::VALUE, Type::VALUE, Type::VALUE},
                         &handle_store_expr},
         BuiltinFunction{"store",
                         4,
                         Mode::Single,
                         {Type::VALUE, Type::VALUE, Type::LITERAL, Type::VALUE},
                         &handle_store_single},
     }},
    {"exit", {BuiltinFunction{"exit", 0, Mode::Expr, {}, &handle_exit}}},
};

} // namespace

const std::map<std::string, std::vector<BuiltinFunction>>&
get_builtin_functions() {
    return builtin_functions;
}

} // namespace infix2postfix