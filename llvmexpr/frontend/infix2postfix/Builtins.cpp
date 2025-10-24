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
    // Signature: dyn($clip, x, y)
    // Default boundary mode is clamped.
    PostfixBuilder b;
    b.append(codegen->generate_expr(expr.args[1].get()).postfix);
    b.append(codegen->generate_expr(expr.args[2].get()).postfix);

    auto clip_res = codegen->generate_expr(expr.args[0].get());
    if (clip_res.type != Type::CLIP) {
        throw CodeGenError("The first argument to dyn() must be a clip.",
                           expr.range);
    }
    std::string clip_name = clip_res.postfix.get_expression();

    b.add_dyn_pixel_access_expr(clip_name, ":c");
    return b;
}

PostfixBuilder handle_dyn_expr_4args(CodeGenerator* codegen,
                                     const CallExpr& expr) {
    // Signature: dyn($clip, x, y, boundary_mode)
    PostfixBuilder b;
    b.append(codegen->generate_expr(expr.args[1].get()).postfix);
    b.append(codegen->generate_expr(expr.args[2].get()).postfix);

    auto clip_res = codegen->generate_expr(expr.args[0].get());
    if (clip_res.type != Type::CLIP) {
        throw CodeGenError("The first argument to dyn() must be a clip.",
                           expr.range);
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
            expr.range);
    }
    b.add_dyn_pixel_access_expr(clip_name, suffix);
    return b;
}

PostfixBuilder handle_dyn_single(CodeGenerator* codegen, const CallExpr& expr) {
    // Signature: dyn($clip, x, y, plane)
    auto clip_res = codegen->generate_expr(expr.args[0].get());
    if (clip_res.type != Type::CLIP) {
        throw CodeGenError("The first argument to dyn() must be a clip.",
                           expr.range);
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
    if (prop_name_expr == nullptr) {
        throw CodeGenError("set_prop() requires a property name identifier as "
                           "the first argument.",
                           expr.range);
    }
    if (prop_name_expr->name.value.starts_with('$')) {
        throw CodeGenError("Property names in set_prop() cannot be $-prefixed "
                           "constants.",
                           expr.range);
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
    {"sin",
     {BuiltinFunction{.name = "sin",
                      .arity = 1,
                      .mode_restriction = std::nullopt,
                      .param_types = {Type::VALUE}}}},
    {"cos",
     {BuiltinFunction{.name = "cos",
                      .arity = 1,
                      .mode_restriction = std::nullopt,
                      .param_types = {Type::VALUE}}}},
    {"tan",
     {BuiltinFunction{.name = "tan",
                      .arity = 1,
                      .mode_restriction = std::nullopt,
                      .param_types = {Type::VALUE}}}},
    {"asin",
     {BuiltinFunction{.name = "asin",
                      .arity = 1,
                      .mode_restriction = std::nullopt,
                      .param_types = {Type::VALUE}}}},
    {"acos",
     {BuiltinFunction{.name = "acos",
                      .arity = 1,
                      .mode_restriction = std::nullopt,
                      .param_types = {Type::VALUE}}}},
    {"atan",
     {BuiltinFunction{.name = "atan",
                      .arity = 1,
                      .mode_restriction = std::nullopt,
                      .param_types = {Type::VALUE}}}},
    {"atan2",
     {BuiltinFunction{.name = "atan2",
                      .arity = 2,
                      .mode_restriction = std::nullopt,
                      .param_types = {Type::VALUE, Type::VALUE}}}},
    {"exp",
     {BuiltinFunction{.name = "exp",
                      .arity = 1,
                      .mode_restriction = std::nullopt,
                      .param_types = {Type::VALUE}}}},
    {"exp2",
     {BuiltinFunction{.name = "exp2",
                      .arity = 1,
                      .mode_restriction = std::nullopt,
                      .param_types = {Type::VALUE}}}},
    {"log",
     {BuiltinFunction{.name = "log",
                      .arity = 1,
                      .mode_restriction = std::nullopt,
                      .param_types = {Type::VALUE}}}},
    {"log2",
     {BuiltinFunction{.name = "log2",
                      .arity = 1,
                      .mode_restriction = std::nullopt,
                      .param_types = {Type::VALUE}}}},
    {"log10",
     {BuiltinFunction{.name = "log10",
                      .arity = 1,
                      .mode_restriction = std::nullopt,
                      .param_types = {Type::VALUE}}}},
    {"sqrt",
     {BuiltinFunction{.name = "sqrt",
                      .arity = 1,
                      .mode_restriction = std::nullopt,
                      .param_types = {Type::VALUE}}}},
    {"abs",
     {BuiltinFunction{.name = "abs",
                      .arity = 1,
                      .mode_restriction = std::nullopt,
                      .param_types = {Type::VALUE}}}},
    {"sgn",
     {BuiltinFunction{.name = "sgn",
                      .arity = 1,
                      .mode_restriction = std::nullopt,
                      .param_types = {Type::VALUE}}}},
    {"floor",
     {BuiltinFunction{.name = "floor",
                      .arity = 1,
                      .mode_restriction = std::nullopt,
                      .param_types = {Type::VALUE}}}},
    {"ceil",
     {BuiltinFunction{.name = "ceil",
                      .arity = 1,
                      .mode_restriction = std::nullopt,
                      .param_types = {Type::VALUE}}}},
    {"round",
     {BuiltinFunction{.name = "round",
                      .arity = 1,
                      .mode_restriction = std::nullopt,
                      .param_types = {Type::VALUE}}}},
    {"trunc",
     {BuiltinFunction{.name = "trunc",
                      .arity = 1,
                      .mode_restriction = std::nullopt,
                      .param_types = {Type::VALUE}}}},
    {"min",
     {BuiltinFunction{.name = "min",
                      .arity = 2,
                      .mode_restriction = std::nullopt,
                      .param_types = {Type::VALUE, Type::VALUE}}}},
    {"max",
     {BuiltinFunction{.name = "max",
                      .arity = 2,
                      .mode_restriction = std::nullopt,
                      .param_types = {Type::VALUE, Type::VALUE}}}},
    {"copysign",
     {BuiltinFunction{.name = "copysign",
                      .arity = 2,
                      .mode_restriction = std::nullopt,
                      .param_types = {Type::VALUE, Type::VALUE}}}},
    {"clamp",
     {BuiltinFunction{.name = "clamp",
                      .arity = 3,
                      .mode_restriction = std::nullopt,
                      .param_types = {Type::VALUE, Type::VALUE, Type::VALUE}}}},
    {"fma",
     {BuiltinFunction{.name = "fma",
                      .arity = 3,
                      .mode_restriction = std::nullopt,
                      .param_types = {Type::VALUE, Type::VALUE, Type::VALUE}}}},

    // Mode-specific & special handling
    {"set_prop",
     {BuiltinFunction{.name = "set_prop",
                      .arity = 2,
                      .mode_restriction = Mode::Single,
                      .param_types = {Type::LITERAL_STRING, Type::VALUE},
                      .special_handler = &handle_set_prop,
                      .returns_value = false}}},
    {"dyn",
     {
         BuiltinFunction{.name = "dyn",
                         .arity = 3,
                         .mode_restriction = Mode::Expr,
                         .param_types = {Type::CLIP, Type::VALUE, Type::VALUE},
                         .special_handler = &handle_dyn_expr_3args},
         BuiltinFunction{.name = "dyn",
                         .arity = 4,
                         .mode_restriction = Mode::Expr,
                         .param_types = {Type::CLIP, Type::VALUE, Type::VALUE,
                                         Type::LITERAL},
                         .special_handler = &handle_dyn_expr_4args},
         BuiltinFunction{.name = "dyn",
                         .arity = 4,
                         .mode_restriction = Mode::Single,
                         .param_types = {Type::CLIP, Type::VALUE, Type::VALUE,
                                         Type::LITERAL},
                         .special_handler = &handle_dyn_single},
     }},
    {"store",
     {
         BuiltinFunction{.name = "store",
                         .arity = 3,
                         .mode_restriction = Mode::Expr,
                         .param_types = {Type::VALUE, Type::VALUE, Type::VALUE},
                         .special_handler = &handle_store_expr,
                         .returns_value = false},
         BuiltinFunction{.name = "store",
                         .arity = 4,
                         .mode_restriction = Mode::Single,
                         .param_types = {Type::VALUE, Type::VALUE,
                                         Type::LITERAL, Type::VALUE},
                         .special_handler = &handle_store_single,
                         .returns_value = false},
     }},
    {"exit",
     {BuiltinFunction{.name = "exit",
                      .arity = 0,
                      .mode_restriction = Mode::Expr,
                      .param_types = {},
                      .special_handler = &handle_exit,
                      .returns_value = false}}},
};

} // namespace

const std::map<std::string, std::vector<BuiltinFunction>>&
get_builtin_functions() {
    return builtin_functions;
}

} // namespace infix2postfix