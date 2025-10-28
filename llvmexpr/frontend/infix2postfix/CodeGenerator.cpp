#include "CodeGenerator.hpp"
#include "Builtins.hpp"
#include "PostfixBuilder.hpp"
#include "PostfixHelper.hpp"
#include "types.hpp"
#include <format>
#include <functional>
#include <utility>

namespace infix2postfix {

CodeGenerator::CodeGenerator(Mode mode, int num_inputs)
    : mode(mode), num_inputs(num_inputs) {}

std::string CodeGenerator::generate(Program* program) {
    PostfixBuilder builder;
    for (const auto& stmt : program->statements) {
        builder.append(generate(stmt.get()));
    }

    if (mode == Mode::Expr) {
        builder.add_variable_load("RESULT");
    }

    return builder.get_expression();
}

CodeGenerator::ExprResult CodeGenerator::generate(Expr* expr) {
    if (expr == nullptr) {
        return {.postfix = {}, .type = Type::VALUE};
    }
    return std::visit([this](auto& e) { return this->handle(e); }, expr->value);
}

PostfixBuilder CodeGenerator::generate(Stmt* stmt) {
    if (stmt == nullptr) {
        return {};
    }
    return std::visit([this](auto& s) { return this->handle(s); }, stmt->value);
}

CodeGenerator::ExprResult CodeGenerator::handle(const NumberExpr& expr) {
    PostfixBuilder b;
    b.add_number(expr.value.value);
    return {.postfix = b, .type = Type::LITERAL};
}

CodeGenerator::ExprResult CodeGenerator::handle(const VariableExpr& expr) {
    PostfixBuilder b;
    std::string name = expr.name.value;

    if (param_substitutions.contains(name)) {
        return generate(param_substitutions.at(name));
    }

    if (name.starts_with("$")) {
        std::string base_name = name.substr(1);
        b.add_constant(base_name);

        if (is_clip_name(base_name)) {
            return {.postfix = b, .type = Type::CLIP};
        }
        return {.postfix = b, .type = Type::VALUE};
    }

    std::string var_name = name;
    if (var_rename_map.contains(name)) {
        var_name = var_rename_map.at(name);
    } else if (expr.symbol) {
        var_name = expr.symbol->name;
    }

    b.add_variable_load(var_name);

    if (expr.symbol) {
        return {.postfix = b, .type = expr.symbol->type};
    }

    return {.postfix = b, .type = Type::VALUE};
}

CodeGenerator::ExprResult CodeGenerator::handle(const UnaryExpr& expr) {
    if (expr.op.type == TokenType::Minus) {
        if (auto* num = get_if<NumberExpr>(expr.right.get())) {
            std::string val = num->value.value;
            if (!val.starts_with("-")) {
                val = std::format("-{}", val);
            }
            PostfixBuilder b;
            b.add_number(val);
            return {.postfix = b, .type = Type::LITERAL};
        }
    }

    auto right = generate(expr.right.get());
    PostfixBuilder b = right.postfix;

    if (expr.op.type == TokenType::Not) {
        b.add_number("0");
        b.add_op(TokenType::Eq);
        return {.postfix = b, .type = Type::VALUE};
    }

    b.add_unary_op(expr.op.type);
    return {.postfix = b, .type = Type::VALUE};
}

CodeGenerator::ExprResult CodeGenerator::handle(const BinaryExpr& expr) {
    auto left = generate(expr.left.get());
    auto right = generate(expr.right.get());

    PostfixBuilder b;
    b.append(left.postfix);

    if (expr.op.type == TokenType::LogicalAnd ||
        expr.op.type == TokenType::LogicalOr) {
        b.add_number("0");
        b.add_op(TokenType::Eq);
        b.add_unary_op(TokenType::Not);

        b.append(right.postfix);
        b.add_number("0");
        b.add_op(TokenType::Eq);
        b.add_unary_op(TokenType::Not);

        b.add_op(expr.op.type);
        return {.postfix = b, .type = Type::VALUE};
    }

    b.append(right.postfix);
    b.add_op(expr.op.type);
    return {.postfix = b, .type = Type::VALUE};
}

CodeGenerator::ExprResult CodeGenerator::handle(const TernaryExpr& expr) {
    auto cond = generate(expr.cond.get());
    auto true_branch = generate(expr.true_expr.get());
    auto false_branch = generate(expr.false_expr.get());

    PostfixBuilder b;
    b.append(cond.postfix);
    b.add_number("0");
    b.add_op(TokenType::Eq);
    b.add_unary_op(TokenType::Not);
    b.append(true_branch.postfix);
    b.append(false_branch.postfix);
    b.add_ternary_op();
    return {.postfix = b, .type = Type::VALUE};
}

CodeGenerator::ExprResult CodeGenerator::handle(const CallExpr& expr) {
    // User-defined functions are inlined
    if (expr.resolved_signature != nullptr && expr.resolved_def != nullptr) {
        const auto& sig = *expr.resolved_signature;
        FunctionDef* func_def = expr.resolved_def;

        PostfixBuilder b =
            inline_function_call(sig, func_def, expr.args, expr.range);
        return {.postfix = b, .type = Type::VALUE};
    }

    // Built-in functions
    if (expr.resolved_builtin != nullptr) {
        const BuiltinFunction* builtin = expr.resolved_builtin;

        if (builtin->special_handler) {
            return {.postfix = builtin->special_handler(this, expr),
                    .type = Type::VALUE};
        }

        PostfixBuilder b;
        for (size_t i = 0; i < expr.args.size(); ++i) {
            if (builtin->param_types[i] != Type::LITERAL_STRING) {
                auto res = generate(expr.args[i].get());
                b.append(res.postfix);
            }
        }
        b.add_function_call(expr.callee);
        return {.postfix = b, .type = Type::VALUE};
    }

    // nth_N functions
    if (expr.callee.starts_with("nth_")) {
        std::string n_str = expr.callee.substr(4);
        int n = std::stoi(n_str);
        int arg_count = static_cast<int>(expr.args.size());
        PostfixBuilder b;
        for (const auto& arg : expr.args) {
            auto res = generate(arg.get());
            b.append(res.postfix);
        }
        b.add_sortN(arg_count);
        b.add_dropN(n - 1);
        b.add_swapN(arg_count - n);
        b.add_dropN(arg_count - n);

        return {.postfix = b, .type = Type::VALUE};
    }

    std::unreachable();
}

CodeGenerator::ExprResult CodeGenerator::handle(const PropAccessExpr& expr) {
    PostfixBuilder b;
    std::string clip_name = expr.clip.value;
    if (param_substitutions.contains(clip_name)) {
        auto* subst_expr = param_substitutions.at(clip_name);
        auto res = generate(subst_expr);
        clip_name = res.postfix.get_expression();
    } else {
        clip_name = clip_name.substr(1);
    }

    b.add_prop_access(clip_name, expr.prop.value);
    return {.postfix = b, .type = Type::VALUE};
}

CodeGenerator::ExprResult
CodeGenerator::handle(const StaticRelPixelAccessExpr& expr) {
    std::string clip_name = expr.clip.value;
    if (param_substitutions.contains(clip_name)) {
        auto* subst_expr = param_substitutions.at(clip_name);
        auto res = generate(subst_expr);
        clip_name = res.postfix.get_expression();
    } else {
        clip_name = clip_name.substr(1);
    }

    PostfixBuilder b;
    b.add_static_pixel_access(clip_name, expr.offsetX.value, expr.offsetY.value,
                              expr.boundary_suffix);
    return {.postfix = b, .type = Type::VALUE};
}

CodeGenerator::ExprResult
CodeGenerator::handle(const FrameDimensionExpr& expr) {
    auto plane_res = generate(expr.plane_index_expr.get());
    std::string plane_idx_str = plane_res.postfix.get_expression();

    PostfixBuilder b;
    b.add_frame_dimension(expr.dimension_name, plane_idx_str);
    return {.postfix = b, .type = Type::VALUE};
}

CodeGenerator::ExprResult CodeGenerator::handle(const ArrayAccessExpr& expr) {
    // Get the array variable name
    std::string array_name;
    auto* var_expr = get_if<VariableExpr>(expr.array.get());
    if (var_expr != nullptr) {
        array_name = var_expr->name.value;

        if (var_rename_map.contains(array_name)) {
            array_name = var_rename_map.at(array_name);
        } else if (expr.array_symbol) {
            array_name = expr.array_symbol->name;
        }
    } else if (expr.array_symbol) {
        array_name = expr.array_symbol->name;
    }

    auto index_result = generate(expr.index.get());

    PostfixBuilder b;
    b.append(index_result.postfix);
    b.add_array_load(array_name);

    return {.postfix = b, .type = Type::VALUE};
}

PostfixBuilder CodeGenerator::handle(const ExprStmt& stmt) {
    PostfixBuilder b = generate(stmt.expr.get()).postfix;
    check_stack_effect(b.get_expression(), 0, stmt.range);
    return b;
}

PostfixBuilder CodeGenerator::handle(const AssignStmt& stmt) {
    std::string name = stmt.name.value;

    std::string var_name = name;
    if (var_rename_map.contains(name)) {
        var_name = var_rename_map.at(name);
    } else if (stmt.symbol) {
        var_name = stmt.symbol->name;
    }
    // Check if this is a new() or resize() call for array allocation
    if (auto* call_expr = get_if<CallExpr>(stmt.value.get())) {
        if (call_expr->callee == "new" || call_expr->callee == "resize") {
            PostfixBuilder b;

            if (mode == Mode::Expr) {
                auto* arg_expr = call_expr->args[0].get();

                std::string size_value;

                if (auto* num_expr = get_if<NumberExpr>(arg_expr)) {
                    size_value = num_expr->value.value;
                } else if (auto* var_expr = get_if<VariableExpr>(arg_expr)) {
                    std::string var_name_arg = var_expr->name.value;
                    if (param_substitutions.contains(var_name_arg)) {
                        auto* subst_expr = param_substitutions.at(var_name_arg);
                        if (auto* subst_num = get_if<NumberExpr>(subst_expr)) {
                            size_value = subst_num->value.value;
                        }
                    }
                }

                b.add_array_alloc_static(var_name, size_value);
            } else {
                auto size_result = generate(call_expr->args[0].get());
                b.append(size_result.postfix);
                b.add_array_alloc_dynamic(var_name);
            }

            check_stack_effect(b.get_expression(), 0, stmt.range);
            return b;
        }
    }

    auto value_code = generate(stmt.value.get());

    PostfixBuilder b;
    b.append(value_code.postfix);
    b.add_variable_store(var_name);

    check_stack_effect(b.get_expression(), 0, stmt.range);

    return b;
}

PostfixBuilder CodeGenerator::handle(const ArrayAssignStmt& stmt) {
    auto* array_access = get_if<ArrayAccessExpr>(stmt.target.get());

    std::string array_name;
    auto* var_expr = get_if<VariableExpr>(array_access->array.get());
    if (var_expr != nullptr) {
        array_name = var_expr->name.value;

        if (var_rename_map.contains(array_name)) {
            array_name = var_rename_map.at(array_name);
        } else if (array_access->array_symbol) {
            array_name = array_access->array_symbol->name;
        }
    } else if (array_access->array_symbol) {
        array_name = array_access->array_symbol->name;
    }

    PostfixBuilder b;

    auto value_result = generate(stmt.value.get());
    b.append(value_result.postfix);

    auto index_result = generate(array_access->index.get());
    b.append(index_result.postfix);

    b.add_array_store(array_name);

    check_stack_effect(b.get_expression(), 0, stmt.range);

    return b;
}

PostfixBuilder CodeGenerator::handle(const BlockStmt& stmt) {
    PostfixBuilder b;
    for (const auto& s : stmt.statements) {
        b.append(generate(s.get()));
    }
    return b;
}

PostfixBuilder CodeGenerator::handle(const IfStmt& stmt) {
    std::string else_label = std::format("__internal_else_{}", label_counter++);
    std::string endif_label =
        std::format("__internal_endif_{}", label_counter++);

    PostfixBuilder b;
    b.append(generate(stmt.condition.get()).postfix);
    b.add_number("0");
    b.add_op(TokenType::Eq);
    b.add_conditional_jump(else_label);

    b.append(generate(stmt.then_branch.get()));

    if (stmt.else_branch) {
        b.add_unconditional_jump(endif_label);
        b.add_label(else_label);
        b.append(generate(stmt.else_branch.get()));
        b.add_label(endif_label);
    } else {
        b.add_label(else_label);
    }
    return b;
}

PostfixBuilder CodeGenerator::handle(const WhileStmt& stmt) {
    std::string start_label =
        std::format("__internal_while_start_{}", label_counter++);
    std::string end_label =
        std::format("__internal_while_end_{}", label_counter++);

    PostfixBuilder b;
    b.add_label(start_label);
    b.append(generate(stmt.condition.get()).postfix);
    b.add_number("0");
    b.add_op(TokenType::Eq);
    b.add_conditional_jump(end_label);
    b.append(generate(stmt.body.get()));
    b.add_unconditional_jump(start_label);
    b.add_label(end_label);
    return b;
}

PostfixBuilder CodeGenerator::handle(const ReturnStmt& stmt) {
    PostfixBuilder b;

    if (stmt.value) {
        if (!current_function->returns_value) {
            throw CodeGenError(
                std::format("Function '{}' should not return a value.",
                            current_function->name),
                stmt.range);
        }
        auto result = generate(stmt.value.get());
        b.append(result.postfix);
        const int call_id = call_site_id_stack.back();
        std::string ret_var_name = std::format("__internal_ret_{}_{}",
                                               current_function->name, call_id);
        b.add_variable_store(ret_var_name);
    } else {
        if (current_function->returns_value) {
            throw CodeGenError(std::format("Function '{}' must return a value.",
                                           current_function->name),
                               stmt.range);
        }
    }

    const int call_id = call_site_id_stack.back();
    std::string ret_label = std::format("__internal_ret_label_{}_{}",
                                        current_function->name, call_id);
    b.add_unconditional_jump(ret_label);

    return b;
}

PostfixBuilder CodeGenerator::handle(const LabelStmt& stmt) {
    PostfixBuilder b;
    std::string label_name = stmt.symbol ? stmt.symbol->name : stmt.name.value;
    b.add_label(label_name);
    return b;
}

PostfixBuilder CodeGenerator::handle(const GotoStmt& stmt) {
    PostfixBuilder b;
    std::string label_name = stmt.target_label_symbol
                                 ? stmt.target_label_symbol->name
                                 : stmt.label.value;
    if (stmt.condition) {
        b.append(generate(stmt.condition.get()).postfix);
        b.add_number("0");
        b.add_op(TokenType::Eq);
        b.add_unary_op(TokenType::Not);
        b.add_conditional_jump(label_name);
    } else {
        b.add_unconditional_jump(label_name);
    }
    return b;
}

PostfixBuilder CodeGenerator::handle([[maybe_unused]] const FunctionDef& stmt) {
    // Inlined at call sites
    return {};
}

PostfixBuilder CodeGenerator::handle([[maybe_unused]] const GlobalDecl& stmt) {
    // Global declarations don't generate code
    return {};
}

void CodeGenerator::check_stack_effect(const std::string& s, int expected,
                                       const Range& range) {
    int effect = compute_stack_effect(s, range);
    if (effect != expected) {
        throw CodeGenError(std::format("Unbalanced stack. Expected {}, got {}",
                                       expected, effect),
                           range);
    }
}

int CodeGenerator::compute_stack_effect(const std::string& s,
                                        const Range& range) {
    PostfixMode postfix_mode =
        (mode == Mode::Expr) ? PostfixMode::EXPR : PostfixMode::SINGLE_EXPR;

    try {
        return compute_postfix_stack_effect(s, postfix_mode, range.start.line,
                                            num_inputs);
    } catch (const std::exception& e) {
        throw CodeGenError(e.what(), range);
    }
}

PostfixBuilder CodeGenerator::inline_function_call(
    const FunctionSignature& sig, FunctionDef* func_def,
    const std::vector<std::unique_ptr<Expr>>& args, const Range& call_range) {

    const auto& func_name = sig.name;
    const int call_id = call_site_counter++;
    call_site_id_stack.push_back(call_id);

    // Save current state
    auto saved_param_substitutions = param_substitutions;
    auto saved_var_rename_map = var_rename_map;
    const auto* saved_current_function = current_function;

    param_substitutions.clear();
    std::map<std::string, std::string> param_map;

    PostfixBuilder param_assignments;

    for (size_t i = 0; i < sig.params.size(); ++i) {
        const auto& param_info = sig.params[i];
        const std::string& param_name = param_info.name;
        const Type param_type = param_info.type;

        auto* arg_expr = args[i].get();

        if (param_type == Type::LITERAL || param_type == Type::CLIP) {
            param_substitutions[param_name] = arg_expr;
        } else if (param_type == Type::ARRAY) {
            auto* var_expr = get_if<VariableExpr>(arg_expr);
            if (var_expr != nullptr) {
                std::string arg_var_name = var_expr->name.value;
                if (saved_var_rename_map.contains(arg_var_name)) {
                    param_map[param_name] =
                        saved_var_rename_map.at(arg_var_name);
                } else if (var_expr->symbol) {
                    param_map[param_name] = var_expr->symbol->name;
                } else {
                    param_map[param_name] = arg_var_name;
                }
            }
        } else {
            std::string renamed_param = std::format(
                "__internal_func_{}_{}_{}", func_name, call_id, param_name);

            PostfixBuilder arg_value = generate(args[i].get()).postfix;
            param_assignments.append(arg_value);
            param_assignments.add_variable_store(renamed_param);
            param_map[param_name] = renamed_param;
        }
    }

    // Collect local variables from function body
    std::function<void(Stmt*)> collect_locals = [&](Stmt* stmt) {
        if (!stmt) {
            return;
        }
        if (auto* assign = get_if<AssignStmt>(stmt)) {
            std::string var_name = assign->name.value;
            if (!param_map.contains(var_name)) {
                std::string renamed_var = std::format(
                    "__internal_func_{}_{}_{}", func_name, call_id, var_name);
                param_map[var_name] = renamed_var;
            }
        } else if (auto* block = get_if<BlockStmt>(stmt)) {
            for (const auto& s : block->statements) {
                collect_locals(s.get());
            }
        } else if (auto* if_stmt = get_if<IfStmt>(stmt)) {
            collect_locals(if_stmt->then_branch.get());
            if (if_stmt->else_branch) {
                collect_locals(if_stmt->else_branch.get());
            }
        } else if (auto* while_stmt = get_if<WhileStmt>(stmt)) {
            collect_locals(while_stmt->body.get());
        }
    };

    for (const auto& s : func_def->body->statements) {
        collect_locals(s.get());
    }

    // Update context for function body generation
    var_rename_map = param_map;
    current_function = &sig;

    std::string label_prefix =
        std::format("__internal_{}_{}_", func_name, call_id);

    PostfixBuilder body_builder = handle(*func_def->body);
    body_builder.prefix_labels(label_prefix);

    std::string ret_label =
        std::format("__internal_ret_label_{}_{}", func_name, call_id);
    body_builder.add_label(ret_label);

    if (sig.returns_value) {
        std::string ret_var =
            std::format("__internal_ret_{}_{}", func_name, call_id);
        body_builder.add_variable_load(ret_var);
    }

    // Restore state
    param_substitutions = saved_param_substitutions;
    var_rename_map = saved_var_rename_map;
    current_function = saved_current_function;
    call_site_id_stack.pop_back();

    PostfixBuilder inlined_builder;
    inlined_builder.append(param_assignments);
    inlined_builder.append(body_builder);

    int expected_effect = sig.returns_value ? 1 : 0;
    try {
        int actual_effect =
            compute_stack_effect(inlined_builder.get_expression(), call_range);
        if (actual_effect != expected_effect) {
            throw CodeGenError(
                std::format("Function '{}' has unbalanced stack. Expected "
                            "effect: {}, actual: {}",
                            func_name, expected_effect, actual_effect),
                sig.range);
        }
    } catch (const CodeGenError& e) {
        throw CodeGenError(
            std::format("In function '{}': {}", func_name, e.what()),
            sig.range);
    }

    return inlined_builder;
}

} // namespace infix2postfix
