#include "CodeGenerator.hpp"
#include "Builtins.hpp"
#include "PostfixBuilder.hpp"
#include "PostfixHelper.hpp"
#include "types.hpp"
#include <algorithm>
#include <format>
#include <functional>

namespace infix2postfix {

namespace {
bool is_constant_infix_internal(const std::string& s) {
    static const std::set<std::string> constants = {"$pi", "$N",     "$X",
                                                    "$Y",  "$width", "$height"};
    return constants.count(s);
}

bool is_clip_postfix_internal(const std::string& s) {
    if (s.length() == 1 && s[0] >= 'a' && s[0] <= 'z')
        return true;
    if (s.rfind("src", 0) == 0) {
        for (size_t i = 3; i < s.length(); ++i) {
            if (!std::isdigit(s[i]))
                return false;
        }
        return true;
    }
    return false;
}

bool is_convertible_internal(Type from, Type to) {
    return from == to || to == Type::VALUE;
}
} // namespace

CodeGenerator::CodeGenerator(Mode mode, int num_inputs)
    : mode(mode), num_inputs(num_inputs) {}

std::string CodeGenerator::generate(Program* program) {
    // Collect global labels
    for (const auto& stmt : program->statements) {
        if (auto* label_def = get_if<LabelStmt>(stmt.get())) {
            if (global_labels.count(label_def->name.value)) {
                throw CodeGenError(
                    std::format("Duplicate label '{}' in global scope",
                                label_def->name.value),
                    label_def->line);
            }
            global_labels.insert(label_def->name.value);
        }
    }

    for (const auto& stmt : program->statements) {
        if (auto* func_def = get_if<FunctionDef>(stmt.get())) {
            if (functions.count(func_def->name.value)) {
                for (const auto& existing_sig :
                     functions.at(func_def->name.value)) {
                    if (existing_sig.params.size() == func_def->params.size()) {
                        bool same = true;
                        for (size_t i = 0; i < func_def->params.size(); ++i) {
                            if (existing_sig.params[i].type !=
                                func_def->params[i].type) {
                                same = false;
                                break;
                            }
                        }
                        if (same) {
                            throw CodeGenError(
                                std::format(
                                    "Duplicate function signature for '{}'",
                                    func_def->name.value),
                                func_def->line);
                        }
                    }
                }
            }

            FunctionSignature sig;
            sig.name = func_def->name.value;
            for (const auto& p : func_def->params) {
                sig.params.push_back({p.name.value, p.type});
            }
            sig.line = func_def->line;
            sig.has_return = false;
            for (const auto& s : func_def->body->statements) {
                if (get_if<ReturnStmt>(s.get()))
                    sig.has_return = true;
            }
            if (func_def->global_decl) {
                sig.global_mode = func_def->global_decl->mode;
                for (const auto& g : func_def->global_decl->globals) {
                    sig.specific_globals.insert(g.value);
                }
            }
            functions[sig.name].push_back(sig);
            function_defs[sig.name].push_back(func_def);
        }
    }

    PostfixBuilder builder;
    for (const auto& stmt : program->statements) {
        builder.append(generate(stmt.get()));
    }

    if (mode == Mode::Expr && !has_result) {
        throw CodeGenError(
            "Final result must be assigned to variable 'RESULT'!", 0);
    }

    if (mode == Mode::Expr) {
        builder.add_variable_load("RESULT");
    }

    return builder.get_expression();
}

CodeGenerator::ExprResult CodeGenerator::generate(Expr* expr) {
    if (!expr)
        return {{}, Type::VALUE}; // Should not happen with valid AST
    return std::visit([this](auto& e) { return this->handle(e); }, expr->value);
}

PostfixBuilder CodeGenerator::generate(Stmt* stmt) {
    if (!stmt)
        return {};
    return std::visit([this](auto& s) { return this->handle(s); }, stmt->value);
}

CodeGenerator::ExprResult CodeGenerator::handle(const NumberExpr& expr) {
    PostfixBuilder b;
    b.add_number(expr.value.value);
    return {b, Type::LITERAL};
}

CodeGenerator::ExprResult CodeGenerator::handle(const VariableExpr& expr) {
    PostfixBuilder b;
    std::string name = expr.name.value;

    if (param_substitutions.count(name)) {
        return generate(param_substitutions.at(name));
    }

    if (name.starts_with("$")) {
        std::string base_name = name.substr(1);

        if ((base_name == "X" || base_name == "Y") && mode == Mode::Single) {
            throw CodeGenError(
                "X and Y coordinates are only available in Expr mode. "
                "SingleExpr processes the entire frame at once, not "
                "pixel-by-pixel.",
                expr.line);
        }

        b.add_constant(base_name);

        if (is_clip_name(base_name)) {
            return {b, Type::CLIP};
        }
        return {b, Type::VALUE};
    }

    check_variable_defined(name, expr.line);

    std::string renamed = rename_variable(name);

    if (literals_in_scope.count(renamed)) {
        b.add_raw(renamed);
        return {b, Type::VALUE}; // Assuming literals are values
    }

    b.add_variable_load(renamed);
    return {b, Type::VALUE};
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
            return {b, Type::LITERAL};
        }
    }

    auto right = generate(expr.right.get());
    if (!is_convertible(right.type, Type::VALUE)) {
        throw CodeGenError(
            std::format("Cannot apply unary operator '{}' to type "
                        "'{}' which is not convertible to a "
                        "value.",
                        token_type_to_string(expr.op.type),
                        to_string(right.type)),
            expr.line);
    }

    PostfixBuilder b = right.postfix;
    b.add_unary_op(expr.op.type);
    return {b, Type::VALUE};
}

CodeGenerator::ExprResult CodeGenerator::handle(const BinaryExpr& expr) {
    auto left = generate(expr.left.get());
    auto right = generate(expr.right.get());

    if (!is_convertible(left.type, Type::VALUE)) {
        throw CodeGenError(
            std::format("Left operand of binary operator '{}' has type "
                        "'{}' which is not convertible to a value.",
                        token_type_to_string(expr.op.type),
                        to_string(left.type)),
            expr.line);
    }
    if (!is_convertible(right.type, Type::VALUE)) {
        throw CodeGenError(
            std::format("Right operand of binary operator '{}' has type "
                        "'{}' which is not convertible to a value.",
                        token_type_to_string(expr.op.type),
                        to_string(right.type)),
            expr.line);
    }

    PostfixBuilder b;
    b.append(left.postfix);
    b.append(right.postfix);
    b.add_op(expr.op.type);
    return {b, Type::VALUE};
}

CodeGenerator::ExprResult CodeGenerator::handle(const TernaryExpr& expr) {
    auto cond = generate(expr.cond.get());
    if (!is_convertible(cond.type, Type::VALUE)) {
        throw CodeGenError(
            std::format("Ternary condition has type '{}' which is not "
                        "convertible to a value.",
                        to_string(cond.type)),
            expr.line);
    }

    auto true_branch = generate(expr.true_expr.get());
    auto false_branch = generate(expr.false_expr.get());

    // TODO: The type of the ternary expression is the type of its branches
    if (!is_convertible(true_branch.type, Type::VALUE) ||
        !is_convertible(false_branch.type, Type::VALUE)) {
        throw CodeGenError(
            "Both branches of a ternary expression must be convertible to a "
            "value.",
            expr.line);
    }

    PostfixBuilder b;
    b.append(cond.postfix);
    b.append(true_branch.postfix);
    b.append(false_branch.postfix);
    b.add_ternary_op();
    return {b, Type::VALUE};
}

CodeGenerator::ExprResult CodeGenerator::handle(const CallExpr& expr) {
    // User-defined functions are inlined
    if (functions.count(expr.callee)) {
        const auto& overloads = functions.at(expr.callee);
        const auto& def_overloads = function_defs.at(expr.callee);

        std::vector<ExprResult> arg_results;
        arg_results.reserve(expr.args.size());
        for (const auto& arg : expr.args) {
            arg_results.push_back(generate(arg.get()));
        }

        struct Candidate {
            const FunctionSignature* sig;
            FunctionDef* def;
            int conversion_count;
            int first_conversion_index;
        };
        std::vector<Candidate> candidates;

        for (size_t i = 0; i < overloads.size(); ++i) {
            const auto& sig = overloads[i];
            if (sig.params.size() != arg_results.size())
                continue;

            int conversion_count = 0;
            int first_conversion_index = -1;
            bool possible = true;
            for (size_t j = 0; j < arg_results.size(); ++j) {
                Type arg_type = arg_results[j].type;
                Type param_type = sig.params[j].type;
                if (arg_type != param_type) {
                    if (is_convertible(arg_type, param_type)) {
                        conversion_count++;
                        if (first_conversion_index == -1) {
                            first_conversion_index = static_cast<int>(j);
                        }
                    } else {
                        possible = false;
                        break;
                    }
                }
            }

            if (possible) {
                candidates.push_back({&sig, def_overloads[i], conversion_count,
                                      first_conversion_index});
            }
        }

        if (candidates.empty()) {
            std::string arg_types_str;
            for (size_t i = 0; i < arg_results.size(); ++i) {
                arg_types_str += to_string(arg_results[i].type);
                if (i < arg_results.size() - 1)
                    arg_types_str += ", ";
            }
            throw CodeGenError(
                std::format(
                    "No matching user-defined function for call to '{}({})'",
                    expr.callee, arg_types_str),
                expr.line);
        }

        Candidate* best_candidate = &candidates[0];
        for (size_t i = 1; i < candidates.size(); ++i) {
            if (candidates[i].conversion_count <
                best_candidate->conversion_count) {
                best_candidate = &candidates[i];
            } else if (candidates[i].conversion_count ==
                       best_candidate->conversion_count) {
                if (candidates[i].first_conversion_index >
                    best_candidate->first_conversion_index) {
                    best_candidate = &candidates[i];
                }
            }
        }

        int best_conversion_count = best_candidate->conversion_count;
        int best_first_conversion_index =
            best_candidate->first_conversion_index;
        int num_best = 0;
        for (const auto& cand : candidates) {
            if (cand.conversion_count == best_conversion_count &&
                cand.first_conversion_index == best_first_conversion_index) {
                num_best++;
            }
        }

        if (num_best > 1) {
            throw CodeGenError(
                std::format("Ambiguous call to overloaded function '{}'",
                            expr.callee),
                expr.line);
        }

        PostfixBuilder b = inline_function_call(
            *best_candidate->sig, best_candidate->def, expr.args, expr.line);
        // FIXME: Assume functions return a single value for now.
        return {b, Type::VALUE};
    }

    const auto& builtins = get_builtin_functions();

    if (builtins.count(expr.callee)) {
        const auto& overloads = builtins.at(expr.callee);

        std::vector<std::optional<ExprResult>> arg_results(expr.args.size());

        struct Candidate {
            const BuiltinFunction* builtin;
            int conversion_count;
            int first_conversion_index;
        };
        std::vector<Candidate> candidates;

        for (const auto& builtin : overloads) {
            if (builtin.arity != (int)expr.args.size())
                continue;
            if (builtin.mode_restriction.has_value() &&
                builtin.mode_restriction.value() != mode)
                continue;

            int conversion_count = 0;
            int first_conversion_index = -1;
            bool possible = true;
            for (size_t j = 0; j < expr.args.size(); ++j) {
                Type param_type = builtin.param_types[j];

                if (param_type == Type::LITERAL_STRING) {
                    auto* var_expr = get_if<VariableExpr>(expr.args[j].get());
                    if (!var_expr || var_expr->name.value.starts_with("$")) {
                        possible = false;
                        break;
                    }
                    continue;
                }

                if (!arg_results[j].has_value()) {
                    arg_results[j] = generate(expr.args[j].get());
                }
                Type arg_type = arg_results[j]->type;

                if (arg_type != param_type) {
                    if (is_convertible(arg_type, param_type)) {
                        conversion_count++;
                        if (first_conversion_index == -1) {
                            first_conversion_index = static_cast<int>(j);
                        }
                    } else {
                        possible = false;
                        break;
                    }
                }
            }

            if (possible) {
                candidates.push_back(
                    {&builtin, conversion_count, first_conversion_index});
            }
        }

        if (candidates.empty()) {
            std::string arg_types_str;
            for (size_t i = 0; i < expr.args.size(); ++i) {
                if (builtin_param_type_is_evaluatable(overloads, i)) {
                    if (!arg_results[i].has_value()) {
                        arg_results[i] = generate(expr.args[i].get());
                    }
                    arg_types_str += to_string(arg_results[i]->type);
                } else {
                    arg_types_str += "LiteralString";
                }

                if (i < expr.args.size() - 1)
                    arg_types_str += ", ";
            }
            throw CodeGenError(
                std::format("No matching overload for function '{}({})' in {} "
                            "mode.",
                            expr.callee, arg_types_str,
                            mode == Mode::Expr ? "Expr" : "SingleExpr"),
                expr.line);
        }

        Candidate* best_candidate = &candidates[0];
        for (size_t i = 1; i < candidates.size(); ++i) {
            if (candidates[i].conversion_count <
                best_candidate->conversion_count) {
                best_candidate = &candidates[i];
            } else if (candidates[i].conversion_count ==
                       best_candidate->conversion_count) {
                if (candidates[i].first_conversion_index >
                    best_candidate->first_conversion_index) {
                    best_candidate = &candidates[i];
                }
            }
        }

        int best_conversion_count = best_candidate->conversion_count;
        int best_first_conversion_index =
            best_candidate->first_conversion_index;
        int num_best = 0;
        for (const auto& cand : candidates) {
            if (cand.conversion_count == best_conversion_count &&
                cand.first_conversion_index == best_first_conversion_index) {
                num_best++;
            }
        }

        if (num_best > 1) {
            throw CodeGenError(
                std::format(
                    "Ambiguous call to overloaded built-in function '{}'",
                    expr.callee),
                expr.line);
        }

        if (best_candidate->builtin->special_handler) {
            return {best_candidate->builtin->special_handler(this, expr),
                    Type::VALUE};
        }

        PostfixBuilder b;
        for (size_t i = 0; i < expr.args.size(); ++i) {
            if (best_candidate->builtin->param_types[i] !=
                Type::LITERAL_STRING) {
                if (!arg_results[i].has_value()) {
                    arg_results[i] = generate(expr.args[i].get());
                }
                b.append(arg_results[i]->postfix);
            }
        }
        b.add_function_call(expr.callee);
        return {b, Type::VALUE};

    } else if (expr.callee.starts_with("nth_")) {
        std::string n_str = expr.callee.substr(4);
        if (n_str.empty() ||
            !std::all_of(n_str.begin(), n_str.end(), ::isdigit)) {
            throw CodeGenError(
                std::format("Invalid nth_N function name '{}'", expr.callee),
                expr.line);
        }
        int n = std::stoi(n_str);
        if ((int)expr.args.size() < n) {
            throw CodeGenError(std::format("Function '{}' requires at least {} "
                                           "arguments, but {} were provided",
                                           expr.callee, n, expr.args.size()),
                               expr.line);
        }
    } else {
        throw CodeGenError(std::format("Unknown function '{}'", expr.callee),
                           expr.line);
    }

    PostfixBuilder b;
    for (const auto& arg : expr.args) {
        auto res = generate(arg.get());
        if (!is_convertible(res.type, Type::VALUE)) {
            throw CodeGenError(
                std::format("Argument to function '{}' has type '{}' which "
                            "is not convertible to a value.",
                            expr.callee, to_string(res.type)),
                arg->line());
        }
        b.append(res.postfix);
    }
    b.add_function_call(expr.callee);
    return {b, Type::VALUE};
}

CodeGenerator::ExprResult CodeGenerator::handle(const PropAccessExpr& expr) {
    PostfixBuilder b;
    std::string clip_name = expr.clip.value;
    if (param_substitutions.count(clip_name)) {
        auto* subst_expr = param_substitutions.at(clip_name);
        auto res = generate(subst_expr);
        if (res.type != Type::CLIP) {
            throw CodeGenError(
                std::format("Parameter '{}' is used as a clip for property "
                            "access but was not a clip constant.",
                            expr.clip.value),
                expr.line);
        }
        clip_name = res.postfix.get_expression();
    } else {
        if (!clip_name.starts_with("$")) {
            throw CodeGenError(
                std::format("Clip '{}' is used as a property access target but "
                            "was not a clip constant.",
                            expr.clip.value),
                expr.line);
        }
        clip_name = clip_name.substr(1);
    }

    if (!is_clip_name(clip_name)) {
        throw CodeGenError(
            std::format("Invalid clip identifier '{}' for property access.",
                        expr.clip.value),
            expr.line);
    }

    b.add_prop_access(clip_name, expr.prop.value);
    return {b, Type::VALUE};
}

CodeGenerator::ExprResult
CodeGenerator::handle(const StaticRelPixelAccessExpr& expr) {
    if (mode == Mode::Single) {
        throw CodeGenError("Static relative pixel access (clip[x,y]) is only "
                           "available in Expr mode. "
                           "Use dyn(clip, x, y, plane) for absolute access in "
                           "SingleExpr mode.",
                           expr.line);
    }
    std::string clip_name = expr.clip.value;
    if (param_substitutions.count(clip_name)) {
        auto* subst_expr = param_substitutions.at(clip_name);
        auto res = generate(subst_expr);
        if (res.type != Type::CLIP) {
            throw CodeGenError(
                std::format("Parameter '{}' is used as a clip for pixel "
                            "access but was not a clip constant.",
                            expr.clip.value),
                expr.line);
        }
        clip_name = res.postfix.get_expression();
    } else {
        if (!clip_name.starts_with("$")) {
            throw CodeGenError(
                std::format("Clip '{}' is used for pixel access but "
                            "was not a clip constant.",
                            expr.clip.value),
                expr.line);
        }
        clip_name = clip_name.substr(1);
    }

    if (!is_clip_name(clip_name)) {
        throw CodeGenError(
            std::format("Invalid clip identifier '{}' for pixel access.",
                        expr.clip.value),
            expr.line);
    }

    PostfixBuilder b;
    b.add_static_pixel_access(clip_name, expr.offsetX.value, expr.offsetY.value,
                              expr.boundary_suffix);
    return {b, Type::VALUE};
}

CodeGenerator::ExprResult
CodeGenerator::handle(const FrameDimensionExpr& expr) {
    if (mode == Mode::Expr) {
        throw CodeGenError("frame.width[N] and frame.height[N] are only "
                           "available in SingleExpr mode. "
                           "Use width and height directly in Expr mode.",
                           expr.line);
    }

    auto plane_res = generate(expr.plane_index_expr.get());
    if (plane_res.type != Type::LITERAL) {
        throw CodeGenError("Plane index must be a literal constant.",
                           expr.plane_index_expr->line());
    }

    std::string plane_idx_str = plane_res.postfix.get_expression();
    try {
        std::stoi(plane_idx_str);
    } catch (...) {
        throw CodeGenError(
            std::format("Plane index must be an integer constant, got: {}",
                        plane_idx_str),
            expr.plane_index_expr->line());
    }

    PostfixBuilder b;
    b.add_frame_dimension(expr.dimension_name, plane_idx_str);
    return {b, Type::VALUE};
}

PostfixBuilder CodeGenerator::handle(const ExprStmt& stmt) {
    PostfixBuilder b = generate(stmt.expr.get()).postfix;
    check_stack_effect(b.get_expression(), 0, stmt.line);
    return b;
}

PostfixBuilder CodeGenerator::handle(const AssignStmt& stmt) {
    if (stmt.name.value == "RESULT")
        has_result = true;

    auto value_code = generate(stmt.value.get());

    std::string var_name = stmt.name.value;
    std::string renamed_var = rename_variable(var_name);

    PostfixBuilder b;
    b.append(value_code.postfix);
    b.add_variable_store(renamed_var);

    check_stack_effect(b.get_expression(), 0, stmt.line);

    define_variable_in_current_scope(renamed_var);

    if (current_function == nullptr) {
        defined_globals.insert(var_name);
    } else {
        local_scope_vars.insert(var_name);
    }
    return b;
}

PostfixBuilder CodeGenerator::handle(const BlockStmt& stmt) {
    enter_scope();

    PostfixBuilder b;
    for (const auto& s : stmt.statements) {
        b.append(generate(s.get()));
    }

    exit_scope();

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
    b.add_unary_op(TokenType::Not);
    b.add_conditional_jump(end_label);
    b.append(generate(stmt.body.get()));
    b.add_unconditional_jump(start_label);
    b.add_label(end_label);
    return b;
}

PostfixBuilder CodeGenerator::handle(const ReturnStmt& stmt) {
    if (current_function == nullptr) {
        throw CodeGenError(
            "'return' statements are not allowed in the global scope.",
            stmt.line);
    }
    if (stmt.value) {
        return generate(stmt.value.get()).postfix;
    }
    return {};
}

PostfixBuilder CodeGenerator::handle(const LabelStmt& stmt) {
    PostfixBuilder b;
    b.add_label(stmt.name.value);
    return b;
}

PostfixBuilder CodeGenerator::handle(const GotoStmt& stmt) {
    if (current_function != nullptr) {
        // Inside a function
        if (global_labels.count(stmt.label.value)) {
            throw CodeGenError(std::format("goto from function '{}' to global "
                                           "label '{}' is not allowed",
                                           current_function->name,
                                           stmt.label.value),
                               stmt.line);
        }
        if (current_function_labels.find(stmt.label.value) ==
            current_function_labels.end()) {
            throw CodeGenError(
                std::format("goto target '{}' not found in function '{}'",
                            stmt.label.value, current_function->name),
                stmt.line);
        }
    } else {
        // Global scope
        if (global_labels.find(stmt.label.value) == global_labels.end()) {
            throw CodeGenError(
                std::format("goto target '{}' not found in global scope",
                            stmt.label.value),
                stmt.line);
        }
    }

    PostfixBuilder b;
    if (stmt.condition) {
        b.append(generate(stmt.condition.get()).postfix);
        b.add_conditional_jump(stmt.label.value);
    } else {
        b.add_unconditional_jump(stmt.label.value);
    }
    return b;
}

PostfixBuilder CodeGenerator::handle([[maybe_unused]] const FunctionDef& stmt) {
    // Handled in first pass
    return {};
}
PostfixBuilder CodeGenerator::handle([[maybe_unused]] const GlobalDecl& stmt) {
    // Handled by parser
    return {};
}

void CodeGenerator::check_stack_effect(const std::string& s, int expected,
                                       int line) {
    int effect = compute_stack_effect(s, line);
    if (effect != expected) {
        throw CodeGenError(std::format("Unbalanced stack. Expected {}, got {}",
                                       expected, effect),
                           line);
    }
}

int CodeGenerator::compute_stack_effect(const std::string& s, int line) {
    PostfixMode postfix_mode =
        (mode == Mode::Expr) ? PostfixMode::EXPR : PostfixMode::SINGLE_EXPR;

    try {
        return compute_postfix_stack_effect(s, postfix_mode, line, num_inputs);
    } catch (const std::exception& e) {
        throw CodeGenError(e.what(), line);
    }
}

std::string CodeGenerator::rename_variable(const std::string& var_name) {
    if (var_rename_map.count(var_name)) {
        return var_rename_map[var_name];
    }
    return var_name;
}

bool CodeGenerator::is_constant_infix(const std::string& name) {
    return is_constant_infix_internal(name);
}

bool CodeGenerator::is_clip_name(const std::string& s) {
    return is_clip_postfix_internal(s);
}

bool CodeGenerator::is_convertible(Type from, Type to) {
    return is_convertible_internal(from, to);
}

PostfixBuilder CodeGenerator::inline_function_call(
    const FunctionSignature& sig, FunctionDef* func_def,
    const std::vector<std::unique_ptr<Expr>>& args, int call_line) {

    const auto& func_name = sig.name;

    // Check for return statements
    const auto& stmts = func_def->body->statements;
    for (size_t i = 0; i < stmts.size(); ++i) {
        const auto& stmt_ptr = stmts[i];

        std::function<void(Stmt*)> find_returns_in_blocks = [&](Stmt* s) {
            if (!s)
                return;
            if (get_if<ReturnStmt>(s)) {
                throw CodeGenError(
                    "'return' is not allowed inside blocks within a function.",
                    s->line());
            }
            if (auto* if_s = get_if<IfStmt>(s)) {
                find_returns_in_blocks(if_s->then_branch.get());
                if (if_s->else_branch)
                    find_returns_in_blocks(if_s->else_branch.get());
            } else if (auto* while_s = get_if<WhileStmt>(s)) {
                find_returns_in_blocks(while_s->body.get());
            } else if (auto* block = get_if<BlockStmt>(s)) {
                for (const auto& inner_s : block->statements) {
                    find_returns_in_blocks(inner_s.get());
                }
            }
        };

        if (get_if<ReturnStmt>(stmt_ptr.get())) {
            if (i != stmts.size() - 1) {
                throw CodeGenError(
                    "'return' must be the last statement in a function body.",
                    stmt_ptr->line());
            }
        } else {
            find_returns_in_blocks(stmt_ptr.get());
        }
    }

    auto saved_current_function_labels = current_function_labels;
    current_function_labels.clear();
    std::function<void(Stmt*)> collect_labels = [&](Stmt* s) {
        if (!s)
            return;
        if (auto* label = get_if<LabelStmt>(s)) {
            if (current_function_labels.count(label->name.value)) {
                throw CodeGenError(
                    std::format("Duplicate label '{}' in function '{}'",
                                label->name.value, sig.name),
                    label->line);
            }
            current_function_labels.insert(label->name.value);
        } else if (auto* block = get_if<BlockStmt>(s)) {
            for (auto& inner_s : block->statements) {
                collect_labels(inner_s.get());
            }
        } else if (auto* if_s = get_if<IfStmt>(s)) {
            collect_labels(if_s->then_branch.get());
            if (if_s->else_branch) {
                collect_labels(if_s->else_branch.get());
            }
        } else if (auto* while_s = get_if<WhileStmt>(s)) {
            collect_labels(while_s->body.get());
        }
    };
    for (const auto& s : func_def->body->statements) {
        collect_labels(s.get());
    }

    if (sig.global_mode == GlobalMode::SPECIFIC) {
        for (const auto& gv : sig.specific_globals) {
            if (!defined_globals.count(gv)) {
                throw CodeGenError(
                    std::format("Global variable '{}' used in function '{}' is "
                                "not defined before its first call",
                                gv, func_name),
                    call_line);
            }
        }
    }

    auto saved_rename_map = var_rename_map;
    auto saved_literals = literals_in_scope;
    auto saved_local_vars = local_scope_vars;
    auto saved_all_defined = all_defined_vars_in_scope;
    auto saved_scope_stack = scope_stack;
    auto saved_param_substitutions = param_substitutions;
    const FunctionSignature* saved_current_function = current_function;

    param_substitutions.clear();
    std::map<std::string, std::string> param_map;
    std::set<std::string> effective_globals;
    std::set<std::string> new_local_vars;

    if (sig.global_mode == GlobalMode::ALL) {
        effective_globals = defined_globals;
    } else if (sig.global_mode == GlobalMode::SPECIFIC) {
        effective_globals = sig.specific_globals;
    }

    // Function has its own scope
    scope_stack.clear();
    all_defined_vars_in_scope.clear();

    for (const auto& g : effective_globals) {
        all_defined_vars_in_scope.insert(g);
    }

    PostfixBuilder param_assignments;
    std::set<std::string> new_literals;

    for (size_t i = 0; i < sig.params.size(); ++i) {
        const auto& param_info = sig.params[i];
        const std::string& param_name = param_info.name;
        const Type param_type = param_info.type;
        std::string renamed_param =
            std::format("__internal_func_{}_{}", func_name, param_name);

        if (!effective_globals.count(param_name)) {
            auto* arg_expr = args[i].get();

            if (param_type == Type::LITERAL || param_type == Type::CLIP) {
                param_substitutions[param_name] = arg_expr;
                param_map[param_name] = param_name;
                new_local_vars.insert(param_name);
                all_defined_vars_in_scope.insert(param_name);
            } else { // Type::VALUE
                // Restore scope temporarily to generate argument value
                auto temp_all_defined = all_defined_vars_in_scope;
                auto temp_scope_stack = scope_stack;
                all_defined_vars_in_scope = saved_all_defined;
                scope_stack = saved_scope_stack;

                // Generate argument value
                PostfixBuilder arg_value = generate(args[i].get()).postfix;

                // Restore function scope
                all_defined_vars_in_scope = temp_all_defined;
                scope_stack = temp_scope_stack;

                param_assignments.append(arg_value);
                param_assignments.add_variable_store(renamed_param);
                param_map[param_name] = renamed_param;
                new_local_vars.insert(param_name);
                // Define parameter in function scope
                all_defined_vars_in_scope.insert(renamed_param);
            }
        } else {
            param_map[param_name] = param_name;
        }
    }

    // Collect local variables from function body
    std::function<void(Stmt*)> collect_locals = [&](Stmt* stmt) {
        if (auto* assign = get_if<AssignStmt>(stmt)) {
            std::string var_name = assign->name.value;
            if (!effective_globals.count(var_name) &&
                var_name.find("__internal_func_") != 0) {
                std::string renamed_var =
                    std::format("__internal_func_{}_{}", func_name, var_name);
                param_map[var_name] = renamed_var;
                new_local_vars.insert(var_name);
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

    std::function<void(BlockStmt*)> collect_locals_block =
        [&](BlockStmt* block) {
            for (const auto& s : block->statements) {
                if (auto* assign = get_if<AssignStmt>(s.get())) {
                    std::string var_name = assign->name.value;
                    if (!effective_globals.count(var_name) &&
                        var_name.find("__internal_func_") != 0) {
                        std::string renamed_var = std::format(
                            "__internal_func_{}_{}", func_name, var_name);
                        param_map[var_name] = renamed_var;
                        new_local_vars.insert(var_name);
                    }
                } else if (auto* block_s = get_if<BlockStmt>(s.get())) {
                    collect_locals_block(block_s);
                } else if (auto* if_stmt = get_if<IfStmt>(s.get())) {
                    collect_locals(if_stmt->then_branch.get());
                    if (if_stmt->else_branch) {
                        collect_locals(if_stmt->else_branch.get());
                    }
                } else if (auto* while_stmt = get_if<WhileStmt>(s.get())) {
                    collect_locals(while_stmt->body.get());
                }
            }
        };

    collect_locals_block(func_def->body.get());

    // Update context for function body generation
    var_rename_map = param_map;
    literals_in_scope = new_literals;
    local_scope_vars = new_local_vars;
    local_scope_vars.insert(effective_globals.begin(), effective_globals.end());
    current_function = &sig;

    std::string label_prefix = std::format("__internal_{}_", func_name);

    PostfixBuilder body_builder = handle(*func_def->body);
    body_builder.prefix_labels(label_prefix);

    var_rename_map = saved_rename_map;
    literals_in_scope = saved_literals;
    local_scope_vars = saved_local_vars;
    all_defined_vars_in_scope = saved_all_defined;
    scope_stack = saved_scope_stack;
    param_substitutions = saved_param_substitutions;
    current_function = saved_current_function;
    current_function_labels = saved_current_function_labels;

    PostfixBuilder inlined_builder;
    inlined_builder.append(param_assignments);
    inlined_builder.append(body_builder);

    int expected_effect = sig.has_return ? 1 : 0;
    try {
        int actual_effect =
            compute_stack_effect(inlined_builder.get_expression(), call_line);
        if (actual_effect != expected_effect) {
            throw CodeGenError(
                std::format("Function '{}' has unbalanced stack. Expected "
                            "effect: {}, actual: {}",
                            func_name, expected_effect, actual_effect),
                sig.line);
        }
    } catch (const CodeGenError& e) {
        throw CodeGenError(
            std::format("In function '{}': {}", func_name, e.what()), sig.line);
    }

    return inlined_builder;
}

void CodeGenerator::check_variable_defined(const std::string& var_name,
                                           int line) {
    if (var_name == "frame") {
        return;
    }

    std::string actual_name = rename_variable(var_name);

    if (all_defined_vars_in_scope.count(actual_name) ||
        all_defined_vars_in_scope.count(var_name)) {
        return;
    }

    if (literals_in_scope.count(actual_name) ||
        literals_in_scope.count(var_name)) {
        return;
    }

    if (local_scope_vars.count(var_name) ||
        local_scope_vars.count(actual_name)) {
        return;
    }

    if (defined_globals.count(var_name)) {
        if (current_function != nullptr) {
            if (current_function->global_mode == GlobalMode::ALL) {
                return;
            } else if (current_function->global_mode == GlobalMode::SPECIFIC) {
                if (current_function->specific_globals.count(var_name)) {
                    return;
                }
            }
            throw CodeGenError(
                std::format(
                    "Variable '{}' is a global variable but is not accessible "
                    "in function '{}'. Use <global<{}>> or <global.all> before "
                    "the function definition to grant access.",
                    var_name, current_function->name, var_name),
                line);
        }
        return;
    }

    throw CodeGenError(
        std::format("Variable '{}' is used before being defined", var_name),
        line);
}

void CodeGenerator::enter_scope() {
    scope_stack.push_back(std::set<std::string>());
}

void CodeGenerator::exit_scope() {
    if (!scope_stack.empty()) {
        const auto& scope_vars = scope_stack.back();
        for (const auto& var : scope_vars) {
            all_defined_vars_in_scope.erase(var);
        }
        scope_stack.pop_back();
    }
}

void CodeGenerator::define_variable_in_current_scope(
    const std::string& var_name) {
    if (!scope_stack.empty()) {
        scope_stack.back().insert(var_name);
    }
    all_defined_vars_in_scope.insert(var_name);
}

bool CodeGenerator::builtin_param_type_is_evaluatable(
    const std::vector<BuiltinFunction>& overloads, size_t param_idx) {
    for (const auto& o : overloads) {
        if (o.param_types.size() > param_idx &&
            o.param_types[param_idx] == Type::LITERAL_STRING) {
            return false;
        }
    }
    return true;
}

} // namespace infix2postfix