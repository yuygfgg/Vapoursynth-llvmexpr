#include "SemanticAnalyzer.hpp"
#include "Builtins.hpp"
#include "PostfixHelper.hpp"
#include <algorithm>
#include <format>
#include <functional>

namespace infix2postfix {

namespace {
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
    return from == to || (to == Type::VALUE && from != Type::LITERAL_STRING);
}
} // namespace

SemanticAnalyzer::SemanticAnalyzer(Mode mode, int num_inputs)
    : mode(mode), num_inputs(num_inputs) {}

bool SemanticAnalyzer::analyze(Program* program) {
    if (!program) {
        reportError("Program AST is null", 0);
        return false;
    }

    // Collect global labels
    for (const auto& stmt : program->statements) {
        if (auto* label_def = get_if<LabelStmt>(stmt.get())) {
            if (global_labels.count(label_def->name.value)) {
                reportError(std::format("Duplicate label '{}' in global scope",
                                        label_def->name.value),
                            label_def->line);
            }
            global_labels.insert(label_def->name.value);
        }
    }

    // Collect function signatures
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
                            reportError(
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

    // Analyze all statements
    for (const auto& stmt : program->statements) {
        analyzeStmt(stmt.get());
    }

    // Check if RESULT is defined in Expr mode
    if (mode == Mode::Expr && !has_result) {
        reportError("Final result must be assigned to variable 'RESULT'!", 0);
    }

    if (mode == Mode::Expr && !result_defined_in_global_scope) {
        reportError(
            "'RESULT' must be defined in the global scope in Expr mode.", 0);
    }

    return !hasErrors();
}

bool SemanticAnalyzer::hasErrors() const {
    for (const auto& diag : diagnostics) {
        if (diag.severity == DiagnosticSeverity::ERROR) {
            return true;
        }
    }
    return false;
}

void SemanticAnalyzer::reportError(const std::string& message, int line) {
    diagnostics.emplace_back(DiagnosticSeverity::ERROR, message, line);
}

void SemanticAnalyzer::reportWarning(const std::string& message, int line) {
    diagnostics.emplace_back(DiagnosticSeverity::WARNING, message, line);
}

Type SemanticAnalyzer::analyzeExpr(Expr* expr) {
    if (!expr)
        return Type::VALUE;
    return std::visit([this](auto& e) { return this->analyze(e); },
                      expr->value);
}

void SemanticAnalyzer::analyzeStmt(Stmt* stmt) {
    if (!stmt)
        return;
    std::visit([this](auto& s) { this->analyze(s); }, stmt->value);
}

Type SemanticAnalyzer::analyze([[maybe_unused]] const NumberExpr& expr) {
    return Type::LITERAL;
}

Type SemanticAnalyzer::analyze(const VariableExpr& expr) {
    std::string name = expr.name.value;

    if (param_substitutions.count(name)) {
        return analyzeExpr(param_substitutions.at(name));
    }

    if (name.starts_with("$")) {
        std::string base_name = name.substr(1);

        if ((base_name == "X" || base_name == "Y") && mode == Mode::Single) {
            reportError("X and Y coordinates are only available in Expr mode. "
                        "SingleExpr processes the entire frame at once, not "
                        "pixel-by-pixel.",
                        expr.line);
        }

        if (isClipName(base_name)) {
            return Type::CLIP;
        }
        return Type::VALUE;
    }

    checkVariableDefined(name, expr.line);

    std::string renamed = renameVariable(name);
    auto type_it = variable_types.find(renamed);
    if (type_it != variable_types.end()) {
        return type_it->second;
    }

    return Type::VALUE;
}

Type SemanticAnalyzer::analyze(const UnaryExpr& expr) {
    if (expr.op.type == TokenType::Minus) {
        if (get_if<NumberExpr>(expr.right.get())) {
            return Type::LITERAL;
        }
    }

    auto right_type = analyzeExpr(expr.right.get());
    if (!isConvertible(right_type, Type::VALUE)) {
        reportError(std::format("Cannot apply unary operator '{}' to type "
                                "'{}' which is not convertible to a value.",
                                token_type_to_string(expr.op.type),
                                to_string(right_type)),
                    expr.line);
    }

    return Type::VALUE;
}

Type SemanticAnalyzer::analyze(const BinaryExpr& expr) {
    auto left_type = analyzeExpr(expr.left.get());
    auto right_type = analyzeExpr(expr.right.get());

    if (!isConvertible(left_type, Type::VALUE)) {
        reportError(std::format("Left operand of binary operator '{}' has type "
                                "'{}' which is not convertible to a value.",
                                token_type_to_string(expr.op.type),
                                to_string(left_type)),
                    expr.line);
    }
    if (!isConvertible(right_type, Type::VALUE)) {
        reportError(
            std::format("Right operand of binary operator '{}' has type "
                        "'{}' which is not convertible to a value.",
                        token_type_to_string(expr.op.type),
                        to_string(right_type)),
            expr.line);
    }

    return Type::VALUE;
}

Type SemanticAnalyzer::analyze(const TernaryExpr& expr) {
    auto cond_type = analyzeExpr(expr.cond.get());
    if (!isConvertible(cond_type, Type::VALUE)) {
        reportError(std::format("Ternary condition has type '{}' which is not "
                                "convertible to a value.",
                                to_string(cond_type)),
                    expr.line);
    }

    auto true_type = analyzeExpr(expr.true_expr.get());
    auto false_type = analyzeExpr(expr.false_expr.get());

    if (!isConvertible(true_type, Type::VALUE) ||
        !isConvertible(false_type, Type::VALUE)) {
        reportError("Both branches of a ternary expression must be convertible "
                    "to a value.",
                    expr.line);
    }

    return Type::VALUE;
}

Type SemanticAnalyzer::analyze(const CallExpr& expr) {
    // User-defined functions
    if (functions.count(expr.callee)) {
        const auto& overloads = functions.at(expr.callee);

        std::vector<Type> arg_types;
        arg_types.reserve(expr.args.size());
        for (const auto& arg : expr.args) {
            if (auto* var_expr = get_if<VariableExpr>(arg.get())) {
                std::string var_name = var_expr->name.value;
                if (!var_name.starts_with("$")) {
                    std::string renamed_var = renameVariable(var_name);
                    auto type_it = variable_types.find(renamed_var);
                    if (type_it != variable_types.end() &&
                        type_it->second == Type::ARRAY) {
                        arg_types.push_back(Type::ARRAY);
                        continue;
                    }
                }
            }
            arg_types.push_back(analyzeExpr(arg.get()));
        }

        struct Candidate {
            const FunctionSignature* sig;
            int conversion_count;
            int first_conversion_index;
        };
        std::vector<Candidate> candidates;

        for (const auto& sig : overloads) {
            if (sig.params.size() != arg_types.size())
                continue;

            int conversion_count = 0;
            int first_conversion_index = -1;
            bool possible = true;
            for (size_t j = 0; j < arg_types.size(); ++j) {
                Type arg_type = arg_types[j];
                Type param_type = sig.params[j].type;
                if (arg_type != param_type) {
                    if (isConvertible(arg_type, param_type)) {
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
                    {&sig, conversion_count, first_conversion_index});
            }
        }

        if (candidates.empty()) {
            std::string arg_types_str;
            for (size_t i = 0; i < arg_types.size(); ++i) {
                arg_types_str += to_string(arg_types[i]);
                if (i < arg_types.size() - 1)
                    arg_types_str += ", ";
            }
            reportError(
                std::format(
                    "No matching user-defined function for call to '{}({})'",
                    expr.callee, arg_types_str),
                expr.line);
            return Type::VALUE;
        }

        // Check for ambiguous calls
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
            reportError(
                std::format("Ambiguous call to overloaded function '{}'",
                            expr.callee),
                expr.line);
        }

        return Type::VALUE;
    }

    const auto& builtins = get_builtin_functions();

    if (builtins.count(expr.callee)) {
        const auto& overloads = builtins.at(expr.callee);

        std::vector<std::optional<Type>> arg_types(expr.args.size());

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

                if (!arg_types[j].has_value()) {
                    arg_types[j] = analyzeExpr(expr.args[j].get());
                }
                Type arg_type = arg_types[j].value();

                if (arg_type != param_type) {
                    if (isConvertible(arg_type, param_type)) {
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
                if (builtinParamTypeIsEvaluatable(overloads, i)) {
                    if (!arg_types[i].has_value()) {
                        arg_types[i] = analyzeExpr(expr.args[i].get());
                    }
                    arg_types_str += to_string(arg_types[i].value());
                } else {
                    arg_types_str += "LiteralString";
                }

                if (i < expr.args.size() - 1)
                    arg_types_str += ", ";
            }
            reportError(
                std::format(
                    "No matching overload for function '{}({})' in {} mode.",
                    expr.callee, arg_types_str,
                    mode == Mode::Expr ? "Expr" : "SingleExpr"),
                expr.line);
            return Type::VALUE;
        }

        // Check for ambiguous calls
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
            reportError(
                std::format(
                    "Ambiguous call to overloaded built-in function '{}'",
                    expr.callee),
                expr.line);
        }

        return Type::VALUE;

    } else if (expr.callee.starts_with("nth_")) {
        std::string n_str = expr.callee.substr(4);
        if (n_str.empty() ||
            !std::all_of(n_str.begin(), n_str.end(), ::isdigit)) {
            reportError(
                std::format("Invalid nth_N function name '{}'", expr.callee),
                expr.line);
            return Type::VALUE;
        }
        int n = std::stoi(n_str);
        int arg_count = expr.args.size();
        if (arg_count < n) {
            reportError(std::format("Function '{}' requires at least {} "
                                    "arguments, but {} were provided.",
                                    expr.callee, n, arg_count),
                        expr.line);
        }
        if (n < 1) {
            reportError(
                std::format("Invalid nth_N function name '{}'", expr.callee),
                expr.line);
        }
        for (const auto& arg : expr.args) {
            auto arg_type = analyzeExpr(arg.get());
            if (!isConvertible(arg_type, Type::VALUE)) {
                reportError(
                    std::format("Argument to function '{}' has type '{}' which "
                                "is not convertible to Value.",
                                expr.callee, to_string(arg_type)),
                    arg->line());
            }
        }

        return Type::VALUE;
    } else {
        reportError(std::format("Unknown function '{}'", expr.callee),
                    expr.line);
        return Type::VALUE;
    }
}

Type SemanticAnalyzer::analyze(const PropAccessExpr& expr) {
    std::string clip_name = expr.clip.value;
    if (param_substitutions.count(clip_name)) {
        auto* subst_expr = param_substitutions.at(clip_name);
        auto res_type = analyzeExpr(subst_expr);
        if (res_type != Type::CLIP) {
            reportError(
                std::format("Parameter '{}' is used as a clip for property "
                            "access but was not a clip constant.",
                            expr.clip.value),
                expr.line);
        }
    } else if (clip_name.starts_with("$")) {
        clip_name = clip_name.substr(1);
        if (!isClipName(clip_name)) {
            reportError(
                std::format("Invalid clip identifier '{}' for property access.",
                            expr.clip.value),
                expr.line);
        }
    } else {
        // Check if it's a function parameter of Clip type
        std::string renamed = renameVariable(clip_name);
        auto type_it = variable_types.find(renamed);
        if (type_it == variable_types.end() || type_it->second != Type::CLIP) {
            reportError(
                std::format("Clip '{}' is used as a property access target but "
                            "was not a clip constant or Clip parameter.",
                            expr.clip.value),
                expr.line);
        }
    }

    return Type::VALUE;
}

Type SemanticAnalyzer::analyze(const StaticRelPixelAccessExpr& expr) {
    if (mode == Mode::Single) {
        reportError("Static relative pixel access (clip[x,y]) is only "
                    "available in Expr mode. "
                    "Use dyn(clip, x, y, plane) for absolute access in "
                    "SingleExpr mode.",
                    expr.line);
    }

    std::string clip_name = expr.clip.value;
    if (param_substitutions.count(clip_name)) {
        auto* subst_expr = param_substitutions.at(clip_name);
        auto res_type = analyzeExpr(subst_expr);
        if (res_type != Type::CLIP) {
            reportError(
                std::format("Parameter '{}' is used as a clip for pixel "
                            "access but was not a clip constant.",
                            expr.clip.value),
                expr.line);
        }
    } else if (clip_name.starts_with("$")) {
        clip_name = clip_name.substr(1);
        if (!isClipName(clip_name)) {
            reportError(
                std::format("Invalid clip identifier '{}' for pixel access.",
                            expr.clip.value),
                expr.line);
        }
    } else {
        // Check if it's a function parameter of Clip type
        std::string renamed = renameVariable(clip_name);
        auto type_it = variable_types.find(renamed);
        if (type_it == variable_types.end() || type_it->second != Type::CLIP) {
            reportError(
                std::format("Clip '{}' is used for pixel access but "
                            "was not a clip constant or Clip parameter.",
                            expr.clip.value),
                expr.line);
        }
    }

    return Type::VALUE;
}

Type SemanticAnalyzer::analyze(const FrameDimensionExpr& expr) {
    if (mode == Mode::Expr) {
        reportError("frame.width[N] and frame.height[N] are only "
                    "available in SingleExpr mode. "
                    "Use width and height directly in Expr mode.",
                    expr.line);
    }

    auto plane_type = analyzeExpr(expr.plane_index_expr.get());
    if (plane_type != Type::LITERAL) {
        reportError("Plane index must be a literal constant.",
                    expr.plane_index_expr->line());
    }

    return Type::VALUE;
}

Type SemanticAnalyzer::analyze(const ArrayAccessExpr& expr) {
    auto* var_expr = get_if<VariableExpr>(expr.array.get());
    if (!var_expr) {
        reportError("Array access requires a variable as the array.",
                    expr.line);
        return Type::VALUE;
    }

    std::string array_name = var_expr->name.value;
    std::string renamed_array = renameVariable(array_name);

    checkVariableDefined(array_name, expr.line);

    auto type_it = variable_types.find(renamed_array);
    if (type_it == variable_types.end() || type_it->second != Type::ARRAY) {
        reportError(std::format("Variable '{}' is not an array.", array_name),
                    expr.line);
    }

    auto index_type = analyzeExpr(expr.index.get());
    if (!isConvertible(index_type, Type::VALUE)) {
        reportError("Array index must be convertible to a value.",
                    expr.index->line());
    }

    return Type::VALUE;
}

void SemanticAnalyzer::analyze(const ExprStmt& stmt) {
    analyzeExpr(stmt.expr.get());
}

void SemanticAnalyzer::analyze(const AssignStmt& stmt) {
    if (stmt.name.value == "RESULT") {
        has_result = true;
        if (mode == Mode::Expr) {
            if (current_function == nullptr && scope_stack.empty()) {
                result_defined_in_global_scope = true;
            }
        }
    }

    // Check if this is a new() or resize() call for array allocation
    if (auto* call_expr = get_if<CallExpr>(stmt.value.get())) {
        if (call_expr->callee == "new" || call_expr->callee == "resize") {
            if (call_expr->args.size() != 1) {
                reportError(
                    std::format("{}() requires exactly 1 argument (size).",
                                call_expr->callee),
                    stmt.line);
            }

            std::string var_name = stmt.name.value;
            std::string renamed_var = renameVariable(var_name);

            auto type_it = variable_types.find(renamed_var);
            bool is_already_array = (type_it != variable_types.end() &&
                                     type_it->second == Type::ARRAY);

            if (call_expr->callee == "resize") {
                if (mode == Mode::Expr) {
                    reportError(
                        "resize() is only available in SingleExpr mode.",
                        stmt.line);
                }
                if (!is_already_array) {
                    reportError(
                        std::format(
                            "Variable '{}' is undefined or not an array. ",
                            var_name),
                        stmt.line);
                }
            } else { // new()
                if (is_already_array) {
                    reportError(
                        std::format("Cannot reallocate array '{}'.", var_name),
                        stmt.line);
                }
            }

            if (mode == Mode::Expr) {
                auto* num_expr = get_if<NumberExpr>(call_expr->args[0].get());
                if (!num_expr) {
                    reportError(
                        "In Expr mode, array size must be a numeric literal.",
                        stmt.line);
                }
            } else {
                auto size_type = analyzeExpr(call_expr->args[0].get());
                if (!isConvertible(size_type, Type::VALUE)) {
                    reportError("Array size must be convertible to a value.",
                                stmt.line);
                }
            }

            defineVariableInCurrentScope(renamed_var, Type::ARRAY);

            if (current_function == nullptr) {
                if (scope_stack.empty()) {
                    defined_globals.insert(var_name);
                }
            } else {
                local_scope_vars.insert(var_name);
            }
            return;
        }
    }

    auto value_type = analyzeExpr(stmt.value.get());

    if (value_type == Type::ARRAY) {
        reportError(std::format("Cannot assign array to variable '{}'.",
                                stmt.name.value),
                    stmt.line);
        return; // Don't continue after error
    }

    std::string var_name = stmt.name.value;
    std::string renamed_var = renameVariable(var_name);

    auto type_it = variable_types.find(renamed_var);
    if (type_it != variable_types.end() && type_it->second == Type::ARRAY) {
        reportError(
            std::format(
                "Variable '{}' is an array and cannot be reassigned to a "
                "non-array value.",
                var_name),
            stmt.line);
    }

    defineVariableInCurrentScope(renamed_var, Type::VALUE);

    if (current_function == nullptr) {
        if (scope_stack.empty()) {
            defined_globals.insert(var_name);
        }
    } else {
        local_scope_vars.insert(var_name);
    }
}

void SemanticAnalyzer::analyze(const ArrayAssignStmt& stmt) {
    auto* array_access = get_if<ArrayAccessExpr>(stmt.target.get());
    if (!array_access) {
        reportError("Array assignment target must be an array access.",
                    stmt.line);
        return;
    }

    auto* var_expr = get_if<VariableExpr>(array_access->array.get());
    if (!var_expr) {
        reportError("Array access requires a variable as the array.",
                    stmt.line);
        return;
    }

    std::string array_name = var_expr->name.value;
    std::string renamed_array = renameVariable(array_name);

    checkVariableDefined(array_name, stmt.line);

    auto type_it = variable_types.find(renamed_array);
    if (type_it == variable_types.end() || type_it->second != Type::ARRAY) {
        reportError(std::format("Variable '{}' is not an array.", array_name),
                    stmt.line);
    }

    auto value_type = analyzeExpr(stmt.value.get());
    if (!isConvertible(value_type, Type::VALUE)) {
        reportError("Array assignment value must be convertible to a value.",
                    stmt.value->line());
    }

    auto index_type = analyzeExpr(array_access->index.get());
    if (!isConvertible(index_type, Type::VALUE)) {
        reportError("Array index must be convertible to a value.",
                    array_access->index->line());
    }
}

void SemanticAnalyzer::analyze(const BlockStmt& stmt) {
    enterScope();

    for (const auto& s : stmt.statements) {
        analyzeStmt(s.get());
    }

    exitScope();
}

void SemanticAnalyzer::analyze(const IfStmt& stmt) {
    auto cond_type = analyzeExpr(stmt.condition.get());
    if (!isConvertible(cond_type, Type::VALUE)) {
        reportError(std::format("If condition has type '{}' which is not "
                                "convertible to a value.",
                                to_string(cond_type)),
                    stmt.condition->line());
    }

    analyzeStmt(stmt.then_branch.get());

    if (stmt.else_branch) {
        analyzeStmt(stmt.else_branch.get());
    }
}

void SemanticAnalyzer::analyze(const WhileStmt& stmt) {
    auto cond_type = analyzeExpr(stmt.condition.get());
    if (!isConvertible(cond_type, Type::VALUE)) {
        reportError(std::format("While condition has type '{}' which is not "
                                "convertible to a value.",
                                to_string(cond_type)),
                    stmt.condition->line());
    }

    analyzeStmt(stmt.body.get());
}

void SemanticAnalyzer::analyze(const ReturnStmt& stmt) {
    if (current_function == nullptr) {
        reportError("'return' statements are not allowed in the global scope.",
                    stmt.line);
    }
    if (stmt.value) {
        auto result_type = analyzeExpr(stmt.value.get());
        if (result_type == Type::ARRAY) {
            reportError("Functions cannot return arrays.", stmt.line);
        }
    }
}

void SemanticAnalyzer::analyze([[maybe_unused]] const LabelStmt& stmt) {
    // Labels are collected in the first pass
}

void SemanticAnalyzer::analyze(const GotoStmt& stmt) {
    if (current_function != nullptr) {
        // Inside a function
        if (global_labels.count(stmt.label.value)) {
            reportError(std::format("goto from function '{}' to global "
                                    "label '{}' is not allowed",
                                    current_function->name, stmt.label.value),
                        stmt.line);
        }
        if (current_function_labels.find(stmt.label.value) ==
            current_function_labels.end()) {
            reportError(
                std::format("goto target '{}' not found in function '{}'",
                            stmt.label.value, current_function->name),
                stmt.line);
        }
    } else {
        // Global scope
        if (global_labels.find(stmt.label.value) == global_labels.end()) {
            reportError(
                std::format("goto target '{}' not found in global scope",
                            stmt.label.value),
                stmt.line);
        }
    }

    if (stmt.condition) {
        analyzeExpr(stmt.condition.get());
    }
}

void SemanticAnalyzer::analyze(const FunctionDef& stmt) {
    // Function definitions are handled in the first pass
    // Here we validate the function body structure

    // Collect labels in function
    auto saved_current_function_labels = current_function_labels;
    current_function_labels.clear();
    std::function<void(Stmt*)> collect_labels = [&](Stmt* s) {
        if (!s)
            return;
        if (auto* label = get_if<LabelStmt>(s)) {
            if (current_function_labels.count(label->name.value)) {
                reportError(std::format("Duplicate label '{}' in function '{}'",
                                        label->name.value, stmt.name.value),
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
    for (const auto& s : stmt.body->statements) {
        collect_labels(s.get());
    }

    // Find the function signature
    const FunctionSignature* sig = nullptr;
    if (functions.count(stmt.name.value)) {
        for (const auto& s : functions.at(stmt.name.value)) {
            if (s.params.size() == stmt.params.size()) {
                bool match = true;
                for (size_t i = 0; i < s.params.size(); ++i) {
                    if (s.params[i].type != stmt.params[i].type) {
                        match = false;
                        break;
                    }
                }
                if (match) {
                    sig = &s;
                    break;
                }
            }
        }
    }

    if (!sig) {
        reportError(std::format("Could not find signature for function '{}'",
                                stmt.name.value),
                    stmt.line);
        current_function_labels = saved_current_function_labels;
        return;
    }

    // Check return statement placement
    const auto& stmts = stmt.body->statements;
    std::function<void(Stmt*, int)> check_returns = [&](Stmt* s, int depth) {
        if (!s)
            return;
        if (get_if<ReturnStmt>(s)) {
            if (depth > 0) {
                reportError(
                    "'return' is not allowed inside blocks within a function.",
                    s->line());
            }
        } else if (auto* block = get_if<BlockStmt>(s)) {
            for (const auto& inner_s : block->statements) {
                check_returns(inner_s.get(), depth + 1);
            }
        } else if (auto* if_s = get_if<IfStmt>(s)) {
            check_returns(if_s->then_branch.get(), depth + 1);
            if (if_s->else_branch) {
                check_returns(if_s->else_branch.get(), depth + 1);
            }
        } else if (auto* while_s = get_if<WhileStmt>(s)) {
            check_returns(while_s->body.get(), depth + 1);
        }
    };

    for (size_t i = 0; i < stmts.size(); ++i) {
        const auto& stmt_ptr = stmts[i];
        if (get_if<ReturnStmt>(stmt_ptr.get())) {
            if (i != stmts.size() - 1) {
                reportError(
                    "'return' must be the last statement in a function body.",
                    stmt_ptr->line());
            }
        } else {
            check_returns(stmt_ptr.get(), 0);
        }
    }

    // Analyze function body with proper scope
    auto saved_current_function = current_function;
    current_function = sig;

    auto saved_local_vars = local_scope_vars;
    auto saved_all_defined = all_defined_vars_in_scope;
    auto saved_scope_stack = scope_stack;
    auto saved_defined_globals = defined_globals;

    // Set up function scope
    local_scope_vars.clear();
    all_defined_vars_in_scope.clear();
    scope_stack.clear();

    // Add globals accessible in this function
    std::set<std::string> effective_globals;
    if (sig->global_mode == GlobalMode::ALL) {
        effective_globals = saved_defined_globals;
    } else if (sig->global_mode == GlobalMode::SPECIFIC) {
        effective_globals = sig->specific_globals;
    }

    for (const auto& g : effective_globals) {
        all_defined_vars_in_scope.insert(g);
    }

    // Add parameters
    for (const auto& param : stmt.params) {
        std::string param_name = param.name.value;
        all_defined_vars_in_scope.insert(param_name);
        local_scope_vars.insert(param_name);
        variable_types[param_name] = param.type;
    }

    // Analyze function body
    analyze(*stmt.body);

    // Restore scope
    current_function = saved_current_function;
    local_scope_vars = saved_local_vars;
    all_defined_vars_in_scope = saved_all_defined;
    scope_stack = saved_scope_stack;
    defined_globals = saved_defined_globals;
    current_function_labels = saved_current_function_labels;
}

void SemanticAnalyzer::analyze([[maybe_unused]] const GlobalDecl& stmt) {
    // Global declarations are handled by the parser
}

void SemanticAnalyzer::checkVariableDefined(const std::string& var_name,
                                            int line) {
    if (var_name == "frame") {
        return;
    }

    std::string actual_name = renameVariable(var_name);

    if (all_defined_vars_in_scope.count(actual_name) ||
        all_defined_vars_in_scope.count(var_name)) {
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
            reportError(
                std::format(
                    "Variable '{}' is a global variable but is not accessible "
                    "in function '{}'. Use <global<{}>> or <global.all> before "
                    "the function definition to grant access.",
                    var_name, current_function->name, var_name),
                line);
            return;
        }
        return;
    }

    reportError(
        std::format("Variable '{}' is used before being defined", var_name),
        line);
}

void SemanticAnalyzer::enterScope() {
    scope_stack.push_back(std::set<std::string>());
}

void SemanticAnalyzer::exitScope() {
    if (!scope_stack.empty()) {
        const auto& scope_vars = scope_stack.back();
        for (const auto& var : scope_vars) {
            all_defined_vars_in_scope.erase(var);
            variable_types.erase(var);
        }
        scope_stack.pop_back();
    }
}

void SemanticAnalyzer::defineVariableInCurrentScope(const std::string& var_name,
                                                    Type type) {
    if (all_defined_vars_in_scope.find(var_name) ==
        all_defined_vars_in_scope.end()) {
        if (!scope_stack.empty()) {
            scope_stack.back().insert(var_name);
        }
    }
    all_defined_vars_in_scope.insert(var_name);
    variable_types[var_name] = type;
}

std::string SemanticAnalyzer::renameVariable(const std::string& var_name) {
    if (var_rename_map.count(var_name)) {
        return var_rename_map[var_name];
    }
    return var_name;
}

bool SemanticAnalyzer::isClipName(const std::string& s) {
    return is_clip_postfix_internal(s);
}

bool SemanticAnalyzer::isConvertible(Type from, Type to) {
    return is_convertible_internal(from, to);
}

bool SemanticAnalyzer::builtinParamTypeIsEvaluatable(
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
