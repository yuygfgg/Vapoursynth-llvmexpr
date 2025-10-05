#include "CodeGenerator.hpp"
#include "PostfixHelper.hpp"
#include <format>
#include <functional>

namespace infix2postfix {

namespace {
bool is_clip_postfix(const std::string& s) {
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

bool is_constant_infix_internal(const std::string& s) {
    static const std::set<std::string> constants = {"$pi", "$N",     "$X",
                                                    "$Y",  "$width", "$height"};
    return constants.count(s);
}
} // namespace

CodeGenerator::CodeGenerator(Mode mode, int num_inputs)
    : mode(mode), num_inputs(num_inputs) {}

std::string CodeGenerator::generate(Program* program) {
    std::string result;
    for (const auto& stmt : program->statements) {
        if (auto* func_def = dynamic_cast<FunctionDef*>(stmt.get())) {
            FunctionSignature sig;
            sig.name = func_def->name.value;
            for (const auto& p : func_def->params)
                sig.params.push_back(p.value);
            sig.line = func_def->line;
            sig.has_return = false;
            for (const auto& s : func_def->body->statements) {
                if (dynamic_cast<ReturnStmt*>(s.get()))
                    sig.has_return = true;
            }
            if (func_def->global_decl) {
                sig.global_mode = func_def->global_decl->mode;
                for (const auto& g : func_def->global_decl->globals) {
                    sig.specific_globals.insert(g.value);
                }
            }
            functions[sig.name] = sig;
            function_defs[sig.name] = func_def;
        }
    }

    for (const auto& stmt : program->statements) {
        result += generate(stmt.get()) + " ";
    }

    if (mode == Mode::Expr && !has_result) {
        throw CodeGenError(
            "Final result must be assigned to variable 'RESULT'!", 0);
    }

    std::string final_code;
    final_code.reserve(result.size());
    bool last_was_space = false;
    for (char c : result) {
        if (c == ' ' || c == '\n') {
            if (!last_was_space)
                final_code += ' ';
            last_was_space = true;
        } else {
            final_code += c;
            last_was_space = false;
        }
    }

    if (mode == Mode::Expr) {
        final_code += "RESULT@";
    }

    return final_code;
}

std::string CodeGenerator::generate(Node* node) {
    if (auto* e = dynamic_cast<Expr*>(node))
        return e->accept(*this);
    if (auto* s = dynamic_cast<Stmt*>(node))
        return s->accept(*this);
    return "";
}

std::string CodeGenerator::visit(NumberExpr& expr) { return expr.value.value; }

std::string CodeGenerator::visit(VariableExpr& expr) {
    std::string name = expr.name.value;

    if (name.starts_with("$")) {
        std::string base_name = name.substr(1);

        if ((base_name == "X" || base_name == "Y") && mode == Mode::Single) {
            throw CodeGenError(
                "X and Y coordinates are only available in Expr mode. "
                "SingleExpr processes the entire frame at once, not "
                "pixel-by-pixel.",
                expr.line);
        }

        return base_name;
    }

    check_variable_defined(name, expr.line);

    std::string renamed = rename_variable(name);

    if (literals_in_scope.count(renamed)) {
        return renamed;
    }

    return std::format("{}@", renamed);
}

std::string CodeGenerator::visit(UnaryExpr& expr) {
    if (expr.op.type == TokenType::Minus) {
        if (auto* num = dynamic_cast<NumberExpr*>(expr.right.get())) {
            std::string val = num->value.value;
            if (!val.starts_with("-")) {
                val = std::format("-{}", val);
            }
            return val;
        }
    }

    return std::format("{} {}", generate(expr.right.get()),
                       get_unary_op_postfix(expr.op.type));
}

std::string CodeGenerator::visit(BinaryExpr& expr) {
    return std::format("{} {} {}", generate(expr.left.get()),
                       generate(expr.right.get()),
                       get_op_postfix(expr.op.type));
}

std::string CodeGenerator::visit(TernaryExpr& expr) {
    return std::format("{} {} {} ?", generate(expr.cond.get()),
                       generate(expr.true_expr.get()),
                       generate(expr.false_expr.get()));
}

std::string CodeGenerator::visit(CallExpr& expr) {
    if (functions.count(expr.callee)) {
        return inline_function_call(expr.callee, expr.args, expr.line);
    }

    if (expr.callee == "set_prop") {
        if (mode == Mode::Expr) {
            throw CodeGenError(
                "set_prop() is only available in SingleExpr mode. "
                "Use it to write frame properties in per-frame processing.",
                expr.line);
        }
        if (expr.args.size() != 2)
            throw CodeGenError("set_prop() requires 2 arguments: "
                               "set_prop(property_name, value)",
                               expr.line);

        auto prop_name = dynamic_cast<VariableExpr*>(expr.args[0].get());
        if (!prop_name)
            throw CodeGenError("First argument to set_prop() must be a "
                               "property name identifier",
                               expr.line);

        std::string name = prop_name->name.value;
        std::string value_code = generate(expr.args[1].get());
        return std::format("{} {}$", value_code, name);
    }

    std::string result;
    for (const auto& arg : expr.args) {
        result += generate(arg.get()) + " ";
    }

    if (expr.callee == "dyn") {
        // Expr mode: dyn(clip, x, y)
        // SingleExpr mode: dyn(clip, x, y, plane)
        auto get_clip_name = [](VariableExpr* clip, int line) -> std::string {
            if (!clip)
                throw CodeGenError(
                    "First argument to dyn() must be a clip identifier", line);
            std::string name = clip->name.value;
            if (!name.starts_with("$"))
                throw CodeGenError("First argument to dyn() must be a clip "
                                   "(use $ prefix, e.g., $x, $y, $src0)",
                                   line);
            name = name.substr(1); // Remove $
            if (!is_clip_postfix(name))
                throw CodeGenError(
                    std::format("Invalid clip identifier: ${}", name), line);
            return name;
        };

        if (mode == Mode::Expr) {
            if (expr.args.size() != 3)
                throw CodeGenError(
                    "dyn() requires 3 arguments in Expr mode: dyn(clip, x, y)",
                    expr.line);
            auto clip = dynamic_cast<VariableExpr*>(expr.args[0].get());
            std::string clip_name = get_clip_name(clip, expr.line);
            return std::format("{} {} {}[]{}", generate(expr.args[1].get()),
                               generate(expr.args[2].get()), clip_name,
                               expr.boundary_suffix);
        } else {
            if (expr.args.size() != 4)
                throw CodeGenError("dyn() requires 4 arguments in SingleExpr "
                                   "mode: dyn(clip, x, y, plane)",
                                   expr.line);
            auto clip = dynamic_cast<VariableExpr*>(expr.args[0].get());
            std::string clip_name = get_clip_name(clip, expr.line);
            auto plane = dynamic_cast<NumberExpr*>(expr.args[3].get());
            if (!plane)
                throw CodeGenError("Fourth argument to dyn() (plane) must be "
                                   "an integer constant",
                                   expr.line);
            return std::format("{} {} {}^{}[]", generate(expr.args[1].get()),
                               generate(expr.args[2].get()), clip_name,
                               plane->value.value);
        }
    }
    if (expr.callee == "store") {
        // Expr mode: store(x, y, value)
        // SingleExpr mode: store(x, y, plane, value)
        if (mode == Mode::Expr) {
            if (expr.args.size() != 3)
                throw CodeGenError("store() requires 3 arguments in Expr mode: "
                                   "store(x, y, value)",
                                   expr.line);
            return std::format("{} {} {} @[]", generate(expr.args[2].get()),
                               generate(expr.args[0].get()),
                               generate(expr.args[1].get()));
        } else {
            if (expr.args.size() != 4)
                throw CodeGenError("store() requires 4 arguments in SingleExpr "
                                   "mode: store(x, y, plane, value)",
                                   expr.line);
            auto plane = dynamic_cast<NumberExpr*>(expr.args[2].get());
            if (!plane)
                throw CodeGenError("Third argument to store() (plane) must be "
                                   "an integer constant",
                                   expr.line);
            return std::format("{} {} {} @[]^{}", generate(expr.args[3].get()),
                               generate(expr.args[0].get()),
                               generate(expr.args[1].get()),
                               plane->value.value);
        }
    }
    if (expr.callee == "exit") {
        if (mode == Mode::Single) {
            throw CodeGenError(
                "exit() is only available in Expr mode. "
                "In SingleExpr mode, the stack must be empty at the end.",
                expr.line);
        }
        if (!expr.args.empty())
            throw CodeGenError("exit() takes no arguments", expr.line);
        return "^exit^";
    }

    result += expr.callee;
    return result;
}

std::string CodeGenerator::visit(PropAccessExpr& expr) {
    return std::format("{}.{}", expr.clip.value, expr.prop.value);
}

std::string CodeGenerator::visit(StaticRelPixelAccessExpr& expr) {
    if (mode == Mode::Single) {
        throw CodeGenError("Static relative pixel access (clip[x,y]) is only "
                           "available in Expr mode. "
                           "Use dyn(clip, x, y, plane) for absolute access in "
                           "SingleExpr mode.",
                           expr.line);
    }
    std::string clip_name = expr.clip.value;
    if (clip_name.starts_with("$")) {
        clip_name = clip_name.substr(1);
    }
    return std::format("{}[{},{}]{}", clip_name, expr.offsetX.value,
                       expr.offsetY.value, expr.boundary_suffix);
}

std::string CodeGenerator::visit(FrameDimensionExpr& expr) {
    if (mode == Mode::Expr) {
        throw CodeGenError("frame.width[N] and frame.height[N] are only "
                           "available in SingleExpr mode. "
                           "Use width and height directly in Expr mode.",
                           expr.line);
    }
    try {
        std::stoi(expr.plane_index.value);
    } catch (...) {
        throw CodeGenError(
            std::format("Plane index must be an integer constant, got: {}",
                        expr.plane_index.value),
            expr.line);
    }
    return std::format("{}^{}", expr.dimension_name, expr.plane_index.value);
}

std::string CodeGenerator::visit(ExprStmt& stmt) {
    std::string s = generate(stmt.expr.get());
    check_stack_effect(s, 0, stmt.line);
    return s;
}

std::string CodeGenerator::visit(AssignStmt& stmt) {
    if (stmt.name.value == "RESULT")
        has_result = true;

    std::string value_code = generate(stmt.value.get());

    std::string var_name = stmt.name.value;
    std::string renamed_var = rename_variable(var_name);

    std::string s = std::format("{} {}!", value_code, renamed_var);
    check_stack_effect(s, 0, stmt.line);

    define_variable_in_current_scope(renamed_var);

    if (current_function == nullptr) {
        defined_globals.insert(var_name);
    } else {
        local_scope_vars.insert(var_name);
    }
    return s;
}

std::string CodeGenerator::visit(BlockStmt& stmt) {
    enter_scope();

    std::string result;
    for (const auto& s : stmt.statements) {
        result += generate(s.get()) + " ";
    }

    exit_scope();

    return result;
}

std::string CodeGenerator::visit(IfStmt& stmt) {
    std::string else_label = std::format("__internal_else_{}", label_counter++);
    std::string endif_label =
        std::format("__internal_endif_{}", label_counter++);

    // Generate condition first before entering any new scope
    std::string condition_code = generate(stmt.condition.get());

    std::string result = std::format("{} 0 = {}# ", condition_code, else_label);

    // Then branch creates its own scope
    result += generate(stmt.then_branch.get()) + " ";

    if (stmt.else_branch) {
        result += std::format("1 {}# #{} ", endif_label, else_label);
        // Else branch creates its own scope
        result += generate(stmt.else_branch.get()) + " ";
    }
    result += "#" + (stmt.else_branch ? endif_label : else_label);
    return result;
}

std::string CodeGenerator::visit(WhileStmt& stmt) {
    std::string start_label =
        std::format("__internal_while_start_{}", label_counter++);
    std::string end_label =
        std::format("__internal_while_end_{}", label_counter++);
    std::string result = std::format("#{} ", start_label);
    result +=
        std::format("{} not {}# ", generate(stmt.condition.get()), end_label);
    result += generate(stmt.body.get()) + " ";
    result += std::format("1 {}# #{}", start_label, end_label);
    return result;
}

std::string CodeGenerator::visit(ReturnStmt& stmt) {
    if (stmt.value) {
        return generate(stmt.value.get());
    }
    return "";
}

std::string CodeGenerator::visit(LabelStmt& stmt) {
    return std::format("#{}", stmt.name.value);
}

std::string CodeGenerator::visit(GotoStmt& stmt) {
    if (stmt.condition) {
        return std::format("{} {}#", generate(stmt.condition.get()),
                           stmt.label.value);
    }
    return std::format("1 {}#", stmt.label.value);
}

std::string CodeGenerator::visit([[maybe_unused]] FunctionDef& stmt) {
    // Handled in first pass
    return "";
}
std::string CodeGenerator::visit([[maybe_unused]] GlobalDecl& stmt) {
    // Handled by parser
    return "";
}

std::string CodeGenerator::get_op_postfix(TokenType type) {
    switch (type) {
    case TokenType::Plus:
        return "+";
    case TokenType::Minus:
        return "-";
    case TokenType::Star:
        return "*";
    case TokenType::Slash:
        return "/";
    case TokenType::Percent:
        return "%";
    case TokenType::StarStar:
        return "pow";
    case TokenType::LogicalAnd:
        return "and";
    case TokenType::LogicalOr:
        return "or";
    case TokenType::BitAnd:
        return "bitand";
    case TokenType::BitOr:
        return "bitor";
    case TokenType::BitXor:
        return "bitxor";
    case TokenType::Eq:
        return "=";
    case TokenType::Ne:
        return "= not";
    case TokenType::Lt:
        return "<";
    case TokenType::Le:
        return "<=";
    case TokenType::Gt:
        return ">";
    case TokenType::Ge:
        return ">=";
    case TokenType::Not:
        return "not";
    case TokenType::BitNot:
        return "bitnot";
    default:
        return "";
    }
}

std::string CodeGenerator::get_unary_op_postfix(TokenType type) {
    switch (type) {
    case TokenType::Minus:
        return "neg";
    case TokenType::Not:
        return "not";
    case TokenType::BitNot:
        return "bitnot";
    default:
        return get_op_postfix(type);
    }
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

std::string CodeGenerator::inline_function_call(
    const std::string& func_name,
    const std::vector<std::unique_ptr<Expr>>& args, int call_line) {

    if (!function_defs.count(func_name)) {
        throw CodeGenError(
            std::format("Function '{}' not found in definitions", func_name),
            call_line);
    }

    const auto& sig = functions[func_name];
    FunctionDef* func_def = function_defs[func_name];

    if (args.size() != sig.params.size()) {
        throw CodeGenError(
            std::format(
                "Function '{}' requires {} parameters, but {} were provided",
                func_name, sig.params.size(), args.size()),
            call_line);
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
    const FunctionSignature* saved_current_function = current_function;

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

    std::string param_assignments;
    std::set<std::string> new_literals;

    for (size_t i = 0; i < sig.params.size(); ++i) {
        const std::string& param_name = sig.params[i];
        std::string renamed_param =
            std::format("__internal_func_{}_{}", func_name, param_name);

        // Restore scope temporarily to generate argument value
        auto temp_all_defined = all_defined_vars_in_scope;
        auto temp_scope_stack = scope_stack;
        all_defined_vars_in_scope = saved_all_defined;
        scope_stack = saved_scope_stack;

        // Generate argument value
        std::string arg_value = generate(args[i].get());

        // Restore function scope
        all_defined_vars_in_scope = temp_all_defined;
        scope_stack = temp_scope_stack;

        if (!effective_globals.count(param_name)) {
            param_assignments +=
                std::format("{} {}! ", arg_value, renamed_param);
            param_map[param_name] = renamed_param;
            new_local_vars.insert(param_name);
            // Define parameter in function scope
            all_defined_vars_in_scope.insert(renamed_param);
        } else {
            param_map[param_name] = param_name;
        }
    }

    // Collect local variables from function body
    std::function<void(Stmt*)> collect_locals = [&](Stmt* stmt) {
        if (auto* assign = dynamic_cast<AssignStmt*>(stmt)) {
            std::string var_name = assign->name.value;
            if (!effective_globals.count(var_name) &&
                var_name.find("__internal_func_") != 0) {
                std::string renamed_var =
                    std::format("__internal_func_{}_{}", func_name, var_name);
                param_map[var_name] = renamed_var;
                new_local_vars.insert(var_name);
            }
        } else if (auto* block = dynamic_cast<BlockStmt*>(stmt)) {
            for (const auto& s : block->statements) {
                collect_locals(s.get());
            }
        } else if (auto* if_stmt = dynamic_cast<IfStmt*>(stmt)) {
            collect_locals(if_stmt->then_branch.get());
            if (if_stmt->else_branch) {
                collect_locals(if_stmt->else_branch.get());
            }
        } else if (auto* while_stmt = dynamic_cast<WhileStmt*>(stmt)) {
            collect_locals(while_stmt->body.get());
        }
    };

    collect_locals(func_def->body.get());

    // Update context for function body generation
    var_rename_map = param_map;
    literals_in_scope = new_literals;
    local_scope_vars = new_local_vars;
    local_scope_vars.insert(effective_globals.begin(), effective_globals.end());
    current_function = &sig;

    std::string label_prefix = std::format("__internal_{}_", func_name);

    std::string body_code = generate(func_def->body.get());

    // Add label prefix to all labels and gotos in the body
    size_t pos = 0;
    std::string result = body_code;
    while ((pos = result.find('#', pos)) != std::string::npos) {
        // Check if it's a label definition (#label) or goto (label#)
        if (pos + 1 < result.length()) {
            size_t end_pos = pos + 1;
            // Find the label name
            while (end_pos < result.length() &&
                   (std::isalnum(result[end_pos]) || result[end_pos] == '_')) {
                end_pos++;
            }

            if (end_pos > pos + 1) {
                std::string label = result.substr(pos + 1, end_pos - pos - 1);
                // Skip internal labels that are already prefixed
                if (label.find("__internal_") != 0) {
                    result.insert(pos + 1, label_prefix);
                    pos = end_pos + label_prefix.length();
                } else {
                    pos = end_pos;
                }
            } else {
                pos++;
            }
        } else {
            pos++;
        }
    }

    var_rename_map = saved_rename_map;
    literals_in_scope = saved_literals;
    local_scope_vars = saved_local_vars;
    all_defined_vars_in_scope = saved_all_defined;
    scope_stack = saved_scope_stack;
    current_function = saved_current_function;

    std::string inlined_code = param_assignments + result;

    int expected_effect = sig.has_return ? 1 : 0;
    try {
        int actual_effect = compute_stack_effect(inlined_code, call_line);
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

    return inlined_code;
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

} // namespace infix2postfix
