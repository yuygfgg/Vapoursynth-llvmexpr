#include "SemanticAnalyzer.hpp"
#include "../Tokenizer.hpp"
#include "Builtins.hpp"
#include <algorithm>
#include <cstddef>
#include <format>
#include <functional>

namespace infix2postfix {

namespace {
int get_clip_index(const std::string& s) {
    if (s.length() == 1 && s[0] >= 'a' && s[0] <= 'z')
        return parse_std_clip_idx(s[0]);
    if (s.rfind("src", 0) == 0) {
        for (size_t i = 3; i < s.length(); ++i) {
            if (!std::isdigit(s[i]))
                return -1;
        }
        return std::stoi(s.substr(3));
    }
    return -1;
}

} // namespace

SemanticAnalyzer::SemanticAnalyzer(Mode mode, int num_inputs)
    : mode(mode), num_inputs(num_inputs),
      global_scope(std::make_unique<SymbolTable>()),
      current_scope(global_scope.get()) {}

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

    // Collect function signatures and build function symbols
    for (const auto& stmt : program->statements) {
        if (auto* func_def = get_if<FunctionDef>(stmt.get())) {
            if (function_signatures.count(func_def->name.value)) {
                for (const auto& existing_sig :
                     function_signatures.at(func_def->name.value)) {
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
            std::optional<bool> returns_value_opt;

            std::function<void(Stmt*)> find_returns = [&](Stmt* s) {
                if (!s)
                    return;

                if (auto* ret = get_if<ReturnStmt>(s)) {
                    sig.has_return = true;
                    bool current_has_value = (ret->value != nullptr);
                    if (!returns_value_opt.has_value()) {
                        returns_value_opt = current_has_value;
                    } else if (returns_value_opt.value() != current_has_value) {
                        reportError(
                            std::format(
                                "Function '{}' has inconsistent return "
                                "statements (some with values, some without).",
                                func_def->name.value),
                            ret->line);
                    }
                } else if (auto* block = get_if<BlockStmt>(s)) {
                    for (const auto& inner_s : block->statements)
                        find_returns(inner_s.get());
                } else if (auto* if_s = get_if<IfStmt>(s)) {
                    find_returns(if_s->then_branch.get());
                    if (if_s->else_branch)
                        find_returns(if_s->else_branch.get());
                } else if (auto* while_s = get_if<WhileStmt>(s)) {
                    find_returns(while_s->body.get());
                }
            };
            for (const auto& s : func_def->body->statements) {
                find_returns(s.get());
            }
            sig.returns_value = returns_value_opt.value_or(false);

            if (func_def->global_decl) {
                sig.global_mode = func_def->global_decl->mode;
                for (const auto& g : func_def->global_decl->globals) {
                    sig.specific_globals.insert(g.value);
                }
            }

            std::set<std::string> local_vars;
            for (const auto& p : func_def->params) {
                local_vars.insert(p.name.value);
            }

            std::function<void(Stmt*)> collect_local_defs = [&](Stmt* s) {
                if (!s)
                    return;
                if (auto* assign = get_if<AssignStmt>(s)) {
                    local_vars.insert(assign->name.value);
                } else if (auto* block = get_if<BlockStmt>(s)) {
                    for (const auto& stmt : block->statements) {
                        collect_local_defs(stmt.get());
                    }
                } else if (auto* if_stmt = get_if<IfStmt>(s)) {
                    collect_local_defs(if_stmt->then_branch.get());
                    if (if_stmt->else_branch) {
                        collect_local_defs(if_stmt->else_branch.get());
                    }
                } else if (auto* while_stmt = get_if<WhileStmt>(s)) {
                    collect_local_defs(while_stmt->body.get());
                }
            };
            for (const auto& s : func_def->body->statements) {
                collect_local_defs(s.get());
            }

            for (const auto& s : func_def->body->statements) {
                collectUsedGlobalsInStmt(s.get(), sig.used_globals);
            }

            for (const auto& local_var : local_vars) {
                sig.used_globals.erase(local_var);
            }

            function_signatures[sig.name].push_back(sig);
            function_defs[sig.name].push_back(func_def);
        }
    }

    // Analyze all statements
    for (const auto& stmt : program->statements) {
        analyzeStmt(stmt.get());

        validateGlobalDependencies(stmt.get());

        if (auto* assign = get_if<AssignStmt>(stmt.get())) {
            if (current_scope == global_scope.get()) {
                defined_global_vars.insert(assign->name.value);
            }
        }
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

// Symbol table management
void SemanticAnalyzer::enterScope() {
    auto new_scope = std::make_unique<SymbolTable>(current_scope);
    current_scope = new_scope.get();
    scope_stack.push_back(std::move(new_scope));
}

void SemanticAnalyzer::exitScope() {
    if (!scope_stack.empty()) {
        current_scope = current_scope->get_parent();
        scope_stack.pop_back();
    }
}

std::shared_ptr<Symbol> SemanticAnalyzer::defineSymbol(SymbolKind kind,
                                                       const std::string& name,
                                                       Type type, int line) {
    auto symbol = std::make_shared<Symbol>();
    symbol->kind = kind;
    symbol->name = name;
    symbol->type = type;
    symbol->definition_line = line;

    if (!current_scope->define(symbol)) {
        return current_scope->resolve(name);
    }

    return symbol;
}

std::shared_ptr<Symbol> SemanticAnalyzer::resolveSymbol(const std::string& name,
                                                        int line) {
    auto symbol = current_scope->resolve(name);
    if (!symbol) {
        reportError(
            std::format("Variable '{}' is used before being defined", name),
            line);
    }
    return symbol;
}

// Expression analysis
Type SemanticAnalyzer::analyzeExpr(Expr* expr) {
    if (!expr)
        return Type::VALUE;
    return std::visit([this](auto& e) -> Type { return this->analyze(e); },
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

Type SemanticAnalyzer::analyze(VariableExpr& expr) {
    std::string name = expr.name.value;

    if (name.starts_with("$")) {
        std::string base_name = name.substr(1);

        if (base_name == "X" || base_name == "Y") {
            if (mode == Mode::Single) {
                reportError(
                    "X and Y coordinates are only available in Expr mode. "
                    "SingleExpr processes the entire frame at once, not "
                    "pixel-by-pixel.",
                    expr.line);
            }
            return Type::VALUE;
        } else if (get_clip_index(base_name) != -1) {
            if (get_clip_index(base_name) > num_inputs - 1) {
                reportError(
                    std::format("Clip index '{}' is out of range", base_name),
                    expr.line);
            }
            return Type::CLIP;
        } else if (base_name == "width" || base_name == "height" ||
                   base_name == "pi" || base_name == "N") {
            return Type::VALUE;
        }
        reportError(std::format("Invalid identifier '{}'.", base_name),
                    expr.line);
        return Type::VALUE;
    }

    auto symbol = current_scope->resolve(name);

    if (!symbol && current_function) {
        if (current_function->global_mode == GlobalMode::ALL) {
            symbol = global_scope->resolve(name);
            // Global declaration is forward declared
            if (!symbol) {
                symbol = std::make_shared<Symbol>();
                symbol->kind = SymbolKind::VARIABLE;
                symbol->name = name;
                symbol->type = Type::VALUE;
                symbol->definition_line = expr.line;
            }
        } else if (current_function->global_mode == GlobalMode::SPECIFIC) {
            // Global declaration is forward declared
            if (current_function->specific_globals.count(name)) {
                symbol = global_scope->resolve(name);
                if (!symbol) {
                    symbol = std::make_shared<Symbol>();
                    symbol->kind = SymbolKind::VARIABLE;
                    symbol->name = name;
                    symbol->type = Type::VALUE;
                    symbol->definition_line = expr.line;
                }
            }
        }
    }

    if (!symbol) {
        reportError(
            std::format("Variable '{}' is used before being defined", name),
            expr.line);
        return Type::VALUE;
    }

    expr.symbol = symbol;
    return symbol->type;
}

Type SemanticAnalyzer::analyze(UnaryExpr& expr) {
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

Type SemanticAnalyzer::analyze(BinaryExpr& expr) {
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

Type SemanticAnalyzer::analyze(TernaryExpr& expr) {
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

Type SemanticAnalyzer::analyze(CallExpr& expr) {
    auto signature = resolveOverload(expr.callee, expr.args, expr.line, &expr);
    expr.resolved_signature = signature;

    return Type::VALUE;
}

const FunctionSignature* SemanticAnalyzer::resolveOverload(
    const std::string& name, const std::vector<std::unique_ptr<Expr>>& args,
    int line, CallExpr* call_expr) {
    // User-defined functions
    if (function_signatures.count(name)) {
        const auto& overloads = function_signatures.at(name);

        std::vector<Type> arg_types;
        arg_types.reserve(args.size());
        for (const auto& arg : args) {
            if (auto* var_expr = get_if<VariableExpr>(arg.get())) {
                auto symbol = current_scope->resolve(var_expr->name.value);
                if (symbol && symbol->type == Type::ARRAY) {
                    arg_types.push_back(Type::ARRAY);
                    continue;
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
                    name, arg_types_str),
                line);
            return nullptr;
        }

        // Select best candidate
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
                std::format("Ambiguous call to overloaded function '{}'", name),
                line);
        }

        // Find and set the resolved FunctionDef
        if (call_expr && function_defs.count(name)) {
            const auto& defs = function_defs.at(name);
            for (auto* def : defs) {
                if (def->params.size() == best_candidate->sig->params.size()) {
                    bool match = true;
                    for (size_t i = 0; i < def->params.size(); ++i) {
                        if (def->params[i].type !=
                            best_candidate->sig->params[i].type) {
                            match = false;
                            break;
                        }
                    }
                    if (match) {
                        call_expr->resolved_def = def;
                        break;
                    }
                }
            }
        }

        return best_candidate->sig;
    }

    const auto& builtins = get_builtin_functions();

    if (builtins.count(name)) {
        const auto& overloads = builtins.at(name);

        std::vector<std::optional<Type>> arg_types(args.size());

        struct Candidate {
            const BuiltinFunction* builtin;
            int conversion_count;
            int first_conversion_index;
        };
        std::vector<Candidate> candidates;

        for (const auto& builtin : overloads) {
            if (builtin.arity != (int)args.size())
                continue;
            if (builtin.mode_restriction.has_value() &&
                builtin.mode_restriction.value() != mode)
                continue;

            int conversion_count = 0;
            int first_conversion_index = -1;
            bool possible = true;
            for (size_t j = 0; j < args.size(); ++j) {
                Type param_type = builtin.param_types[j];

                if (param_type == Type::LITERAL_STRING) {
                    auto* var_expr = get_if<VariableExpr>(args[j].get());
                    if (!var_expr || var_expr->name.value.starts_with("$")) {
                        possible = false;
                        break;
                    }
                    continue;
                }

                if (!arg_types[j].has_value()) {
                    arg_types[j] = analyzeExpr(args[j].get());
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
            for (size_t i = 0; i < args.size(); ++i) {
                if (builtinParamTypeIsEvaluatable(overloads, i)) {
                    if (!arg_types[i].has_value()) {
                        arg_types[i] = analyzeExpr(args[i].get());
                    }
                    arg_types_str += to_string(arg_types[i].value());
                } else {
                    arg_types_str += "LiteralString";
                }

                if (i < args.size() - 1)
                    arg_types_str += ", ";
            }
            reportError(
                std::format(
                    "No matching overload for function '{}({})' in {} mode.",
                    name, arg_types_str,
                    mode == Mode::Expr ? "Expr" : "SingleExpr"),
                line);
            return nullptr;
        }

        // Select best candidate
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
                    name),
                line);
        }

        // Built-in functions don't have FunctionSignature, return nullptr
        // CodeGenerator will check for built-ins separately
        return nullptr;

    } else if (name.starts_with("nth_")) {
        std::string n_str = name.substr(4);
        if (n_str.empty() ||
            !std::all_of(n_str.begin(), n_str.end(), ::isdigit)) {
            reportError(std::format("Invalid nth_N function name '{}'", name),
                        line);
            return nullptr;
        }
        int n = std::stoi(n_str);
        int arg_count = args.size();
        if (arg_count < n) {
            reportError(std::format("Function '{}' requires at least {} "
                                    "arguments, but {} were provided.",
                                    name, n, arg_count),
                        line);
        }
        if (n < 1) {
            reportError(std::format("Invalid nth_N function name '{}'", name),
                        line);
        }
        for (const auto& arg : args) {
            auto arg_type = analyzeExpr(arg.get());
            if (!isConvertible(arg_type, Type::VALUE)) {
                reportError(
                    std::format("Argument to function '{}' has type '{}' which "
                                "is not convertible to Value.",
                                name, to_string(arg_type)),
                    arg->line());
            }
        }

        return nullptr;
    } else {
        reportError(std::format("Unknown function '{}'", name), line);
        return nullptr;
    }
}

void SemanticAnalyzer::validateClipReference(const std::string& clip_name,
                                             int line) {
    std::string name = clip_name;

    if (name.starts_with("$")) {
        name = name.substr(1);
        if (get_clip_index(name) == -1) {
            reportError(std::format("Invalid clip identifier '{}'.", clip_name),
                        line);
        }
        if (get_clip_index(name) > num_inputs - 1) {
            reportError(std::format("Clip index '{}' is out of range", name),
                        line);
        }
    } else {
        // Check if it's a function parameter of Clip type
        auto symbol = current_scope->resolve(name);
        if (!symbol || symbol->type != Type::CLIP) {
            reportError(
                std::format(
                    "Clip '{}' is not a clip constant or Clip parameter.",
                    clip_name),
                line);
        }
    }
}

Type SemanticAnalyzer::analyze(const PropAccessExpr& expr) {
    validateClipReference(expr.clip.value, expr.line);
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

    validateClipReference(expr.clip.value, expr.line);
    return Type::VALUE;
}

Type SemanticAnalyzer::analyze(FrameDimensionExpr& expr) {
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

Type SemanticAnalyzer::analyze(ArrayAccessExpr& expr) {
    auto* var_expr = get_if<VariableExpr>(expr.array.get());
    if (!var_expr) {
        reportError("Array access requires a variable as the array.",
                    expr.line);
        return Type::VALUE;
    }

    std::string array_name = var_expr->name.value;
    auto symbol = resolveSymbol(array_name, expr.line);

    if (!symbol || symbol->type != Type::ARRAY) {
        reportError(std::format("Variable '{}' is not an array.", array_name),
                    expr.line);
    }

    expr.array_symbol = symbol;

    auto index_type = analyzeExpr(expr.index.get());
    if (!isConvertible(index_type, Type::VALUE)) {
        reportError("Array index must be convertible to a value.",
                    expr.index->line());
    }

    return Type::VALUE;
}

// Statement analysis
void SemanticAnalyzer::analyze(const ExprStmt& stmt) {
    analyzeExpr(stmt.expr.get());
}

void SemanticAnalyzer::analyze(AssignStmt& stmt) {
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
            auto existing_symbol = current_scope->resolve(var_name);
            bool is_already_array =
                (existing_symbol && existing_symbol->type == Type::ARRAY);

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

            auto symbol = defineSymbol(SymbolKind::VARIABLE, var_name,
                                       Type::ARRAY, stmt.line);
            stmt.symbol = symbol;

            return;
        }
    }

    auto value_type = analyzeExpr(stmt.value.get());

    if (value_type == Type::ARRAY) {
        reportError(std::format("Cannot assign array to variable '{}'.",
                                stmt.name.value),
                    stmt.line);
        return;
    }

    std::string var_name = stmt.name.value;
    auto existing_symbol = current_scope->resolve(var_name);

    if (existing_symbol && existing_symbol->type == Type::ARRAY) {
        reportError(
            std::format(
                "Variable '{}' is an array and cannot be reassigned to a "
                "non-array value.",
                var_name),
            stmt.line);
    }

    auto symbol =
        defineSymbol(SymbolKind::VARIABLE, var_name, Type::VALUE, stmt.line);
    stmt.symbol = symbol;
}

void SemanticAnalyzer::analyze(ArrayAssignStmt& stmt) {
    auto* array_access = get_if<ArrayAccessExpr>(stmt.target.get());
    if (!array_access) {
        reportError("Array assignment target must be an array access.",
                    stmt.line);
        return;
    }

    analyzeExpr(stmt.target.get());

    auto value_type = analyzeExpr(stmt.value.get());
    if (!isConvertible(value_type, Type::VALUE)) {
        reportError("Array assignment value must be convertible to a value.",
                    stmt.value->line());
    }
}

void SemanticAnalyzer::analyze(BlockStmt& stmt) {
    ScopeGuard scope_guard(this);

    for (const auto& s : stmt.statements) {
        analyzeStmt(s.get());
    }
}

void SemanticAnalyzer::analyze(IfStmt& stmt) {
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

void SemanticAnalyzer::analyze(WhileStmt& stmt) {
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

void SemanticAnalyzer::analyze(LabelStmt& stmt) {
    auto symbol = defineSymbol(SymbolKind::LABEL, stmt.name.value, Type::VALUE,
                               stmt.line);
    stmt.symbol = symbol;
}

void SemanticAnalyzer::analyze(GotoStmt& stmt) {
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

    auto symbol = current_scope->resolve(stmt.label.value);
    stmt.target_label_symbol = symbol;

    if (stmt.condition) {
        analyzeExpr(stmt.condition.get());
    }
}

void SemanticAnalyzer::analyze(FunctionDef& stmt) {
    // Collect labels in function
    auto saved_current_function_labels = current_function_labels;
    current_function_labels.clear();
    for (const auto& s : stmt.body->statements) {
        collectLabels(s.get(), current_function_labels, stmt.name.value,
                      stmt.line);
    }

    // Find the function signature
    const FunctionSignature* sig = nullptr;
    if (function_signatures.count(stmt.name.value)) {
        for (const auto& s : function_signatures.at(stmt.name.value)) {
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

    if (sig->has_return && sig->returns_value) {
        bool body_always_returns = false;
        for (const auto& s : stmt.body->statements) {
            if (path_always_returns(s.get())) {
                body_always_returns = true;
                break;
            }
        }
        if (!body_always_returns) {
            reportError(
                std::format(
                    "Not all control paths in function '{}' return a value.",
                    stmt.name.value),
                stmt.line);
        }
    }

    auto func_symbol = defineSymbol(SymbolKind::FUNCTION, stmt.name.value,
                                    Type::VALUE, stmt.line);
    func_symbol->signature = sig;
    stmt.symbol = func_symbol;

    // Analyze function body with proper scope
    auto saved_current_function = current_function;
    current_function = sig;

    {
        // Enter function scope with RAII guard
        ScopeGuard scope_guard(this);

        // Add parameters
        for (const auto& param : stmt.params) {
            auto param_symbol = defineSymbol(
                SymbolKind::PARAMETER, param.name.value, param.type, stmt.line);
        }

        // Analyze function body
        analyze(*stmt.body);
    } // Scope automatically exits here

    // Restore context
    current_function = saved_current_function;
    current_function_labels = saved_current_function_labels;
}

void SemanticAnalyzer::analyze([[maybe_unused]] const GlobalDecl& stmt) {
    // Global declarations are handled by the parser
}

void SemanticAnalyzer::collectLabels(Stmt* stmt, std::set<std::string>& labels,
                                     const std::string& context,
                                     int context_line) {
    if (!stmt)
        return;
    if (auto* label = get_if<LabelStmt>(stmt)) {
        if (labels.count(label->name.value)) {
            reportError(std::format("Duplicate label '{}' in function '{}'",
                                    label->name.value, context),
                        label->line);
        }
        labels.insert(label->name.value);
    } else if (auto* block = get_if<BlockStmt>(stmt)) {
        for (auto& inner_s : block->statements) {
            collectLabels(inner_s.get(), labels, context, context_line);
        }
    } else if (auto* if_s = get_if<IfStmt>(stmt)) {
        collectLabels(if_s->then_branch.get(), labels, context, context_line);
        if (if_s->else_branch) {
            collectLabels(if_s->else_branch.get(), labels, context,
                          context_line);
        }
    } else if (auto* while_s = get_if<WhileStmt>(stmt)) {
        collectLabels(while_s->body.get(), labels, context, context_line);
    }
}

bool SemanticAnalyzer::path_always_returns(Stmt* stmt) {
    if (!stmt)
        return false;

    if (get_if<ReturnStmt>(stmt)) {
        return true;
    }
    if (auto* if_s = get_if<IfStmt>(stmt)) {
        return path_always_returns(if_s->then_branch.get()) &&
               (if_s->else_branch &&
                path_always_returns(if_s->else_branch.get()));
    }
    if (auto* block = get_if<BlockStmt>(stmt)) {
        for (const auto& s : block->statements) {
            if (path_always_returns(s.get())) {
                return true;
            }
        }
        return false;
    }
    return false;
}

bool SemanticAnalyzer::isConvertible(Type from, Type to) {
    return from == to || (to == Type::VALUE && from != Type::LITERAL_STRING);
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

void SemanticAnalyzer::validateGlobalDependencies(Stmt* stmt) {
    if (!stmt)
        return;

    std::function<void(Expr*)> check_expr = [&](Expr* expr) {
        if (!expr)
            return;
        if (auto* call = get_if<CallExpr>(expr)) {
            validateFunctionCall(*call);
        } else if (auto* binary = get_if<BinaryExpr>(expr)) {
            check_expr(binary->left.get());
            check_expr(binary->right.get());
        } else if (auto* unary = get_if<UnaryExpr>(expr)) {
            check_expr(unary->right.get());
        } else if (auto* ternary = get_if<TernaryExpr>(expr)) {
            check_expr(ternary->cond.get());
            check_expr(ternary->true_expr.get());
            check_expr(ternary->false_expr.get());
        }
    };

    if (auto* expr_stmt = get_if<ExprStmt>(stmt)) {
        check_expr(expr_stmt->expr.get());
    } else if (auto* assign_stmt = get_if<AssignStmt>(stmt)) {
        check_expr(assign_stmt->value.get());
    } else if (auto* if_stmt = get_if<IfStmt>(stmt)) {
        check_expr(if_stmt->condition.get());
        validateGlobalDependencies(if_stmt->then_branch.get());
        if (if_stmt->else_branch) {
            validateGlobalDependencies(if_stmt->else_branch.get());
        }
    } else if (auto* while_stmt = get_if<WhileStmt>(stmt)) {
        check_expr(while_stmt->condition.get());
        validateGlobalDependencies(while_stmt->body.get());
    } else if (auto* block = get_if<BlockStmt>(stmt)) {
        for (const auto& s : block->statements) {
            validateGlobalDependencies(s.get());
        }
    }
}

void SemanticAnalyzer::validateFunctionCall(const CallExpr& expr) {
    if (!function_signatures.count(expr.callee)) {
        return;
    }

    const FunctionSignature* sig = expr.resolved_signature;
    if (!sig) {
        for (const auto& s : function_signatures.at(expr.callee)) {
            if (s.params.size() == expr.args.size()) {
                sig = &s;
                break;
            }
        }
    }

    if (!sig) {
        return;
    }

    if (sig->global_mode == GlobalMode::ALL) {
        for (const auto& global_name : sig->used_globals) {
            if (global_name.starts_with("$")) {
                reportError(
                    std::format("Invalid global variable name {}", global_name),
                    expr.line);
            }

            if (!defined_global_vars.count(global_name)) {
                reportError(
                    std::format(
                        "Function '{}' uses global variable '{}' which is "
                        "not defined in global scope before this call",
                        expr.callee, global_name),
                    expr.line);
            }
        }
    } else if (sig->global_mode == GlobalMode::SPECIFIC) {
        for (const auto& global_name : sig->specific_globals) {
            if (!defined_global_vars.count(global_name)) {
                reportError(
                    std::format(
                        "Function '{}' requires global variable '{}' which is "
                        "not defined in global scope before this call",
                        expr.callee, global_name),
                    expr.line);
            }
        }
    }
}

void SemanticAnalyzer::collectUsedGlobals(Expr* expr,
                                          std::set<std::string>& used_globals) {
    if (!expr)
        return;

    if (auto* var_expr = get_if<VariableExpr>(expr)) {
        std::string name = var_expr->name.value;
        if (!name.starts_with("$") && name != "frame") {
            used_globals.insert(name);
        }
    } else if (auto* binary = get_if<BinaryExpr>(expr)) {
        collectUsedGlobals(binary->left.get(), used_globals);
        collectUsedGlobals(binary->right.get(), used_globals);
    } else if (auto* unary = get_if<UnaryExpr>(expr)) {
        collectUsedGlobals(unary->right.get(), used_globals);
    } else if (auto* ternary = get_if<TernaryExpr>(expr)) {
        collectUsedGlobals(ternary->cond.get(), used_globals);
        collectUsedGlobals(ternary->true_expr.get(), used_globals);
        collectUsedGlobals(ternary->false_expr.get(), used_globals);
    } else if (auto* call = get_if<CallExpr>(expr)) {
        for (const auto& arg : call->args) {
            collectUsedGlobals(arg.get(), used_globals);
        }
    } else if (auto* array_access = get_if<ArrayAccessExpr>(expr)) {
        collectUsedGlobals(array_access->array.get(), used_globals);
        collectUsedGlobals(array_access->index.get(), used_globals);
    } else if (auto* frame_dim = get_if<FrameDimensionExpr>(expr)) {
        collectUsedGlobals(frame_dim->plane_index_expr.get(), used_globals);
    }
}

void SemanticAnalyzer::collectUsedGlobalsInStmt(
    Stmt* stmt, std::set<std::string>& used_globals) {
    if (!stmt)
        return;

    if (auto* expr_stmt = get_if<ExprStmt>(stmt)) {
        collectUsedGlobals(expr_stmt->expr.get(), used_globals);
    } else if (auto* assign_stmt = get_if<AssignStmt>(stmt)) {
        collectUsedGlobals(assign_stmt->value.get(), used_globals);
    } else if (auto* array_assign = get_if<ArrayAssignStmt>(stmt)) {
        collectUsedGlobals(array_assign->target.get(), used_globals);
        collectUsedGlobals(array_assign->value.get(), used_globals);
    } else if (auto* if_stmt = get_if<IfStmt>(stmt)) {
        collectUsedGlobals(if_stmt->condition.get(), used_globals);
        collectUsedGlobalsInStmt(if_stmt->then_branch.get(), used_globals);
        if (if_stmt->else_branch) {
            collectUsedGlobalsInStmt(if_stmt->else_branch.get(), used_globals);
        }
    } else if (auto* while_stmt = get_if<WhileStmt>(stmt)) {
        collectUsedGlobals(while_stmt->condition.get(), used_globals);
        collectUsedGlobalsInStmt(while_stmt->body.get(), used_globals);
    } else if (auto* return_stmt = get_if<ReturnStmt>(stmt)) {
        if (return_stmt->value) {
            collectUsedGlobals(return_stmt->value.get(), used_globals);
        }
    } else if (auto* goto_stmt = get_if<GotoStmt>(stmt)) {
        if (goto_stmt->condition) {
            collectUsedGlobals(goto_stmt->condition.get(), used_globals);
        }
    } else if (auto* block = get_if<BlockStmt>(stmt)) {
        for (const auto& s : block->statements) {
            collectUsedGlobalsInStmt(s.get(), used_globals);
        }
    }
}

} // namespace infix2postfix
