#include "PostfixHelper.hpp"
#include "../Tokenizer.hpp"
#include <format>
#include <stdexcept>

namespace infix2postfix {

int compute_postfix_stack_effect(const std::string& postfix_expr,
                                 PostfixMode mode, int line) {
    ::ExprMode expr_mode = (mode == PostfixMode::EXPR)
                               ? ::ExprMode::EXPR
                               : ::ExprMode::SINGLE_EXPR;

    std::vector<::Token> tokens;
    try {
        tokens = ::tokenize(postfix_expr,
                            114514, // TODO: handle num_inputs correctly
                            expr_mode);
    } catch (const std::exception& e) {
        throw std::runtime_error(
            std::format("Line {}: Failed to tokenize postfix expression: {}",
                        line, e.what()));
    }

    int stack = 0;
    for (const auto& token : tokens) {
        ::TokenBehavior behavior = ::get_token_behavior(token);
        stack += behavior.stack_effect;
        if (stack < 0) {
            throw std::runtime_error(
                std::format("Line {}: Stack underflow while processing '{}'",
                            line, token.text));
        }
    }
    return stack;
}

} // namespace infix2postfix
