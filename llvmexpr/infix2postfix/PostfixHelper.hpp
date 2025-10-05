#ifndef LLVMEXPR_INFIX2POSTFIX_POSTFIXHELPER_HPP
#define LLVMEXPR_INFIX2POSTFIX_POSTFIXHELPER_HPP

#include <string>

namespace infix2postfix {

enum class PostfixMode {
    EXPR,
    SINGLE_EXPR,
};

int compute_postfix_stack_effect(const std::string& postfix_expr,
                                 PostfixMode mode, int line);

} // namespace infix2postfix

#endif