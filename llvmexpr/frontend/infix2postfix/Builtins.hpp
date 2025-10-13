#ifndef LLVMEXPR_INFIX2POSTFIX_BUILTINS_HPP
#define LLVMEXPR_INFIX2POSTFIX_BUILTINS_HPP

#include "AST.hpp"
#include "CodeGenerator.hpp"
#include "types.hpp"
#include <functional>
#include <map>
#include <optional>
#include <string>
#include <vector>

namespace infix2postfix {

struct CallExpr; // Forward declaration

struct BuiltinFunction {
    std::string name;
    int arity;
    std::optional<Mode> mode_restriction;
    std::vector<Type> param_types;
    std::function<PostfixBuilder(CodeGenerator*, const CallExpr&)>
        special_handler = nullptr;
};

const std::map<std::string, std::vector<BuiltinFunction>>&
get_builtin_functions();

} // namespace infix2postfix

#endif
