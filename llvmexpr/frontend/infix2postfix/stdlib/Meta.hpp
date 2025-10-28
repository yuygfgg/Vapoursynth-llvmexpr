#ifndef LLVMEXPR_INFIX2POSTFIX_STDLIB_META_HPP
#define LLVMEXPR_INFIX2POSTFIX_STDLIB_META_HPP

#include "LibraryBase.hpp"

namespace infix2postfix::stdlib {

struct meta {
    static constexpr std::string_view name = "meta";

    // NOLINTNEXTLINE(modernize-avoid-c-arrays,cppcoreguidelines-avoid-c-arrays)
    static constexpr char code_data[] = {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wc23-extensions"
#embed "meta.expr"
#pragma clang diagnostic pop
        ,
        0 // null terminator
    };
    static constexpr std::string_view code = std::string_view(
        static_cast<const char*>(code_data), sizeof(code_data) - 1);

    using dependencies = std::tuple<>;

    static constexpr std::array<ExportedFunction, 5> exports = {
        {ExportedFunction{.name = "ASSERT_CONST", .param_count = 3},
         ExportedFunction{.name = "ERROR", .param_count = 1},
         ExportedFunction{.name = "JOIN", .param_count = 3},
         ExportedFunction{.name = "PASTE", .param_count = 2},
         ExportedFunction{.name = "UNROLL", .param_count = 2}}
        };
};

} // namespace infix2postfix::stdlib

#endif // LLVMEXPR_INFIX2POSTFIX_STDLIB_META_HPP
