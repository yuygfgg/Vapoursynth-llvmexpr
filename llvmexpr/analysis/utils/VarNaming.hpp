/**
 * Copyright (C) 2025 yuygfgg
 * 
 * This file is part of Vapoursynth-llvmexpr.
 * 
 * Vapoursynth-llvmexpr is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Vapoursynth-llvmexpr is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with Vapoursynth-llvmexpr.  If not, see <https://www.gnu.org/licenses/>.
 */

#ifndef LLVMEXPR_ANALYSIS_UTILS_VAR_NAMING_HPP
#define LLVMEXPR_ANALYSIS_UTILS_VAR_NAMING_HPP

#include <string>
#include <string_view>

namespace analysis::var_naming {

inline constexpr std::string_view ARRAY_SUFFIX = "{}";

inline std::string getArrayName(std::string_view base_name) {
    return std::string(base_name) + std::string(ARRAY_SUFFIX);
}

inline bool isArrayName(std::string_view name) {
    return name.ends_with(ARRAY_SUFFIX);
}

inline std::string getBaseName(std::string_view array_name) {
    if (isArrayName(array_name)) {
        return std::string(
            array_name.substr(0, array_name.size() - ARRAY_SUFFIX.size()));
    }
    return std::string(array_name);
}

} // namespace analysis::var_naming

#endif // LLVMEXPR_ANALYSIS_UTILS_VAR_NAMING_HPP
