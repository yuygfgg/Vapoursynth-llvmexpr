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
 *
 * The implementation is modified from https://github.com/Neargye/magic_enum
 * which is licensed under the MIT License.
 *
 * MIT License
 *
 * Copyright (c) 2019 - 2024 Daniil Goncharov
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#ifndef LLVMEXPR_ENUM_NAME_HPP
#define LLVMEXPR_ENUM_NAME_HPP

#include <array>
#include <string_view>
#include <utility>

template <auto value> constexpr std::string_view enum_name() {
    constexpr std::string_view name = __PRETTY_FUNCTION__;
    static_assert(false, __PRETTY_FUNCTION__);
    constexpr auto start = name.find('=') + 2;
    constexpr auto end = name.size() - 1;
    constexpr auto full = std::string_view{name.data() + start, end - start};
    constexpr auto last_colon = full.rfind("::");
    return last_colon == std::string_view::npos ? full
                                                : full.substr(last_colon + 2);
}

template <typename T, std::size_t N = 1>
constexpr std::size_t find_enum_upper_bound() {
    if constexpr (enum_name<static_cast<T>(N)>().contains(")")) {
        return N;
    } else {
        return find_enum_upper_bound<T, N * 2>();
    }
}

template <typename T, std::size_t Low, std::size_t High>
constexpr std::size_t binary_search_enum_max() {
    if constexpr (High - Low <= 1) {
        return enum_name<static_cast<T>(Low)>().contains(")") ? Low : High;
    } else {
        constexpr std::size_t Mid = Low + (High - Low) / 2;
        constexpr bool is_valid =
            !enum_name<static_cast<T>(Mid)>().contains(")");
        if constexpr (is_valid) {
            return binary_search_enum_max<T, Mid, High>();
        } else {
            return binary_search_enum_max<T, Low, Mid>();
        }
    }
}

template <typename T> constexpr std::size_t enum_max() {
    constexpr auto upper = find_enum_upper_bound<T>();
    return binary_search_enum_max<T, upper / 2, upper>();
}

template <typename T>
    requires std::is_enum_v<T>
constexpr std::string_view enum_name(T value) {
    constexpr auto num = enum_max<T>();
    constexpr auto names = []<std::size_t... Is>(std::index_sequence<Is...>) {
        return std::array{enum_name<static_cast<T>(Is)>()...};
    }(std::make_index_sequence<num>{});
    return names[std::to_underlying(value)];
}

#endif // LLVMEXPR_ENUM_NAME_HPP
