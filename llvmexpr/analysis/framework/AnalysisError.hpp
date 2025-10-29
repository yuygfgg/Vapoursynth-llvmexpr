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

#ifndef LLVMEXPR_ANALYSIS_FRAMEWORK_ANALYSIS_ERROR_HPP
#define LLVMEXPR_ANALYSIS_FRAMEWORK_ANALYSIS_ERROR_HPP

#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>

namespace analysis {

class AnalysisError : public std::runtime_error {
  public:
    explicit AnalysisError(const std::string& message)
        : std::runtime_error(message) {}

    AnalysisError(const std::string& message, int token_idx)
        : std::runtime_error(message), token_idx(token_idx) {}

    AnalysisError(const std::string& message, int token_idx,
                  std::string_view token_text)
        : std::runtime_error(message), token_idx(token_idx),
          token_text(token_text) {}

    [[nodiscard]] std::optional<int> getTokenIndex() const {
        return token_idx;
    }

    [[nodiscard]] std::optional<std::string> getTokenText() const {
        return token_text;
    }

  private:
    std::optional<int> token_idx;
    std::optional<std::string> token_text;
};

} // namespace analysis

#endif // LLVMEXPR_ANALYSIS_FRAMEWORK_ANALYSIS_ERROR_HPP
