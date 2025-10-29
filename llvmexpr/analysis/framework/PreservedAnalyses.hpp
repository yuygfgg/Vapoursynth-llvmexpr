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

#ifndef LLVMEXPR_ANALYSIS_FRAMEWORK_PRESERVED_ANALYSES_HPP
#define LLVMEXPR_ANALYSIS_FRAMEWORK_PRESERVED_ANALYSES_HPP

#include <typeindex>
#include <unordered_set>

namespace analysis {

class PreservedAnalyses {
  public:
    static PreservedAnalyses all() { return PreservedAnalyses(true); }

    static PreservedAnalyses none() { return PreservedAnalyses(false); }

    [[nodiscard]] bool preservedAll() const { return preserved_all; }

    template <typename PassT> void preserve() {
        if (!preserved_all) {
            preserved_passes.insert(std::type_index(typeid(PassT)));
        }
    }

    template <typename PassT> [[nodiscard]] bool isPreserved() const {
        if (preserved_all) {
            return true;
        }
        return preserved_passes.contains(std::type_index(typeid(PassT)));
    }

    [[nodiscard]] bool isPreserved(const std::type_index& pass_type) const {
        if (preserved_all) {
            return true;
        }
        return preserved_passes.contains(pass_type);
    }

  private:
    explicit PreservedAnalyses(bool all) : preserved_all(all) {}

    bool preserved_all;
    std::unordered_set<std::type_index> preserved_passes;
};

} // namespace analysis

#endif // LLVMEXPR_ANALYSIS_FRAMEWORK_PRESERVED_ANALYSES_HPP
