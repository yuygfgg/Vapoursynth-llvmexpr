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

#include "Sorting.hpp"

void oem_merge_pairs(std::vector<std::pair<int, int>>& pairs, int lo, int n,
                     int r) {
    int m = r * 2;
    if (m < n) {
        oem_merge_pairs(pairs, lo, n, m);
        oem_merge_pairs(pairs, lo + r, n, m);
        for (int i = lo + r; i < lo + n - r; i += m) {
            pairs.push_back({i, i + r});
        }
    } else {
        pairs.push_back({lo, lo + r});
    }
}

void generate_oem_sort_pairs(std::vector<std::pair<int, int>>& pairs, int lo,
                             int n) {
    if (n > 1) {
        int m = n / 2;
        generate_oem_sort_pairs(pairs, lo, m);
        generate_oem_sort_pairs(pairs, lo + m, m);
        oem_merge_pairs(pairs, lo, n, 1);
    }
}
