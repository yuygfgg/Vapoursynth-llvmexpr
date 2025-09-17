"""
Copyright (C) 2025 yuygfgg

This file is part of Vapoursynth-llvmexpr.

Vapoursynth-llvmexpr is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Vapoursynth-llvmexpr is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Vapoursynth-llvmexpr.  If not, see <https://www.gnu.org/licenses/>.
"""

import os
import re
import sys
import requests
from bs4 import BeautifulSoup, Tag


def fetch_html(url):
    try:
        print(f"Downloading data from {url}...", file=sys.stderr)
        response = requests.get(url, timeout=10)
        response.raise_for_status()
        print("Download successful.", file=sys.stderr)
        return response.text
    except requests.RequestException as e:
        print(f"Error: Could not get web content: {e}", file=sys.stderr)
        sys.exit(1)


def extract_optimal_networks_data(html_content):
    soup = BeautifulSoup(html_content, "lxml")
    # { num_inputs: {'depth': int, 'ce': int, 'stages': list_of_stages} }
    optimal_networks = {}

    networks_table = soup.find("table", id="Networks")
    if not isinstance(networks_table, Tag):
        print(
            "Error: Could not find table with id 'Networks' in HTML.", file=sys.stderr
        )
        return {}

    for row in networks_table.find_all("tr"):
        if not isinstance(row, Tag):
            continue
        data_cell = row.find("td")
        if not isinstance(data_cell, Tag):
            continue

        description_text = data_cell.get_text()
        inputs_match = re.search(r"for (\d+) inputs", description_text)
        depth_match = re.search(r"(\d+) layers", description_text)
        ce_match = re.search(r"(\d+)\s+CEs", description_text)

        if not inputs_match or not depth_match or not ce_match:
            continue

        num_inputs = int(inputs_match.group(1))
        depth = int(depth_match.group(1))
        ce = int(ce_match.group(1))

        mono_p = data_cell.find("p", class_="mono")
        if not isinstance(mono_p, Tag):
            continue

        structure_text = mono_p.get_text(separator="\n")
        structure_lines = structure_text.strip().split("\n")

        network_stages = []
        for line in structure_lines:
            try:
                pairs = re.findall(r"\((\d+),(\d+)\)", line)
                stage = [(int(p[0]), int(p[1])) for p in pairs]
                if stage:
                    network_stages.append(stage)
            except (ValueError, IndexError):
                print(f"Warning: Failed to parse line: {line}", file=sys.stderr)

        if not network_stages:
            continue

        flat_count = sum(len(stage) for stage in network_stages)
        if flat_count != ce:
            print(
                f"Warning: CE count mismatch for N={num_inputs}: parsed={ce}, computed={flat_count}",
                file=sys.stderr,
            )

        if (
            num_inputs not in optimal_networks
            or ce < optimal_networks[num_inputs]["ce"]
            or (
                ce == optimal_networks[num_inputs]["ce"]
                and depth < optimal_networks[num_inputs]["depth"]
            )
        ):
            optimal_networks[num_inputs] = {
                "depth": depth,
                "ce": ce,
                "stages": network_stages,
            }

    return optimal_networks


def generate_cpp_header(networks, output_path, source_url):
    if not networks:
        print(
            "No valid network data found, cannot generate header file.", file=sys.stderr
        )
        return

    all_comparators = []
    meta_data = []
    offset = 0

    sorted_n = sorted(networks.keys())

    for n in sorted_n:
        data = networks[n]
        stages = data["stages"]
        depth = data["depth"]

        pairs = [pair for stage in stages for pair in stage]
        count = len(pairs)

        meta_data.append({"n": n, "offset": offset, "count": count, "depth": depth})
        all_comparators.extend(pairs)
        offset += count

    output_lines = [
        "/**",
        " * Copyright (C) 2025 yuygfgg",
        " * ",
        " * This file is part of Vapoursynth-llvmexpr.",
        " * ",
        " * Vapoursynth-llvmexpr is free software: you can redistribute it and/or modify",
        " * it under the terms of the GNU General Public License as published by",
        " * the Free Software Foundation, either version 3 of the License, or",
        " * (at your option) any later version.",
        " * ",
        " * Vapoursynth-llvmexpr is distributed in the hope that it will be useful,",
        " * but WITHOUT ANY WARRANTY; without even the implied warranty of",
        " * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the",
        " * GNU General Public License for more details.",
        " * ",
        " * You should have received a copy of the GNU General Public License",
        " * along with Vapoursynth-llvmexpr.  If not, see <https://www.gnu.org/licenses/>.",
        " */",
        "",
        "#include <array>",
        "#include <utility>",
        "#include <cstddef>",
        "",
        "using Comparator = std::pair<int, int>;",
        "",
        f"// Generated by {os.path.basename(__file__)}",
        f"// From: {source_url}",
        "",
        f"constexpr std::array<Comparator, {len(all_comparators)}> all_sorting_network_comparators = {{{{",
    ]

    if all_comparators:
        comparators_str = ", ".join([f"{{{p[0]}, {p[1]}}}" for p in all_comparators])
        output_lines.append(f"    {comparators_str}")

    output_lines.extend(
        [
            "}};",
            "",
            "struct SortingNetwork {",
            "    int n_inputs;",
            "    size_t offset;",
            "    size_t count;",
            "};",
            "",
            f"constexpr std::array<SortingNetwork, {len(meta_data)}> optimal_sorting_networks_meta = {{{{",
        ]
    )

    for meta in meta_data:
        comment = (
            f"// N={meta['n']}, {meta['count']} comparators, {meta['depth']} layers"
        )
        output_lines.append(
            f"    {{ {meta['n']}, {meta['offset']}u, {meta['count']}u }}, {comment}"
        )

    output_lines.extend(
        [
            "}};",
            "",
            "struct SortingNetworkView {",
            "    const Comparator* data = nullptr;",
            "    size_t count = 0;",
            "    constexpr auto begin() const { return data; }",
            "    constexpr auto end() const { return data + count; }",
            "    constexpr bool empty() const { return count == 0; }",
            "};",
            "",
            "constexpr SortingNetworkView get_optimal_sorting_network(int n) {",
            "    for (const auto& meta : optimal_sorting_networks_meta) {",
            "        if (meta.n_inputs == n) {",
            "            return { &all_sorting_network_comparators[meta.offset], meta.count };",
            "        }",
            "    }",
            "    return {};",
            "}",
            "",
        ]
    )

    try:
        output_dir = os.path.dirname(output_path)
        if output_dir:
            os.makedirs(output_dir, exist_ok=True)

        with open(output_path, "w", encoding="utf-8") as f:
            f.write("\n".join(output_lines))

        print(f"Successfully generated C++ header file: {output_path}", file=sys.stderr)

    except IOError as e:
        print(f"Error: Could not write file {output_path}: {e}", file=sys.stderr)
        sys.exit(1)


def main():
    source_url = "https://bertdobbelaere.github.io/sorting_networks_extended.html"
    cpp_output_path = os.path.join("..", "llvmexpr", "optimal_sorting_networks.hpp")

    html_content = fetch_html(source_url)
    networks_data = extract_optimal_networks_data(html_content)

    generate_cpp_header(networks_data, cpp_output_path, source_url)


if __name__ == "__main__":
    main()
