import regex as re
import sys


def parse_line(line):
    try:
        pairs = re.findall(r"\((\d+),(\d+)\)", line)
        return [(int(p[0]), int(p[1])) for p in pairs]
    except (ValueError, IndexError):
        print(f"Error parsing line: {line}", file=sys.stderr)
        return []


def format_stage(stage):
    return "{" + ", ".join([f"{{{p[0]}, {p[1]}}}" for p in stage]) + "}"


def main():
    input_file = "./optimal_networks.txt"
    try:
        with open(input_file, "r") as f:
            content = f.read()
    except FileNotFoundError:
        print(f"Error: Input file '{input_file}' not found.", file=sys.stderr)
        sys.exit(1)

    networks = {}
    blocks = re.split(r"\n\s*\n", content.strip())

    for block in blocks:
        lines = block.strip().split("\n")
        try:
            n_inputs = int(lines[0])
            stages_lines = lines[1:]

            network_stages = []
            for line in stages_lines:
                stage = parse_line(line)
                if stage:
                    network_stages.append(stage)

            if network_stages:
                networks[n_inputs] = network_stages
        except (ValueError, IndexError):
            print(f"Skipping invalid block:\n---\n{block}\n---", file=sys.stderr)
            continue

    if not networks:
        print("No valid networks found in the file.", file=sys.stderr)
        return

    flat_networks = {}
    for n, stages in networks.items():
        flat_networks[n] = [pair for stage in stages for pair in stage]

    all_comparators = []
    meta_data = []
    offset = 0

    sorted_n = sorted(flat_networks.keys())

    for n in sorted_n:
        pairs = flat_networks[n]
        count = len(pairs)
        meta_data.append(
            {"n": n, "offset": offset, "count": count, "num_comparators": count}
        )
        all_comparators.extend(pairs)
        offset += count

    print("#pragma once")
    print()
    print("#include <array>")
    print("#include <utility>")
    print("#include <cstddef>")
    print()

    print("using Comparator = std::pair<int, int>;")
    print()

    print(
        "// Modified from https://bertdobbelaere.github.io/sorting_networks_extended.html"
    )
    print(
        f"constexpr std::array<Comparator, {len(all_comparators)}> all_sorting_network_comparators = {{{{"
    )
    if all_comparators:
        print("    " + ", ".join([f"{{{p[0]}, {p[1]}}}" for p in all_comparators]))
    print("}};")
    print()

    print("struct SortingNetwork {")
    print("    int n_inputs;")
    print("    size_t offset;")
    print("    size_t count;")
    print("};")
    print()

    print(
        f"constexpr std::array<SortingNetwork, {len(meta_data)}> optimal_sorting_networks_meta = {{{{"
    )
    for meta in meta_data:
        print(
            f"    {{ {meta['n']}, {meta['offset']}u, {meta['count']}u }}, // N={meta['n']}, {meta['num_comparators']} comparators"
        )
    print("}};")
    print()

    print("struct SortingNetworkView {")
    print("    const Comparator* data = nullptr;")
    print("    size_t count = 0;")
    print("    constexpr auto begin() const { return data; }")
    print("    constexpr auto end() const { return data + count; }")
    print("    constexpr bool empty() const { return count == 0; }")
    print("};")
    print()

    print("constexpr SortingNetworkView get_optimal_sorting_network(int n) {")
    print("    for (const auto& meta : optimal_sorting_networks_meta) {")
    print("        if (meta.n_inputs == n) {")
    print(
        "            return { &all_sorting_network_comparators[meta.offset], meta.count };"
    )
    print("        }")
    print("    }")
    print("    return {};")
    print("}")


if __name__ == "__main__":
    main()
