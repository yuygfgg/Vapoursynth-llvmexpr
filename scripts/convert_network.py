import os
import re
import sys
import requests
from bs4 import BeautifulSoup

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
    soup = BeautifulSoup(html_content, 'lxml')
    # { num_inputs: {'depth': int, 'stages': list_of_stages} }
    optimal_networks = {}

    networks_table = soup.find('table', id='Networks')
    if not networks_table:
        print("Error: Could not find table with id 'Networks' in HTML.", file=sys.stderr)
        return {}

    for row in networks_table.find_all('tr'):
        data_cell = row.find('td')
        if not data_cell:
            continue

        description_text = data_cell.get_text()
        inputs_match = re.search(r"for (\d+) inputs", description_text)
        depth_match = re.search(r"(\d+) layers", description_text)

        if not inputs_match or not depth_match:
            continue

        num_inputs = int(inputs_match.group(1))
        depth = int(depth_match.group(1))

        mono_p = data_cell.find('p', class_='mono')
        if not mono_p:
            continue

        structure_html = mono_p.decode_contents()
        structure_lines = structure_html.replace('<br>', '\n').replace('<br/>', '\n').strip().split('\n')

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

        if num_inputs not in optimal_networks or depth < optimal_networks[num_inputs]['depth']:
            optimal_networks[num_inputs] = {
                'depth': depth,
                'stages': network_stages
            }

    return optimal_networks

def generate_cpp_header(networks, output_path, source_url):
    if not networks:
        print("No valid network data found, cannot generate header file.", file=sys.stderr)
        return

    all_comparators = []
    meta_data = []
    offset = 0
    
    sorted_n = sorted(networks.keys())

    for n in sorted_n:
        data = networks[n]
        stages = data['stages']
        depth = data['depth']
        
        pairs = [pair for stage in stages for pair in stage]
        count = len(pairs)
        
        meta_data.append({"n": n, "offset": offset, "count": count, "depth": depth})
        all_comparators.extend(pairs)
        offset += count

    output_lines = [
        "#include <array>",
        "#include <utility>",
        "#include <cstddef>",
        "",
        "using Comparator = std::pair<int, int>;",
        "",
        f"// Generated from {source_url}",
        f"constexpr std::array<Comparator, {len(all_comparators)}> all_sorting_network_comparators = {{{{",
    ]

    if all_comparators:
        comparators_str = ", ".join([f"{{{p[0]}, {p[1]}}}" for p in all_comparators])
        output_lines.append(f"    {comparators_str}")

    output_lines.extend([
        "}};",
        "",
        "struct SortingNetwork {",
        "    int n_inputs;",
        "    size_t offset;",
        "    size_t count;",
        "};",
        "",
        f"constexpr std::array<SortingNetwork, {len(meta_data)}> optimal_sorting_networks_meta = {{{{",
    ])

    for meta in meta_data:
        comment = f"// N={meta['n']}, {meta['count']} comparators, {meta['depth']} layers"
        output_lines.append(
            f"    {{ {meta['n']}, {meta['offset']}u, {meta['count']}u }}, {comment}"
        )
    
    output_lines.extend([
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
        ""
    ])
    
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
    output_path = os.path.join("..", "llvmexpr", "optimal_sorting_networks.hpp")
    
    html_content = fetch_html(source_url)
    networks_data = extract_optimal_networks_data(html_content)
    generate_cpp_header(networks_data, output_path, source_url)

if __name__ == "__main__":
    main()