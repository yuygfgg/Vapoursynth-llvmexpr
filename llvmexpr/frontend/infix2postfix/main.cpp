#include <format>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

#include "AnalysisEngine.hpp"
#include "CodeGenerator.hpp"
#include "Tokenizer.hpp"

using namespace infix2postfix;

void print_usage() {
    std::cerr << "Usage: infix2postfix <in.expr> -m [expr/single] -o <out.expr>"
              << std::endl;
}

int main(int argc, char* argv[]) {
    std::string input_file;
    std::string output_file;
    Mode mode = Mode::Expr;

    if (argc != 6) {
        print_usage();
        return 1;
    }

    for (int i = 1; i < argc; ++i) {
        std::string arg = argv[i];
        if (arg == "-o") {
            if (i + 1 < argc)
                output_file = argv[++i];
        } else if (arg == "-m") {
            if (i + 1 < argc) {
                std::string mode_str = argv[++i];
                if (mode_str == "single") {
                    mode = Mode::Single;
                } else if (mode_str != "expr") {
                    std::cerr << std::format(
                        "Error: Invalid mode '{}'. Use 'expr' or 'single'.\n",
                        mode_str);
                    return 1;
                }
            }
        } else {
            if (input_file.empty()) {
                input_file = arg;
            } else {
                print_usage();
                return 1;
            }
        }
    }

    if (input_file.empty() || output_file.empty()) {
        print_usage();
        return 1;
    }

    std::ifstream in_stream(input_file);
    if (!in_stream) {
        std::cerr << std::format("Error: Cannot open input file '{}'\n",
                                 input_file);
        return 1;
    }
    std::stringstream buffer;
    buffer << in_stream.rdbuf();
    std::string source = buffer.str();

    try {
        Tokenizer tokenizer(source);
        auto tokens = tokenizer.tokenize();

        AnalysisEngine engine(tokens, mode, 114514); // 1919810
        bool success = engine.runAnalysis();

        if (!success) {
            std::string diagnostics = engine.formatDiagnostics();
            std::cerr << std::format("Analysis failed:\n{}\n", diagnostics);
            return 1;
        }

        CodeGenerator generator(mode, 114514);
        std::string postfix_code = generator.generate(engine.getAST());

        std::ofstream out_stream(output_file);
        if (!out_stream) {
            std::cerr << std::format("Error: Cannot open output file '{}'\n",
                                     output_file);
            return 1;
        }
        out_stream << postfix_code << std::endl;

        std::cout << std::format("Successfully converted '{}' to '{}'\n",
                                 input_file, output_file);

    } catch (const std::exception& e) {
        std::cerr << std::format("An error occurred: {}\n", e.what());
        return 1;
    }

    return 0;
}
