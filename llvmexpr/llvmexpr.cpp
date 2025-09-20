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

#include <algorithm>
#include <cctype>
#include <cstdint>
#include <format>
#include <functional>
#include <map>
#include <memory>
#include <mutex>
#include <numbers>
#include <numeric>
#include <optional>
#include <regex>
#include <sstream>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>
#include <variant>
#include <vector>

#include "VSHelper.h"
#include "VapourSynth.h"

#include "llvm/ADT/SmallVector.h"
#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/CodeGen.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"
#include "llvm/TargetParser/Triple.h"

#include "optimal_sorting_networks.hpp"

constexpr unsigned ALIGNMENT = 32; // Vapoursynth should guarantee this

namespace {

constexpr uint32_t EXIT_NAN_PAYLOAD = 0x7FC0E71F; // qNaN with payload 0xE71F
constexpr uint32_t PROP_NAN_PAYLOAD = 0x7FC0BEEF; // qNaN with payload 0xBEEF

enum class TokenType {
    // Literals & Constants
    NUMBER,
    CONSTANT_X,
    CONSTANT_Y,
    CONSTANT_WIDTH,
    CONSTANT_HEIGHT,
    CONSTANT_N,
    CONSTANT_PI,

    // Variable Ops
    VAR_STORE, // my_var!
    VAR_LOAD,  // my_var@

    // Data Access
    CLIP_REL,    // src[x,y]
    CLIP_ABS,    // src[]
    CLIP_CUR,    // src
    PROP_ACCESS, // src.prop

    // Binary Operators
    ADD,
    SUB,
    MUL,
    DIV,
    MOD,
    GT,
    LT,
    GE,
    LE,
    EQ,
    AND,
    OR,
    XOR,
    BITAND,
    BITOR,
    BITXOR,
    POW,
    ATAN2,
    COPYSIGN,
    MIN,
    MAX,

    // Unary Operators
    NOT,
    BITNOT,
    SQRT,
    EXP,
    LOG,
    ABS,
    FLOOR,
    CEIL,
    TRUNC,
    ROUND,
    SIN,
    COS,
    TAN,
    ASIN,
    ACOS,
    ATAN,
    EXP2,
    LOG10,
    LOG2,
    SINH,
    COSH,
    TANH,
    SGN,
    NEG,

    // Ternary and other multi-arg
    TERNARY, // ?
    CLIP,
    CLAMP, // same op, 3 args
    FMA,   // 3 args

    // Stack manipulation
    DUP,
    DROP,
    SWAP,
    SORTN,

    // Control Flow
    LABEL_DEF, // #my_label
    JUMP,      // my_label#

    // Custom output control
    EXIT_NO_WRITE, // ^exit^
    STORE_ABS,     // @[]
};

struct TokenPayload_Number {
    double value;
};
struct TokenPayload_Var {
    std::string name;
};
struct TokenPayload_Label {
    std::string name;
};
struct TokenPayload_StackOp {
    int n;
};
struct TokenPayload_ClipAccess {
    int clip_idx;
    int rel_x = 0;
    int rel_y = 0;
    bool use_mirror = false;
    bool has_mode = false;
};
struct TokenPayload_PropAccess {
    int clip_idx;
    std::string prop_name;
};

struct Token {
    TokenType type;
    std::string text;
    std::variant<std::monostate, TokenPayload_Number, TokenPayload_Var,
                 TokenPayload_Label, TokenPayload_StackOp,
                 TokenPayload_ClipAccess, TokenPayload_PropAccess>
        payload;
};

namespace {
const std::regex re_rel_bracket{
    R"(^(?:src(\d+)|([x-za-w]))\[\s*(-?\d+)\s*,\s*(-?\d+)\s*\](?::([cm]))?$)"};
const std::regex re_abs{R"(^(?:src(\d+)|([x-za-w]))\[\]$)"};
const std::regex re_cur{R"(^(?:src(\d+)|([x-za-w]))$)"};
const std::regex re_prop{
    R"(^(?:src(\d+)|([x-za-w]))\.([a-zA-Z_][a-zA-Z0-9_]*)$)"};

int parse_clip_idx_from_match(const std::smatch& match) {
    if (match[1].matched) {
        return std::stoi(match[1].str());
    }
    if (match[2].matched) {
        char c = match[2].str()[0];
        if (c >= 'x' && c <= 'z') {
            return c - 'x';
        }
        return c - 'a' + 3;
    }
    std::unreachable();
}
} // namespace

constexpr std::optional<TokenType> resolve_keyword(const std::string_view s) {
    switch (s.size()) {
    case 1:
        switch (s[0]) {
        case '+':
            return TokenType::ADD;
        case '-':
            return TokenType::SUB;
        case '*':
            return TokenType::MUL;
        case '/':
            return TokenType::DIV;
        case '%':
            return TokenType::MOD;
        case '>':
            return TokenType::GT;
        case '<':
            return TokenType::LT;
        case '=':
            return TokenType::EQ;
        case '?':
            return TokenType::TERNARY;
        case 'X':
            return TokenType::CONSTANT_X;
        case 'Y':
            return TokenType::CONSTANT_Y;
        case 'N':
            return TokenType::CONSTANT_N;
        }
        break;
    case 2:
        if (s == ">=")
            return TokenType::GE;
        if (s == "<=")
            return TokenType::LE;
        if (s == "**")
            return TokenType::POW;
        if (s == "or")
            return TokenType::OR;
        if (s == "pi")
            return TokenType::CONSTANT_PI;
        break;
    case 3:
        if (s == "and")
            return TokenType::AND;
        if (s == "xor")
            return TokenType::XOR;
        if (s == "not")
            return TokenType::NOT;
        if (s == "pow")
            return TokenType::POW;
        if (s == "min")
            return TokenType::MIN;
        if (s == "max")
            return TokenType::MAX;
        if (s == "fma")
            return TokenType::FMA;
        if (s == "exp")
            return TokenType::EXP;
        if (s == "log")
            return TokenType::LOG;
        if (s == "abs")
            return TokenType::ABS;
        if (s == "sin")
            return TokenType::SIN;
        if (s == "cos")
            return TokenType::COS;
        if (s == "tan")
            return TokenType::TAN;
        if (s == "sgn")
            return TokenType::SGN;
        if (s == "neg")
            return TokenType::NEG;
        if (s == "@[]")
            return TokenType::STORE_ABS;
        break;
    case 4:
        if (s == "clip")
            return TokenType::CLIP;
        if (s == "sqrt")
            return TokenType::SQRT;
        if (s == "ceil")
            return TokenType::CEIL;
        if (s == "asin")
            return TokenType::ASIN;
        if (s == "acos")
            return TokenType::ACOS;
        if (s == "atan")
            return TokenType::ATAN;
        if (s == "exp2")
            return TokenType::EXP2;
        if (s == "log2")
            return TokenType::LOG2;
        if (s == "sinh")
            return TokenType::SINH;
        if (s == "cosh")
            return TokenType::COSH;
        if (s == "tanh")
            return TokenType::TANH;
        break;
    case 5:
        if (s == "bitor")
            return TokenType::BITOR;
        if (s == "atan2")
            return TokenType::ATAN2;
        if (s == "clamp")
            return TokenType::CLAMP;
        if (s == "floor")
            return TokenType::FLOOR;
        if (s == "trunc")
            return TokenType::TRUNC;
        if (s == "round")
            return TokenType::ROUND;
        if (s == "log10")
            return TokenType::LOG10;
        if (s == "width")
            return TokenType::CONSTANT_WIDTH;
        break;
    case 6:
        if (s == "bitand")
            return TokenType::BITAND;
        if (s == "bitxor")
            return TokenType::BITXOR;
        if (s == "bitnot")
            return TokenType::BITNOT;
        if (s == "height")
            return TokenType::CONSTANT_HEIGHT;
        if (s == "^exit^")
            return TokenType::EXIT_NO_WRITE;
        break;
    case 8:
        if (s == "copysign")
            return TokenType::COPYSIGN;
        break;
    }
    return std::nullopt;
}

std::vector<Token> tokenize(const std::string& expr, int num_inputs) {
    std::vector<Token> tokens;
    std::stringstream ss(expr);
    std::string str_token;

    int idx = 0;
    while (ss >> str_token) {
        Token t;
        t.text = str_token;
        std::smatch match;

        if (auto keyword_type = resolve_keyword(str_token); keyword_type) {
            t.type = *keyword_type;
        } else if (std::regex_match(str_token, match,
                                    std::regex(R"(^dup(\d*)$)"))) {
            t.type = TokenType::DUP;
            int n = match[1].str().empty() ? 0 : std::stoi(match[1].str());
            if (n < 0)
                throw std::runtime_error(std::format(
                    "Invalid dupN operator: {} (idx {})", str_token, idx));
            t.payload = TokenPayload_StackOp{n};
        } else if (std::regex_match(str_token, match,
                                    std::regex(R"(^drop(\d*)$)"))) {
            t.type = TokenType::DROP;
            int n = match[1].str().empty() ? 1 : std::stoi(match[1].str());
            if (n < 0)
                throw std::runtime_error(std::format(
                    "Invalid dropN operator: {} (idx {})", str_token, idx));
            t.payload = TokenPayload_StackOp{n};
        } else if (std::regex_match(str_token, match,
                                    std::regex(R"(^swap(\d*)$)"))) {
            t.type = TokenType::SWAP;
            int n = match[1].str().empty() ? 1 : std::stoi(match[1].str());
            if (n < 0)
                throw std::runtime_error(std::format(
                    "Invalid swapN operator: {} (idx {})", str_token, idx));
            t.payload = TokenPayload_StackOp{n};
        } else if (std::regex_match(str_token, match,
                                    std::regex(R"(^sort(\d+)$)"))) {
            t.type = TokenType::SORTN;
            int n = std::stoi(match[1].str());
            if (n < 0)
                throw std::runtime_error(std::format(
                    "Invalid sortN operator: {} (idx {})", str_token, idx));
            t.payload = TokenPayload_StackOp{n};
        } else if (!str_token.empty() && str_token.front() == '#') {
            t.type = TokenType::LABEL_DEF;
            t.payload = TokenPayload_Label{.name = str_token.substr(1)};
        } else if (!str_token.empty() && str_token.back() == '#') {
            t.type = TokenType::JUMP;
            t.payload = TokenPayload_Label{
                .name = str_token.substr(0, str_token.size() - 1)};
        } else if (std::regex_match(str_token, match, re_rel_bracket)) {
            t.type = TokenType::CLIP_REL;
            TokenPayload_ClipAccess data;
            data.clip_idx = parse_clip_idx_from_match(match);
            data.rel_x = std::stoi(match[3].str());
            data.rel_y = std::stoi(match[4].str());
            if (match[5].matched) {
                data.has_mode = true;
                data.use_mirror = (match[5].str() == "m");
            }
            t.payload = data;
        } else if (std::regex_match(str_token, match, re_abs)) {
            t.type = TokenType::CLIP_ABS;
            t.payload = TokenPayload_ClipAccess{
                .clip_idx = parse_clip_idx_from_match(match)};
        } else if (std::regex_match(str_token, match, re_cur)) {
            t.type = TokenType::CLIP_CUR;
            t.payload = TokenPayload_ClipAccess{
                .clip_idx = parse_clip_idx_from_match(match)};
        } else if (std::regex_match(str_token, match, re_prop)) {
            t.type = TokenType::PROP_ACCESS;
            t.payload = TokenPayload_PropAccess{
                .clip_idx = parse_clip_idx_from_match(match),
                .prop_name = match[3].str()};
        } else if (!str_token.empty() && str_token.back() == '!') {
            t.type = TokenType::VAR_STORE;
            t.payload = TokenPayload_Var{
                .name = str_token.substr(0, str_token.size() - 1)};
        } else if (!str_token.empty() && str_token.back() == '@') {
            t.type = TokenType::VAR_LOAD;
            t.payload = TokenPayload_Var{
                .name = str_token.substr(0, str_token.size() - 1)};
        } else {
            t.type = TokenType::NUMBER;
            try {
                size_t pos = 0;
                double val =
                    static_cast<double>(std::stoll(str_token, &pos, 0));
                if (pos != str_token.size()) { // Not a full integer match
                    pos = 0;
                    val = std::stod(str_token, &pos);
                    if (pos != str_token.size()) {
                        throw std::runtime_error(
                            std::format("Invalid number format (idx {})", idx));
                    }
                }
                t.payload = TokenPayload_Number{val};
            } catch (const std::exception&) {
                throw std::runtime_error(
                    std::format("Invalid token: {} (idx {})", str_token, idx));
            }
        }

        if (t.type == TokenType::CLIP_REL || t.type == TokenType::CLIP_ABS ||
            t.type == TokenType::CLIP_CUR) {
            if (std::get<TokenPayload_ClipAccess>(t.payload).clip_idx < 0 ||
                std::get<TokenPayload_ClipAccess>(t.payload).clip_idx >=
                    num_inputs) {
                throw std::runtime_error(
                    std::format("Invalid clip index in token: {} (idx {})",
                                str_token, idx));
            }
        } else if (t.type == TokenType::PROP_ACCESS) {
            if (std::get<TokenPayload_PropAccess>(t.payload).clip_idx < 0 ||
                std::get<TokenPayload_PropAccess>(t.payload).clip_idx >=
                    num_inputs) {
                throw std::runtime_error(
                    std::format("Invalid clip index in token: {} (idx {})",
                                str_token, idx));
            }
        }

        tokens.push_back(t);
        idx++;
    }
    return tokens;
}

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

class OrcJit {
  private:
    std::unique_ptr<llvm::orc::LLJIT> lljit;

  public:
    explicit OrcJit(bool no_nans_fp_math) {
        static struct LLVMInitializer {
            LLVMInitializer() {
                llvm::InitializeNativeTarget();
                llvm::InitializeNativeTargetAsmPrinter();
                llvm::InitializeNativeTargetAsmParser();
            }
        } initializer;

        auto jtmb =
            llvm::cantFail(llvm::orc::JITTargetMachineBuilder::detectHost());
        jtmb.setCodeGenOptLevel(llvm::CodeGenOptLevel::Aggressive);

        llvm::StringMap<bool> host_features = llvm::sys::getHostCPUFeatures();
        if (host_features.size() > 0) {
            std::vector<std::string> features;
            for (auto& f : host_features) {
                if (f.getValue()) {
                    features.push_back("+" + f.getKey().str());
                }
            }
            jtmb.addFeatures(features);
        }

        llvm::TargetOptions Opts;
        Opts.AllowFPOpFusion = llvm::FPOpFusion::Fast;
        Opts.UnsafeFPMath = true;
        Opts.NoInfsFPMath = true;
        Opts.NoNaNsFPMath = no_nans_fp_math;
        jtmb.setOptions(Opts);

        auto jit_builder = llvm::orc::LLJITBuilder();
        jit_builder.setJITTargetMachineBuilder(std::move(jtmb));
        auto temp_jit = jit_builder.create();
        if (!temp_jit) {
            llvm::errs() << "Failed to create LLJIT instance: "
                         << llvm::toString(temp_jit.takeError()) << "\n";
            throw std::runtime_error("LLJIT creation failed");
        }
        lljit = std::move(*temp_jit);
    }

    const llvm::DataLayout& getDataLayout() const {
        return lljit->getDataLayout();
    }

    void addModule(std::unique_ptr<llvm::Module> M,
                   std::unique_ptr<llvm::LLVMContext> Ctx) {
        llvm::cantFail(lljit->addIRModule(
            llvm::orc::ThreadSafeModule(std::move(M), std::move(Ctx))));
    }

    void* getFunctionAddress(const std::string& name) {
        auto sym = lljit->lookup(name);
        if (!sym) {
            llvm::errs() << "Failed to find symbol: "
                         << llvm::toString(sym.takeError()) << "\n";
            return nullptr;
        }
        return sym->toPtr<void*>();
    }
};

using ProcessProc = void (*)(uint8_t** rwptrs, const int* strides,
                             const float* props, int width, int height);

struct CompiledFunction {
    ProcessProc func_ptr = nullptr;
};

std::unordered_map<std::string, CompiledFunction> jit_cache;
std::mutex cache_mutex;
OrcJit global_jit_fast(true);
OrcJit global_jit_nan_safe(false);

class Compiler {
  private:
    std::vector<Token> tokens;
    const VSVideoInfo* vo;
    const std::vector<const VSVideoInfo*>& vi;
    int num_inputs;
    bool mirror_boundary;
    std::string dump_ir_path;
    const std::map<std::pair<int, std::string>, int>& prop_map;
    std::string func_name;
    bool uses_x = false;
    bool uses_y = false;

    std::unique_ptr<llvm::LLVMContext> context;
    std::unique_ptr<llvm::Module> module;
    llvm::IRBuilder<> builder;

    llvm::Function* func;
    llvm::Value* rwptrs_arg;
    llvm::Value* strides_arg;
    llvm::Value* props_arg;
    llvm::Value* width_arg;
    llvm::Value* height_arg;

    // Loop-invariant caches
    std::vector<llvm::Value*> preloaded_base_ptrs;
    std::vector<llvm::Value*> preloaded_strides;

    // Alias scope metadata (per rwptrs element)
    llvm::MDNode* alias_scope_domain = nullptr;
    std::vector<llvm::MDNode*> alias_scopes; // distinct scopes for each pointer
    std::vector<llvm::MDNode*> alias_scope_lists;   // !alias.scope lists (self)
    std::vector<llvm::MDNode*> noalias_scope_lists; // !noalias lists (others)

    // CFG and validation
    struct CFGBlock {
        int start_token_idx;
        int end_token_idx; // exclusive
        std::vector<int> successors;
        std::vector<int> predecessors;

        int stack_effect = 0;
        int min_stack_needed =
            0; // min stack depth *during* the block, relative to start
    };
    std::vector<CFGBlock> cfg_blocks;
    std::map<std::string, int> label_to_block_idx;
    struct RelYAccess {
        int clip_idx;
        int rel_y;
        bool use_mirror;

        bool operator<(const RelYAccess& other) const {
            return std::tie(clip_idx, rel_y, use_mirror) <
                   std::tie(other.clip_idx, other.rel_y, other.use_mirror);
        }
    };
    std::vector<RelYAccess> unique_rel_y_accesses;
    std::map<RelYAccess, llvm::Value*> row_ptr_cache;

    std::vector<int> stack_depth_in;

  public:
    Compiler(std::vector<Token>&& tokens_in, const VSVideoInfo* out_vi,
             const std::vector<const VSVideoInfo*>& in_vi, bool mirror,
             std::string dump_path,
             const std::map<std::pair<int, std::string>, int>& p_map,
             std::string function_name)
        : tokens(std::move(tokens_in)), vo(out_vi), vi(in_vi),
          num_inputs(in_vi.size()), mirror_boundary(mirror),
          dump_ir_path(std::move(dump_path)), prop_map(p_map),
          func_name(std::move(function_name)),
          context(std::make_unique<llvm::LLVMContext>()),
          module(std::make_unique<llvm::Module>("ExprJITModule", *context)),
          builder(*context) {
        for (const auto& token : tokens) {
            if (token.type == TokenType::CONSTANT_X)
                uses_x = true;
            if (token.type == TokenType::CONSTANT_Y)
                uses_y = true;
            if (uses_x && uses_y)
                break;
        }
    }

    CompiledFunction compile() {
        bool needs_nans = false;
        for (const auto& token : tokens) {
            if (token.type ==
                TokenType::EXIT_NO_WRITE) { // Akarin.Expr uses fast math even
                                            // when props not exist, so we only
                                            // check for `^exit^`.
                needs_nans = true;
                break;
            }
        }

        OrcJit& jit = needs_nans ? global_jit_nan_safe : global_jit_fast;

        llvm::FastMathFlags FMF;
        FMF.setFast();
        FMF.setNoNaNs(!needs_nans);
        builder.setFastMathFlags(FMF);

        validate_and_build_cfg();
        collect_rel_y_accesses();

        define_function_signature();

        {
            llvm::AttrBuilder FuncAttrs(func->getContext());
            if (FMF.allowContract()) {
                FuncAttrs.addAttribute("fp-contract", "fast");
            }
            if (FMF.approxFunc()) {
                FuncAttrs.addAttribute("approx-func-fp-math", "true");
            }
            if (FMF.noInfs()) {
                FuncAttrs.addAttribute("no-infs-fp-math", "true");
            }
            if (FMF.noNaNs()) {
                FuncAttrs.addAttribute("no-nans-fp-math", "true");
            }
            if (FMF.noSignedZeros()) {
                FuncAttrs.addAttribute("no-signed-zeros-fp-math", "true");
            }
            if (FMF.allowReciprocal()) {
                FuncAttrs.addAttribute("allow-reciprocal-fp-math", "true");
            }

            FuncAttrs.addAttribute(llvm::Attribute::NoUnwind);
            FuncAttrs.addAttribute(llvm::Attribute::WillReturn);
            func->addFnAttrs(FuncAttrs);
        }

        generate_loops();

        module->setDataLayout(jit.getDataLayout());

        if (llvm::verifyModule(*module, &llvm::errs())) {
            module->print(llvm::errs(), nullptr);
            throw std::runtime_error(
                "LLVM module verification failed (pre-opt).");
        }

        // Dump pre-optimization IR
        if (!dump_ir_path.empty()) {
            std::error_code EC;
            std::string pre_path = dump_ir_path + ".pre.ll";
            llvm::raw_fd_ostream dest_pre(pre_path, EC, llvm::sys::fs::OF_None);
            if (!EC) {
                module->print(dest_pre, nullptr);
                dest_pre.flush();
            }
        }

        {
            llvm::LoopAnalysisManager LAM;
            llvm::FunctionAnalysisManager FAM;
            llvm::CGSCCAnalysisManager CGAM;
            llvm::ModuleAnalysisManager MAM;

            llvm::PassBuilder PB;
            PB.registerModuleAnalyses(MAM);
            PB.registerFunctionAnalyses(FAM);
            PB.registerCGSCCAnalyses(CGAM);
            PB.registerLoopAnalyses(LAM);
            PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

            llvm::ModulePassManager MPM;
            if (auto Err = PB.parsePassPipeline(MPM, "default<O3>")) {
                llvm::errs() << "Failed to parse 'default<O3>' pipeline: "
                             << llvm::toString(std::move(Err)) << "\n";
                throw std::runtime_error(
                    "Failed to create default optimization pipeline.");
            }
            MPM.run(*module, MAM);
        }

        if (llvm::verifyModule(*module, &llvm::errs())) {
            module->print(llvm::errs(), nullptr);
            throw std::runtime_error("LLVM module verification failed.");
        }

        if (!dump_ir_path.empty()) {
            std::error_code EC;
            llvm::raw_fd_ostream dest(dump_ir_path, EC, llvm::sys::fs::OF_None);
            if (EC) {
                throw std::runtime_error(
                    "Could not open file: " + EC.message() +
                    " for writing IR to " + dump_ir_path);
            } else {
                module->print(dest, nullptr);
                dest.flush();
            }
        }

        jit.addModule(std::move(module), std::move(context));
        void* func_addr = jit.getFunctionAddress(func_name);

        if (!func_addr) {
            throw std::runtime_error("Failed to get JIT'd function address.");
        }

        CompiledFunction compiled;
        compiled.func_ptr = reinterpret_cast<ProcessProc>(func_addr);
        return compiled;
    }

  private:
    llvm::AllocaInst* createAllocaInEntry(llvm::Type* type,
                                          const std::string& name) {
        llvm::IRBuilder<> entryBuilder(&func->getEntryBlock(),
                                       func->getEntryBlock().begin());
        return entryBuilder.CreateAlloca(type, nullptr, name);
    }

    llvm::Value* createUnaryIntrinsicCall(llvm::Intrinsic::ID intrinsic_id,
                                          llvm::Value* arg) {
        auto* callee = llvm::Intrinsic::getOrInsertDeclaration(
            module.get(), intrinsic_id, {arg->getType()});
        auto* call = builder.CreateCall(callee, {arg});
        call->setFastMathFlags(builder.getFastMathFlags());
        return call;
    }

    llvm::Value* createBinaryIntrinsicCall(llvm::Intrinsic::ID intrinsic_id,
                                           llvm::Value* arg1,
                                           llvm::Value* arg2) {
        auto* callee = llvm::Intrinsic::getOrInsertDeclaration(
            module.get(), intrinsic_id, {arg1->getType()});
        auto* call = builder.CreateCall(callee, {arg1, arg2});
        call->setFastMathFlags(builder.getFastMathFlags());
        return call;
    }

    void assumeAligned(llvm::Value* ptrValue, unsigned alignment) {
        llvm::Function* assumeFn = llvm::Intrinsic::getOrInsertDeclaration(
            module.get(), llvm::Intrinsic::assume);
        llvm::Value* cond = builder.getInt1(true);
        llvm::SmallVector<llvm::Value*, 2> args;
        args.push_back(ptrValue);
        args.push_back(builder.getInt64(static_cast<uint64_t>(alignment)));
        llvm::OperandBundleDefT<llvm::Value*> alignBundle("align", args);
        builder.CreateCall(assumeFn, {cond}, {alignBundle});
    }

    template <typename MemInstT>
    void setMemoryInstAttrs(MemInstT* inst, unsigned alignment,
                            int rwptr_index) {
        inst->setAlignment(llvm::Align(alignment));
        inst->setMetadata(llvm::LLVMContext::MD_alias_scope,
                          alias_scope_lists[rwptr_index]);
        inst->setMetadata(llvm::LLVMContext::MD_noalias,
                          noalias_scope_lists[rwptr_index]);
    }

    void define_function_signature() {
        llvm::Type* void_ty = llvm::Type::getVoidTy(*context);
        llvm::Type* ptr_ty = llvm::PointerType::get(*context, 0);
        llvm::Type* i8_ptr_ptr_ty =
            ptr_ty; // opaque pointer (represents uint8_t**)
        llvm::Type* i32_ptr_ty = ptr_ty; // opaque pointer (represents int32_t*)
        llvm::Type* float_ptr_ty = ptr_ty; // opaque pointer (represents float*)
        llvm::Type* i32_ty = llvm::Type::getInt32Ty(*context);

        llvm::FunctionType* func_ty = llvm::FunctionType::get(
            void_ty, {i8_ptr_ptr_ty, i32_ptr_ty, float_ptr_ty, i32_ty, i32_ty},
            false);

        func = llvm::Function::Create(func_ty, llvm::Function::ExternalLinkage,
                                      func_name, module.get());

        auto args = func->arg_begin();
        rwptrs_arg = &*args++;
        rwptrs_arg->setName("rwptrs");
        strides_arg = &*args++;
        strides_arg->setName("strides");
        props_arg = &*args++;
        props_arg->setName("props");
        width_arg = &*args++;
        width_arg->setName("width");
        height_arg = &*args++;
        height_arg->setName("height");

        func->addParamAttr(1, llvm::Attribute::ReadOnly); // strides (int32_t*)
        func->addParamAttr(2, llvm::Attribute::ReadOnly); // props (float*)
    }

    void collect_rel_y_accesses() {
        std::set<RelYAccess> seen;
        for (const auto& token : tokens) {
            if (token.type == TokenType::CLIP_REL) {
                const auto& payload =
                    std::get<TokenPayload_ClipAccess>(token.payload);
                bool use_mirror =
                    payload.has_mode ? payload.use_mirror : mirror_boundary;
                RelYAccess access{payload.clip_idx, payload.rel_y, use_mirror};
                if (seen.find(access) == seen.end()) {
                    seen.insert(access);
                    unique_rel_y_accesses.push_back(access);
                }
            }
        }
    }

    llvm::Value* get_final_coord(llvm::Value* coord, llvm::Value* max_dim,
                                 bool use_mirror) {
        llvm::Value* zero = builder.getInt32(0);
        llvm::Value* one = builder.getInt32(1);

        llvm::Value* result;
        if (use_mirror) {
            // idx = abs(idx); N = 2*(len-1); if (N==0) N=1; r = idx % N;
            // result = (len-1) - abs(r - (len-1))
            llvm::Function* abs_func = llvm::Intrinsic::getOrInsertDeclaration(
                module.get(), llvm::Intrinsic::abs, {builder.getInt32Ty()});
            auto abs_coord =
                builder.CreateCall(abs_func, {coord, builder.getInt1(false)});

            auto dim_minus_1 = builder.CreateSub(max_dim, one);
            auto twice_dim_minus_1 =
                builder.CreateMul(dim_minus_1, builder.getInt32(2));

            // base = base | (base==0 ? 1 : 0)
            auto base_is_zero = builder.CreateICmpEQ(twice_dim_minus_1, zero);
            auto one_if_zero =
                builder.CreateZExt(base_is_zero, builder.getInt32Ty());
            auto safe_mod_base =
                builder.CreateOr(twice_dim_minus_1, one_if_zero);

            auto rem = builder.CreateURem(abs_coord, safe_mod_base);

            // result = (len-1) - abs(rem - (len-1))
            auto diff = builder.CreateSub(rem, dim_minus_1);
            auto abs_diff =
                builder.CreateCall(abs_func, {diff, builder.getInt1(false)});
            result = builder.CreateSub(dim_minus_1, abs_diff);
        } else { // Clamping
            // clamp(coord, 0, max_dim - 1)
            auto dim_minus_1 = builder.CreateSub(max_dim, one);

            llvm::Function* smax_func = llvm::Intrinsic::getOrInsertDeclaration(
                module.get(), llvm::Intrinsic::smax, {builder.getInt32Ty()});
            llvm::Function* smin_func = llvm::Intrinsic::getOrInsertDeclaration(
                module.get(), llvm::Intrinsic::smin, {builder.getInt32Ty()});

            auto clamped_at_zero = builder.CreateCall(smax_func, {coord, zero});
            result =
                builder.CreateCall(smin_func, {clamped_at_zero, dim_minus_1});
        }

        return result;
    }

    llvm::Value* generate_load_from_row_ptr(llvm::Value* row_ptr, int clip_idx,
                                            llvm::Value* x, int rel_x,
                                            bool use_mirror) {
        const VSVideoInfo* vinfo = vi[clip_idx];
        llvm::Value* clip_width = builder.getInt32(vinfo->width);
        llvm::Value* coord_x = builder.CreateAdd(x, builder.getInt32(rel_x));
        llvm::Value* final_x = get_final_coord(coord_x, clip_width, use_mirror);

        const VSFormat* format = vinfo->format;
        int bpp = format->bytesPerSample;
        int vs_clip_idx = clip_idx + 1;

        llvm::Value* x_offset =
            builder.CreateMul(final_x, builder.getInt32(bpp));
        llvm::Value* pixel_addr =
            builder.CreateGEP(builder.getInt8Ty(), row_ptr, x_offset);

        int pixel_align = std::gcd(ALIGNMENT, bpp);
        assumeAligned(pixel_addr, static_cast<unsigned>(pixel_align));

        llvm::Value* loaded_val;
        if (format->sampleType == stInteger) {
            llvm::Type* load_type =
                bpp == 1
                    ? builder.getInt8Ty()
                    : (bpp == 2 ? builder.getInt16Ty() : builder.getInt32Ty());
            llvm::LoadInst* li = builder.CreateLoad(load_type, pixel_addr);
            setMemoryInstAttrs(li, static_cast<unsigned>(pixel_align),
                               vs_clip_idx);
            loaded_val = builder.CreateZExtOrBitCast(li, builder.getInt32Ty());
            return builder.CreateUIToFP(loaded_val, builder.getFloatTy());
        } else { // stFloat
            if (bpp == 4) {
                llvm::LoadInst* li =
                    builder.CreateLoad(builder.getFloatTy(), pixel_addr);
                setMemoryInstAttrs(li, static_cast<unsigned>(pixel_align),
                                   vs_clip_idx);
                return li;
            } else if (bpp == 2) {
                llvm::LoadInst* li =
                    builder.CreateLoad(builder.getHalfTy(), pixel_addr);
                setMemoryInstAttrs(li, static_cast<unsigned>(pixel_align),
                                   vs_clip_idx);
                return builder.CreateFPExt(li, builder.getFloatTy());
            } else {
                throw std::runtime_error("Unsupported float sample size.");
            }
        }
    }

    void generate_loops() {
        llvm::BasicBlock* entry_bb =
            llvm::BasicBlock::Create(*context, "entry", func);
        builder.SetInsertPoint(entry_bb);

        llvm::Function* parent_func = builder.GetInsertBlock()->getParent();
        llvm::BasicBlock* loop_y_header =
            llvm::BasicBlock::Create(*context, "loop_y_header", parent_func);
        llvm::BasicBlock* loop_y_body =
            llvm::BasicBlock::Create(*context, "loop_y_body", parent_func);
        llvm::BasicBlock* loop_y_exit =
            llvm::BasicBlock::Create(*context, "loop_y_exit", parent_func);

        llvm::Value* y_var =
            builder.CreateAlloca(builder.getInt32Ty(), nullptr, "y.var");
        llvm::Value* x_var_entry =
            builder.CreateAlloca(builder.getInt32Ty(), nullptr, "x.var");
        builder.CreateStore(builder.getInt32(0), y_var);

        llvm::Value* x_fp_var = nullptr;
        if (uses_x) {
            x_fp_var = createAllocaInEntry(builder.getFloatTy(), "x_fp.var");
        }
        llvm::Value* y_fp_var = nullptr;
        if (uses_y) {
            y_fp_var = createAllocaInEntry(builder.getFloatTy(), "y_fp.var");
            builder.CreateStore(
                llvm::ConstantFP::get(builder.getFloatTy(), 0.0), y_fp_var);
        }

        // Index 0 = dst, 1..num_inputs = sources
        preloaded_base_ptrs.resize(num_inputs + 1);
        preloaded_strides.resize(num_inputs + 1);
        for (int i = 0; i <= num_inputs; ++i) {
            llvm::Value* base_ptr_i = builder.CreateLoad(
                llvm::PointerType::get(*context, 0),
                builder.CreateGEP(llvm::PointerType::get(*context, 0),
                                  rwptrs_arg, builder.getInt32(i)));
            llvm::Value* stride_i = builder.CreateLoad(
                builder.getInt32Ty(),
                builder.CreateGEP(builder.getInt32Ty(), strides_arg,
                                  builder.getInt32(i)));
            preloaded_base_ptrs[i] = base_ptr_i;
            preloaded_strides[i] = stride_i;

            // Assume each base pointer is 32-byte aligned (VapourSynth
            // guarantee)
            assumeAligned(base_ptr_i, ALIGNMENT);
        }

        // Build alias scopes so distinct rwptrs[i] are considered noalias
        alias_scope_domain = llvm::MDNode::getDistinct(*context, {});
        alias_scopes.resize(num_inputs + 1);
        for (int i = 0; i <= num_inputs; ++i) {
            // Create self-referential scope nodes
            llvm::SmallVector<llvm::Metadata*, 2> elems;
            elems.push_back(nullptr); // placeholder for self-reference
            // Wrap the string in an MDNode
            llvm::Metadata* name_node = llvm::MDNode::get(
                *context, {llvm::MDString::get(
                              *context, std::format("rwptrs_{}", i).c_str())});
            elems.push_back(name_node);
            alias_scopes[i] = llvm::MDNode::getDistinct(*context, elems);
            // Replace the placeholder with self-reference
            alias_scopes[i]->replaceOperandWith(0, alias_scopes[i]);
        }
        alias_scope_lists.resize(num_inputs + 1);
        noalias_scope_lists.resize(num_inputs + 1);
        for (int i = 0; i <= num_inputs; ++i) {
            std::vector<llvm::Metadata*> self_list = {alias_scopes[i]};
            alias_scope_lists[i] = llvm::MDNode::get(*context, self_list);
            std::vector<llvm::Metadata*> others;
            for (int j = 0; j <= num_inputs; ++j) {
                if (j == i)
                    continue;
                others.push_back(alias_scopes[j]);
            }
            noalias_scope_lists[i] = llvm::MDNode::get(*context, others);
        }

        builder.CreateBr(loop_y_header);

        builder.SetInsertPoint(loop_y_header);
        llvm::Value* y_val =
            builder.CreateLoad(builder.getInt32Ty(), y_var, "y");
        llvm::Value* y_cond =
            builder.CreateICmpSLT(y_val, height_arg, "y.cond");
        builder.CreateCondBr(y_cond, loop_y_body, loop_y_exit);

        builder.SetInsertPoint(loop_y_body);

        // Pre-calculate and cache row pointers for all unique relative Y accesses
        row_ptr_cache.clear();
        for (const auto& access : unique_rel_y_accesses) {
            const VSVideoInfo* vinfo = vi[access.clip_idx];
            llvm::Value* clip_height = builder.getInt32(vinfo->height);
            llvm::Value* coord_y =
                builder.CreateAdd(y_val, builder.getInt32(access.rel_y));
            llvm::Value* final_y =
                get_final_coord(coord_y, clip_height, access.use_mirror);

            int vs_clip_idx = access.clip_idx + 1;
            llvm::Value* base_ptr = preloaded_base_ptrs[vs_clip_idx];
            llvm::Value* stride = preloaded_strides[vs_clip_idx];

            llvm::Value* y_offset = builder.CreateMul(final_y, stride);
            llvm::Value* row_ptr = builder.CreateGEP(
                builder.getInt8Ty(), base_ptr, y_offset, "row_ptr");
            row_ptr_cache[access] = row_ptr;
        }

        llvm::BasicBlock* loop_x_header =
            llvm::BasicBlock::Create(*context, "loop_x_header", parent_func);
        llvm::BasicBlock* loop_x_body =
            llvm::BasicBlock::Create(*context, "loop_x_body", parent_func);
        llvm::BasicBlock* loop_x_exit =
            llvm::BasicBlock::Create(*context, "loop_x_exit", parent_func);

        // Reset x.var at the start of each row
        llvm::Value* x_var = x_var_entry;
        builder.CreateStore(builder.getInt32(0), x_var);
        if (uses_x) {
            builder.CreateStore(
                llvm::ConstantFP::get(builder.getFloatTy(), 0.0), x_fp_var);
        }
        builder.CreateBr(loop_x_header);

        builder.SetInsertPoint(loop_x_header);
        llvm::Value* x_val =
            builder.CreateLoad(builder.getInt32Ty(), x_var, "x");
        llvm::Value* x_cond = builder.CreateICmpSLT(x_val, width_arg, "x.cond");

        // Add loop metadata to hint vectorization/interleaving
        llvm::Metadata* enable_vec[] = {
            llvm::MDString::get(*context, "llvm.loop.vectorize.enable"),
            llvm::ConstantAsMetadata::get(
                llvm::ConstantInt::get(llvm::Type::getInt1Ty(*context), 1))};
        llvm::MDNode* enable_vec_node = llvm::MDNode::get(*context, enable_vec);

        llvm::Metadata* interleave_md[] = {
            llvm::MDString::get(*context, "llvm.loop.interleave.count"),
            llvm::ConstantAsMetadata::get(
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context), 4))};
        llvm::MDNode* interleave_node =
            llvm::MDNode::get(*context, interleave_md);

        llvm::SmallVector<llvm::Metadata*, 4> loop_md_elems;
        loop_md_elems.push_back(nullptr); // to be replaced with self reference
        loop_md_elems.push_back(enable_vec_node);
        loop_md_elems.push_back(interleave_node);
        llvm::MDNode* loop_id =
            llvm::MDNode::getDistinct(*context, loop_md_elems);
        loop_id->replaceOperandWith(0, loop_id);

        llvm::BranchInst* loop_br =
            builder.CreateCondBr(x_cond, loop_x_body, loop_x_exit);
        loop_br->setMetadata(llvm::LLVMContext::MD_loop, loop_id);

        builder.SetInsertPoint(loop_x_body);
        llvm::Value* x_fp = nullptr;
        if (uses_x) {
            x_fp = builder.CreateLoad(builder.getFloatTy(), x_fp_var, "x_fp");
        }
        llvm::Value* y_fp = nullptr;
        if (uses_y) {
            y_fp = builder.CreateLoad(builder.getFloatTy(), y_fp_var, "y_fp");
        }
        generate_ir_from_tokens(x_val, y_val, x_fp, y_fp);

        llvm::Value* x_next = builder.CreateAdd(x_val, builder.getInt32(1));
        builder.CreateStore(x_next, x_var);
        if (uses_x) {
            llvm::Value* x_fp_next = builder.CreateFAdd(
                x_fp, llvm::ConstantFP::get(builder.getFloatTy(), 1.0));
            builder.CreateStore(x_fp_next, x_fp_var);
        }
        builder.CreateBr(loop_x_header);

        builder.SetInsertPoint(loop_x_exit);
        llvm::Value* y_next = builder.CreateAdd(y_val, builder.getInt32(1));
        builder.CreateStore(y_next, y_var);
        if (uses_y) {
            llvm::Value* y_fp_val =
                builder.CreateLoad(builder.getFloatTy(), y_fp_var);
            llvm::Value* y_fp_next = builder.CreateFAdd(
                y_fp_val, llvm::ConstantFP::get(builder.getFloatTy(), 1.0));
            builder.CreateStore(y_fp_next, y_fp_var);
        }
        builder.CreateBr(loop_y_header);

        builder.SetInsertPoint(loop_y_exit);
        builder.CreateRetVoid();
    }

    int get_stack_effect(const Token& token) {
        switch (token.type) {
        // PUSH 1
        case TokenType::NUMBER:
        case TokenType::CONSTANT_X:
        case TokenType::CONSTANT_Y:
        case TokenType::CONSTANT_WIDTH:
        case TokenType::CONSTANT_HEIGHT:
        case TokenType::CONSTANT_N:
        case TokenType::CONSTANT_PI:
        case TokenType::VAR_LOAD:
        case TokenType::CLIP_REL:
        case TokenType::CLIP_CUR:
        case TokenType::PROP_ACCESS:
            return 1;

        // PUSH 1, POP 2
        case TokenType::CLIP_ABS:
            return -1;

        // PUSH 1, from thin air
        case TokenType::EXIT_NO_WRITE:
            return 1;

        // UNARY: PUSH 1, POP 1
        case TokenType::NOT:
        case TokenType::BITNOT:
        case TokenType::SQRT:
        case TokenType::EXP:
        case TokenType::LOG:
        case TokenType::ABS:
        case TokenType::FLOOR:
        case TokenType::CEIL:
        case TokenType::TRUNC:
        case TokenType::ROUND:
        case TokenType::SIN:
        case TokenType::COS:
        case TokenType::TAN:
        case TokenType::ASIN:
        case TokenType::ACOS:
        case TokenType::ATAN:
        case TokenType::EXP2:
        case TokenType::LOG10:
        case TokenType::LOG2:
        case TokenType::SINH:
        case TokenType::COSH:
        case TokenType::TANH:
        case TokenType::SGN:
        case TokenType::NEG:
            return 0;

        // BINARY: PUSH 1, POP 2
        case TokenType::ADD:
        case TokenType::SUB:
        case TokenType::MUL:
        case TokenType::DIV:
        case TokenType::MOD:
        case TokenType::GT:
        case TokenType::LT:
        case TokenType::GE:
        case TokenType::LE:
        case TokenType::EQ:
        case TokenType::AND:
        case TokenType::OR:
        case TokenType::XOR:
        case TokenType::BITAND:
        case TokenType::BITOR:
        case TokenType::BITXOR:
        case TokenType::POW:
        case TokenType::ATAN2:
        case TokenType::COPYSIGN:
        case TokenType::MIN:
        case TokenType::MAX:
            return -1;

        // TERNARY: PUSH 1, POP 3
        case TokenType::TERNARY:
        case TokenType::CLIP:
        case TokenType::CLAMP:
        case TokenType::FMA:
            return -2;

        // POP 3
        case TokenType::STORE_ABS:
            return -3;

        // STACK
        case TokenType::DUP:
            return 1;
        case TokenType::DROP: {
            const auto& payload = std::get<TokenPayload_StackOp>(token.payload);
            return -payload.n;
        }
        case TokenType::SWAP:
            return 0;
        case TokenType::SORTN:
            return 0; // pops N, pushes N

        // VAR
        case TokenType::VAR_STORE:
            return -1;

        // CONTROL FLOW
        case TokenType::LABEL_DEF:
            return 0;
        case TokenType::JUMP: // Consumes one value from stack for condition
            return -1;
        }
        return 0; // Should not happen
    }

    void validate_and_build_cfg() {
        if (tokens.empty()) {
            throw std::runtime_error("Expression cannot be empty.");
        }

        // Clear previous state
        cfg_blocks.clear();
        label_to_block_idx.clear();
        stack_depth_in.clear();

        std::map<int, int> token_idx_to_block_idx;

        int current_token_idx = 0;
        while (static_cast<size_t>(current_token_idx) < tokens.size()) {
            int block_idx = cfg_blocks.size();
            CFGBlock block;
            block.start_token_idx = current_token_idx;

            int block_start_idx = current_token_idx;
            token_idx_to_block_idx[block_start_idx] = block_idx;

            if (tokens[current_token_idx].type == TokenType::LABEL_DEF) {
                const auto& payload = std::get<TokenPayload_Label>(
                    tokens[current_token_idx].payload);
                if (label_to_block_idx.count(payload.name)) {
                    throw std::runtime_error(
                        std::format("Duplicate label: {} (idx {})",
                                    payload.name, current_token_idx));
                }
                label_to_block_idx[payload.name] = block_idx;
            }

            int scan_idx = current_token_idx;
            while (static_cast<size_t>(scan_idx) < tokens.size()) {
                const auto& token = tokens[scan_idx];
                if (token.type == TokenType::JUMP) {
                    scan_idx++;
                    break;
                }
                if (scan_idx > block_start_idx &&
                    token.type == TokenType::LABEL_DEF) {
                    break;
                }
                scan_idx++;
            }
            block.end_token_idx = scan_idx;
            cfg_blocks.push_back(block);
            current_token_idx = scan_idx;
        }

        for (size_t i = 0; i < cfg_blocks.size(); ++i) {
            CFGBlock& block = cfg_blocks[i];
            int current_stack = 0;
            for (int j = block.start_token_idx; j < block.end_token_idx; ++j) {
                const auto& token = tokens[j];
                int effect = get_stack_effect(token);

                if (current_stack + effect < 0) {
                    block.min_stack_needed = std::min(block.min_stack_needed,
                                                      current_stack + effect);
                }
                current_stack += effect;
            }
            block.stack_effect = current_stack;
            block.min_stack_needed = -block.min_stack_needed;

            const auto& last_token = tokens[block.end_token_idx - 1];
            if (last_token.type == TokenType::JUMP) {
                const auto& payload =
                    std::get<TokenPayload_Label>(last_token.payload);
                if (label_to_block_idx.find(payload.name) ==
                    label_to_block_idx.end()) {
                    throw std::runtime_error(
                        std::format("Undefined label for jump: {} (idx {})",
                                    payload.name, block.end_token_idx - 1));
                }
                int target_block_idx = label_to_block_idx.at(payload.name);
                block.successors.push_back(target_block_idx);
                cfg_blocks[target_block_idx].predecessors.push_back(i);

                if (static_cast<size_t>(block.end_token_idx) < tokens.size()) {
                    int fallthrough_block_idx =
                        token_idx_to_block_idx.at(block.end_token_idx);
                    block.successors.push_back(fallthrough_block_idx);
                    cfg_blocks[fallthrough_block_idx].predecessors.push_back(i);
                }
            } else {
                if (static_cast<size_t>(block.end_token_idx) < tokens.size()) {
                    int fallthrough_block_idx =
                        token_idx_to_block_idx.at(block.end_token_idx);
                    block.successors.push_back(fallthrough_block_idx);
                    cfg_blocks[fallthrough_block_idx].predecessors.push_back(i);
                }
            }
        }

        // Data-flow analysis for initialized variables
        std::set<std::string> all_vars;
        for (const auto& token : tokens) {
            if (token.type == TokenType::VAR_STORE ||
                token.type == TokenType::VAR_LOAD) {
                all_vars.insert(std::get<TokenPayload_Var>(token.payload).name);
            }
        }

        std::vector<std::set<std::string>> gen_sets(cfg_blocks.size());
        for (size_t i = 0; i < cfg_blocks.size(); ++i) {
            for (int j = cfg_blocks[i].start_token_idx;
                 j < cfg_blocks[i].end_token_idx; ++j) {
                if (tokens[j].type == TokenType::VAR_STORE) {
                    gen_sets[i].insert(
                        std::get<TokenPayload_Var>(tokens[j].payload).name);
                }
            }
        }

        std::vector<std::set<std::string>> in_sets(cfg_blocks.size());
        std::vector<std::set<std::string>> out_sets(cfg_blocks.size(),
                                                    all_vars);

        bool changed = true;
        while (changed) {
            changed = false;
            for (size_t i = 0; i < cfg_blocks.size(); ++i) {
                // IN[i] = Intersection of OUT[p] for all predecessors p
                std::set<std::string> new_in;
                if (cfg_blocks[i].predecessors.empty()) {
                    // Entry block or unreachable. IN set is empty.
                    new_in.clear();
                } else {
                    new_in = out_sets[cfg_blocks[i].predecessors[0]];
                    for (size_t j = 1; j < cfg_blocks[i].predecessors.size();
                         ++j) {
                        int p_idx = cfg_blocks[i].predecessors[j];
                        std::set<std::string> temp_intersect;
                        std::set_intersection(
                            new_in.begin(), new_in.end(),
                            out_sets[p_idx].begin(), out_sets[p_idx].end(),
                            std::inserter(temp_intersect,
                                          temp_intersect.begin()));
                        new_in = temp_intersect;
                    }
                }
                in_sets[i] = new_in;

                // OUT[i] = GEN[i] U IN[i]
                std::set<std::string> new_out = gen_sets[i];
                new_out.insert(in_sets[i].begin(), in_sets[i].end());

                if (new_out != out_sets[i]) {
                    out_sets[i] = new_out;
                    changed = true;
                }
            }
        }

        // Validate variables
        for (size_t i = 0; i < cfg_blocks.size(); ++i) {
            std::set<std::string> defined_in_block = in_sets[i];
            for (int j = cfg_blocks[i].start_token_idx;
                 j < cfg_blocks[i].end_token_idx; ++j) {
                const auto& token = tokens[j];
                if (token.type == TokenType::VAR_LOAD) {
                    const auto& payload =
                        std::get<TokenPayload_Var>(token.payload);
                    if (defined_in_block.find(payload.name) ==
                        defined_in_block.end()) {
                        throw std::runtime_error(std::format(
                            "Variable is uninitialized: {} (idx {})",
                            payload.name, j));
                    }
                } else if (token.type == TokenType::VAR_STORE) {
                    defined_in_block.insert(
                        std::get<TokenPayload_Var>(token.payload).name);
                }
            }
        }

        stack_depth_in.assign(cfg_blocks.size(), -1);
        std::vector<int> worklist;

        if (!cfg_blocks.empty()) {
            stack_depth_in[0] = 0;
            worklist.push_back(0);
        }

        size_t processed_count = 0;
        while (!worklist.empty()) {
            processed_count++;
            if (processed_count >
                cfg_blocks.size() * cfg_blocks.size()) { // Heuristic
                throw std::runtime_error(std::format(
                    "Failed to prove stack safety; check loops. "
                    "processed_count = {}, "
                    "cfg_blocks = {}, heuristic_limit = {}, worklist_size = "
                    "{}, next_block_candidate = {}",
                    processed_count, cfg_blocks.size(),
                    cfg_blocks.size() * cfg_blocks.size(), worklist.size(),
                    worklist.empty() ? -1 : worklist.back()));
            }

            int block_idx = worklist.back();
            worklist.pop_back();

            const auto& block = cfg_blocks[block_idx];
            int depth_in = stack_depth_in[block_idx];

            if (depth_in < block.min_stack_needed) {
                throw std::runtime_error(std::format(
                    "Stack underflow before executing block {}: depth_in = {}, "
                    "min_needed = {}. "
                    "start token '{}' (idx {}).",
                    block_idx, depth_in, block.min_stack_needed,
                    tokens[block.start_token_idx].text, block.start_token_idx));
            }

            int depth_out = depth_in + block.stack_effect;

            for (int succ_idx : block.successors) {
                if (stack_depth_in[succ_idx] == -1) {
                    stack_depth_in[succ_idx] = depth_out;
                    worklist.push_back(succ_idx);
                } else if (stack_depth_in[succ_idx] != depth_out) {
                    throw std::runtime_error(std::format(
                        "Stack depth mismatch on converging paths: block {} -> "
                        "successor {}. "
                        "incoming depth = {}, recorded successor depth = {}. "
                        "current start token '{}' (idx {}), successor start "
                        "token '{}' (idx {}).",
                        block_idx, succ_idx, depth_out,
                        stack_depth_in[succ_idx],
                        tokens[block.start_token_idx].text,
                        block.start_token_idx,
                        tokens[cfg_blocks[succ_idx].start_token_idx].text,
                        cfg_blocks[succ_idx].start_token_idx));
                }
            }
        }

        for (size_t i = 0; i < cfg_blocks.size(); ++i) {
            if (stack_depth_in[i] != -1 &&
                cfg_blocks[i].successors.empty()) { // Reachable terminal block
                int final_depth =
                    stack_depth_in[i] + cfg_blocks[i].stack_effect;
                if (final_depth != 1) {
                    throw std::runtime_error(
                        std::format("Expression stack not balanced on "
                                    "reachable terminal block {}: "
                                    "final depth = {}, expected = 1. start "
                                    "token '{}' (idx {}).",
                                    i, final_depth,
                                    tokens[cfg_blocks[i].start_token_idx].text,
                                    cfg_blocks[i].start_token_idx));
                }
            }
        }
    }

    void generate_ir_from_tokens(llvm::Value* x, llvm::Value* y,
                                 llvm::Value* x_fp, llvm::Value* y_fp) {
        llvm::Type* float_ty = builder.getFloatTy();
        llvm::Type* i32_ty = builder.getInt32Ty();
        llvm::Function* parent_func = builder.GetInsertBlock()->getParent();

        if (tokens.empty()) {
            generate_pixel_store(llvm::ConstantFP::get(float_ty, 0.0), x, y);
            return;
        }

        // Pre-scan all variables and allocate them in the entry block
        std::unordered_map<std::string, llvm::Value*> named_vars;
        std::set<std::string> all_vars;

        for (const auto& token : tokens) {
            if (token.type == TokenType::VAR_STORE ||
                token.type == TokenType::VAR_LOAD) {
                const auto& payload = std::get<TokenPayload_Var>(token.payload);
                all_vars.insert(payload.name);
            }
        }

        // Allocate all variables in the entry block
        for (const std::string& var_name : all_vars) {
            named_vars[var_name] = createAllocaInEntry(float_ty, var_name);
        }

        // Create all Basic Blocks
        std::map<int, llvm::BasicBlock*> llvm_blocks;
        for (size_t i = 0; i < cfg_blocks.size(); ++i) {
            std::string name = "b" + std::to_string(i);
            for (const auto& [label_name, block_idx] : label_to_block_idx) {
                if (block_idx == static_cast<int>(i)) {
                    name = label_name;
                    break;
                }
            }
            llvm_blocks[i] =
                llvm::BasicBlock::Create(*context, name, parent_func);
        }
        llvm::BasicBlock* exit_bb =
            llvm::BasicBlock::Create(*context, "exit", parent_func);

        // Branch from current block to the first CFG block
        builder.CreateBr(llvm_blocks[0]);

        // Initial PHI generation for merge blocks
        std::map<int, std::vector<llvm::Value*>> block_initial_stacks;
        for (size_t i = 0; i < cfg_blocks.size(); ++i) {
            if (cfg_blocks[i].predecessors.size() > 1) {
                builder.SetInsertPoint(llvm_blocks[i]);
                std::vector<llvm::Value*> initial_stack;
                int depth = stack_depth_in[i];
                for (int j = 0; j < depth; ++j) {
                    initial_stack.push_back(builder.CreatePHI(
                        float_ty, cfg_blocks[i].predecessors.size()));
                }
                block_initial_stacks[i] = initial_stack;
            }
        }

        // Process blocks
        std::map<int, std::vector<llvm::Value*>> block_final_stacks;

        for (size_t i = 0; i < cfg_blocks.size(); ++i) {
            const auto& block_info = cfg_blocks[i];
            builder.SetInsertPoint(llvm_blocks[i]);

            std::vector<llvm::Value*> rpn_stack;
            if (block_info.predecessors.empty()) {
                // Entry block, empty stack
            } else if (block_info.predecessors.size() == 1) {
                int pred_idx = block_info.predecessors[0];
                // This can fail on first pass of a loop, so we use
                // get-or-create
                if (block_final_stacks.count(pred_idx)) {
                    rpn_stack = block_final_stacks.at(pred_idx);
                }
            } else {
                rpn_stack = block_initial_stacks.at(i);
            }

            for (int j = block_info.start_token_idx;
                 j < block_info.end_token_idx; ++j) {
                const auto& token = tokens[j];

                auto applyBinaryOp = [&](auto opCallable) {
                    auto b = rpn_stack.back();
                    rpn_stack.pop_back();
                    auto a = rpn_stack.back();
                    rpn_stack.pop_back();
                    rpn_stack.push_back(opCallable(a, b));
                };

                auto applyBinaryIntrinsic =
                    [&](llvm::Intrinsic::ID intrinsic_id) {
                        auto b = rpn_stack.back();
                        rpn_stack.pop_back();
                        auto a = rpn_stack.back();
                        rpn_stack.pop_back();
                        rpn_stack.push_back(
                            createBinaryIntrinsicCall(intrinsic_id, a, b));
                    };

                auto applyBinaryCmp = [&](llvm::CmpInst::Predicate pred) {
                    auto b = rpn_stack.back();
                    rpn_stack.pop_back();
                    auto a = rpn_stack.back();
                    rpn_stack.pop_back();
                    auto cmp = builder.CreateFCmp(pred, a, b);
                    rpn_stack.push_back(builder.CreateSelect(
                        cmp, llvm::ConstantFP::get(float_ty, 1.0),
                        llvm::ConstantFP::get(float_ty, 0.0)));
                };

                enum class BoolBinOp { And, Or, Xor };
                auto applyLogicalOp = [&](BoolBinOp which) {
                    auto b_val = rpn_stack.back();
                    rpn_stack.pop_back();
                    auto a_val = rpn_stack.back();
                    rpn_stack.pop_back();
                    auto a_bool = builder.CreateFCmpOGT(
                        a_val, llvm::ConstantFP::get(float_ty, 0.0));
                    auto b_bool = builder.CreateFCmpOGT(
                        b_val, llvm::ConstantFP::get(float_ty, 0.0));
                    llvm::Value* logic_res = nullptr;
                    switch (which) {
                    case BoolBinOp::And:
                        logic_res = builder.CreateAnd(a_bool, b_bool);
                        break;
                    case BoolBinOp::Or:
                        logic_res = builder.CreateOr(a_bool, b_bool);
                        break;
                    case BoolBinOp::Xor:
                        logic_res = builder.CreateXor(a_bool, b_bool);
                        break;
                    }
                    rpn_stack.push_back(builder.CreateSelect(
                        logic_res, llvm::ConstantFP::get(float_ty, 1.0),
                        llvm::ConstantFP::get(float_ty, 0.0)));
                };

                enum class IntBinOp { And, Or, Xor };
                auto applyBitwiseOp = [&](IntBinOp which) {
                    auto b = rpn_stack.back();
                    rpn_stack.pop_back();
                    auto a = rpn_stack.back();
                    rpn_stack.pop_back();
                    auto ai = builder.CreateFPToSI(a, i32_ty);
                    auto bi = builder.CreateFPToSI(b, i32_ty);
                    llvm::Value* resi = nullptr;
                    switch (which) {
                    case IntBinOp::And:
                        resi = builder.CreateAnd(ai, bi);
                        break;
                    case IntBinOp::Or:
                        resi = builder.CreateOr(ai, bi);
                        break;
                    case IntBinOp::Xor:
                        resi = builder.CreateXor(ai, bi);
                        break;
                    }
                    rpn_stack.push_back(builder.CreateSIToFP(resi, float_ty));
                };

                auto applyUnaryIntrinsic =
                    [&](llvm::Intrinsic::ID intrinsic_id) {
                        auto a = rpn_stack.back();
                        rpn_stack.pop_back();
                        rpn_stack.push_back(
                            createUnaryIntrinsicCall(intrinsic_id, a));
                    };

                switch (token.type) {
                // Literals & Constants
                case TokenType::NUMBER: {
                    const auto& payload =
                        std::get<TokenPayload_Number>(token.payload);
                    rpn_stack.push_back(
                        llvm::ConstantFP::get(float_ty, payload.value));
                    break;
                }
                case TokenType::CONSTANT_X:
                    rpn_stack.push_back(x_fp);
                    break;
                case TokenType::CONSTANT_Y:
                    rpn_stack.push_back(y_fp);
                    break;
                case TokenType::CONSTANT_WIDTH:
                    rpn_stack.push_back(
                        builder.CreateSIToFP(width_arg, float_ty));
                    break;
                case TokenType::CONSTANT_HEIGHT:
                    rpn_stack.push_back(
                        builder.CreateSIToFP(height_arg, float_ty));
                    break;
                case TokenType::CONSTANT_N:
                    rpn_stack.push_back(builder.CreateLoad(
                        float_ty, builder.CreateGEP(float_ty, props_arg,
                                                    builder.getInt32(0))));
                    break;
                case TokenType::CONSTANT_PI:
                    rpn_stack.push_back(
                        llvm::ConstantFP::get(float_ty, std::numbers::pi));
                    break;

                // Variable Ops
                case TokenType::VAR_STORE: {
                    const auto& payload =
                        std::get<TokenPayload_Var>(token.payload);
                    llvm::Value* val_to_store = rpn_stack.back();
                    rpn_stack.pop_back();
                    llvm::Value* var_ptr = named_vars[payload.name];
                    builder.CreateStore(val_to_store, var_ptr);
                    break;
                }
                case TokenType::VAR_LOAD: {
                    const auto& payload =
                        std::get<TokenPayload_Var>(token.payload);
                    llvm::Value* var_ptr = named_vars[payload.name];
                    rpn_stack.push_back(builder.CreateLoad(float_ty, var_ptr));
                    break;
                }

                // Data Access
                case TokenType::CLIP_REL: {
                    const auto& payload =
                        std::get<TokenPayload_ClipAccess>(token.payload);
                    bool use_mirror =
                        payload.has_mode ? payload.use_mirror : mirror_boundary;
                    RelYAccess access{payload.clip_idx, payload.rel_y,
                                      use_mirror};
                    llvm::Value* row_ptr = row_ptr_cache.at(access);
                    rpn_stack.push_back(
                        generate_load_from_row_ptr(row_ptr, payload.clip_idx, x,
                                                   payload.rel_x, use_mirror));
                    break;
                }
                case TokenType::CLIP_ABS: {
                    const auto& payload =
                        std::get<TokenPayload_ClipAccess>(token.payload);
                    llvm::Value* coord_y_f = rpn_stack.back();
                    rpn_stack.pop_back();
                    llvm::Value* coord_x_f = rpn_stack.back();
                    rpn_stack.pop_back();

                    llvm::Value* coord_y = builder.CreateCall(
                        llvm::Intrinsic::getOrInsertDeclaration(
                            module.get(), llvm::Intrinsic::rint, {float_ty}),
                        {coord_y_f});
                    coord_y = builder.CreateFPToSI(coord_y, i32_ty);

                    llvm::Value* coord_x = builder.CreateCall(
                        llvm::Intrinsic::getOrInsertDeclaration(
                            module.get(), llvm::Intrinsic::rint, {float_ty}),
                        {coord_x_f});
                    coord_x = builder.CreateFPToSI(coord_x, i32_ty);

                    rpn_stack.push_back(generate_pixel_load(
                        payload.clip_idx, coord_x, coord_y, mirror_boundary));
                    break;
                }
                case TokenType::CLIP_CUR: {
                    const auto& payload =
                        std::get<TokenPayload_ClipAccess>(token.payload);
                    rpn_stack.push_back(generate_pixel_load(
                        payload.clip_idx, x, y, mirror_boundary));
                    break;
                }
                case TokenType::PROP_ACCESS: {
                    const auto& payload =
                        std::get<TokenPayload_PropAccess>(token.payload);
                    auto key =
                        std::make_pair(payload.clip_idx, payload.prop_name);
                    int prop_idx = prop_map.at(key);
                    llvm::Value* prop_val = builder.CreateLoad(
                        float_ty,
                        builder.CreateGEP(float_ty, props_arg,
                                          builder.getInt32(prop_idx)));
                    rpn_stack.push_back(prop_val);
                    break;
                }

                // Binary Operators
                case TokenType::ADD: {
                    applyBinaryOp([&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateFAdd(a, b);
                    });
                    break;
                }
                case TokenType::SUB: {
                    applyBinaryOp([&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateFSub(a, b);
                    });
                    break;
                }
                case TokenType::MUL: {
                    applyBinaryOp([&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateFMul(a, b);
                    });
                    break;
                }
                case TokenType::DIV: {
                    applyBinaryOp([&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateFDiv(a, b);
                    });
                    break;
                }
                case TokenType::MOD: {
                    applyBinaryOp([&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateFRem(a, b);
                    });
                    break;
                }
                case TokenType::POW: {
                    applyBinaryIntrinsic(llvm::Intrinsic::pow);
                    break;
                }
                case TokenType::ATAN2: {
                    applyBinaryIntrinsic(llvm::Intrinsic::atan2);
                    break;
                }
                case TokenType::COPYSIGN: {
                    applyBinaryIntrinsic(llvm::Intrinsic::copysign);
                    break;
                }
                case TokenType::MIN: {
                    applyBinaryIntrinsic(llvm::Intrinsic::minnum);
                    break;
                }
                case TokenType::MAX: {
                    applyBinaryIntrinsic(llvm::Intrinsic::maxnum);
                    break;
                }

                // Binary comparisons
                case TokenType::GT: {
                    applyBinaryCmp(llvm::CmpInst::FCMP_OGT);
                    break;
                }
                case TokenType::LT: {
                    applyBinaryCmp(llvm::CmpInst::FCMP_OLT);
                    break;
                }
                case TokenType::GE: {
                    applyBinaryCmp(llvm::CmpInst::FCMP_OGE);
                    break;
                }
                case TokenType::LE: {
                    applyBinaryCmp(llvm::CmpInst::FCMP_OLE);
                    break;
                }
                case TokenType::EQ: {
                    applyBinaryCmp(llvm::CmpInst::FCMP_OEQ);
                    break;
                }

                // Logical ops on booleanized floats
                case TokenType::AND: {
                    applyLogicalOp(BoolBinOp::And);
                    break;
                }
                case TokenType::OR: {
                    applyLogicalOp(BoolBinOp::Or);
                    break;
                }
                case TokenType::XOR: {
                    applyLogicalOp(BoolBinOp::Xor);
                    break;
                }

                // Bitwise ops on converted ints
                case TokenType::BITAND: {
                    applyBitwiseOp(IntBinOp::And);
                    break;
                }
                case TokenType::BITOR: {
                    applyBitwiseOp(IntBinOp::Or);
                    break;
                }
                case TokenType::BITXOR: {
                    applyBitwiseOp(IntBinOp::Xor);
                    break;
                }

                // Unary Operators
                case TokenType::SQRT: {
                    // Akarin.Expr and JITASM std.Expr both apply max(0, x)
                    // before sqrt, so we do the same for compatibility.
                    // See:
                    // https://github.com/vapoursynth/vapoursynth/issues/1112
                    auto a = rpn_stack.back();
                    rpn_stack.pop_back();
                    auto zero = llvm::ConstantFP::get(float_ty, 0.0);
                    auto max_val = createBinaryIntrinsicCall(
                        llvm::Intrinsic::maxnum, a, zero);
                    rpn_stack.push_back(createUnaryIntrinsicCall(
                        llvm::Intrinsic::sqrt, max_val));
                    break;
                }
                case TokenType::EXP: {
                    applyUnaryIntrinsic(llvm::Intrinsic::exp);
                    break;
                }
                case TokenType::LOG: {
                    applyUnaryIntrinsic(llvm::Intrinsic::log);
                    break;
                }
                case TokenType::ABS: {
                    applyUnaryIntrinsic(llvm::Intrinsic::fabs);
                    break;
                }
                case TokenType::FLOOR: {
                    applyUnaryIntrinsic(llvm::Intrinsic::floor);
                    break;
                }
                case TokenType::CEIL: {
                    applyUnaryIntrinsic(llvm::Intrinsic::ceil);
                    break;
                }
                case TokenType::TRUNC: {
                    applyUnaryIntrinsic(llvm::Intrinsic::trunc);
                    break;
                }
                case TokenType::ROUND: {
                    applyUnaryIntrinsic(llvm::Intrinsic::round);
                    break;
                }
                case TokenType::SIN: {
                    applyUnaryIntrinsic(llvm::Intrinsic::sin);
                    break;
                }
                case TokenType::COS: {
                    applyUnaryIntrinsic(llvm::Intrinsic::cos);
                    break;
                }
                case TokenType::TAN: {
                    applyUnaryIntrinsic(llvm::Intrinsic::tan);
                    break;
                }
                case TokenType::ASIN: {
                    applyUnaryIntrinsic(llvm::Intrinsic::asin);
                    break;
                }
                case TokenType::ACOS: {
                    applyUnaryIntrinsic(llvm::Intrinsic::acos);
                    break;
                }
                case TokenType::ATAN: {
                    applyUnaryIntrinsic(llvm::Intrinsic::atan);
                    break;
                }
                case TokenType::EXP2: {
                    applyUnaryIntrinsic(llvm::Intrinsic::exp2);
                    break;
                }
                case TokenType::LOG10: {
                    applyUnaryIntrinsic(llvm::Intrinsic::log10);
                    break;
                }
                case TokenType::LOG2: {
                    applyUnaryIntrinsic(llvm::Intrinsic::log2);
                    break;
                }
                case TokenType::SINH: {
                    applyUnaryIntrinsic(llvm::Intrinsic::sinh);
                    break;
                }
                case TokenType::COSH: {
                    applyUnaryIntrinsic(llvm::Intrinsic::cosh);
                    break;
                }
                case TokenType::TANH: {
                    applyUnaryIntrinsic(llvm::Intrinsic::tanh);
                    break;
                }
                case TokenType::SGN: {
                    auto x = rpn_stack.back();
                    rpn_stack.pop_back();
                    auto zero = llvm::ConstantFP::get(float_ty, 0.0);
                    auto one = llvm::ConstantFP::get(float_ty, 1.0);
                    auto nonzero = builder.CreateFCmpONE(x, zero);
                    auto sign = builder.CreateCall(
                        llvm::Intrinsic::getOrInsertDeclaration(
                            module.get(), llvm::Intrinsic::copysign,
                            {float_ty}),
                        {one, x});
                    rpn_stack.push_back(
                        builder.CreateSelect(nonzero, sign, zero));
                    break;
                }
                case TokenType::NEG: {
                    auto a = rpn_stack.back();
                    rpn_stack.pop_back();
                    rpn_stack.push_back(builder.CreateFNeg(a));
                    break;
                }

                case TokenType::NOT: {
                    auto a = rpn_stack.back();
                    rpn_stack.pop_back();
                    rpn_stack.push_back(builder.CreateSelect(
                        builder.CreateFCmpOLE(
                            a, llvm::ConstantFP::get(float_ty, 0.0)),
                        llvm::ConstantFP::get(float_ty, 1.0),
                        llvm::ConstantFP::get(float_ty, 0.0)));
                    break;
                }
                case TokenType::BITNOT: {
                    auto a = rpn_stack.back();
                    rpn_stack.pop_back();
                    rpn_stack.push_back(builder.CreateSIToFP(
                        builder.CreateNot(builder.CreateFPToSI(a, i32_ty)),
                        float_ty));
                    break;
                }

                // Ternary and other multi-arg
                case TokenType::TERNARY: {
                    auto c = rpn_stack.back();
                    rpn_stack.pop_back();
                    auto b = rpn_stack.back();
                    rpn_stack.pop_back();
                    auto a = rpn_stack.back();
                    rpn_stack.pop_back();
                    rpn_stack.push_back(builder.CreateSelect(
                        builder.CreateFCmpOGT(
                            a, llvm::ConstantFP::get(float_ty, 0.0)),
                        b, c));
                    break;
                }
                case TokenType::CLIP:
                case TokenType::CLAMP: {
                    auto max_val = rpn_stack.back();
                    rpn_stack.pop_back();
                    auto min_val = rpn_stack.back();
                    rpn_stack.pop_back();
                    auto val = rpn_stack.back();
                    rpn_stack.pop_back();
                    auto temp = createBinaryIntrinsicCall(
                        llvm::Intrinsic::maxnum, val, min_val);
                    auto clamped = createBinaryIntrinsicCall(
                        llvm::Intrinsic::minnum, temp, max_val);
                    rpn_stack.push_back(clamped);
                    break;
                }
                case TokenType::FMA: {
                    auto c = rpn_stack.back();
                    rpn_stack.pop_back();
                    auto b = rpn_stack.back();
                    rpn_stack.pop_back();
                    auto a = rpn_stack.back();
                    rpn_stack.pop_back();
                    rpn_stack.push_back(builder.CreateCall(
                        llvm::Intrinsic::getOrInsertDeclaration(
                            module.get(), llvm::Intrinsic::fmuladd,
                            {builder.getFloatTy()}),
                        {a, b, c}));
                    break;
                }

                // Custom output control
                case TokenType::STORE_ABS: {
                    llvm::Value* coord_y_f = rpn_stack.back();
                    rpn_stack.pop_back();
                    llvm::Value* coord_x_f = rpn_stack.back();
                    rpn_stack.pop_back();
                    llvm::Value* val_to_store = rpn_stack.back();
                    rpn_stack.pop_back();
                    llvm::Value* coord_y =
                        builder.CreateFPToSI(coord_y_f, i32_ty);
                    llvm::Value* coord_x =
                        builder.CreateFPToSI(coord_x_f, i32_ty);
                    generate_pixel_store(val_to_store, coord_x, coord_y);
                    break;
                }
                case TokenType::EXIT_NO_WRITE: {
                    rpn_stack.push_back(llvm::ConstantFP::get(
                        float_ty,
                        std::bit_cast<float>(
                            EXIT_NAN_PAYLOAD))); // Use special NaN for `^exit^`
                    break;
                }

                // Stack manipulation
                case TokenType::DUP: {
                    const auto& payload =
                        std::get<TokenPayload_StackOp>(token.payload);
                    rpn_stack.push_back(
                        rpn_stack[rpn_stack.size() - 1 - payload.n]);
                    break;
                }
                case TokenType::DROP: {
                    const auto& payload =
                        std::get<TokenPayload_StackOp>(token.payload);
                    if (payload.n > 0) {
                        rpn_stack.resize(rpn_stack.size() - payload.n);
                    }
                    break;
                }
                case TokenType::SWAP: {
                    const auto& payload =
                        std::get<TokenPayload_StackOp>(token.payload);
                    std::swap(rpn_stack.back(),
                              rpn_stack[rpn_stack.size() - 1 - payload.n]);
                    break;
                }
                case TokenType::SORTN: {
                    const auto& payload =
                        std::get<TokenPayload_StackOp>(token.payload);
                    int n = payload.n;
                    if (n < 2)
                        break;

                    std::vector<llvm::Value*> values;
                    values.reserve(n);
                    for (int k = 0; k < n; ++k) {
                        values.push_back(rpn_stack.back());
                        rpn_stack.pop_back();
                    }
                    std::reverse(values.begin(), values.end());

                    auto compare_swap = [&](int i_idx, int j_idx) {
                        llvm::Value* val_i = values[i_idx];
                        llvm::Value* val_j = values[j_idx];
                        llvm::Value* cond = builder.CreateFCmpOGT(val_i, val_j);
                        values[i_idx] =
                            builder.CreateSelect(cond, val_j, val_i); // min
                        values[j_idx] =
                            builder.CreateSelect(cond, val_i, val_j); // max
                    };

                    const auto network = get_optimal_sorting_network(n);
                    if (!network.empty()) {
                        for (const auto& pair : network) {
                            if (pair.second < n) { // Bounds check
                                compare_swap(pair.first, pair.second);
                            }
                        }
                    } else {
                        std::vector<std::pair<int, int>> pairs;
                        int p = 1;
                        while (p < n)
                            p <<= 1;
                        generate_oem_sort_pairs(pairs, 0, p);
                        for (const auto& pair : pairs) {
                            if (pair.second < n) {
                                compare_swap(pair.first, pair.second);
                            }
                        }
                    }

                    for (int k = 0; k < n; ++k) {
                        rpn_stack.push_back(values[k]);
                    }
                    break;
                }

                // Control Flow (no-op during this pass)
                case TokenType::LABEL_DEF:
                case TokenType::JUMP:
                    break;
                }
            }

            // Create Terminator
            if (block_info.successors.empty()) {
                builder.CreateBr(exit_bb);
            } else if (block_info.successors.size() == 1) {
                builder.CreateBr(llvm_blocks[block_info.successors[0]]);
            } else { // size is 2, from a JUMP
                llvm::Value* cond_val = rpn_stack.back();
                llvm::Value* cond = builder.CreateFCmpOGT(
                    cond_val, llvm::ConstantFP::get(float_ty, 0.0));
                builder.CreateCondBr(cond,
                                     llvm_blocks[block_info.successors[0]],
                                     llvm_blocks[block_info.successors[1]]);

                // Pop the condition value from stack after creating the
                // terminator
                rpn_stack.pop_back();
            }

            // Save the final stack state (without condition for JUMP blocks)
            block_final_stacks[i] = rpn_stack;
        }

        // Populate PHI nodes
        for (size_t i = 0; i < cfg_blocks.size(); ++i) {
            if (cfg_blocks[i].predecessors.size() > 1) {
                auto& phis = block_initial_stacks.at(i);
                for (int pred_idx : cfg_blocks[i].predecessors) {
                    auto& incoming_stack = block_final_stacks.at(pred_idx);
                    auto* incoming_block = llvm_blocks.at(pred_idx);
                    for (size_t j = 0; j < phis.size(); ++j) {
                        // All predecessor blocks now provide stacks of the
                        // correct depth
                        if (j < incoming_stack.size()) {
                            static_cast<llvm::PHINode*>(phis[j])->addIncoming(
                                incoming_stack[j], incoming_block);
                        }
                    }
                }
            }
        }

        // Final Result PHI
        builder.SetInsertPoint(exit_bb);
        std::vector<std::pair<llvm::Value*, llvm::BasicBlock*>> final_values;
        for (size_t i = 0; i < cfg_blocks.size(); ++i) {
            if (cfg_blocks[i].successors.empty()) { // Terminal block
                final_values.push_back(
                    {block_final_stacks.at(i).back(), llvm_blocks.at(i)});
            }
        }

        llvm::Value* result_val;
        if (final_values.empty()) {
            result_val = llvm::UndefValue::get(float_ty);
        } else if (final_values.size() == 1) {
            result_val = final_values[0].first;
        } else {
            llvm::PHINode* phi =
                builder.CreatePHI(float_ty, final_values.size(), "result_phi");
            for (const auto& pair : final_values) {
                phi->addIncoming(pair.first, pair.second);
            }
            result_val = phi;
        }

        bool has_exit = false;
        for (const auto& token : tokens) {
            if (token.type == TokenType::EXIT_NO_WRITE) {
                has_exit = true;
                break;
            }
        }

        if (has_exit) {
            llvm::Value* result_int =
                builder.CreateBitCast(result_val, builder.getInt32Ty());
            llvm::Value* exit_nan_int = builder.getInt32(EXIT_NAN_PAYLOAD);
            llvm::Value* is_exit_val =
                builder.CreateICmpEQ(result_int, exit_nan_int);

            llvm::BasicBlock* store_block = llvm::BasicBlock::Create(
                *context, "do_default_store", parent_func);
            llvm::BasicBlock* after_store_block = llvm::BasicBlock::Create(
                *context, "after_default_store", parent_func);

            builder.CreateCondBr(is_exit_val, after_store_block, store_block);

            builder.SetInsertPoint(store_block);
            generate_pixel_store(result_val, x, y);
            builder.CreateBr(after_store_block);

            builder.SetInsertPoint(after_store_block);
        } else {
            generate_pixel_store(result_val, x, y);
        }
    }

    llvm::Value* generate_pixel_load(int clip_idx, llvm::Value* x,
                                     llvm::Value* y, bool mirror) {
        const VSVideoInfo* vinfo = vi[clip_idx];
        llvm::Value* clip_width = builder.getInt32(vinfo->width);
        llvm::Value* clip_height = builder.getInt32(vinfo->height);

        llvm::Value* final_x = get_final_coord(x, clip_width, mirror);
        llvm::Value* final_y = get_final_coord(y, clip_height, mirror);

        int vs_clip_idx = clip_idx + 1; // 0 is dst
        llvm::Value* base_ptr = preloaded_base_ptrs[vs_clip_idx];
        llvm::Value* stride = preloaded_strides[vs_clip_idx];

        llvm::Value* y_offset = builder.CreateMul(final_y, stride);
        llvm::Value* row_ptr =
            builder.CreateGEP(builder.getInt8Ty(), base_ptr, y_offset);

        return generate_load_from_row_ptr(row_ptr, clip_idx, final_x, 0,
                                          mirror);
    }

    void generate_pixel_store(llvm::Value* value_to_store, llvm::Value* x,
                              llvm::Value* y) {
        const VSFormat* format = vo->format;
        int bpp = format->bytesPerSample;
        int dst_idx = 0;

        llvm::Value* base_ptr = preloaded_base_ptrs[dst_idx];
        llvm::Value* stride = preloaded_strides[dst_idx];

        llvm::Value* y_offset = builder.CreateMul(y, stride);
        llvm::Value* x_offset = builder.CreateMul(x, builder.getInt32(bpp));
        llvm::Value* total_offset = builder.CreateAdd(y_offset, x_offset);
        llvm::Value* pixel_addr =
            builder.CreateGEP(builder.getInt8Ty(), base_ptr, total_offset);

        int pixel_align = std::gcd(ALIGNMENT, bpp);
        assumeAligned(pixel_addr, static_cast<unsigned>(pixel_align));

        llvm::Value* final_val;
        if (format->sampleType == stInteger) {
            int max_val = (1 << format->bitsPerSample) - 1;
            llvm::Value* zero_f =
                llvm::ConstantFP::get(builder.getFloatTy(), 0.0);
            llvm::Value* max_f = llvm::ConstantFP::get(
                builder.getFloatTy(), static_cast<double>(max_val));

            llvm::Value* temp = createBinaryIntrinsicCall(
                llvm::Intrinsic::maxnum, value_to_store, zero_f);
            llvm::Value* clamped_f =
                createBinaryIntrinsicCall(llvm::Intrinsic::minnum, temp, max_f);

            llvm::Value* rounded_f =
                createUnaryIntrinsicCall(llvm::Intrinsic::roundeven, clamped_f);
            final_val = builder.CreateFPToUI(rounded_f, builder.getInt32Ty());

            llvm::Type* store_type =
                bpp == 1
                    ? builder.getInt8Ty()
                    : (bpp == 2 ? builder.getInt16Ty() : builder.getInt32Ty());
            final_val = builder.CreateTruncOrBitCast(final_val, store_type);
            llvm::StoreInst* si = builder.CreateStore(final_val, pixel_addr);
            setMemoryInstAttrs(si, static_cast<unsigned>(pixel_align), dst_idx);
        } else { // stFloat
            if (bpp == 4) {
                llvm::StoreInst* si =
                    builder.CreateStore(value_to_store, pixel_addr);
                setMemoryInstAttrs(si, static_cast<unsigned>(pixel_align),
                                   dst_idx);
            } else if (bpp == 2) {
                llvm::Value* truncated_val =
                    builder.CreateFPTrunc(value_to_store, builder.getHalfTy());
                llvm::StoreInst* si =
                    builder.CreateStore(truncated_val, pixel_addr);
                setMemoryInstAttrs(si, static_cast<unsigned>(pixel_align),
                                   dst_idx);
            } else {
                throw std::runtime_error("Unsupported float sample size.");
            }
        }
    }
};

enum PlaneOp { PO_PROCESS, PO_COPY };

struct ExprData {
    std::vector<VSNodeRef*> nodes;
    VSVideoInfo vi = {};
    int num_inputs;

    PlaneOp plane_op[3] = {};
    std::vector<Token> tokens[3];
    CompiledFunction compiled[3];
    bool mirror_boundary;
    std::string dump_ir_path;

    std::vector<std::pair<int, std::string>> required_props;
    std::map<std::pair<int, std::string>, int> prop_map;
};

std::string
generate_cache_key(const std::string& expr, const VSVideoInfo* vo,
                   const std::vector<const VSVideoInfo*>& vi, bool mirror,
                   const std::map<std::pair<int, std::string>, int>& prop_map) {
    std::string result =
        std::format("expr={}|mirror={}|out={}", expr, mirror, vo->format->id);

    for (size_t i = 0; i < vi.size(); ++i) {
        result += std::format("|in{}={}", i, vi[i]->format->id);
    }

    for (const auto& [key, val] : prop_map) {
        result += std::format("|prop{}={}.{}", val, key.first, key.second);
    }

    return result;
}

void VS_CC exprInit([[maybe_unused]] VSMap* in, [[maybe_unused]] VSMap* out,
                    void** instanceData, [[maybe_unused]] VSNode* node,
                    [[maybe_unused]] VSCore* core, const VSAPI* vsapi) {
    ExprData* d = static_cast<ExprData*>(*instanceData);
    vsapi->setVideoInfo(&d->vi, 1, node);
}

const VSFrameRef* VS_CC exprGetFrame(int n, int activationReason,
                                     void** instanceData,
                                     [[maybe_unused]] void** frameData,
                                     VSFrameContext* frameCtx, VSCore* core,
                                     const VSAPI* vsapi) {
    ExprData* d = static_cast<ExprData*>(*instanceData);

    if (activationReason == arInitial) {
        for (int i = 0; i < d->num_inputs; ++i) {
            vsapi->requestFrameFilter(n, d->nodes[i], frameCtx);
        }
    } else if (activationReason == arAllFramesReady) {
        std::vector<const VSFrameRef*> src_frames(d->num_inputs);
        for (int i = 0; i < d->num_inputs; ++i) {
            src_frames[i] = vsapi->getFrameFilter(n, d->nodes[i], frameCtx);
        }

        const VSFrameRef* plane_src[3] = {
            d->plane_op[0] == PO_COPY ? src_frames[0] : nullptr,
            d->plane_op[1] == PO_COPY ? src_frames[0] : nullptr,
            d->plane_op[2] == PO_COPY ? src_frames[0] : nullptr};
        int planes[3] = {0, 1, 2};
        VSFrameRef* dst_frame =
            vsapi->newVideoFrame2(d->vi.format, d->vi.width, d->vi.height,
                                  plane_src, planes, src_frames[0], core);

        std::vector<uint8_t*> rwptrs(d->num_inputs + 1);
        std::vector<int> strides(d->num_inputs + 1);
        std::vector<float> props(1 + d->required_props.size());
        props[0] = static_cast<float>(n);

        for (size_t i = 0; i < d->required_props.size(); ++i) {
            const auto& prop_info = d->required_props[i];
            int clip_idx = prop_info.first;
            const std::string& prop_name = prop_info.second;
            int prop_array_idx = i + 1;

            const VSMap* props_map =
                vsapi->getFramePropsRO(src_frames[clip_idx]);
            int err = 0;
            int type = vsapi->propGetType(props_map, prop_name.c_str());

            if (type == ptInt) {
                props[prop_array_idx] = static_cast<float>(
                    vsapi->propGetInt(props_map, prop_name.c_str(), 0, &err));
            } else if (type == ptFloat) {
                props[prop_array_idx] = static_cast<float>(
                    vsapi->propGetFloat(props_map, prop_name.c_str(), 0, &err));
            } else if (type == ptData) {
                if (vsapi->propGetDataSize(props_map, prop_name.c_str(), 0,
                                           &err) > 0 &&
                    !err)
                    props[prop_array_idx] =
                        static_cast<float>(vsapi->propGetData(
                            props_map, prop_name.c_str(), 0, &err)[0]);
                else
                    err = 1;
            } else {
                err = 1;
            }

            if (err) {
                props[prop_array_idx] = std::bit_cast<float>(PROP_NAN_PAYLOAD);
            }
        }

        for (int plane = 0; plane < d->vi.format->numPlanes; ++plane) {
            if (d->plane_op[plane] == PO_PROCESS) {
                rwptrs[0] = vsapi->getWritePtr(dst_frame, plane);
                strides[0] = vsapi->getStride(dst_frame, plane);
                for (int i = 0; i < d->num_inputs; ++i) {
                    rwptrs[i + 1] = const_cast<uint8_t*>(
                        vsapi->getReadPtr(src_frames[i], plane));
                    strides[i + 1] = vsapi->getStride(src_frames[i], plane);
                }

                int width = vsapi->getFrameWidth(dst_frame, plane);
                int height = vsapi->getFrameHeight(dst_frame, plane);

                d->compiled[plane].func_ptr(rwptrs.data(), strides.data(),
                                            props.data(), width, height);
            }
        }

        for (const auto& frame : src_frames) {
            vsapi->freeFrame(frame);
        }
        return dst_frame;
    }

    return nullptr;
}

void VS_CC exprFree(void* instanceData, [[maybe_unused]] VSCore* core,
                    const VSAPI* vsapi) {
    ExprData* d = static_cast<ExprData*>(instanceData);
    for (auto* node : d->nodes) {
        vsapi->freeNode(node);
    }
    delete d;
}

void VS_CC exprCreate(const VSMap* in, VSMap* out,
                      [[maybe_unused]] void* userData, VSCore* core,
                      const VSAPI* vsapi) {
    auto d = std::make_unique<ExprData>();
    int err = 0;

    try {
        d->num_inputs = vsapi->propNumElements(in, "clips");
        if (d->num_inputs == 0)
            throw std::runtime_error("At least one clip must be provided.");
        d->nodes.resize(d->num_inputs);
        for (int i = 0; i < d->num_inputs; ++i) {
            d->nodes[i] = vsapi->propGetNode(in, "clips", i, &err);
        }

        std::vector<const VSVideoInfo*> vi(d->num_inputs);
        for (int i = 0; i < d->num_inputs; ++i) {
            vi[i] = vsapi->getVideoInfo(d->nodes[i]);
            if (!isConstantFormat(vi[i]))
                throw std::runtime_error(
                    "Only constant format clips are supported.");
        }
        for (int i = 1; i < d->num_inputs; ++i) {
            if (vi[i]->width != vi[0]->width ||
                vi[i]->height != vi[0]->height) {
                throw std::runtime_error(
                    "All clips must have the same dimensions.");
            }
        }

        d->vi = *vi[0];
        const int format_id =
            static_cast<int>(vsapi->propGetInt(in, "format", 0, &err));
        if (!err) {
            const VSFormat* f = vsapi->getFormatPreset(format_id, core);
            if (f) {
                if (d->vi.format->colorFamily == cmCompat) {
                    throw std::runtime_error(
                        "Compat formats are not supported.");
                }
                if (d->vi.format->numPlanes != f->numPlanes) {
                    throw std::runtime_error("The number of planes in the "
                                             "inputs and output must match.");
                }
                d->vi.format = vsapi->registerFormat(
                    d->vi.format->colorFamily, f->sampleType, f->bitsPerSample,
                    d->vi.format->subSamplingW, d->vi.format->subSamplingH,
                    core);
                if (!d->vi.format) {
                    throw std::runtime_error("Failed to register new format.");
                }
            }
        }

        const int nexpr = vsapi->propNumElements(in, "expr");
        if (nexpr == 0)
            throw std::runtime_error(
                "At least one expression must be provided.");

        std::string expr_strs[3];
        for (int i = 0; i < nexpr; ++i) {
            expr_strs[i] = vsapi->propGetData(in, "expr", i, &err);
        }
        for (int i = nexpr; i < d->vi.format->numPlanes; ++i) {
            expr_strs[i] = expr_strs[nexpr - 1];
        }

        // Tokenize and extract properties
        for (int i = 0; i < d->vi.format->numPlanes; ++i) {
            if (expr_strs[i].empty()) {
                d->plane_op[i] = PO_COPY;
                continue;
            }
            d->plane_op[i] = PO_PROCESS;
            d->tokens[i] = tokenize(expr_strs[i], d->num_inputs);

            for (const auto& token : d->tokens[i]) {
                if (token.type == TokenType::PROP_ACCESS) {
                    const auto& payload =
                        std::get<TokenPayload_PropAccess>(token.payload);
                    auto key =
                        std::make_pair(payload.clip_idx, payload.prop_name);
                    if (d->prop_map.find(key) == d->prop_map.end()) {
                        d->prop_map[key] =
                            1 +
                            d->required_props.size(); // 0 is for frame number N
                        d->required_props.push_back(key);
                    }
                }
            }
        }

        d->mirror_boundary = vsapi->propGetInt(in, "boundary", 0, &err) != 0;

        const char* dump_path = vsapi->propGetData(in, "dump_ir", 0, &err);
        if (!err && dump_path) {
            d->dump_ir_path = dump_path;
        }

        for (int i = 0; i < d->vi.format->numPlanes; ++i) {
            if (d->plane_op[i] != PO_PROCESS)
                continue;

            std::string key = generate_cache_key(
                expr_strs[i], &d->vi, vi, d->mirror_boundary, d->prop_map);

            std::lock_guard<std::mutex> lock(cache_mutex);
            if (jit_cache.count(key)) {
                d->compiled[i] = jit_cache.at(key);
            } else {
                vsapi->logMessage(
                    mtDebug,
                    std::format("JIT compiling expression for plane {}: {}", i,
                                expr_strs[i])
                        .c_str());
                // Generate unique function name per compiled expression
                size_t key_hash = std::hash<std::string>{}(key);
                std::string func_name =
                    std::format("process_plane_{}_{}", i, key_hash);

                Compiler compiler(std::move(d->tokens[i]), &d->vi, vi,
                                  d->mirror_boundary, d->dump_ir_path,
                                  d->prop_map, func_name);
                d->compiled[i] = compiler.compile();
                jit_cache[key] = d->compiled[i];
            }
        }

    } catch (const std::exception& e) {
        for (auto* node : d->nodes) {
            if (node)
                vsapi->freeNode(node);
        }
        vsapi->setError(out, std::format("Expr: {}", e.what()).c_str());
        return;
    }

    vsapi->createFilter(in, out, "Expr", exprInit, exprGetFrame, exprFree,
                        fmParallel, 0, d.release(), core);
}

} // anonymous namespace

VS_EXTERNAL_API(void)
VapourSynthPluginInit(VSConfigPlugin configFunc,
                      VSRegisterFunction registerFunc, VSPlugin* plugin) {
    configFunc("com.yuygfgg.llvmexpr", "llvmexpr",
               "LLVM JIT RPN Expression Filter", VAPOURSYNTH_API_VERSION, 1,
               plugin);
    registerFunc("Expr",
                 "clips:clip[];expr:data[];format:int:opt;boundary:int:opt;"
                 "dump_ir:data:opt;",
                 exprCreate, nullptr, plugin);
}