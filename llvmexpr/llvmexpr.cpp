#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdint>
#include <format>
#include <functional>
#include <map>
#include <memory>
#include <mutex>
#include <numbers>
#include <numeric>
#include <regex>
#include <sstream>
#include <string>
#include <system_error>
#include <unordered_map>
#include <unordered_set>
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
};

// Payloads for different token types
struct TokenPayload_Number {
    double value;
};
struct TokenPayload_Var {
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
                 TokenPayload_StackOp, TokenPayload_ClipAccess,
                 TokenPayload_PropAccess>
        payload;
};

namespace {
const std::regex re_rel{
    R"(^(?:src(\d+)|([x-za-w]))\((-?\d+),(-?\d+)\)(?::([cm]))?$)"};
const std::regex re_rel_bracket{
    R"(^(?:src(\d+)|([x-za-w]))\[(-?\d+),(-?\d+)\](?::([cm]))?$)"};
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

std::vector<Token> tokenize(const std::string& expr, int num_inputs) {
    std::vector<Token> tokens;
    std::stringstream ss(expr);
    std::string str_token;

    static const std::unordered_map<std::string, TokenType> keyword_map = {
        {"+", TokenType::ADD},
        {"-", TokenType::SUB},
        {"*", TokenType::MUL},
        {"/", TokenType::DIV},
        {"%", TokenType::MOD},
        {">", TokenType::GT},
        {"<", TokenType::LT},
        {">=", TokenType::GE},
        {"<=", TokenType::LE},
        {"=", TokenType::EQ},
        {"and", TokenType::AND},
        {"or", TokenType::OR},
        {"xor", TokenType::XOR},
        {"not", TokenType::NOT},
        {"bitand", TokenType::BITAND},
        {"bitor", TokenType::BITOR},
        {"bitxor", TokenType::BITXOR},
        {"bitnot", TokenType::BITNOT},
        {"pow", TokenType::POW},
        {"**", TokenType::POW},
        {"min", TokenType::MIN},
        {"max", TokenType::MAX},
        {"atan2", TokenType::ATAN2},
        {"copysign", TokenType::COPYSIGN},
        {"?", TokenType::TERNARY},
        {"clip", TokenType::CLIP},
        {"clamp", TokenType::CLAMP},
        {"fma", TokenType::FMA},
        {"sqrt", TokenType::SQRT},
        {"exp", TokenType::EXP},
        {"log", TokenType::LOG},
        {"abs", TokenType::ABS},
        {"floor", TokenType::FLOOR},
        {"ceil", TokenType::CEIL},
        {"trunc", TokenType::TRUNC},
        {"round", TokenType::ROUND},
        {"sin", TokenType::SIN},
        {"cos", TokenType::COS},
        {"tan", TokenType::TAN},
        {"asin", TokenType::ASIN},
        {"acos", TokenType::ACOS},
        {"atan", TokenType::ATAN},
        {"exp2", TokenType::EXP2},
        {"log10", TokenType::LOG10},
        {"log2", TokenType::LOG2},
        {"sinh", TokenType::SINH},
        {"cosh", TokenType::COSH},
        {"tanh", TokenType::TANH},
        {"X", TokenType::CONSTANT_X},
        {"Y", TokenType::CONSTANT_Y},
        {"width", TokenType::CONSTANT_WIDTH},
        {"height", TokenType::CONSTANT_HEIGHT},
        {"N", TokenType::CONSTANT_N},
        {"pi", TokenType::CONSTANT_PI},
    };

    while (ss >> str_token) {
        Token t;
        t.text = str_token;
        std::smatch match;

        if (auto it = keyword_map.find(str_token); it != keyword_map.end()) {
            t.type = it->second;
        } else if (str_token.rfind("dup", 0) == 0) {
            t.type = TokenType::DUP;
            int n =
                (str_token.length() > 3) ? std::stoi(str_token.substr(3)) : 0;
            if (n < 0)
                throw std::runtime_error("Invalid dupN operator: " + str_token);
            t.payload = TokenPayload_StackOp{n};
        } else if (str_token.rfind("drop", 0) == 0) {
            t.type = TokenType::DROP;
            int n =
                (str_token.length() > 4) ? std::stoi(str_token.substr(4)) : 1;
            if (n < 0)
                throw std::runtime_error("Invalid dropN operator: " +
                                         str_token);
            t.payload = TokenPayload_StackOp{n};
        } else if (str_token.rfind("swap", 0) == 0) {
            t.type = TokenType::SWAP;
            int n =
                (str_token.length() > 4) ? std::stoi(str_token.substr(4)) : 1;
            if (n < 0)
                throw std::runtime_error("Invalid swapN operator: " +
                                         str_token);
            t.payload = TokenPayload_StackOp{n};
        } else if (str_token.rfind("sort", 0) == 0 && str_token.length() > 4) {
            t.type = TokenType::SORTN;
            int n = std::stoi(str_token.substr(4));
            if (n < 0)
                throw std::runtime_error("Invalid sortN operator: " +
                                         str_token);
            t.payload = TokenPayload_StackOp{n};
        } else if (std::regex_match(str_token, match, re_rel) ||
                   std::regex_match(str_token, match, re_rel_bracket)) {
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
                .name = str_token.substr(0, str_token.length() - 1)};
        } else if (!str_token.empty() && str_token.back() == '@') {
            t.type = TokenType::VAR_LOAD;
            t.payload = TokenPayload_Var{
                .name = str_token.substr(0, str_token.length() - 1)};
        } else {
            t.type = TokenType::NUMBER;
            try {
                size_t pos = 0;
                double val = (double)std::stoll(str_token, &pos, 0);
                if (pos != str_token.length()) { // Not a full integer match
                    pos = 0;
                    val = std::stod(str_token, &pos);
                    if (pos != str_token.length()) {
                        throw std::runtime_error("Invalid number format");
                    }
                }
                t.payload = TokenPayload_Number{val};
            } catch (const std::exception&) {
                throw std::runtime_error("Invalid token: " + str_token);
            }
        }

        if (t.type == TokenType::CLIP_REL || t.type == TokenType::CLIP_ABS ||
            t.type == TokenType::CLIP_CUR) {
            if (std::get<TokenPayload_ClipAccess>(t.payload).clip_idx < 0 ||
                std::get<TokenPayload_ClipAccess>(t.payload).clip_idx >=
                    num_inputs) {
                throw std::runtime_error("Invalid clip index in token: " +
                                         str_token);
            }
        } else if (t.type == TokenType::PROP_ACCESS) {
            if (std::get<TokenPayload_PropAccess>(t.payload).clip_idx < 0 ||
                std::get<TokenPayload_PropAccess>(t.payload).clip_idx >=
                    num_inputs) {
                throw std::runtime_error("Invalid clip index in token: " +
                                         str_token);
            }
        }

        tokens.push_back(t);
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
    OrcJit() {
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
        Opts.NoNaNsFPMath = true;
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
OrcJit global_jit;

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
        llvm::FastMathFlags FMF;
        FMF.setFast();
        builder.setFastMathFlags(FMF);
    }

    CompiledFunction compile() {
        define_function_signature();
        generate_loops();

        module->setDataLayout(global_jit.getDataLayout());

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

            llvm::ModulePassManager MPM =
                PB.buildPerModuleDefaultPipeline(llvm::OptimizationLevel::O3);
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

        global_jit.addModule(std::move(module), std::move(context));
        void* func_addr = global_jit.getFunctionAddress(func_name);

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
        return builder.CreateCall(
            llvm::Intrinsic::getOrInsertDeclaration(module.get(), intrinsic_id,
                                                    {builder.getFloatTy()}),
            {arg});
    }

    llvm::Value* createBinaryIntrinsicCall(llvm::Intrinsic::ID intrinsic_id,
                                           llvm::Value* arg1,
                                           llvm::Value* arg2) {
        return builder.CreateCall(
            llvm::Intrinsic::getOrInsertDeclaration(module.get(), intrinsic_id,
                                                    {builder.getFloatTy()}),
            {arg1, arg2});
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
        llvm::BasicBlock* loop_x_header =
            llvm::BasicBlock::Create(*context, "loop_x_header", parent_func);
        llvm::BasicBlock* loop_x_body =
            llvm::BasicBlock::Create(*context, "loop_x_body", parent_func);
        llvm::BasicBlock* loop_x_exit =
            llvm::BasicBlock::Create(*context, "loop_x_exit", parent_func);

        // Reset x.var at the start of each row
        llvm::Value* x_var = x_var_entry;
        builder.CreateStore(builder.getInt32(0), x_var);
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
        generate_ir_from_tokens(x_val, y_val);

        llvm::Value* x_next = builder.CreateAdd(x_val, builder.getInt32(1));
        builder.CreateStore(x_next, x_var);
        builder.CreateBr(loop_x_header);

        builder.SetInsertPoint(loop_x_exit);
        llvm::Value* y_next = builder.CreateAdd(y_val, builder.getInt32(1));
        builder.CreateStore(y_next, y_var);
        builder.CreateBr(loop_y_header);

        builder.SetInsertPoint(loop_y_exit);
        builder.CreateRetVoid();
    }

    void generate_ir_from_tokens(llvm::Value* x, llvm::Value* y) {
        llvm::Type* float_ty = builder.getFloatTy();
        llvm::Type* i32_ty = builder.getInt32Ty();

        std::vector<llvm::Value*> stack;
        stack.reserve(128);
        std::unordered_set<std::string> defined_vars;
        std::unordered_map<std::string, llvm::Value*> named_vars;

        auto require = [&](int need, const std::string& op_text) {
            if (stack.size() < static_cast<size_t>(need)) {
                throw std::runtime_error("Stack underflow on '" + op_text +
                                         "'");
            }
        };
        auto push = [&](llvm::Value* val) { stack.push_back(val); };
        auto pop = [&]() -> llvm::Value* {
            llvm::Value* v = stack.back();
            stack.pop_back();
            return v;
        };

        for (const auto& token : tokens) {
            switch (token.type) {
            // Literals & Constants
            case TokenType::NUMBER: {
                const auto& payload =
                    std::get<TokenPayload_Number>(token.payload);
                push(llvm::ConstantFP::get(float_ty, payload.value));
                break;
            }
            case TokenType::CONSTANT_X:
                push(builder.CreateSIToFP(x, float_ty));
                break;
            case TokenType::CONSTANT_Y:
                push(builder.CreateSIToFP(y, float_ty));
                break;
            case TokenType::CONSTANT_WIDTH:
                push(builder.CreateSIToFP(width_arg, float_ty));
                break;
            case TokenType::CONSTANT_HEIGHT:
                push(builder.CreateSIToFP(height_arg, float_ty));
                break;
            case TokenType::CONSTANT_N:
                push(builder.CreateLoad(
                    float_ty, builder.CreateGEP(float_ty, props_arg,
                                                builder.getInt32(0))));
                break;
            case TokenType::CONSTANT_PI:
                push(llvm::ConstantFP::get(float_ty, std::numbers::pi));
                break;

            // Variable Ops
            case TokenType::VAR_STORE: {
                require(1, token.text);
                const auto& payload = std::get<TokenPayload_Var>(token.payload);
                llvm::Value* val_to_store = pop();
                llvm::Value* var_ptr;
                if (named_vars.find(payload.name) == named_vars.end()) {
                    var_ptr = createAllocaInEntry(float_ty, payload.name);
                    named_vars[payload.name] = var_ptr;
                } else {
                    var_ptr = named_vars[payload.name];
                }
                builder.CreateStore(val_to_store, var_ptr);
                defined_vars.insert(payload.name);
                break;
            }
            case TokenType::VAR_LOAD: {
                const auto& payload = std::get<TokenPayload_Var>(token.payload);
                if (defined_vars.find(payload.name) == defined_vars.end()) {
                    throw std::runtime_error("Unknown variable: " +
                                             payload.name);
                }
                llvm::Value* var_ptr = named_vars[payload.name];
                push(builder.CreateLoad(float_ty, var_ptr));
                break;
            }

            // Data Access
            case TokenType::CLIP_REL: {
                const auto& payload =
                    std::get<TokenPayload_ClipAccess>(token.payload);
                llvm::Value* coord_x =
                    builder.CreateAdd(x, builder.getInt32(payload.rel_x));
                llvm::Value* coord_y =
                    builder.CreateAdd(y, builder.getInt32(payload.rel_y));
                bool use_mirror =
                    payload.has_mode ? payload.use_mirror : mirror_boundary;
                push(generate_pixel_load(payload.clip_idx, coord_x, coord_y,
                                         use_mirror));
                break;
            }
            case TokenType::CLIP_ABS: {
                require(2, token.text);
                const auto& payload =
                    std::get<TokenPayload_ClipAccess>(token.payload);
                llvm::Value* coord_y = builder.CreateFPToSI(pop(), i32_ty);
                llvm::Value* coord_x = builder.CreateFPToSI(pop(), i32_ty);
                push(generate_pixel_load(payload.clip_idx, coord_x, coord_y,
                                         mirror_boundary));
                break;
            }
            case TokenType::CLIP_CUR: {
                const auto& payload =
                    std::get<TokenPayload_ClipAccess>(token.payload);
                push(generate_pixel_load(payload.clip_idx, x, y,
                                         mirror_boundary));
                break;
            }
            case TokenType::PROP_ACCESS: {
                const auto& payload =
                    std::get<TokenPayload_PropAccess>(token.payload);
                auto key = std::make_pair(payload.clip_idx, payload.prop_name);
                if (prop_map.count(key) == 0) {
                    std::unreachable();
                }
                int prop_idx = prop_map.at(key);
                llvm::Value* prop_val = builder.CreateLoad(
                    float_ty, builder.CreateGEP(float_ty, props_arg,
                                                builder.getInt32(prop_idx)));
                push(prop_val);
                break;
            }

            // Binary Operators
            case TokenType::ADD: {
                require(2, token.text);
                auto b = pop();
                auto a = pop();
                push(builder.CreateFAdd(a, b));
                break;
            }
            case TokenType::SUB: {
                require(2, token.text);
                auto b = pop();
                auto a = pop();
                push(builder.CreateFSub(a, b));
                break;
            }
            case TokenType::MUL: {
                require(2, token.text);
                auto b = pop();
                auto a = pop();
                push(builder.CreateFMul(a, b));
                break;
            }
            case TokenType::DIV: {
                require(2, token.text);
                auto b = pop();
                auto a = pop();
                push(builder.CreateFDiv(a, b));
                break;
            }
            case TokenType::MOD: {
                require(2, token.text);
                auto b = pop();
                auto a = pop();
                push(builder.CreateFRem(a, b));
                break;
            }
            case TokenType::GT: {
                require(2, token.text);
                auto b = pop();
                auto a = pop();
                push(
                    builder.CreateSelect(builder.CreateFCmpOGT(a, b),
                                         llvm::ConstantFP::get(float_ty, 1.0),
                                         llvm::ConstantFP::get(float_ty, 0.0)));
                break;
            }
            case TokenType::LT: {
                require(2, token.text);
                auto b = pop();
                auto a = pop();
                push(
                    builder.CreateSelect(builder.CreateFCmpOLT(a, b),
                                         llvm::ConstantFP::get(float_ty, 1.0),
                                         llvm::ConstantFP::get(float_ty, 0.0)));
                break;
            }
            case TokenType::GE: {
                require(2, token.text);
                auto b = pop();
                auto a = pop();
                push(
                    builder.CreateSelect(builder.CreateFCmpOGE(a, b),
                                         llvm::ConstantFP::get(float_ty, 1.0),
                                         llvm::ConstantFP::get(float_ty, 0.0)));
                break;
            }
            case TokenType::LE: {
                require(2, token.text);
                auto b = pop();
                auto a = pop();
                push(
                    builder.CreateSelect(builder.CreateFCmpOLE(a, b),
                                         llvm::ConstantFP::get(float_ty, 1.0),
                                         llvm::ConstantFP::get(float_ty, 0.0)));
                break;
            }
            case TokenType::EQ: {
                require(2, token.text);
                auto b = pop();
                auto a = pop();
                push(
                    builder.CreateSelect(builder.CreateFCmpOEQ(a, b),
                                         llvm::ConstantFP::get(float_ty, 1.0),
                                         llvm::ConstantFP::get(float_ty, 0.0)));
                break;
            }
            case TokenType::AND: {
                require(2, token.text);
                auto b = pop();
                auto a = pop();
                push(builder.CreateSelect(
                    builder.CreateAnd(
                        builder.CreateFCmpONE(
                            a, llvm::ConstantFP::get(float_ty, 0.0)),
                        builder.CreateFCmpONE(
                            b, llvm::ConstantFP::get(float_ty, 0.0))),
                    llvm::ConstantFP::get(float_ty, 1.0),
                    llvm::ConstantFP::get(float_ty, 0.0)));
                break;
            }
            case TokenType::OR: {
                require(2, token.text);
                auto b = pop();
                auto a = pop();
                push(builder.CreateSelect(
                    builder.CreateOr(
                        builder.CreateFCmpONE(
                            a, llvm::ConstantFP::get(float_ty, 0.0)),
                        builder.CreateFCmpONE(
                            b, llvm::ConstantFP::get(float_ty, 0.0))),
                    llvm::ConstantFP::get(float_ty, 1.0),
                    llvm::ConstantFP::get(float_ty, 0.0)));
                break;
            }
            case TokenType::XOR: {
                require(2, token.text);
                auto b = pop();
                auto a = pop();
                push(builder.CreateSelect(
                    builder.CreateXor(
                        builder.CreateFCmpONE(
                            a, llvm::ConstantFP::get(float_ty, 0.0)),
                        builder.CreateFCmpONE(
                            b, llvm::ConstantFP::get(float_ty, 0.0))),
                    llvm::ConstantFP::get(float_ty, 1.0),
                    llvm::ConstantFP::get(float_ty, 0.0)));
                break;
            }
            case TokenType::BITAND: {
                require(2, token.text);
                auto b = pop();
                auto a = pop();
                push(builder.CreateSIToFP(
                    builder.CreateAnd(builder.CreateFPToSI(a, i32_ty),
                                      builder.CreateFPToSI(b, i32_ty)),
                    float_ty));
                break;
            }
            case TokenType::BITOR: {
                require(2, token.text);
                auto b = pop();
                auto a = pop();
                push(builder.CreateSIToFP(
                    builder.CreateOr(builder.CreateFPToSI(a, i32_ty),
                                     builder.CreateFPToSI(b, i32_ty)),
                    float_ty));
                break;
            }
            case TokenType::BITXOR: {
                require(2, token.text);
                auto b = pop();
                auto a = pop();
                push(builder.CreateSIToFP(
                    builder.CreateXor(builder.CreateFPToSI(a, i32_ty),
                                      builder.CreateFPToSI(b, i32_ty)),
                    float_ty));
                break;
            }
            case TokenType::POW: {
                require(2, token.text);
                auto b = pop();
                auto a = pop();
                push(createBinaryIntrinsicCall(llvm::Intrinsic::pow, a, b));
                break;
            }
            case TokenType::ATAN2: {
                require(2, token.text);
                auto b = pop();
                auto a = pop();
                push(createBinaryIntrinsicCall(llvm::Intrinsic::atan2, a, b));
                break;
            }
            case TokenType::COPYSIGN: {
                require(2, token.text);
                auto b = pop();
                auto a = pop();
                push(
                    createBinaryIntrinsicCall(llvm::Intrinsic::copysign, a, b));
                break;
            }
            case TokenType::MIN: {
                require(2, token.text);
                auto b = pop();
                auto a = pop();
                push(createBinaryIntrinsicCall(llvm::Intrinsic::minnum, a, b));
                break;
            }
            case TokenType::MAX: {
                require(2, token.text);
                auto b = pop();
                auto a = pop();
                push(createBinaryIntrinsicCall(llvm::Intrinsic::maxnum, a, b));
                break;
            }

            // Unary Operators
            case TokenType::NOT: {
                require(1, token.text);
                auto a = pop();
                push(builder.CreateSelect(
                    builder.CreateFCmpOEQ(a,
                                          llvm::ConstantFP::get(float_ty, 0.0)),
                    llvm::ConstantFP::get(float_ty, 1.0),
                    llvm::ConstantFP::get(float_ty, 0.0)));
                break;
            }
            case TokenType::BITNOT: {
                require(1, token.text);
                auto a = pop();
                push(builder.CreateSIToFP(
                    builder.CreateNot(builder.CreateFPToSI(a, i32_ty)),
                    float_ty));
                break;
            }
            case TokenType::SQRT:
                require(1, token.text);
                push(createUnaryIntrinsicCall(llvm::Intrinsic::sqrt, pop()));
                break;
            case TokenType::EXP:
                require(1, token.text);
                push(createUnaryIntrinsicCall(llvm::Intrinsic::exp, pop()));
                break;
            case TokenType::LOG:
                require(1, token.text);
                push(createUnaryIntrinsicCall(llvm::Intrinsic::log, pop()));
                break;
            case TokenType::ABS:
                require(1, token.text);
                push(createUnaryIntrinsicCall(llvm::Intrinsic::fabs, pop()));
                break;
            case TokenType::FLOOR:
                require(1, token.text);
                push(createUnaryIntrinsicCall(llvm::Intrinsic::floor, pop()));
                break;
            case TokenType::CEIL:
                require(1, token.text);
                push(createUnaryIntrinsicCall(llvm::Intrinsic::ceil, pop()));
                break;
            case TokenType::TRUNC:
                require(1, token.text);
                push(createUnaryIntrinsicCall(llvm::Intrinsic::trunc, pop()));
                break;
            case TokenType::ROUND:
                require(1, token.text);
                push(createUnaryIntrinsicCall(llvm::Intrinsic::round, pop()));
                break;
            case TokenType::SIN:
                require(1, token.text);
                push(createUnaryIntrinsicCall(llvm::Intrinsic::sin, pop()));
                break;
            case TokenType::COS:
                require(1, token.text);
                push(createUnaryIntrinsicCall(llvm::Intrinsic::cos, pop()));
                break;
            case TokenType::TAN:
                require(1, token.text);
                push(createUnaryIntrinsicCall(llvm::Intrinsic::tan, pop()));
                break;
            case TokenType::ASIN:
                require(1, token.text);
                push(createUnaryIntrinsicCall(llvm::Intrinsic::asin, pop()));
                break;
            case TokenType::ACOS:
                require(1, token.text);
                push(createUnaryIntrinsicCall(llvm::Intrinsic::acos, pop()));
                break;
            case TokenType::ATAN:
                require(1, token.text);
                push(createUnaryIntrinsicCall(llvm::Intrinsic::atan, pop()));
                break;
            case TokenType::EXP2:
                require(1, token.text);
                push(createUnaryIntrinsicCall(llvm::Intrinsic::exp2, pop()));
                break;
            case TokenType::LOG10:
                require(1, token.text);
                push(createUnaryIntrinsicCall(llvm::Intrinsic::log10, pop()));
                break;
            case TokenType::LOG2:
                require(1, token.text);
                push(createUnaryIntrinsicCall(llvm::Intrinsic::log2, pop()));
                break;
            case TokenType::SINH:
                require(1, token.text);
                push(createUnaryIntrinsicCall(llvm::Intrinsic::sinh, pop()));
                break;
            case TokenType::COSH:
                require(1, token.text);
                push(createUnaryIntrinsicCall(llvm::Intrinsic::cosh, pop()));
                break;
            case TokenType::TANH:
                require(1, token.text);
                push(createUnaryIntrinsicCall(llvm::Intrinsic::tanh, pop()));
                break;

            // Ternary and other multi-arg
            case TokenType::TERNARY: {
                require(3, token.text);
                auto c = pop();
                auto b = pop();
                auto a = pop();
                push(builder.CreateSelect(
                    builder.CreateFCmpONE(a,
                                          llvm::ConstantFP::get(float_ty, 0.0)),
                    b, c));
                break;
            }
            case TokenType::CLIP:
            case TokenType::CLAMP: {
                require(3, token.text);
                auto max_val = pop();
                auto min_val = pop();
                auto val = pop();
                auto temp = createBinaryIntrinsicCall(llvm::Intrinsic::maxnum,
                                                      val, min_val);
                auto clamped = createBinaryIntrinsicCall(
                    llvm::Intrinsic::minnum, temp, max_val);
                push(clamped);
                break;
            }
            case TokenType::FMA: {
                require(3, token.text);
                auto c = pop();
                auto b = pop();
                auto a = pop();
                push(builder.CreateCall(llvm::Intrinsic::getOrInsertDeclaration(
                                            module.get(),
                                            llvm::Intrinsic::fmuladd,
                                            {builder.getFloatTy()}),
                                        {a, b, c}));
                break;
            }

            // Stack manipulation
            case TokenType::DUP: {
                const auto& payload =
                    std::get<TokenPayload_StackOp>(token.payload);
                require(1 + payload.n, token.text);
                push(stack[stack.size() - 1 - payload.n]);
                break;
            }
            case TokenType::DROP: {
                const auto& payload =
                    std::get<TokenPayload_StackOp>(token.payload);
                require(payload.n, token.text);
                if (payload.n > 0) {
                    stack.resize(stack.size() - payload.n);
                }
                break;
            }
            case TokenType::SWAP: {
                const auto& payload =
                    std::get<TokenPayload_StackOp>(token.payload);
                require(1 + payload.n, token.text);
                size_t top_index = stack.size() - 1;
                size_t n_index = stack.size() - 1 - payload.n;
                std::swap(stack[top_index], stack[n_index]);
                break;
            }
            case TokenType::SORTN: {
                const auto& payload =
                    std::get<TokenPayload_StackOp>(token.payload);
                int n = payload.n;
                if (n < 2)
                    break;
                require(n, token.text);

                std::vector<llvm::Value*> values;
                values.reserve(n);
                for (int i = 0; i < n; ++i) {
                    values.push_back(pop());
                }
                std::reverse(values.begin(), values.end());

                auto compare_swap = [&](int i, int j) {
                    llvm::Value* val_i = values[i];
                    llvm::Value* val_j = values[j];
                    llvm::Value* cond = builder.CreateFCmpOGT(val_i, val_j);
                    values[i] = builder.CreateSelect(cond, val_j, val_i); // min
                    values[j] = builder.CreateSelect(cond, val_i, val_j); // max
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

                for (int i = 0; i < n; ++i) {
                    push(values[i]);
                }
                break;
            }
            }
        }

        if (stack.size() != 1) {
            throw std::runtime_error(
                std::format("Expression stack not balanced: final stack depth "
                            "= {}, expected 1.",
                            stack.size()));
        }

        llvm::Value* result_val = pop();
        generate_pixel_store(result_val, x, y);
    }

    llvm::Value* generate_pixel_load(int clip_idx, llvm::Value* x,
                                     llvm::Value* y, bool mirror) {
        llvm::Value* clamped_x = generate_boundary_check(x, width_arg, mirror);
        llvm::Value* clamped_y = generate_boundary_check(y, height_arg, mirror);

        const VSFormat* format = vi[clip_idx]->format;
        int bpp = format->bytesPerSample;
        int vs_clip_idx = clip_idx + 1; // 0 is dst

        llvm::Value* base_ptr = preloaded_base_ptrs[vs_clip_idx];
        llvm::Value* stride = preloaded_strides[vs_clip_idx];

        llvm::Value* y_offset = builder.CreateMul(clamped_y, stride);
        llvm::Value* x_offset =
            builder.CreateMul(clamped_x, builder.getInt32(bpp));
        llvm::Value* total_offset = builder.CreateAdd(y_offset, x_offset);
        llvm::Value* pixel_addr =
            builder.CreateGEP(builder.getInt8Ty(), base_ptr, total_offset);

        // Assume pixel address alignment, safe as gcd(32, bpp)
        int pixel_align = std::gcd(32, bpp);
        assumeAligned(pixel_addr, static_cast<unsigned>(pixel_align));

        llvm::Value* loaded_val;
        if (format->sampleType == stInteger) {
            if (bpp == 1) {
                llvm::LoadInst* li =
                    builder.CreateLoad(builder.getInt8Ty(), pixel_addr);
                li->setAlignment(llvm::Align(pixel_align));
                li->setMetadata(llvm::LLVMContext::MD_alias_scope,
                                alias_scope_lists[vs_clip_idx]);
                li->setMetadata(llvm::LLVMContext::MD_noalias,
                                noalias_scope_lists[vs_clip_idx]);
                loaded_val = li;
                loaded_val =
                    builder.CreateZExt(loaded_val, builder.getInt32Ty());
            } else if (bpp == 2) {
                llvm::LoadInst* li =
                    builder.CreateLoad(builder.getInt16Ty(), pixel_addr);
                li->setAlignment(llvm::Align(pixel_align));
                li->setMetadata(llvm::LLVMContext::MD_alias_scope,
                                alias_scope_lists[vs_clip_idx]);
                li->setMetadata(llvm::LLVMContext::MD_noalias,
                                noalias_scope_lists[vs_clip_idx]);
                loaded_val = li;
                loaded_val =
                    builder.CreateZExt(loaded_val, builder.getInt32Ty());
            } else { // bpp == 4
                llvm::LoadInst* li =
                    builder.CreateLoad(builder.getInt32Ty(), pixel_addr);
                li->setAlignment(llvm::Align(pixel_align));
                li->setMetadata(llvm::LLVMContext::MD_alias_scope,
                                alias_scope_lists[vs_clip_idx]);
                li->setMetadata(llvm::LLVMContext::MD_noalias,
                                noalias_scope_lists[vs_clip_idx]);
                loaded_val = li;
            }
            return builder.CreateUIToFP(loaded_val, builder.getFloatTy());
        } else { // stFloat
            if (bpp == 4) {
                llvm::LoadInst* li =
                    builder.CreateLoad(builder.getFloatTy(), pixel_addr);
                li->setAlignment(llvm::Align(pixel_align));
                li->setMetadata(llvm::LLVMContext::MD_alias_scope,
                                alias_scope_lists[vs_clip_idx]);
                li->setMetadata(llvm::LLVMContext::MD_noalias,
                                noalias_scope_lists[vs_clip_idx]);
                return li;
            } else if (bpp == 2) {
                llvm::LoadInst* li =
                    builder.CreateLoad(builder.getHalfTy(), pixel_addr);
                li->setAlignment(llvm::Align(pixel_align));
                li->setMetadata(llvm::LLVMContext::MD_alias_scope,
                                alias_scope_lists[vs_clip_idx]);
                li->setMetadata(llvm::LLVMContext::MD_noalias,
                                noalias_scope_lists[vs_clip_idx]);
                return builder.CreateFPExt(li, builder.getFloatTy());
            } else {
                throw std::runtime_error("Unsupported float sample size.");
            }
        }
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

        // Assume pixel address alignment, safe as gcd(32, bpp)
        int pixel_align = std::gcd(32, bpp);
        assumeAligned(pixel_addr, static_cast<unsigned>(pixel_align));

        llvm::Value* final_val;
        if (format->sampleType == stInteger) {
            int max_val = (1 << format->bitsPerSample) - 1;
            llvm::Value* zero_f =
                llvm::ConstantFP::get(builder.getFloatTy(), 0.0);
            llvm::Value* max_f =
                llvm::ConstantFP::get(builder.getFloatTy(), (double)max_val);

            llvm::Value* temp = createBinaryIntrinsicCall(
                llvm::Intrinsic::maxnum, value_to_store, zero_f);
            llvm::Value* clamped_f =
                createBinaryIntrinsicCall(llvm::Intrinsic::minnum, temp, max_f);

            final_val = builder.CreateFPToUI(clamped_f, builder.getInt32Ty());

            if (bpp == 1) {
                final_val = builder.CreateTrunc(final_val, builder.getInt8Ty());
                llvm::StoreInst* si =
                    builder.CreateStore(final_val, pixel_addr);
                si->setAlignment(llvm::Align(pixel_align));
                si->setMetadata(llvm::LLVMContext::MD_alias_scope,
                                alias_scope_lists[dst_idx]);
                si->setMetadata(llvm::LLVMContext::MD_noalias,
                                noalias_scope_lists[dst_idx]);
            } else if (bpp == 2) {
                final_val =
                    builder.CreateTrunc(final_val, builder.getInt16Ty());
                llvm::StoreInst* si =
                    builder.CreateStore(final_val, pixel_addr);
                si->setAlignment(llvm::Align(pixel_align));
                si->setMetadata(llvm::LLVMContext::MD_alias_scope,
                                alias_scope_lists[dst_idx]);
                si->setMetadata(llvm::LLVMContext::MD_noalias,
                                noalias_scope_lists[dst_idx]);
            } else { // bpp == 4
                llvm::StoreInst* si =
                    builder.CreateStore(final_val, pixel_addr);
                si->setAlignment(llvm::Align(pixel_align));
                si->setMetadata(llvm::LLVMContext::MD_alias_scope,
                                alias_scope_lists[dst_idx]);
                si->setMetadata(llvm::LLVMContext::MD_noalias,
                                noalias_scope_lists[dst_idx]);
            }
        } else { // stFloat
            if (bpp == 4) {
                llvm::StoreInst* si =
                    builder.CreateStore(value_to_store, pixel_addr);
                si->setAlignment(llvm::Align(pixel_align));
                si->setMetadata(llvm::LLVMContext::MD_alias_scope,
                                alias_scope_lists[dst_idx]);
                si->setMetadata(llvm::LLVMContext::MD_noalias,
                                noalias_scope_lists[dst_idx]);
            } else if (bpp == 2) {
                llvm::Value* truncated_val =
                    builder.CreateFPTrunc(value_to_store, builder.getHalfTy());
                llvm::StoreInst* si =
                    builder.CreateStore(truncated_val, pixel_addr);
                si->setAlignment(llvm::Align(pixel_align));
                si->setMetadata(llvm::LLVMContext::MD_alias_scope,
                                alias_scope_lists[dst_idx]);
                si->setMetadata(llvm::LLVMContext::MD_noalias,
                                noalias_scope_lists[dst_idx]);
            } else {
                throw std::runtime_error("Unsupported float sample size.");
            }
        }
    }

    llvm::Value* generate_boundary_check(llvm::Value* coord, llvm::Value* max,
                                         bool mirror) {
        if (mirror) {
            auto c0 = builder.getInt32(0);
            auto c2 = builder.getInt32(2);

            auto lt0 = builder.CreateICmpSLT(coord, c0);
            auto v_lt0 = builder.CreateSub(builder.getInt32(-1), coord);

            auto ge_max = builder.CreateICmpSGE(coord, max);
            auto v_ge_max = builder.CreateSub(
                builder.CreateSub(builder.CreateMul(c2, max), c2), coord);

            auto result = builder.CreateSelect(lt0, v_lt0, coord);
            result = builder.CreateSelect(ge_max, v_ge_max, result);
            return result;

        } else { // Clamp
            auto c0 = builder.getInt32(0);
            auto max_minus_1 = builder.CreateSub(max, builder.getInt32(1));

            auto ge0 = builder.CreateICmpSGE(coord, c0);
            auto v_ge0 = builder.CreateSelect(ge0, coord, c0);

            auto lt_max = builder.CreateICmpSLT(v_ge0, max);
            auto v_lt_max = builder.CreateSelect(lt_max, v_ge0, max_minus_1);
            return v_lt_max;
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
        props[0] = (float)n;

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
                props[prop_array_idx] = (float)vsapi->propGetInt(
                    props_map, prop_name.c_str(), 0, &err);
            } else if (type == ptFloat) {
                props[prop_array_idx] = (float)vsapi->propGetFloat(
                    props_map, prop_name.c_str(), 0, &err);
            } else if (type == ptData) {
                if (vsapi->propGetDataSize(props_map, prop_name.c_str(), 0,
                                           &err) > 0 &&
                    !err)
                    props[prop_array_idx] = (float)vsapi->propGetData(
                        props_map, prop_name.c_str(), 0, &err)[0];
                else
                    err = 1;
            } else {
                err = 1;
            }

            if (err) {
                props[prop_array_idx] = NAN;
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
        vsapi->setError(out, (std::string("Expr: ") + e.what()).c_str());
        return;
    }

    vsapi->createFilter(in, out, "Expr", exprInit, exprGetFrame, exprFree,
                        fmParallel, 0, d.release(), core);
}

} // anonymous namespace

VS_EXTERNAL_API(void)
VapourSynthPluginInit(VSConfigPlugin configFunc,
                      VSRegisterFunction registerFunc, VSPlugin* plugin) {
    configFunc("com.yuygfgg.llvmexpr", "yuygfgg",
               "LLVM JIT RPN Expression Filter", VAPOURSYNTH_API_VERSION, 1,
               plugin);
    registerFunc("Expr",
                 "clips:clip[];expr:data[];boundary:int:opt;dump_ir:data:opt;",
                 exprCreate, nullptr, plugin);
}