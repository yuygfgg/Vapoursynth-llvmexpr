#include <algorithm>
#include <cctype>
#include <functional>
#include <map>
#include <memory>
#include <mutex>
#include <regex>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include "VSHelper.h"
#include "VapourSynth.h"

#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/CodeGen.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"

#include "llvm/Support/FileSystem.h"
#include <system_error>

#include "optimal_sorting_networks.hpp"

// For JIT functions to call cmath functions
#include <cmath>

namespace {

void compare_swap_ir(llvm::IRBuilder<>& builder, llvm::Value* array, int i,
                     int j, llvm::Type* float_ty) {
    llvm::Value* val_i_ptr =
        builder.CreateGEP(float_ty, array, builder.getInt32(i));
    llvm::Value* val_j_ptr =
        builder.CreateGEP(float_ty, array, builder.getInt32(j));
    llvm::Value* val_i = builder.CreateLoad(float_ty, val_i_ptr, "val_i");
    llvm::Value* val_j = builder.CreateLoad(float_ty, val_j_ptr, "val_j");

    llvm::Value* cond = builder.CreateFCmpOGT(val_i, val_j);

    llvm::Value* min_val = builder.CreateSelect(cond, val_j, val_i);
    llvm::Value* max_val = builder.CreateSelect(cond, val_i, val_j);

    builder.CreateStore(min_val, val_i_ptr);
    builder.CreateStore(max_val, val_j_ptr);
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

        // Allow JIT to find symbols in the host process (for cmath functions)
        auto& dylib = lljit->getMainJITDylib();
        dylib.addGenerator(llvm::cantFail(
            llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(
                lljit->getDataLayout().getGlobalPrefix())));
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
    static constexpr int kStackSize = 4096; // TODO: remove hard limit
    std::string rpn_expr;
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

    // Create allocas in the function entry block to avoid per-iteration stack
    // growth
    llvm::AllocaInst* createAllocaInEntry(llvm::Type* type,
                                          llvm::Value* arraySize,
                                          const std::string& name) {
        llvm::IRBuilder<> entryBuilder(&func->getEntryBlock(),
                                       func->getEntryBlock().begin());
        return entryBuilder.CreateAlloca(type, arraySize, name);
    }
    llvm::AllocaInst* createAllocaInEntry(llvm::Type* type,
                                          const std::string& name) {
        return createAllocaInEntry(type, nullptr, name);
    }

  public:
    Compiler(std::string expr, const VSVideoInfo* out_vi,
             const std::vector<const VSVideoInfo*>& in_vi, bool mirror,
             std::string dump_path,
             const std::map<std::pair<int, std::string>, int>& p_map,
             std::string function_name)
        : rpn_expr(std::move(expr)), vo(out_vi), vi(in_vi),
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
        // Validate the RPN expression stack behavior before generating IR
        validate_rpn_stack();
        define_function_signature();
        generate_loops();

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

        if (llvm::verifyFunction(*func, &llvm::errs())) {
            module->print(llvm::errs(), nullptr);
            throw std::runtime_error("LLVM function verification failed.");
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
    // Validate stack balance and under/overflow for the RPN expression
    void validate_rpn_stack() {
        const int stack_limit = kStackSize;
        int sp = 0;
        int max_sp = 0;
        std::unordered_set<std::string> defined_vars;

        auto require = [&](int need, const std::string& op) {
            if (sp < need) {
                throw std::runtime_error("Stack underflow on '" + op + "'");
            }
        };
        auto push_once = [&]() {
            ++sp;
            if (sp > max_sp)
                max_sp = sp;
            if (sp > stack_limit) {
                throw std::runtime_error("Stack overflow: requires depth " +
                                         std::to_string(sp) + " but limit is " +
                                         std::to_string(stack_limit));
            }
        };

        std::stringstream ss(rpn_expr);
        std::string token;
        size_t last_search_pos = 0;

        std::regex re_rel(
            R"(^(?:src(\d+)|([x-za-w]))\((-?\d+),(-?\d+)\)(?::([cm]))?$)");
        std::regex re_rel_bracket(
            R"(^(?:src(\d+)|([x-za-w]))\[(-?\d+),(-?\d+)\](?::([cm]))?$)");
        std::regex re_abs(R"(^(?:src(\d+)|([x-za-w]))\[\]$)");
        std::regex re_cur(R"(^(?:src(\d+)|([x-za-w]))$)");
        std::regex re_prop(
            R"(^(?:src(\d+)|([x-za-w]))\.([a-zA-Z_][a-zA-Z0-9_]*)$)");

        while (ss >> token) {
            size_t current_token_start = 0;
            {
                std::streampos pos_end = ss.tellg();
                if (pos_end != std::streampos(-1)) {
                    size_t end_idx = static_cast<size_t>(pos_end);
                    current_token_start = (end_idx >= token.size())
                                              ? (end_idx - token.size())
                                              : 0;
                    last_search_pos = current_token_start + token.size();
                } else {
                    size_t found = rpn_expr.find(token, last_search_pos);
                    if (found != std::string::npos) {
                        current_token_start = found;
                        last_search_pos = found + token.size();
                    } else {
                        current_token_start = 0;
                    }
                }
            }
            auto context_for_current = [&]() -> std::string {
                size_t expr_len = rpn_expr.size();
                size_t s =
                    (current_token_start > 10) ? (current_token_start - 10) : 0;
                size_t e =
                    std::min(current_token_start + token.size() + 10, expr_len);
                return rpn_expr.substr(s, e - s);
            };
            if (token == "+" || token == "-" || token == "*" || token == "/" ||
                token == "%" || token == ">" || token == "<" || token == ">=" ||
                token == "<=" || token == "=" || token == "and" ||
                token == "or" || token == "xor" || token == "bitand" ||
                token == "bitor" || token == "bitxor" || token == "min" ||
                token == "max" || token == "pow" || token == "**" ||
                token == "atan2") {
                require(2, token);
                // 2 -> 1
                --sp;
                continue;
            }

            if (token == "not" || token == "bitnot" || token == "sqrt" ||
                token == "exp" || token == "log" || token == "abs" ||
                token == "floor" || token == "ceil" || token == "trunc" ||
                token == "round" || token == "sin" || token == "cos" ||
                token == "tan" || token == "asin" || token == "acos" ||
                token == "atan") {
                require(1, token);
                // 1 -> 1
                continue;
            }

            if (token == "?" || token == "clip" || token == "clamp") {
                require(3, token);
                // 3 -> 1
                sp -= 2;
                continue;
            }

            if (token == "dup") {
                require(1, token);
                push_once();
                continue;
            }
            if (token.rfind("dup", 0) == 0 && token.length() > 3) {
                try {
                    int n = std::stoi(token.substr(3));
                    if (n < 0)
                        throw std::runtime_error("");
                    if (sp < 1 + n)
                        throw std::runtime_error("Stack underflow on '" +
                                                 token + "'");
                    push_once();
                } catch (...) {
                    throw std::runtime_error("Invalid dupN operator: " + token);
                }
                continue;
            }

            if (token == "drop") {
                require(1, token);
                --sp;
                continue;
            }
            if (token.rfind("drop", 0) == 0 && token.length() > 4) {
                try {
                    int n = std::stoi(token.substr(4));
                    if (n < 0)
                        throw std::runtime_error("");
                    if (sp < n)
                        throw std::runtime_error("Stack underflow on '" +
                                                 token + "'");
                    sp -= n;
                } catch (...) {
                    throw std::runtime_error("Invalid dropN operator: " +
                                             token);
                }
                continue;
            }

            if (token == "swap") {
                require(2, token);
                // unchanged
                continue;
            }
            if (token.rfind("swap", 0) == 0 && token.length() > 4) {
                try {
                    int n = std::stoi(token.substr(4));
                    if (n < 0)
                        throw std::runtime_error("");
                    if (sp < 1 + n)
                        throw std::runtime_error("Stack underflow on '" +
                                                 token + "'");
                } catch (...) {
                    throw std::runtime_error("Invalid swapN operator: " +
                                             token);
                }
                continue;
            }

            if (token.rfind("sort", 0) == 0 && token.length() > 4) {
                try {
                    int n = std::stoi(token.substr(4));
                    if (n < 0)
                        throw std::runtime_error("");
                    if (n < 2)
                        continue; // no-op
                    if (sp < n)
                        throw std::runtime_error("Stack underflow on '" +
                                                 token + "'");
                } catch (...) {
                    throw std::runtime_error("Invalid sortN operator: " +
                                             token);
                }
                continue;
            }

            if (token == "X" || token == "Y" || token == "width" ||
                token == "height" || token == "N" || token == "pi") {
                push_once();
                continue;
            }

            if (!token.empty() && token.back() == '!') {
                std::string var_name = token.substr(0, token.length() - 1);
                require(1, token);
                --sp;
                defined_vars.insert(var_name);
                continue;
            }
            if (!token.empty() && token.back() == '@') {
                std::string var_name = token.substr(0, token.length() - 1);
                if (defined_vars.find(var_name) == defined_vars.end()) {
                    throw std::runtime_error("Unknown variable: " + var_name);
                }
                push_once();
                continue;
            }

            {
                std::smatch match;
                if (std::regex_match(token, match, re_prop)) {
                    int clip_idx = -1;
                    if (match[1].matched)
                        clip_idx = std::stoi(match[1].str());
                    else if (match[2].matched) {
                        char c = match[2].str()[0];
                        if (c >= 'x' && c <= 'z')
                            clip_idx = c - 'x';
                        else
                            clip_idx = c - 'a' + 3;
                    }
                    if (clip_idx < 0 || clip_idx >= num_inputs) {
                        throw std::runtime_error(
                            "Invalid clip index in property access: " + token);
                    }
                    push_once();
                    continue;
                }
            }

            // Clip access or number literal or invalid token
            {
                std::smatch match;
                bool is_rel = std::regex_match(token, match, re_rel) ||
                              std::regex_match(token, match, re_rel_bracket);
                bool is_abs = !is_rel && std::regex_match(token, match, re_abs);
                bool is_cur = !is_rel && !is_abs &&
                              std::regex_match(token, match, re_cur);
                if (is_rel || is_abs || is_cur) {
                    int clip_idx = -1;
                    if (match[1].matched)
                        clip_idx = std::stoi(match[1].str());
                    else if (match[2].matched) {
                        char c = match[2].str()[0];
                        if (c >= 'x' && c <= 'z')
                            clip_idx = c - 'x';
                        else
                            clip_idx = c - 'a' + 3;
                    }
                    if (clip_idx < 0 || clip_idx >= num_inputs) {
                        throw std::runtime_error(
                            "Invalid clip index in token: " + token);
                    }
                    if (is_abs) {
                        require(2, token);
                        sp -= 2;
                        push_once();
                    } else {
                        push_once();
                    }
                    continue;
                }
            }

            // number literal detection
            try {
                size_t pos = 0;
                (void)std::stod(token, &pos);
                if (pos == token.length()) {
                    push_once();
                    continue;
                }
            } catch (...) {
            }
            try {
                size_t pos = 0;
                (void)std::stoll(token, &pos, 0);
                if (pos == token.length()) {
                    push_once();
                    continue;
                }
            } catch (...) {
            }

            {
                bool all_digits = !token.empty();
                for (unsigned char ch : token) {
                    if (!(ch >= '0' && ch <= '9')) {
                        all_digits = false;
                        break;
                    }
                }
                if (all_digits) {
                    push_once();
                    continue;
                }
            }

            throw std::runtime_error("Invalid token: " + token +
                                     " | context: " + context_for_current());
        }

        if (sp != 1) {
            throw std::runtime_error(
                "Expression stack not balanced: final stack depth = " +
                std::to_string(sp) + ", expected 1.");
        }
    }

    // Helper to declare cmath functions in the LLVM module
    llvm::Function* getOrDeclareMathFunc(const std::string& name,
                                         llvm::FunctionType* ty) {
        llvm::Function* f = module->getFunction(name);
        if (!f) {
            f = llvm::Function::Create(ty, llvm::Function::ExternalLinkage,
                                       name, module.get());
        }
        return f;
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

        // Add metadata to hint loop vectorization
        llvm::MDNode* loop_metadata = llvm::MDNode::get(
            *context,
            {llvm::MDString::get(*context, "llvm.loop.vectorize.enable"),
             llvm::ConstantAsMetadata::get(
                 llvm::ConstantInt::get(llvm::Type::getInt1Ty(*context), 1))});
        llvm::BranchInst* loop_br =
            builder.CreateCondBr(x_cond, loop_x_body, loop_x_exit);
        loop_br->setMetadata(llvm::LLVMContext::MD_loop, loop_metadata);

        builder.SetInsertPoint(loop_x_body);
        generate_rpn_logic(x_val, y_val);

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

    void generate_rpn_logic(llvm::Value* x, llvm::Value* y) {
        llvm::Type* float_ty = builder.getFloatTy();
        llvm::Type* i32_ty = builder.getInt32Ty();

        std::unordered_map<std::string, llvm::Value*> named_vars;

        std::vector<llvm::Value*> stack;
        stack.reserve(128);

        auto push = [&](llvm::Value* val) { stack.push_back(val); };
        auto pop = [&]() -> llvm::Value* {
            llvm::Value* v = stack.back();
            stack.pop_back();
            return v;
        };

        std::stringstream ss(rpn_expr);
        std::string token;
        size_t last_search_pos = 0;

        std::regex re_rel(
            R"(^(?:src(\d+)|([x-za-w]))\((-?\d+),(-?\d+)\)(?::([cm]))?$)");
        std::regex re_rel_bracket(
            R"(^(?:src(\d+)|([x-za-w]))\[(-?\d+),(-?\d+)\](?::([cm]))?$)");
        std::regex re_abs(R"(^(?:src(\d+)|([x-za-w]))\[\]$)");
        std::regex re_cur(R"(^(?:src(\d+)|([x-za-w]))$)");
        std::regex re_prop(
            R"(^(?:src(\d+)|([x-za-w]))\.([a-zA-Z_][a-zA-Z0-9_]*)$)");

        while (ss >> token) {
            size_t current_token_start = 0;
            {
                std::streampos pos_end = ss.tellg();
                if (pos_end != std::streampos(-1)) {
                    size_t end_idx = static_cast<size_t>(pos_end);
                    current_token_start = (end_idx >= token.size())
                                              ? (end_idx - token.size())
                                              : 0;
                    last_search_pos = current_token_start + token.size();
                } else {
                    size_t found = rpn_expr.find(token, last_search_pos);
                    if (found != std::string::npos) {
                        current_token_start = found;
                        last_search_pos = found + token.size();
                    } else {
                        current_token_start = 0;
                    }
                }
            }
            auto context_for_current = [&]() -> std::string {
                size_t expr_len = rpn_expr.size();
                size_t s =
                    (current_token_start > 10) ? (current_token_start - 10) : 0;
                size_t e =
                    std::min(current_token_start + token.size() + 10, expr_len);
                return rpn_expr.substr(s, e - s);
            };
            if (token == "+") {
                auto b = pop();
                auto a = pop();
                push(builder.CreateFAdd(a, b));
            } else if (token == "-") {
                auto b = pop();
                auto a = pop();
                push(builder.CreateFSub(a, b));
            } else if (token == "*") {
                auto b = pop();
                auto a = pop();
                push(builder.CreateFMul(a, b));
            } else if (token == "/") {
                auto b = pop();
                auto a = pop();
                push(builder.CreateFDiv(a, b));
            } else if (token == "%") {
                auto b = pop();
                auto a = pop();
                push(builder.CreateFRem(a, b));
            }

            else if (token == ">") {
                auto b = pop();
                auto a = pop();
                push(
                    builder.CreateSelect(builder.CreateFCmpOGT(a, b),
                                         llvm::ConstantFP::get(float_ty, 1.0),
                                         llvm::ConstantFP::get(float_ty, 0.0)));
            } else if (token == "<") {
                auto b = pop();
                auto a = pop();
                push(
                    builder.CreateSelect(builder.CreateFCmpOLT(a, b),
                                         llvm::ConstantFP::get(float_ty, 1.0),
                                         llvm::ConstantFP::get(float_ty, 0.0)));
            } else if (token == ">=") {
                auto b = pop();
                auto a = pop();
                push(
                    builder.CreateSelect(builder.CreateFCmpOGE(a, b),
                                         llvm::ConstantFP::get(float_ty, 1.0),
                                         llvm::ConstantFP::get(float_ty, 0.0)));
            } else if (token == "<=") {
                auto b = pop();
                auto a = pop();
                push(
                    builder.CreateSelect(builder.CreateFCmpOLE(a, b),
                                         llvm::ConstantFP::get(float_ty, 1.0),
                                         llvm::ConstantFP::get(float_ty, 0.0)));
            } else if (token == "=") {
                auto b = pop();
                auto a = pop();
                push(
                    builder.CreateSelect(builder.CreateFCmpOEQ(a, b),
                                         llvm::ConstantFP::get(float_ty, 1.0),
                                         llvm::ConstantFP::get(float_ty, 0.0)));
            }

            else if (token == "and") {
                auto b = pop();
                auto a = pop();
                push(builder.CreateSelect(
                    builder.CreateAnd(
                        builder.CreateFCmpOGT(
                            a, llvm::ConstantFP::get(float_ty, 0.0)),
                        builder.CreateFCmpOGT(
                            b, llvm::ConstantFP::get(float_ty, 0.0))),
                    llvm::ConstantFP::get(float_ty, 1.0),
                    llvm::ConstantFP::get(float_ty, 0.0)));
            } else if (token == "or") {
                auto b = pop();
                auto a = pop();
                push(builder.CreateSelect(
                    builder.CreateOr(
                        builder.CreateFCmpOGT(
                            a, llvm::ConstantFP::get(float_ty, 0.0)),
                        builder.CreateFCmpOGT(
                            b, llvm::ConstantFP::get(float_ty, 0.0))),
                    llvm::ConstantFP::get(float_ty, 1.0),
                    llvm::ConstantFP::get(float_ty, 0.0)));
            } else if (token == "xor") {
                auto b = pop();
                auto a = pop();
                push(builder.CreateSelect(
                    builder.CreateXor(
                        builder.CreateFCmpOGT(
                            a, llvm::ConstantFP::get(float_ty, 0.0)),
                        builder.CreateFCmpOGT(
                            b, llvm::ConstantFP::get(float_ty, 0.0))),
                    llvm::ConstantFP::get(float_ty, 1.0),
                    llvm::ConstantFP::get(float_ty, 0.0)));
            } else if (token == "not") {
                auto a = pop();
                push(builder.CreateSelect(
                    builder.CreateFCmpOLE(a,
                                          llvm::ConstantFP::get(float_ty, 0.0)),
                    llvm::ConstantFP::get(float_ty, 1.0),
                    llvm::ConstantFP::get(float_ty, 0.0)));
            }

            else if (token == "bitand") {
                auto b = pop();
                auto a = pop();
                push(builder.CreateSIToFP(
                    builder.CreateAnd(builder.CreateFPToSI(a, i32_ty),
                                      builder.CreateFPToSI(b, i32_ty)),
                    float_ty));
            } else if (token == "bitor") {
                auto b = pop();
                auto a = pop();
                push(builder.CreateSIToFP(
                    builder.CreateOr(builder.CreateFPToSI(a, i32_ty),
                                     builder.CreateFPToSI(b, i32_ty)),
                    float_ty));
            } else if (token == "bitxor") {
                auto b = pop();
                auto a = pop();
                push(builder.CreateSIToFP(
                    builder.CreateXor(builder.CreateFPToSI(a, i32_ty),
                                      builder.CreateFPToSI(b, i32_ty)),
                    float_ty));
            } else if (token == "bitnot") {
                auto a = pop();
                push(builder.CreateSIToFP(
                    builder.CreateNot(builder.CreateFPToSI(a, i32_ty)),
                    float_ty));
            }

            else if (token == "?") {
                auto c = pop();
                auto b = pop();
                auto a = pop();
                push(builder.CreateSelect(
                    builder.CreateFCmpOGT(a,
                                          llvm::ConstantFP::get(float_ty, 0.0)),
                    b, c));
            }

            else if (token == "dup") {
                push(stack.back());
            } else if (token.rfind("dup", 0) == 0 && token.length() > 3) {
                try {
                    int n = std::stoi(token.substr(3));
                    llvm::Value* val = stack[stack.size() - 1 - n];
                    push(val);
                } catch (...) {
                    throw std::runtime_error("Invalid dupN operator: " + token);
                }
            } else if (token == "drop") {
                (void)pop();
            } else if (token.rfind("drop", 0) == 0 && token.length() > 4) {
                try {
                    int n = std::stoi(token.substr(4));
                    stack.resize(stack.size() - n);
                } catch (...) {
                    throw std::runtime_error("Invalid dropN operator: " +
                                             token);
                }
            } else if (token == "swap") {
                std::swap(stack[stack.size() - 1], stack[stack.size() - 2]);
            } else if (token.rfind("swap", 0) == 0 && token.length() > 4) {
                try {
                    int n = std::stoi(token.substr(4));
                    size_t top_index = stack.size() - 1;
                    size_t n_index = stack.size() - 1 - n;
                    std::swap(stack[top_index], stack[n_index]);
                } catch (...) {
                    throw std::runtime_error("Invalid swapN operator: " +
                                             token);
                }
            } else if (token.rfind("sort", 0) == 0 && token.length() > 4) {
                try {
                    int n = std::stoi(token.substr(4));
                    if (n < 2)
                        continue;

                    llvm::Value* array = createAllocaInEntry(
                        float_ty, builder.getInt32(n), "sort_array");

                    for (int i = 0; i < n; ++i) {
                        llvm::Value* val = pop();
                        llvm::Value* elem_ptr = builder.CreateGEP(
                            float_ty, array, builder.getInt32(n - 1 - i));
                        builder.CreateStore(val, elem_ptr);
                    }

                    // Generate sorting network
                    const auto network = get_optimal_sorting_network(n);
                    if (!network.empty()) {
                        for (const auto& pair : network) {
                            if (pair.second < n) { // Bounds check
                                compare_swap_ir(builder, array, pair.first,
                                                pair.second, float_ty);
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
                                compare_swap_ir(builder, array, pair.first,
                                                pair.second, float_ty);
                            }
                        }
                    }

                    // Push the sorted values back onto the stack, so the
                    // smallest is on top
                    for (int i = 0; i < n; ++i) {
                        llvm::Value* elem_ptr = builder.CreateGEP(
                            float_ty, array, builder.getInt32(n - 1 - i));
                        llvm::Value* val =
                            builder.CreateLoad(float_ty, elem_ptr);
                        push(val);
                    }
                } catch (...) {
                    throw std::runtime_error("Invalid sortN operator: " +
                                             token);
                }
            }

            else if (token == "X")
                push(builder.CreateSIToFP(x, float_ty));
            else if (token == "Y")
                push(builder.CreateSIToFP(y, float_ty));
            else if (token == "width")
                push(builder.CreateSIToFP(width_arg, float_ty));
            else if (token == "height")
                push(builder.CreateSIToFP(height_arg, float_ty));
            else if (token == "N")
                push(builder.CreateLoad(
                    float_ty, builder.CreateGEP(float_ty, props_arg,
                                                builder.getInt32(0))));
            else if (token == "pi")
                push(llvm::ConstantFP::get(float_ty, M_PI));

            else if (token == "sqrt") {
                push(builder.CreateCall(
                    llvm::Intrinsic::getOrInsertDeclaration(
                        module.get(), llvm::Intrinsic::sqrt, {float_ty}),
                    {pop()}));
            } else if (token == "pow" || token == "**") {
                auto b = pop();
                auto a = pop();
                push(builder.CreateCall(
                    llvm::Intrinsic::getOrInsertDeclaration(
                        module.get(), llvm::Intrinsic::pow, {float_ty}),
                    {a, b}));
            } else if (token == "exp") {
                push(builder.CreateCall(
                    llvm::Intrinsic::getOrInsertDeclaration(
                        module.get(), llvm::Intrinsic::exp, {float_ty}),
                    {pop()}));
            } else if (token == "log") {
                push(builder.CreateCall(
                    llvm::Intrinsic::getOrInsertDeclaration(
                        module.get(), llvm::Intrinsic::log, {float_ty}),
                    {pop()}));
            } else if (token == "abs") {
                push(builder.CreateCall(
                    llvm::Intrinsic::getOrInsertDeclaration(
                        module.get(), llvm::Intrinsic::fabs, {float_ty}),
                    {pop()}));
            } else if (token == "floor") {
                push(builder.CreateCall(
                    llvm::Intrinsic::getOrInsertDeclaration(
                        module.get(), llvm::Intrinsic::floor, {float_ty}),
                    {pop()}));
            } else if (token == "ceil") {
                push(builder.CreateCall(
                    llvm::Intrinsic::getOrInsertDeclaration(
                        module.get(), llvm::Intrinsic::ceil, {float_ty}),
                    {pop()}));
            } else if (token == "trunc") {
                push(builder.CreateCall(
                    llvm::Intrinsic::getOrInsertDeclaration(
                        module.get(), llvm::Intrinsic::trunc, {float_ty}),
                    {pop()}));
            } else if (token == "round") {
                push(builder.CreateCall(
                    llvm::Intrinsic::getOrInsertDeclaration(
                        module.get(), llvm::Intrinsic::round, {float_ty}),
                    {pop()}));
            } else if (token == "min") {
                auto b = pop();
                auto a = pop();
                push(builder.CreateCall(
                    llvm::Intrinsic::getOrInsertDeclaration(
                        module.get(), llvm::Intrinsic::minnum, {float_ty}),
                    {a, b}));
            } else if (token == "max") {
                auto b = pop();
                auto a = pop();
                push(builder.CreateCall(
                    llvm::Intrinsic::getOrInsertDeclaration(
                        module.get(), llvm::Intrinsic::maxnum, {float_ty}),
                    {a, b}));
            } else if (token == "clip" || token == "clamp") {
                auto max_val = pop();
                auto min_val = pop();
                auto val = pop();
                auto temp = builder.CreateCall(
                    llvm::Intrinsic::getOrInsertDeclaration(
                        module.get(), llvm::Intrinsic::maxnum, {float_ty}),
                    {val, min_val});
                auto clamped = builder.CreateCall(
                    llvm::Intrinsic::getOrInsertDeclaration(
                        module.get(), llvm::Intrinsic::minnum, {float_ty}),
                    {temp, max_val});
                push(clamped);
            }

            else if (token == "sin" || token == "cos") {
                llvm::Intrinsic::ID intrinsic_id = (token == "sin")
                                                       ? llvm::Intrinsic::sin
                                                       : llvm::Intrinsic::cos;
                push(builder.CreateCall(
                    llvm::Intrinsic::getOrInsertDeclaration(
                        module.get(), intrinsic_id, {float_ty}),
                    {pop()}));
            } else if (token == "tan" || token == "asin" || token == "acos" ||
                       token == "atan") {
                llvm::FunctionType* func_ty =
                    llvm::FunctionType::get(float_ty, {float_ty}, false);
                llvm::Function* f = getOrDeclareMathFunc(token + "f", func_ty);
                push(builder.CreateCall(f, {pop()}));
            } else if (token == "atan2") {
                llvm::FunctionType* func_ty = llvm::FunctionType::get(
                    float_ty, {float_ty, float_ty}, false);
                llvm::Function* f = getOrDeclareMathFunc(token + "f", func_ty);
                llvm::Value* arg2 = pop();
                llvm::Value* arg1 = pop();
                push(builder.CreateCall(f, {arg1, arg2}));
            }

            else if (token.back() == '!') {
                std::string var_name = token.substr(0, token.length() - 1);
                llvm::Value* val_to_store = pop();
                llvm::Value* var_ptr;
                if (named_vars.find(var_name) == named_vars.end()) {
                    var_ptr = createAllocaInEntry(float_ty, nullptr, var_name);
                    named_vars[var_name] = var_ptr;
                } else {
                    var_ptr = named_vars[var_name];
                }
                builder.CreateStore(val_to_store, var_ptr);
            } else if (token.back() == '@') {
                std::string var_name = token.substr(0, token.length() - 1);
                if (named_vars.find(var_name) == named_vars.end()) {
                    throw std::runtime_error("Unknown variable: " + var_name);
                }
                llvm::Value* var_ptr = named_vars[var_name];
                push(builder.CreateLoad(float_ty, var_ptr));
            }

            else {
                std::smatch match;
                if (std::regex_match(token, match, re_prop)) {
                    int clip_idx = -1;
                    if (match[1].matched)
                        clip_idx = std::stoi(match[1].str());
                    else if (match[2].matched) {
                        char c = match[2].str()[0];
                        if (c >= 'x' && c <= 'z')
                            clip_idx = c - 'x';
                        else
                            clip_idx = c - 'a' + 3;
                    }
                    std::string prop_name = match[3].str();
                    auto key = std::make_pair(clip_idx, prop_name);
                    if (prop_map.count(key) == 0) {
                        // This should not happen if logic is correct
                        throw std::runtime_error(
                            "Internal error: property not found in map: " +
                            token);
                    }
                    int prop_idx = prop_map.at(key);
                    llvm::Value* prop_val = builder.CreateLoad(
                        float_ty,
                        builder.CreateGEP(float_ty, props_arg,
                                          builder.getInt32(prop_idx)));
                    push(prop_val);
                } else {
                    int clip_idx = -1;
                    bool is_rel =
                        std::regex_match(token, match, re_rel) ||
                        std::regex_match(token, match, re_rel_bracket);
                    bool is_abs =
                        !is_rel && std::regex_match(token, match, re_abs);
                    bool is_cur = !is_rel && !is_abs &&
                                  std::regex_match(token, match, re_cur);

                    if (is_rel || is_abs || is_cur) {
                        if (match[1].matched)
                            clip_idx = std::stoi(match[1].str());
                        else if (match[2].matched) {
                            char c = match[2].str()[0];
                            if (c >= 'x' && c <= 'z') {
                                clip_idx = c - 'x';
                            } else { // a-w
                                clip_idx = c - 'a' + 3;
                            }
                        }

                        if (clip_idx < 0 || clip_idx >= num_inputs)
                            throw std::runtime_error(
                                "Invalid clip index in token: " + token);

                        llvm::Value *coord_x, *coord_y;
                        if (is_rel) {
                            coord_x = builder.CreateAdd(
                                x, builder.getInt32(std::stoi(match[3].str())));
                            coord_y = builder.CreateAdd(
                                y, builder.getInt32(std::stoi(match[4].str())));
                            bool use_mirror = mirror_boundary;
                            if (match[5].matched)
                                use_mirror = (match[5].str() == "m");
                            push(generate_pixel_load(clip_idx, coord_x, coord_y,
                                                     use_mirror));
                        } else if (is_abs) {
                            coord_y = builder.CreateFPToSI(pop(), i32_ty);
                            coord_x = builder.CreateFPToSI(pop(), i32_ty);
                            push(generate_pixel_load(clip_idx, coord_x, coord_y,
                                                     mirror_boundary));
                        } else { // is_cur
                            push(generate_pixel_load(clip_idx, x, y,
                                                     mirror_boundary));
                        }
                    } else { // Number
                        try {
                            size_t pos;
                            long long val =
                                std::stoll(token, &pos, 0); // 0 detects base
                            if (pos == token.length()) {
                                push(llvm::ConstantFP::get(float_ty,
                                                           (double)val));
                                continue;
                            }
                        } catch (...) {
                        }
                        try {
                            size_t pos;
                            double val = std::stod(token, &pos);
                            if (pos == token.length()) {
                                push(llvm::ConstantFP::get(float_ty, val));
                                continue;
                            }
                        } catch (...) {
                        }
                        {
                            bool all_digits = !token.empty();
                            for (unsigned char ch : token) {
                                if (!(ch >= '0' && ch <= '9')) {
                                    all_digits = false;
                                    break;
                                }
                            }
                            if (all_digits) {
                                double val = 0.0;
                                for (unsigned char ch : token) {
                                    val = val * 10.0 + (ch - '0');
                                }
                                push(llvm::ConstantFP::get(float_ty, val));
                                continue;
                            }
                        }
                        throw std::runtime_error(
                            "Invalid token: " + token +
                            " | context: " + context_for_current());
                    }
                }
            }
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

        llvm::Value* base_ptr = builder.CreateLoad(
            llvm::PointerType::get(*context, 0),
            builder.CreateGEP(llvm::PointerType::get(*context, 0), rwptrs_arg,
                              builder.getInt32(vs_clip_idx)));
        llvm::Value* stride = builder.CreateLoad(
            builder.getInt32Ty(),
            builder.CreateGEP(builder.getInt32Ty(), strides_arg,
                              builder.getInt32(vs_clip_idx)));

        llvm::Value* y_offset = builder.CreateMul(clamped_y, stride);
        llvm::Value* x_offset =
            builder.CreateMul(clamped_x, builder.getInt32(bpp));
        llvm::Value* total_offset = builder.CreateAdd(y_offset, x_offset);
        llvm::Value* pixel_addr =
            builder.CreateGEP(builder.getInt8Ty(), base_ptr, total_offset);

        llvm::Value* loaded_val;
        if (format->sampleType == stInteger) {
            if (bpp == 1) {
                loaded_val =
                    builder.CreateLoad(builder.getInt8Ty(), pixel_addr);
                loaded_val =
                    builder.CreateZExt(loaded_val, builder.getInt32Ty());
            } else if (bpp == 2) {
                loaded_val =
                    builder.CreateLoad(builder.getInt16Ty(), pixel_addr);
                loaded_val =
                    builder.CreateZExt(loaded_val, builder.getInt32Ty());
            } else { // bpp == 4
                loaded_val =
                    builder.CreateLoad(builder.getInt32Ty(), pixel_addr);
            }
            return builder.CreateSIToFP(loaded_val, builder.getFloatTy());
        } else { // stFloat
            if (bpp == 4) {
                return builder.CreateLoad(builder.getFloatTy(), pixel_addr);
            } else {
                throw std::runtime_error("16-bit float input not supported.");
            }
        }
    }

    void generate_pixel_store(llvm::Value* value_to_store, llvm::Value* x,
                              llvm::Value* y) {
        const VSFormat* format = vo->format;
        int bpp = format->bytesPerSample;
        int dst_idx = 0;

        llvm::Value* base_ptr = builder.CreateLoad(
            llvm::PointerType::get(*context, 0),
            builder.CreateGEP(llvm::PointerType::get(*context, 0), rwptrs_arg,
                              builder.getInt32(dst_idx)));
        llvm::Value* stride = builder.CreateLoad(
            builder.getInt32Ty(),
            builder.CreateGEP(builder.getInt32Ty(), strides_arg,
                              builder.getInt32(dst_idx)));

        llvm::Value* y_offset = builder.CreateMul(y, stride);
        llvm::Value* x_offset = builder.CreateMul(x, builder.getInt32(bpp));
        llvm::Value* total_offset = builder.CreateAdd(y_offset, x_offset);
        llvm::Value* pixel_addr =
            builder.CreateGEP(builder.getInt8Ty(), base_ptr, total_offset);

        llvm::Value* final_val;
        if (format->sampleType == stInteger) {
            int max_val = (1 << format->bitsPerSample) - 1;
            llvm::Value* zero_f =
                llvm::ConstantFP::get(builder.getFloatTy(), 0.0);
            llvm::Value* max_f =
                llvm::ConstantFP::get(builder.getFloatTy(), (double)max_val);

            llvm::Value* temp =
                builder.CreateCall(llvm::Intrinsic::getOrInsertDeclaration(
                                       module.get(), llvm::Intrinsic::maxnum,
                                       {builder.getFloatTy()}),
                                   {value_to_store, zero_f});
            llvm::Value* clamped_f =
                builder.CreateCall(llvm::Intrinsic::getOrInsertDeclaration(
                                       module.get(), llvm::Intrinsic::minnum,
                                       {builder.getFloatTy()}),
                                   {temp, max_f});

            final_val = builder.CreateFPToSI(clamped_f, builder.getInt32Ty());

            if (bpp == 1) {
                final_val = builder.CreateTrunc(final_val, builder.getInt8Ty());
                builder.CreateStore(final_val, pixel_addr);
            } else if (bpp == 2) {
                final_val =
                    builder.CreateTrunc(final_val, builder.getInt16Ty());
                builder.CreateStore(final_val, pixel_addr);
            } else { // bpp == 4
                builder.CreateStore(final_val, pixel_addr);
            }
        } else { // stFloat
            if (bpp == 4) {
                builder.CreateStore(value_to_store, pixel_addr);
            } else {
                throw std::runtime_error("16-bit float output not supported in "
                                         "this simplified JIT version.");
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
    std::string expr[3];
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
    std::stringstream ss;
    ss << "expr=" << expr << "|mirror=" << mirror << "|out=" << vo->format->id;
    for (size_t i = 0; i < vi.size(); ++i) {
        ss << "|in" << i << "=" << vi[i]->format->id;
    }
    for (const auto& [key, val] : prop_map) {
        ss << "|prop" << val << "=" << key.first << "." << key.second;
    }
    return ss.str();
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

        std::regex prop_re(
            R"((?:src(\d+)|([x-za-w]))\.([a-zA-Z_][a-zA-Z0-9_]*))");

        for (int i = 0; i < d->vi.format->numPlanes; ++i) {
            if (expr_strs[i].empty())
                continue;

            std::stringstream ss(expr_strs[i]);
            std::string token;
            while (ss >> token) {
                std::smatch match;
                if (std::regex_match(token, match, prop_re)) {
                    int clip_idx = -1;
                    if (match[1].matched) {
                        clip_idx = std::stoi(match[1].str());
                    } else { // match[2]
                        char c = match[2].str()[0];
                        if (c >= 'x' && c <= 'z')
                            clip_idx = c - 'x';
                        else
                            clip_idx = c - 'a' + 3;
                    }
                    std::string prop_name = match[3].str();

                    if (clip_idx < 0 || clip_idx >= d->num_inputs) {
                        throw std::runtime_error(
                            "Invalid clip index in property access: " + token);
                    }

                    auto key = std::make_pair(clip_idx, prop_name);
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
            if (expr_strs[i].empty()) {
                d->plane_op[i] = PO_COPY;
                continue;
            }
            d->plane_op[i] = PO_PROCESS;
            d->expr[i] = expr_strs[i];

            std::string key = generate_cache_key(
                d->expr[i], &d->vi, vi, d->mirror_boundary, d->prop_map);

            std::lock_guard<std::mutex> lock(cache_mutex);
            if (jit_cache.count(key)) {
                d->compiled[i] = jit_cache.at(key);
            } else {
                vsapi->logMessage(mtDebug,
                                  ("JIT compiling expression for plane " +
                                   std::to_string(i) + ": " + d->expr[i])
                                      .c_str());
                // Generate unique function name per compiled expression
                size_t key_hash = std::hash<std::string>{}(key);
                std::string func_name = "process_plane_" + std::to_string(i) +
                                        "_" + std::to_string(key_hash);

                Compiler compiler(d->expr[i], &d->vi, vi, d->mirror_boundary,
                                  d->dump_ir_path, d->prop_map, func_name);
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