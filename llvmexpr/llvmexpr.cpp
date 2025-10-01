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

#include <bit>
#include <format>
#include <map>
#include <memory>
#include <mutex>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

#include "VSHelper.h"
#include "VapourSynth.h"

#include "Compiler.hpp"
#include "Jit.hpp"
#include "Tokenizer.hpp"

constexpr uint32_t PROP_NAN_PAYLOAD = 0x7FC0BEEF; // qNaN with payload 0xBEEF

namespace {

enum PlaneOp { PO_PROCESS, PO_COPY };

struct ExprData {
    std::vector<VSNodeRef*> nodes;
    VSVideoInfo vi = {};
    int num_inputs;

    PlaneOp plane_op[3] = {};
    std::string expr_strs[3];
    CompiledFunction compiled[3];
    bool mirror_boundary;
    std::string dump_ir_path;
    int opt_level;
    int approx_math;

    std::vector<std::pair<int, std::string>> required_props;
    std::map<std::pair<int, std::string>, int> prop_map;
};

std::string
generate_cache_key(const std::string& expr, const VSVideoInfo* vo,
                   const std::vector<const VSVideoInfo*>& vi, bool mirror,
                   const std::map<std::pair<int, std::string>, int>& prop_map,
                   int plane_width, int plane_height) {
    std::string result =
        std::format("expr={}|mirror={}|out={}|w={}|h={}", expr, mirror,
                    vo->format->id, plane_width, plane_height);

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

                if (!d->compiled[plane].func_ptr) {
                    int width = vsapi->getFrameWidth(dst_frame, plane);
                    int height = vsapi->getFrameHeight(dst_frame, plane);

                    std::vector<const VSVideoInfo*> vi(d->num_inputs);
                    for (int i = 0; i < d->num_inputs; ++i) {
                        vi[i] = vsapi->getVideoInfo(d->nodes[i]);
                    }

                    const std::string key = generate_cache_key(
                        d->expr_strs[plane], &d->vi, vi, d->mirror_boundary,
                        d->prop_map, width, height);

                    std::lock_guard<std::mutex> lock(cache_mutex);
                    if (!jit_cache.count(key)) {
                        vsapi->logMessage(
                            mtDebug,
                            std::format("JIT compiling expression for plane {} "
                                        "({}x{}): {}",
                                        plane, width, height,
                                        d->expr_strs[plane])
                                .c_str());
                        size_t key_hash = std::hash<std::string>{}(key);
                        std::string func_name =
                            std::format("process_plane_{}_{}", plane, key_hash);

                        try {
                            Compiler compiler(
                                tokenize(d->expr_strs[plane], d->num_inputs),
                                &d->vi, vi, width, height, d->mirror_boundary,
                                d->dump_ir_path, d->prop_map, func_name,
                                d->opt_level, d->approx_math);
                            jit_cache[key] = compiler.compile();
                        } catch (const std::exception& e) {
                            std::string error_msg = std::format(
                                "Compilation error for plane {}: {}", plane,
                                e.what());
                            vsapi->logMessage(mtFatal, error_msg.c_str());
                            for (const auto& frame : src_frames) {
                                vsapi->freeFrame(frame);
                            }
                            vsapi->freeFrame(dst_frame);
                            return nullptr;
                        }
                    }
                    d->compiled[plane] = jit_cache.at(key);
                }

                d->compiled[plane].func_ptr(rwptrs.data(), strides.data(),
                                            props.data());
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

        // Tokenize, validate, and extract properties
        for (int i = 0; i < d->vi.format->numPlanes; ++i) {
            if (expr_strs[i].empty()) {
                d->plane_op[i] = PO_COPY;
                continue;
            }
            d->plane_op[i] = PO_PROCESS;
            d->expr_strs[i] = expr_strs[i];
            auto tokens = tokenize(d->expr_strs[i], d->num_inputs);

            for (const auto& token : tokens) {
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

            const int w =
                vi[0]->width >> (i > 0 ? d->vi.format->subSamplingW : 0);
            const int h =
                vi[0]->height >> (i > 0 ? d->vi.format->subSamplingH : 0);

            Compiler validator(std::move(tokens), &d->vi, vi, w, h,
                               d->mirror_boundary, d->prop_map);
            validator.validate();
        }

        d->mirror_boundary = vsapi->propGetInt(in, "boundary", 0, &err) != 0;

        const char* dump_path = vsapi->propGetData(in, "dump_ir", 0, &err);
        if (!err && dump_path) {
            d->dump_ir_path = dump_path;
        }

        d->opt_level = vsapi->propGetInt(in, "opt_level", 0, &err);
        if (err) {
            d->opt_level = 5;
        }
        if (d->opt_level <= 0) {
            throw std::runtime_error("opt_level must be greater than 0.");
        }

        d->approx_math = vsapi->propGetInt(in, "approx_math", 0, &err);
        if (err) {
            d->approx_math = 2; // Default to auto mode
        }
        if (d->approx_math < 0 || d->approx_math > 2) {
            throw std::runtime_error(
                "approx_math must be 0 (disabled), 1 (enabled), or 2 (auto).");
        }
        // TODO: should we enable approx math only for NEON and x86_64?

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
                 "dump_ir:data:opt;opt_level:int:opt;approx_math:int:opt;",
                 exprCreate, nullptr, plugin);
}
