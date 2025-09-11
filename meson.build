project('llvmexpr', 'cpp', 'c',
  default_options: ['buildtype=release', 'warning_level=2', 'b_lto=true', 'b_ndebug=if-release', 'cpp_std=c++23'],
  license: 'GPL-3.0-or-later',
  meson_version: '>=0.51.0',
  version: '1.0'
)

cxx = meson.get_compiler('cpp')
cc = meson.get_compiler('c')

gcc_syntax = cxx.get_argument_syntax() == 'gcc'

if gcc_syntax
  vapoursynth_dep = dependency('vapoursynth', version: '>=55').partial_dependency(compile_args: true, includes: true)
  install_dir = vapoursynth_dep.get_variable(pkgconfig: 'libdir') / 'vapoursynth'
else
  vapoursynth_dep = dependency('', required: false)
  install_dir = get_option('libdir') / 'vapoursynth'
endif

static_llvm = get_option('static-llvm')
llvm_dep = dependency('llvm', version: '>=20.0.0', method: 'config-tool', modules: ['core', 'orcjit', 'native', 'all-targets'], static: static_llvm)

sources = [
  'llvmexpr/llvmexpr.cpp'
]

shared_module('llvmexpr', sources,
  dependencies: [vapoursynth_dep, llvm_dep],
  install: true,
  install_dir: install_dir,
  gnu_symbol_visibility: 'hidden'
)