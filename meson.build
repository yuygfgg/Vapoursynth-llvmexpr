# Copyright (C) 2025 yuygfgg

# This file is part of Vapoursynth-llvmexpr.

# Vapoursynth-llvmexpr is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# Vapoursynth-llvmexpr is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with Vapoursynth-llvmexpr.  If not, see <https://www.gnu.org/licenses/>.

project('llvmexpr', 'cpp', 'c',
  default_options: ['buildtype=release', 'warning_level=2', 'b_lto=true', 'b_ndebug=if-release', 'cpp_std=c++23', 'c_gnu_symbol_visibility=hidden', 'cpp_gnu_symbol_visibility=hidden'],
  license: 'GPL-3.0-or-later',
  meson_version: '>=1.4.0',
  version: '3.0'
)

cxx = meson.get_compiler('cpp')
cc = meson.get_compiler('c')

gcc_syntax = cxx.get_argument_syntax() == 'gcc'

enable_sanitizers = get_option('enable-sanitizers')
if enable_sanitizers
  if gcc_syntax
    sanitizer_args = [
      '-fno-omit-frame-pointer',
      '-fno-optimize-sibling-calls',
      '-O1'
    ]
    add_project_arguments(sanitizer_args, language: ['cpp', 'c'])
  else
    sanitizer_args = [
      '/fsanitize=address',
      '/Zi',
      '/Od'
    ]
    add_project_arguments(sanitizer_args, language: ['cpp', 'c'])
  endif
endif

if gcc_syntax
  vapoursynth_dep = dependency('vapoursynth', version: '>=55').partial_dependency(compile_args: true, includes: true)
  install_dir = vapoursynth_dep.get_variable(pkgconfig: 'libdir') / 'vapoursynth'
else
  vapoursynth_dep = dependency('', required: false)
  install_dir = get_option('libdir') / 'vapoursynth'
endif

static_llvm = get_option('static-llvm')
llvm_dep = dependency('llvm', version: '>=20.0.0', method: 'config-tool', modules: ['core', 'orcjit', 'native', 'all-targets'], static: static_llvm)

if gcc_syntax
  llvm_inc_dir = include_directories(llvm_dep.get_variable('includedir'), is_system: true)
  llvm_link_dep = llvm_dep.partial_dependency(link_args: true)
  llvm_dep = declare_dependency(include_directories: llvm_inc_dir, dependencies: llvm_link_dep)
endif

dependencies = [vapoursynth_dep, llvm_dep]
link_args = []

if static_llvm
  zlib_dep = dependency('zlib', static: true, required: false)
  if zlib_dep.found()
    dependencies += zlib_dep
  endif

  zstd_dep_check = dependency('libzstd', static: true, required: false)
  if zstd_dep_check.found() and host_machine.system() == 'linux'
    link_args += ['-Wl,-Bstatic', '-lzstd', '-Wl,-Bdynamic']
  elif zstd_dep_check.found() and host_machine.system() == 'windows'
    # On Windows, we need to link zstd statically without the .dll suffix
    link_args += ['-lzstd']
    dependencies += zstd_dep_check
  elif zstd_dep_check.found()
    dependencies += zstd_dep_check
  endif

  xml2_dep = dependency('libxml-2.0', static: true, required: false)
  if xml2_dep.found()
    dependencies += xml2_dep
  endif
endif


if static_llvm and host_machine.system() == 'windows'
  link_args += '-static'
endif

if static_llvm
  if host_machine.system() == 'darwin'
    exported_symbols = configure_file(
      input: 'llvmexpr.exp.in',
      output: 'llvmexpr.exp',
      configuration: {
        'symbol': '_VapourSynthPluginInit2'
      }
    )
    link_args += ['-Wl,-exported_symbols_list,' + exported_symbols.full_path()]
  elif host_machine.system() == 'windows' and not gcc_syntax
    def_file = configure_file(
      input: 'llvmexpr.def.in',
      output: 'llvmexpr.def',
      configuration: {
        'exports': 'EXPORTS\nVapourSynthPluginInit2'
      }
    )
    link_args += ['/DEF:' + def_file.full_path(), '/FORCE:MULTIPLE']
  else
    version_script = configure_file(
      input: 'llvmexpr.ver.in',
      output: 'llvmexpr.ver',
      configuration: {
        'symbol': 'VapourSynthPluginInit2'
      }
    )
    link_args += ['-Wl,--version-script,' + version_script.full_path(), '-Wl,--exclude-libs,ALL']
  endif
endif

sources = [
  'llvmexpr/llvmexpr.cpp',
  'llvmexpr/frontend/Tokenizer.cpp',
  'llvmexpr/frontend/Analysis.cpp',
  'llvmexpr/frontend/InfixConverter.cpp',
  'llvmexpr/frontend/infix2postfix/Builtins.cpp',
  'llvmexpr/frontend/infix2postfix/Tokenizer.cpp',
  'llvmexpr/frontend/infix2postfix/Parser.cpp',
  'llvmexpr/frontend/infix2postfix/CodeGenerator.cpp',
  'llvmexpr/frontend/infix2postfix/PostfixBuilder.cpp',
  'llvmexpr/frontend/infix2postfix/PostfixHelper.cpp',
  'llvmexpr/ir/ExprIRGenerator.cpp',
  'llvmexpr/ir/SingleExprIRGenerator.cpp',
  'llvmexpr/ir/IRGeneratorBase.cpp',
  'llvmexpr/jit/Compiler.cpp',
  'llvmexpr/jit/Jit.cpp',
  'llvmexpr/utils/Diagnostics.cpp',
]

shared_module('llvmexpr', sources,
  dependencies: dependencies,
  link_args: link_args,
  install: true,
  install_dir: install_dir
)

# Build infix2postfix tool
infix2postfix_sources = [
  'llvmexpr/frontend/infix2postfix/main.cpp',
  'llvmexpr/frontend/infix2postfix/Builtins.cpp',
  'llvmexpr/frontend/infix2postfix/Tokenizer.cpp',
  'llvmexpr/frontend/infix2postfix/Parser.cpp',
  'llvmexpr/frontend/infix2postfix/CodeGenerator.cpp',
  'llvmexpr/frontend/infix2postfix/PostfixBuilder.cpp',
  'llvmexpr/frontend/infix2postfix/PostfixHelper.cpp',
  'llvmexpr/frontend/Tokenizer.cpp',  # Need main project's tokenizer for PostfixHelper
]

executable('infix2postfix', infix2postfix_sources,
  dependencies: [llvm_dep],
  install: false
)
