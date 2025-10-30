project('ctre', 'cpp',
    version: '3.10.0',
    license: 'Apache-2.0',
    default_options: ['cpp_std=c++20']
)

ctre_inc = include_directories('include')

ctre_dep = declare_dependency(
    include_directories: ctre_inc
)

meson.override_dependency('ctre', ctre_dep)

