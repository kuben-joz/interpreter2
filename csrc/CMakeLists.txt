# based on https://llvm.org/docs/CMake.html#embedding-llvm-in-your-project
cmake_minimum_required(VERSION 3.20.0)

project(
  Compiler
  LANGUAGES CXX C)


find_package(LLVM 14.0.6 REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# Set your project compile flags.
# E.g. if using the C++ header files
# you will need to enable C++11 support
# for your compiler.

set (CMAKE_CXX_STANDARD 17)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(my_dirs ${PROJECT_SOURCE_DIR}/graphs ${PROJECT_SOURCE_DIR}/passes ${PROJECT_SOURCE_DIR})

include_directories(${LLVM_INCLUDE_DIRS} ${my_dirs})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

set(source_files builtin_funcs.cpp builtin_funcs.h graphs/cfg.cpp graphs/cfg.h graphs/dom_tree.cpp graphs/dom_tree.h ir_builder.h main.cpp passes/mem2reg.cpp passes/mem2reg.h passes/init_pass.cpp passes/init_pass.h passes/pass_util.cpp passes/pass_util.h printer.cpp printer.h skel.cpp skel.h syntax.h treeparse.cpp treeparse.h util.cpp util.h visitor.h)


# Now build our tools
add_executable(compiler ${source_files})
target_include_directories(compiler PRIVATE ${my_dirs})
#target_compile_options(compiler PRIVATE -Wall -Wextra -Wpedantic)

# Find the libraries that correspond to the LLVM components
# that we wish to use
llvm_map_components_to_libnames(llvm_libs support core irreader)
# Link against LLVM libraries
target_link_libraries(compiler ${llvm_libs})