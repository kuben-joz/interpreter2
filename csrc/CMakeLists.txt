cmake_minimum_required(VERSION 3.15)

project(
  Compiler
  LANGUAGES CXX)

  set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

add_executable(compiler reader.cpp skel.cpp skel.h util.cpp util.h)
