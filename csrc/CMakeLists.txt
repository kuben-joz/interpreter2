cmake_minimum_required(VERSION 3.31)

project(
  Compiler
  LANGUAGES CXX)

add_executable(compiler reader.cpp skel.cpp skel.h util.cpp util.h)
