#!/bin/bash

llvm-as -o output.bc output.ll
llvm-link -o out.bc output.ll lib/runtime.ll