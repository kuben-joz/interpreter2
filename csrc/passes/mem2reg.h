#pragma once
#include <llvm-14/llvm/IR/BasicBlock.h>
#include <llvm-14/llvm/IR/Instruction.h>
#include <llvm-14/llvm/IR/Instructions.h>
#include <llvm-14/llvm/IR/Value.h>
#include <llvm-14/llvm/Support/Casting.h>

#include "cfg.h"
#include "dom_tree.h"

// https://doi.org/10.1145/115372.115320
namespace mem2reg {
void transform(CFG &cfg, DomTree &dom);
} // namespace mem2reg