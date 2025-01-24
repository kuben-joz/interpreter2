#pragma once
#include <cstdint>
#include <llvm-14/llvm/IR/BasicBlock.h>
#include <llvm-14/llvm/IR/Instruction.h>
#include <llvm-14/llvm/IR/Instructions.h>
#include <llvm-14/llvm/Support/Casting.h>
#include <set>
#include <vector>

#include "cfg.h"

class Mem2Reg {
  struct var {
    llvm::AllocaInst *alloc;
    // todo maybe chagne to ints for var to int
    std::set<llvm::BasicBlock *> store_blks;
    var(llvm::AllocaInst *alloc) : alloc(alloc) {}
  };

  void impl(CFG &cfg, DomTree &dom) {
    std::vector<var> vars;
    for (auto &inst_ref : cfg.start_blk->getInstList()) {
      llvm::Instruction *inst = &inst_ref;
      if (auto *alloc = llvm::dyn_cast<llvm::AllocaInst>(inst)) {
        vars.emplace_back(alloc);
      } else {
        // all allocs are at beginning of first block
        break;
      }
    }
    std::map<llvm::Instruction *, var *> inst_to_var;
    for (auto &v : vars) {
      for (auto *use : v.alloc->users()) {
        if (auto *load_use = llvm::dyn_cast<llvm::LoadInst>(use)) {
          inst_to_var[load_use] = &v;
        } else if (auto *store_use = llvm::dyn_cast<llvm::StoreInst>(use)) {
          if (store_use->getOperand(1) == v.alloc) { // todo do not add
                                                     // otherwise
            inst_to_var[store_use] = &v;
            v.store_blks.insert(store_use->getParent());
          }
        }
      }
    }
    int num_nodes = dom.idx_to_blk.size();
    DynamicBitset W(num_nodes);
    std::vector<int> work(num_nodes);
    std::vector<int> has_already(num_nodes);
  }
};