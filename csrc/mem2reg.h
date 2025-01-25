#pragma once
#include <cstdint>
#include <llvm-14/llvm/IR/BasicBlock.h>
#include <llvm-14/llvm/IR/Instruction.h>
#include <llvm-14/llvm/IR/Instructions.h>
#include <llvm-14/llvm/Support/Casting.h>
#include <memory>
#include <set>
#include <vector>

#include "cfg.h"
#include "dom_tree.h"

class Mem2Reg {
  struct var {
    llvm::AllocaInst *alloc;
    // todo maybe chagne to ints for var to int
    std::set<llvm::BasicBlock *> store_blks;
    var(llvm::AllocaInst *alloc) : alloc(alloc) {}
  };

  void add_alloca(llvm::AllocaInst *alloc,
                  std::vector<std::unique_ptr<var>> &vars,
                  std::map<llvm::Instruction *, var *> &inst_to_var) {
    std::vector<llvm::Instruction *> insts;
    std::unique_ptr<var> v = std::make_unique<var>(alloc);
    for (auto *use : alloc->users()) {
      if (auto *load_use = llvm::dyn_cast<llvm::LoadInst>(use)) {
        insts.emplace_back(load_use);
      } else if (auto *store_use = llvm::dyn_cast<llvm::StoreInst>(use)) {
        if (store_use->getOperand(1) == v->alloc) {
          insts.emplace_back(store_use);
          v->store_blks.insert(store_use->getParent());
        } else {
          assert(false && "I don't think we ever store alloca pointers");
          return;
        }
      } else {
        assert(false && "I don't think any alloca pointers have other uses");
        return;
      }
    }
    vars.emplace_back(std::move(v));
    for (auto inst : insts) {
      inst_to_var[inst] = vars.back().get();
    }
  }

  void rename_rec(); //todo

  void impl(CFG &cfg, DomTree &dom) {
    std::vector<std::unique_ptr<var>> vars;
    std::map<llvm::Instruction *, var *> inst_to_var;
    assert(cfg.start_blks.size() == 1 && "Dom tree for reverse cfg not implemented, need exactly one start block");
    for (auto &inst_ref : cfg.start_blks.front()->getInstList()) {
      llvm::Instruction *inst = &inst_ref;
      if (auto *alloc = llvm::dyn_cast<llvm::AllocaInst>(inst)) {
        add_alloca(alloc, vars, inst_to_var);
      } else {
        // all allocs are at beginning of first block
        // todo make a test for this
        break;
      }
    }
    for (auto &v : vars) {
      std::vector<llvm::BasicBlock *> it_dom_front =
          dom.it_dom_front(v->store_blks);
      for (auto *blk : it_dom_front) {
        // todo maybe assert blk not empty
        assert(!blk->empty());
        llvm::Instruction *insert_ptr = blk->empty() ? nullptr : &blk->front();
        llvm::PHINode *phi = llvm::PHINode::Create(v->alloc->getAllocatedType(),
                                                   0, "allocphi", insert_ptr);
        inst_to_var[phi] = v.get();                                          
      }
    }
  }
};