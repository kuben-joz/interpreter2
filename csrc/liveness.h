#pragma once

#include "cfg.h"
#include "dom_tree.h"
#include "llvm/IR/InstVisitor.h"
#include <llvm-14/llvm/IR/BasicBlock.h>
#include <llvm-14/llvm/IR/Constant.h>
#include <llvm-14/llvm/IR/InstrTypes.h>
#include <llvm-14/llvm/IR/Instruction.h>
#include <llvm-14/llvm/IR/Instructions.h>
#include <llvm-14/llvm/IR/Value.h>
#include <map>
#include <vector>

namespace liveness {

class LocalLiveVisit : llvm::InstVisitor<LocalLiveVisit> {

  // std::set<llvm::Value *> in;
  std::set<llvm::Value *> out;
  std::set<llvm::Value *> kill;
  std::set<llvm::Value *> use;

  void visitBinaryOperator(llvm::BinaryOperator &op) {
    assert(kill.empty());
    assert(use.empty());
    assert(out.empty());
    out.erase(&op);
    for (auto *subop : op.operand_values()) {
      if (!llvm::isa<llvm::Constant>(subop)) {
        out.insert(subop);
      }
    }
  }

  void visitBranchInst(llvm::BranchInst &br) {
    assert(kill.empty());
    assert(use.empty());
    assert(out.empty());
    if (br.isConditional() && !llvm::isa<llvm::Constant>(br.getCondition())) {
      out.insert(br.getCondition());
    }
  }

  // todo check this returns correctly
  void visitReturnInst(llvm::ReturnInst &ret) {
    assert(kill.empty());
    assert(use.empty());
    assert(out.empty());
    if (ret.getReturnValue() &&
        !llvm::isa<llvm::Constant>(ret.getReturnValue())) {
      out.insert(ret.getReturnValue());
    }
  }

  void visitCallInst(llvm::CallInst &call) {
    assert(kill.empty());
    assert(use.empty());
    assert(out.empty());
    out.erase(&call);
    for(auto *arg : call.operand_values()) {
      if(!llvm::isa<llvm::Constant>(arg)) {
        out.insert(arg);
      }
    }
  }

  // we don't have fcmp
  void visitICmpInst(llvm::ICmpInst &icmp) {
    assert(kill.empty());
    assert(use.empty());
    assert(out.empty());
    out.erase(&icmp);
    for(auto *v : icmp.operand_values()) {
      if(!llvm::isa<llvm::Constant>(v)) {
        out.insert(v);
      }
    }
  }

  void visitLoadInst(llvm::LoadInst &load) {
    assert(kill.empty());
    assert(use.empty());
    assert(out.empty());
    out.erase(&load);
    out.insert(load.getPointerOperand());
  }

  void visitStoreInst(llvm::StoreInst &store) {
    assert(kill.empty());
    assert(use.empty());
    assert(out.empty());
    if(!llvm::isa<llvm::Constant>(store.getValueOperand())) {
      out.insert(store.getValueOperand());
    }
    out.insert(store.getPointerOperand());
  }

  void visitAllocaInst(llvm::AllocaInst &alloc) {
    assert(kill.empty());
    assert(use.empty());
    assert(out.empty());
    out.erase(&alloc);
  }

  // this ahppens at global level
  //void visitPHINode(llvm::PHINode &phi) {
  //  assert(kill.empty());
  //  assert(use.empty());
  //  assert(out.empty());
  //  
  //}

  // todo maybe add if needed
  // switch
  // select
  // todo getelementptr or bitcast????
};



class LiveVals {

public:
  std::vector<std::set<llvm::Value *>> live_in;
  std::vector<std::set<llvm::Value *>> live_out;

  LiveVals(CFG &cfg, DomTree &dom)
      : live_in(cfg.succ.size()), live_out(cfg.succ.size()) {
    bool changed = true;
    while (changed) {
      for (int cur_blk_idx = 0; cur_blk_idx++ < dom.idx_to_blk.size();
           cur_blk_idx++) {
      }
    }
  };

  // actual algorithm
};
} // namespace liveness