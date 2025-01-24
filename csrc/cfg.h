#pragma once
#include <llvm-14/llvm/IR/BasicBlock.h>
#include <llvm-14/llvm/IR/Function.h>
#include <map>
#include <vector>
struct CFG {
  llvm::Function *func;
  llvm::BasicBlock *start_blk;
  std::map<llvm::BasicBlock *,
           std::pair<llvm::BasicBlock *, llvm::BasicBlock *>>
      succ;
  std::map<llvm::BasicBlock *, std::vector<llvm::BasicBlock *>> pred;

  CFG(llvm::Function *func);

  void update();

  std::vector<llvm::BasicBlock *> get_rev_postorder();

private:
  void postorder_rec(llvm::BasicBlock *blk_in,
                     std::set<llvm::BasicBlock *> &visited,
                     std::vector<llvm::BasicBlock *> &res);
};
