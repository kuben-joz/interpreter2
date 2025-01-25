#pragma once
#include <llvm-14/llvm/IR/BasicBlock.h>
#include <llvm-14/llvm/IR/Function.h>
#include <map>
#include <vector>
struct CFG {
  // todo add vector of start blk and end blk for reverse cfg?
  llvm::Function *func;
  std::vector<llvm::BasicBlock *> start_blks;
  std::vector<llvm::BasicBlock *> end_blks;
  std::map<llvm::BasicBlock *, std::vector<llvm::BasicBlock *>> succ;
  std::map<llvm::BasicBlock *, std::vector<llvm::BasicBlock *>> pred;

  CFG(llvm::Function *func);

  void update();

  std::vector<llvm::BasicBlock *> get_rev_postorder();

private:
  void postorder_rec(llvm::BasicBlock *blk_in,
                     std::set<llvm::BasicBlock *> &visited,
                     std::vector<llvm::BasicBlock *> &res);
};
