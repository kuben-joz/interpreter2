#include "cfg.h"
#include <algorithm>
#include <llvm-14/llvm/IR/BasicBlock.h>
#include <llvm-14/llvm/IR/Function.h>
#include <llvm-14/llvm/IR/Instruction.h>
#include <llvm-14/llvm/IR/Instructions.h>
#include <llvm-14/llvm/Support/Casting.h>
#include <utility>
#include <vector>

CFG::CFG(llvm::Function *fn) : func(fn) { update(); }

void CFG::update() {
  succ.clear();
  pred.clear();
  start_blk = &(func->getEntryBlock());
  for (auto &blk_ref : func->getBasicBlockList()) {
    llvm::BasicBlock *blk = &blk_ref;
    std::pair<llvm::BasicBlock *, llvm::BasicBlock *> cur_succ =
        std::make_pair(nullptr, nullptr);
    if (!blk->empty()) {
      llvm::Instruction *instr = &(blk->back());
      if (auto *br = llvm::dyn_cast<llvm::BranchInst>(instr)) {
        cur_succ.first = br->getSuccessor(0);
        if (br->isConditional()) {
          cur_succ.second = br->getSuccessor(1);
        }
        assert(cur_succ.first != cur_succ.second);
      }
    }
    if (cur_succ.first) {
      pred[cur_succ.first].emplace_back(blk);
    }
    if (cur_succ.second) {
      pred[cur_succ.second].emplace_back(blk);
    }
    succ[blk] = std::move(cur_succ);
  }
}

void CFG::postorder_rec(llvm::BasicBlock *blk_in,
                        std::set<llvm::BasicBlock *> &visited,
                        std::vector<llvm::BasicBlock *> &res) {
  visited.insert(blk_in);
  std::pair<llvm::BasicBlock *, llvm::BasicBlock *> cur_succ = succ[blk_in];
  if (cur_succ.first && !visited.count(cur_succ.first)) {
    postorder_rec(cur_succ.first, visited, res);
  }
  if (cur_succ.second && !visited.count(cur_succ.second)) {
    postorder_rec(cur_succ.second, visited, res);
  }
  res.emplace_back(blk_in);
}

std::vector<llvm::BasicBlock *> CFG::get_rev_postorder() {
  std::vector<llvm::BasicBlock *> res;
  std::set<llvm::BasicBlock *> visited;
  postorder_rec(start_blk, visited, res);
  std::reverse(res.begin(), res.end());
  return res;
}

