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
  start_blks.emplace_back(&(func->getEntryBlock()));
  for (auto &blk_ref : func->getBasicBlockList()) {
    llvm::BasicBlock *blk = &blk_ref;
    if (!blk->empty()) {
      llvm::Instruction *instr = &(blk->back());
      if (auto *br = llvm::dyn_cast<llvm::BranchInst>(instr)) {
        for(auto *s : br->successors()) {
          assert(succ[blk].empty() || succ[blk].back() != s);
          succ[blk].emplace_back(s);
          assert(pred[s].empty() || pred[s].back() != blk);
          pred[s].emplace_back(blk);
        }
      }
      else { //todo maybe add a case for switch blocks
        end_blks.emplace_back(blk);
      }
    }
    else {
      end_blks.emplace_back(blk);
    }
  }
}

void CFG::postorder_rec(llvm::BasicBlock *blk_in,
                        std::set<llvm::BasicBlock *> &visited,
                        std::vector<llvm::BasicBlock *> &res) {
  visited.insert(blk_in); // this has to be at beginnign to handle self-loops
  for(auto *succ_blk : succ[blk_in]) {
    if(!visited.count(succ_blk)) {
      postorder_rec(succ_blk, visited, res);
    }
  }
  res.emplace_back(blk_in);
}

std::vector<llvm::BasicBlock *> CFG::get_rev_postorder() {
  assert(start_blks.size() == 1 && "We need exactly 1 start blk, this is probably a reverse cfg");
  std::vector<llvm::BasicBlock *> res;
  std::set<llvm::BasicBlock *> visited;
  postorder_rec(start_blks.front(), visited, res);
  std::reverse(res.begin(), res.end());
  return res;
}

