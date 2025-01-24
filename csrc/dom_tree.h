#pragma once

#include <map>
#include <set>
#include <vector>

#include <llvm-14/llvm/IR/BasicBlock.h>

#include "cfg.h"
#include "util.h"

struct DomTree {
public:
  // todo change this to initilizer
  std::map<llvm::BasicBlock *, int> blk_to_idx;
  std::vector<llvm::BasicBlock *> idx_to_blk;
  std::vector<int> dom_pred;
  std::vector<std::vector<int>> dom_succ;
  std::vector<DynamicBitset> dom_front;

  DomTree(CFG &cfg);

  std::vector<llvm::BasicBlock *> it_dom_front(std::set<llvm::BasicBlock *> &in_bbs);

private:
  void dom_frontier(std::vector<std::vector<int>> &preds);
  inline int intersect(int i, int j);
};