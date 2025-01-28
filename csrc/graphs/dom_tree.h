#pragma once

#include <map>
#include <set>
#include <vector>

#include <llvm-14/llvm/IR/BasicBlock.h>

#include "cfg.h"
#include "util.h"

// https://repository.rice.edu/items/99a574c3-90fe-4a00-adf9-ce73a21df2ed
struct DomTree {
public:
  std::map<llvm::BasicBlock *, int> blk_to_idx;
  std::vector<llvm::BasicBlock *> idx_to_blk;
  std::vector<int> dom_preds;
  std::vector<std::vector<int>> dom_succs;
  std::vector<DynamicBitset> dom_front;
  std::vector<std::vector<int>> cfg_preds;
  std::vector<std::vector<int>> cfg_succs;

  DomTree(CFG &cfg);

  std::vector<llvm::BasicBlock *>
  it_dom_front(std::set<llvm::BasicBlock *> &in_bbs);
  void dom_frontier();

private:
  bool dom_front_calculated;
  inline int intersect(int i, int j);
};