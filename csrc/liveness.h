#pragma once

#include "cfg.h"
#include <llvm-14/llvm/IR/BasicBlock.h>
#include <llvm-14/llvm/IR/Instruction.h>
#include <llvm-14/llvm/IR/Instructions.h>
#include <llvm-14/llvm/IR/Value.h>
#include <map>
#include <vector>

namespace liveness {

class LiveVals {

public:
  std::map<llvm::BasicBlock *, int> blk_to_idx;
  std::vector<llvm::BasicBlock *> idx_to_blk;
  std::vector<std::set<llvm::Value*>> live_in;
  std::vector<std::set<llvm::Value*>> live_out;


  LiveVals(CFG &cfg, std::vector<llvm::Value *> &vars) : idx_to_blk(cfg.get_postorder()), live_in(cfg.succ.size()), live_out(cfg.succ.size()) {
    // todo merge these potentially with code in domtree to not calc twice
    std::map<llvm::BasicBlock *, int> blk_to_idx;
    std::vector<std::vector<int>> preds(idx_to_blk.size());
    std::vector<std::vector<int>> succs(idx_to_blk.size());
    // todo maybe extract out the whole structure so we don't have to
    // recalculate with liveness
    for (int i = 0; i < idx_to_blk.size(); i++) {
      blk_to_idx[idx_to_blk[i]] = i;
    }
    for (int i = 0; i < idx_to_blk.size(); i++) {
      auto &cur_preds = preds[i];
      for (auto cur_pred : cfg.pred[idx_to_blk[i]]) {
        cur_preds.emplace_back(blk_to_idx[cur_pred]);
      }
      auto &cur_succs = succs[i];
      for (auto cur_succ : cfg.succ[idx_to_blk[i]]) {
        cur_succs.emplace_back(blk_to_idx[cur_succ]);
      }
    }

    // blk to list of uses from back to front




    bool changed = true;
    while(changed) {
      for(int cur_blk_idx = 0; cur_blk_idx++ < idx_to_blk.size(); cur_blk_idx++) {

      }
    }
  };

  // actual algorithm
};
} // namespace liveness