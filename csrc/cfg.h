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

struct DomTree {
public:
  // todo change this to initilizer
  std::map<llvm::BasicBlock *, int> blk_to_idx;
  std::vector<llvm::BasicBlock *> idx_to_blk;
  std::vector<int> dom_pred;
  std::vector<std::vector<int>> dom_succ;
  std::vector<std::vector<int>> preds;
  std::vector<std::set<int>> dom_front;

  void dom_frontier() {
    dom_front =
        std::vector<std::set<int>>(dom_pred.size()); // todo add to intilizer
    for (int cur_node = 0; cur_node < dom_pred.size(); cur_node++) {
      if (preds[cur_node].size() >= 2) {
        for (int pred : preds[cur_node]) {
          while (pred != dom_pred[cur_node]) {
            dom_front[pred].insert(cur_node);
            pred = dom_pred[pred];
          }
        }
      }
    }
  }

  int intersect(int i, int j) {
    while (i != j) {
      while (i < j) {
        i = dom_pred[i];
      }
      while (j < i) {
        j = dom_pred[j];
      }
    }
    return i;
  }

  // https://www.cs.tufts.edu/~nr/cs257/archive/keith-cooper/dom14.pdf
  void construct(CFG &cfg) {
    idx_to_blk = cfg.get_rev_postorder();
    preds = std::vector<std::vector<int>>(idx_to_blk.size());
    {
      for (int i = 0; i < idx_to_blk.size(); i++) {
        blk_to_idx[idx_to_blk[i]] = i;
      }
      for (int i = 0; i < idx_to_blk.size(); i++) {
        auto &cur_preds = preds[i];
        for (auto cur_pred : cfg.pred[idx_to_blk[i]]) {
          cur_preds.emplace_back(blk_to_idx[cur_pred]);
        }
      }
    }
    assert(preds[0].size() == 0);
    // todo move to initializer
    dom_pred = std::vector<int>(idx_to_blk.size(), -1);
    dom_succ = std::vector<std::vector<int>>(idx_to_blk.size());
    dom_pred[0] = 0;
    bool changed = true;
    while (changed) {
      changed = false;
      for (int cur_idx = 1; cur_idx < idx_to_blk.size(); cur_idx++) {
        std::vector<int> &pred_idxs = preds[cur_idx];
        int new_idom_idx = -1;
        for (int cur_pred_idx : pred_idxs) {
          if (dom_pred[cur_pred_idx] >= 0) {
            new_idom_idx = cur_pred_idx;
            break;
          }
        }
        assert(new_idom_idx >= 0);
        int orig_idom_idx = new_idom_idx;
        for (int cur_pred_idx : pred_idxs) {
          if (cur_pred_idx == orig_idom_idx || dom_pred[cur_pred_idx] < 0) {
            continue;
          }
          new_idom_idx = intersect(cur_pred_idx, new_idom_idx);
        }
        if (dom_pred[cur_idx] != new_idom_idx) {
          dom_pred[cur_idx] = new_idom_idx;
          changed = true;
        }
      }
    }
    // construct children in tree
    for (int cur_idx = 1; cur_idx < idx_to_blk.size(); cur_idx++) {
      dom_succ[dom_pred[cur_idx]].emplace_back(cur_idx);
    }
  }
};