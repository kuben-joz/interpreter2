#include "dom_tree.h"
#include "util.h"
#include <llvm-14/llvm/IR/BasicBlock.h>

// {REF} bit twiddling adapted from
// https://lemire.me/blog/2018/02/21/iterating-over-set-bits-quickly/
std::vector<llvm::BasicBlock *>
DomTree::it_dom_front(std::set<llvm::BasicBlock *> &in_bbs) {
  // initial dom frontier
  DynamicBitset res_bs(blk_to_idx.size());
  for (llvm::BasicBlock *bb : in_bbs) {
    assert(blk_to_idx.count(bb) && "Block doesn't exist in CFG");
    res_bs.do_or(dom_front[blk_to_idx[bb]]);
  }

  // iteration until fixed-point
  DynamicBitset temp_bs(res_bs.bits.size());
  while (true) {
    for (uint64_t i = 0; i < res_bs.bits.size(); i++) {
      bs_type bit = res_bs.bits[i];
      while (bit != 0) {
        bs_type temp = bit & -bit;
        int r = __builtin_ctzl(bit);
        temp_bs.do_or(dom_front[i * res_bs.mod + r]);
        bit ^= temp;
      }
    }
    if (res_bs.do_or_with_checks(temp_bs)) {
      temp_bs.clear();
    } else {
      break;
    }
  }

  std::vector<llvm::BasicBlock *> res;

  for (uint64_t i = 0; i < res_bs.bits.size(); i++) {
    bs_type bit = res_bs.bits[i];
    while (bit != 0) {
      bs_type temp = bit & -bit;
      int r = __builtin_ctzl(bit);
      res.emplace_back(idx_to_blk[i * res_bs.mod + r]);
      bit ^= temp;
    }
  }
  return res;
}

DomTree::DomTree(CFG &cfg)
    : dom_pred(cfg.succ.size(), -1), dom_succ(cfg.succ.size()),
      dom_front(cfg.succ.size(), DynamicBitset(cfg.succ.size())),
      idx_to_blk(cfg.get_rev_postorder()) {
  std::vector<std::vector<int>> preds(idx_to_blk.size());
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
  // construct dominance frontiers
  dom_frontier(preds);
}

// in original algorithm it's i < j and j < i because they
// number nodes by postorder, but traverse them in reverse post order
inline int DomTree::intersect(int i, int j) {
  while (i != j) {
    while (i > j) {
      i = dom_pred[i];
    }
    while (j > i) {
      j = dom_pred[j];
    }
  }
  return i;
}

void DomTree::dom_frontier(std::vector<std::vector<int>> &preds) {
  for (int cur_node = 0; cur_node < dom_pred.size(); cur_node++) {
    if (preds[cur_node].size() >= 2) {
      for (int pred : preds[cur_node]) {
        while (pred != dom_pred[cur_node]) {
          dom_front[pred].set(cur_node);
          pred = dom_pred[pred];
        }
      }
    }
  }
}