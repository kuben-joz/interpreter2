#include "dom_tree.h"
#include "util.h"
#include <algorithm>
#include <llvm-14/llvm/IR/BasicBlock.h>

// {REF} bit twiddling adapted from
// https://lemire.me/blog/2018/02/21/iterating-over-set-bits-quickly/
std::vector<llvm::BasicBlock *>
DomTree::it_dom_front(std::set<llvm::BasicBlock *> &in_bbs) {
  // initial dom frontier
  assert(dom_front_calculated);
  //if(!dom_front_calculated) {
  //  dom_frontier();
  //}
  DynamicBitset res_bs(blk_to_idx.size());
  for (llvm::BasicBlock *bb : in_bbs) {
    assert(blk_to_idx.count(bb) && "Block doesn't exist in CFG");
    res_bs.do_or(dom_front[blk_to_idx[bb]]);
  }

  // iteration until fixed-point
  DynamicBitset temp_bs(res_bs.sz);
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
    : dom_preds(cfg.succ.size(), -1), dom_succs(cfg.succ.size()),
      dom_front(cfg.succ.size(), DynamicBitset(cfg.succ.size())),
      idx_to_blk(cfg.get_postorder()), cfg_preds(cfg.succ.size()),
      cfg_succs(cfg.succ.size()), dom_front_calculated(false) {
  // todo maybe extract out the whole structure so we don't hav eto recalculate
  // with liveness
  std::reverse(idx_to_blk.begin(), idx_to_blk.end());
  for (int i = 0; i < idx_to_blk.size(); i++) {
    blk_to_idx[idx_to_blk[i]] = i;
  }
  for (int i = 0; i < idx_to_blk.size(); i++) {
    std::vector<int> &cur_preds = cfg_preds[i];
    for (auto cur_pred : cfg.pred[idx_to_blk[i]]) {
      cur_preds.emplace_back(blk_to_idx[cur_pred]);
    }
    std::vector<int> &cur_succs = cfg_succs[i];
    for (auto cur_succ : cfg.succ[idx_to_blk[i]]) {
      cur_succs.emplace_back(blk_to_idx[cur_succ]);
    }
  }

  assert(cfg_preds[0].size() == 0);
  dom_preds[0] = 0;
  bool changed = true;
  while (changed) {
    changed = false;
    for (int cur_idx = 1; cur_idx < idx_to_blk.size(); cur_idx++) {
      std::vector<int> &pred_idxs = cfg_preds[cur_idx];
      int new_idom_idx = -1;
      for (int cur_pred_idx : pred_idxs) {
        if (dom_preds[cur_pred_idx] >= 0) {
          new_idom_idx = cur_pred_idx;
          break;
        }
      }
      assert(new_idom_idx >= 0);
      int orig_idom_idx = new_idom_idx;
      for (int cur_pred_idx : pred_idxs) {
        if (cur_pred_idx == orig_idom_idx || dom_preds[cur_pred_idx] < 0) {
          continue;
        }
        new_idom_idx = intersect(cur_pred_idx, new_idom_idx);
      }
      if (dom_preds[cur_idx] != new_idom_idx) {
        dom_preds[cur_idx] = new_idom_idx;
        changed = true;
      }
    }
  }
  // construct children in tree
  for (int cur_idx = 1; cur_idx < idx_to_blk.size(); cur_idx++) {
    dom_succs[dom_preds[cur_idx]].emplace_back(cur_idx);
  }
}

// in original algorithm it's i < j and j < i because they
// number nodes by postorder, but traverse them in reverse post order
inline int DomTree::intersect(int i, int j) {
  while (i != j) {
    while (i > j) {
      i = dom_preds[i];
    }
    while (j > i) {
      j = dom_preds[j];
    }
  }
  return i;
}

void DomTree::dom_frontier() {
  dom_front_calculated = true;
  for (int cur_node = 0; cur_node < dom_preds.size(); cur_node++) {
    if (cfg_preds[cur_node].size() >= 2) {
      for (int pred : cfg_preds[cur_node]) {
        while (pred != dom_preds[cur_node]) {
          dom_front[pred].set(cur_node);
          pred = dom_preds[pred];
        }
      }
    }
  }
}