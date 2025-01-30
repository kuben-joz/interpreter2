#include "cfg.h"
#include "dom_tree.h"
#include <cassert>
#include <llvm-14/llvm/IR/BasicBlock.h>
#include <llvm-14/llvm/IR/Constant.h>
#include <llvm-14/llvm/IR/Constants.h>
#include <llvm-14/llvm/IR/Function.h>
#include <llvm-14/llvm/IR/InstVisitor.h>
#include <llvm-14/llvm/IR/Instruction.h>
#include <llvm-14/llvm/IR/Instructions.h>
#include <llvm-14/llvm/IR/Type.h>
#include <llvm-14/llvm/IR/Value.h>
#include <llvm-14/llvm/Support/Casting.h>
#include <utility>

namespace clean {
class TreeTrimmer : public llvm::InstVisitor<TreeTrimmer> {
public:
  bool change_glob = false;
  bool change_structure = false;
  CFG &cfg;
  DomTree &dom;
  std::vector<bool> removed;
  std::vector<llvm::Instruction *> inst_to_del;

  TreeTrimmer(CFG &cfg, DomTree &dom)
      : cfg(cfg), dom(dom), removed(dom.blk_to_idx.size(), false) {}

  void clean_insts() {
    if (inst_to_del.empty()) {
      return;
    }
    change_glob = true;
    for (auto *inst : inst_to_del) {
      inst->eraseFromParent();
    }
    inst_to_del.clear();
  }

  void remove_phis(int idx_orig, int idx_dest) {
    llvm::BasicBlock *orig_blk = dom.idx_to_blk[idx_orig];
    llvm::BasicBlock *dest_blk = dom.idx_to_blk[idx_dest];
    std::vector<std::pair<llvm::PHINode *, int>> phis;
    for (auto &inst_ref : dest_blk->getInstList()) {
      if (auto phi = llvm::dyn_cast<llvm::PHINode>(&inst_ref)) {
        int phi_blk_idx = phi->getBasicBlockIndex(orig_blk);
        int num_vals = phi->getNumIncomingValues();
        assert(num_vals > 1 && phi_blk_idx >= 0);
        if (phi_blk_idx < 0 || num_vals <= 1) {
          break;
        }
        phis.emplace_back(phi, phi_blk_idx);
      } else {
        break;
      }
    }
    for (auto &p_i : phis) {
      change_structure = true;
      change_glob = true;
      p_i.first->removeIncomingValue(p_i.second);
    }
  }

  void clean_hanging_dom_rec(int idx) {
    // preorder
    for (int succ : dom.dom_succs[idx]) {
      removed[succ] = true;
    }
    for (int succ : dom.cfg_succs[idx]) {
      if (!removed[succ]) {
        remove_phis(idx, succ);
      }
    }
    // go down
    for (int succ : dom.dom_succs[idx]) {
      clean_hanging_dom_rec(succ);
    }
    // postorder
    for (int succ : dom.dom_succs[idx]) {
      if (dom.idx_to_blk[succ]) {
        dom.blk_to_idx.erase(dom.idx_to_blk[succ]);
        dom.idx_to_blk[succ]->eraseFromParent();
        dom.idx_to_blk[succ] = nullptr;
      }
    }
  }

  void visitBranchInst(llvm::BranchInst &br) {
    if (br.isConditional()) {
      llvm::Value *cond = br.getCondition();
      if (auto const_i = llvm::dyn_cast<llvm::ConstantInt>(cond)) {
        assert(const_i->getBitWidth() == 1);
        int64_t cond_v = const_i->getZExtValue();
        llvm::BasicBlock *keep = br.getSuccessor(0);
        llvm::BasicBlock *skip = br.getSuccessor(1);
        llvm::BasicBlock *cur = br.getParent();
        if (!cond_v) {
          std::swap(keep, skip);
        }
        assert(dom.blk_to_idx.count(cur));
        int cur_idx = dom.blk_to_idx[cur];
        assert(dom.blk_to_idx.count(skip));
        int skip_idx = dom.blk_to_idx[skip];
        assert(dom.blk_to_idx.count(keep));
        int keep_idx = dom.blk_to_idx[keep];
        for (int succ : dom.dom_succs[cur_idx]) {
          if (succ == skip_idx) {
            assert(!removed[skip_idx]);
            int num_preds = 0;
            for (int pred : dom.cfg_preds[skip_idx]) {
              if (!removed[pred]) {
                num_preds++;
              }
            };
            assert(num_preds > 0);
            // orig dominates skip so in theory this means that skip_idx will
            // have an empty dom tree if it has more than one predecessor so
            // cleaning the hanging dom rec won't do anything
            if (num_preds == 1) {
              removed[skip_idx] = true;
            }
            clean_hanging_dom_rec(skip_idx);
            if (removed[skip_idx]) {
              change_glob = true;
              change_structure = true;
              dom.blk_to_idx.erase(dom.idx_to_blk[skip_idx]);
              skip->removeFromParent();
              dom.idx_to_blk[skip_idx] = nullptr;
            }
            break;
          }
        }
        if (!removed[skip_idx]) {
          remove_phis(cur_idx, skip_idx);
        }
        // swap to single br
        inst_to_del.emplace_back(&br);
        llvm::BranchInst::Create(keep, cur);
        change_glob = true;
        change_structure = true;
      }
    }
  }
};

// todo strs_eq_fun ins't absorbed for constant strings
std::pair<bool, bool> trim_tree(CFG &cfg, DomTree &dom) {
  TreeTrimmer trimmer(cfg, dom);
  // preorder
  // cut out branches
  for (int i = 0; i < dom.idx_to_blk.size(); i++) {
    if (!trimmer.removed[i]) {
      llvm::BasicBlock *blk = dom.idx_to_blk[i];
      // assert(!blk->empty());
      if (!blk->empty()) {
        trimmer.visit(blk->back());
        trimmer.clean_insts();
      }
    }
  }
  // trim out blocks with no preds that are not the entry
  for (int i = 0; i < dom.idx_to_blk.size(); i++) {
    if (!trimmer.removed[i]) {
      if (dom.cfg_preds[i].size() == 0) {
        assert(i == 0);
        continue; // root node
      }
      bool has_pred = false;
      for (int pred : dom.cfg_preds[i]) {
        has_pred = has_pred || !trimmer.removed[pred];
      }
      if (!has_pred) {
        trimmer.clean_hanging_dom_rec(i);
        assert(dom.idx_to_blk[i]);
        if (dom.idx_to_blk[i]) {
          trimmer.change_glob = true;
          trimmer.change_structure = true;
          dom.blk_to_idx.erase(dom.idx_to_blk[i]);
          dom.idx_to_blk[i]->eraseFromParent();
          dom.idx_to_blk[i] = nullptr;
        }
      }
    }
  }
  for (auto *blk : cfg.end_blks) {
    if (!dom.blk_to_idx.count(blk)) {
      trimmer.change_glob = true;
      trimmer.change_structure = true;
      blk->eraseFromParent();
    }
  }

  return std::make_pair(trimmer.change_glob, trimmer.change_structure);
}
} // namespace clean