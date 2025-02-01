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
        removed[skip_idx] = true;
        remove_phis(cur_idx, skip_idx);
        // swap to single br
        inst_to_del.emplace_back(&br);
        llvm::BranchInst::Create(keep, cur);
        change_glob = true;
        change_structure = true;
      }
    }
  }
};

void trim_tree_rec(int idx, TreeTrimmer &trimmer, CFG &cfg, DomTree &dom) {
  // postorder
  // cut out branches

  for (int succ : dom.dom_succs[idx]) {
    trim_tree_rec(succ, trimmer, cfg, dom);
  }
  trimmer.visit(dom.idx_to_blk[idx]);
  trimmer.clean_insts();
}

// todo strs_eq_fun ins't absorbed for constant strings
std::pair<bool, bool> trim_tree(CFG &cfg, DomTree &dom) {
  TreeTrimmer trimmer(cfg, dom);
  // preorder
  // cut out branches

  trim_tree_rec(0, trimmer, cfg, dom);

  return std::make_pair(trimmer.change_glob, trimmer.change_structure);
}
} // namespace clean