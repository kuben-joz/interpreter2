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
class TreeFolder : public llvm::InstVisitor<TreeFolder> {
public:
  bool change_glob = false;
  bool change_structure = false;
  CFG &cfg;
  DomTree &dom;
  std::vector<bool> removed;
  std::vector<llvm::Instruction *> inst_to_del;

  TreeFolder(CFG &cfg, DomTree &dom)
      : cfg(cfg), dom(dom), removed(dom.blk_to_idx.size(), false) {}

  void join_succ_single(int orig_idx, int succ_idx) {
    int num_preds = 0;
    for (int pred : dom.cfg_preds[succ_idx]) {
      if (!removed[pred]) {
        num_preds++;
      }
    }
    assert(num_preds > 0);
    if (num_preds != 1) {
      return;
    }
    change_glob = true;
    change_structure = true;
    llvm::BasicBlock *orig = dom.idx_to_blk[orig_idx];
    assert(orig);
    llvm::BasicBlock *join = dom.idx_to_blk[succ_idx];
    assert(join);
    assert(!orig->empty());
    assert(!join->empty());
    assert(llvm::isa<llvm::BranchInst>(orig->back()));
    inst_to_del.emplace_back(&orig->back());
    auto it = join->begin();
    auto end = join->end();
    while (it != end) {
      if (auto *phi = llvm::dyn_cast<llvm::PHINode>(it)) {
        assert(phi->getNumIncomingValues() == 1);
        if(phi->getNumIncomingValues() == 0) {
          continue;
        }
        llvm::Value *v = phi->getIncomingValue(0);
        phi->replaceAllUsesWith(v);
        inst_to_del.emplace_back(phi);
      } else {
        break;
      }
      it++;
    }
    std::vector<llvm::Instruction *> to_move;
    while (it != end) {
      to_move.emplace_back(&it);
      it++;
    }
    for (auto *instr : to_move) {
      instr->removeFromParent();
      instr->insertAfter(&orig->back());
    }
    for (int succ_succ : dom.cfg_succs[succ_idx]) {
      if (!removed[succ_succ]) {
        llvm::BasicBlock *cur_succ = dom.idx_to_blk[succ_succ];
        for (auto &inst : cur_succ->getInstList()) {
          if (auto *phi = llvm::dyn_cast<llvm::PHINode>(&inst)) {
            int blk_idx = phi->getBasicBlockIndex(join);
            assert(blk_idx >= 0);
            phi->setIncomingBlock(blk_idx, orig);
          } else {
            break;
          }
        }
      }
    }
    removed[succ_idx] = true;
    dom.blk_to_idx.erase(join);
    join->eraseFromParent();
    dom.idx_to_blk[succ_idx] = nullptr;
  }

  void join_succ_double(int orig_idx) {
    assert(!removed[orig_idx]);
    llvm::BasicBlock *orig = dom.idx_to_blk[orig_idx];
    assert(orig);

    std::vector<int> succlst;
    for (int succ : dom.cfg_succs[orig_idx]) {
      if (!removed[succ]) {
        succlst.emplace_back(succ);
      }
    }
    assert(succlst.size() == 2);

    // check all have one predecessor and one successor
    for (int succ : succlst) {
      int num_preds = 0;
      for (int pred : dom.cfg_preds[succ]) {
        if (!removed[pred]) {
          num_preds++;
        }
      }
      assert(num_preds > 0);
      if (num_preds > 1) {
        return;
      }
      // todo maybe add to merge if they return the same thing or if all phi
      // nodes agree
    }

    // check both have one branch
    std::vector<llvm::BasicBlock *> succ_succs;
    for (int succ : succlst) {
      assert(dom.idx_to_blk[succ]);
      llvm::BasicBlock *succ_blk = dom.idx_to_blk[succ];
      assert(!succ_blk->empty());
      if (auto *br = llvm::dyn_cast<llvm::BranchInst>(&succ_blk->front())) {
        if (br->isConditional()) {
          return;
        } else {
          llvm::BasicBlock *succ_succ = br->getSuccessor(0);
          assert(dom.blk_to_idx.count(succ_succ));
          assert(!succ_succ->empty());
          succ_succs.emplace_back(succ_succ);
        }
      } else {
        return;
      }
    }
    assert(succ_succs.size() == 2);
    // don't get rid of phi node info
    llvm::BranchInst *orig_br = llvm::dyn_cast<llvm::BranchInst>(&orig->back());
    assert(orig_br);
    assert(orig_br->isConditional());
    if (succ_succs[0] == succ_succs[1]) {
      if (llvm::isa<llvm::PHINode>(succ_succs[0]->front())) {
        return;
      } else {
        inst_to_del.emplace_back(orig_br);
        llvm::BranchInst::Create(succ_succs[0], orig);
      }
    } else { // jump to different places
      for (int i = 0; i < 2; i++) {
        llvm::BasicBlock *succ = dom.idx_to_blk[succlst[i]];
        llvm::BasicBlock *succ_succ = succ_succs[i];
        for (auto &inst : succ_succ->getInstList()) {
          if (auto *phi = llvm::dyn_cast<llvm::PHINode>(&inst)) {
          }
        }
      }
    }
  }

  // if we are the only successor and we don't have any other successors we join
  // his isntructions into ours
  void join_succ(int orig_idx) {
    int num_succ = 0;
    int succ_idx = -1;
    for (int succ : dom.cfg_succs[orig_idx]) {
      if (!removed[succ]) {
        num_succ++;
        succ_idx = succ;
      }
    }
    assert(succ_idx >= 0 || num_succ == 0);
    if (num_succ == 1) {
      join_succ_single(orig_idx, succ_idx);
    } else {
      join_succ_double(orig_idx);
    }
  }

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
            removed[skip_idx] = true;
            clean_hanging_dom_rec(skip_idx);
            dom.blk_to_idx.erase(dom.idx_to_blk[skip_idx]);
            skip->removeFromParent();
            dom.idx_to_blk[skip_idx] = nullptr;
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
std::pair<bool, bool> fold_tree(CFG &cfg, DomTree &dom) {
  TreeFolder folder(cfg, dom);
  // preorder
  // cut out branches
  for (int i = 0; i < dom.idx_to_blk.size(); i++) {
    if (!folder.removed[i]) {
      llvm::BasicBlock *blk = dom.idx_to_blk[i];
      assert(!blk->empty());
      if (!blk->empty()) {
        folder.visit(blk->back());
        folder.clean_insts();
      }
    }
  }
  // trim out blocks with no preds that are not the entry
  for (int i = 0; i < dom.idx_to_blk.size(); i++) {
    if (!folder.removed[i]) {
      if (dom.cfg_preds[i].size() == 0) {
        assert(i == 0);
        continue; // root node
      }
      bool has_pred = false;
      for (int pred : dom.cfg_preds[i]) {
        has_pred = has_pred || !folder.removed[pred];
      }
      if (!has_pred) {
        folder.clean_hanging_dom_rec(i);
        assert(dom.idx_to_blk[i]);
        if(dom.idx_to_blk[i]) {
          folder.change_glob = true;
          folder.change_structure = true;
          dom.blk_to_idx.erase(dom.idx_to_blk[i]);
          dom.idx_to_blk[i]->eraseFromParent();
          dom.idx_to_blk[i] = nullptr;
        }
      }
    }
  }

  // postorder
  // merge blocks on dominated paths
  for (int i = dom.idx_to_blk.size(); i >= 0; i--) {
    if (!folder.removed[i]) {
      llvm::BasicBlock *blk = dom.idx_to_blk[i];
      assert(!blk->empty());
      if (!blk->empty()) {
        folder.join_succ(i);
      }
    }
  }

  return std::make_pair(folder.change_glob, folder.change_structure);
}
} // namespace clean