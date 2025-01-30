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
#include <vector>

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

  bool has_one_pred(int idx) {
    return dom.cfg_preds[idx].size() == 1;
  }


  void join_succ_single(int orig_idx, int succ_idx) {
    llvm::BasicBlock *orig_blk = dom.idx_to_blk[orig_idx];
    llvm::BasicBlock *succ_blk = dom.idx_to_blk[succ_idx];
    if (!has_one_pred(succ_idx)) {
      return;
    }
    if (!llvm::isa<llvm::BranchInst>(&orig_blk->back())) {
      assert(false && "didnt clean cfg before pass");
    }
    inst_to_del.emplace_back(&orig_blk->back());


    for (auto &phi : succ_blk->phis()) {
      inst_to_del.emplace_back(&phi);
      assert(phi.getNumIncomingValues() <= 1);
      if (phi.getNumIncomingValues() == 1) {
        llvm::Value *v = phi.getIncomingValueForBlock(orig_blk);
        phi.replaceAllUsesWith(v);
      }
    }
    llvm::Instruction *cur_inst = succ_blk->getFirstNonPHI();
    if(!cur_inst) {
      clean_insts();
      succ_blk->eraseFromParent();
      return;
    }
    auto it = cur_inst->getIterator();
    auto end = succ_blk->end();
    std::vector<llvm::Instruction *> succ_insts;
    while(it != end) {
      succ_insts.emplace_back(&(*it));
      it++;
    }
    for(auto *inst : succ_insts) {
      inst->removeFromParent();
      inst->insertAfter(&orig_blk->back());
    }
    clean_insts();
    succ_blk->eraseFromParent();
    removed[succ_idx] = true;
  }

  void join_succ_double_bak(int orig_idx, int succ1_idx, int succ2_idx) {
    if(dom.cfg_preds[succ1_idx].size() != 1 || dom.cfg_preds[succ1_idx].size() != 1) {
      return;
    }

    llvm::BasicBlock *orig_blk = dom.idx_to_blk[orig_idx];
    llvm::BasicBlock *succ1_blk = dom.idx_to_blk[succ1_idx];
    llvm::BasicBlock *succ2_blk = dom.idx_to_blk[succ2_idx];

  
   }

  // if we are the only successor and we don't have any other successors we join
  // his isntructions into ours
  void join_succ(int orig_idx) {
    std::vector<int> &succs = dom.cfg_succs[orig_idx];
    switch (succs.size()) {
    case 1:
      join_succ_single(orig_idx, succs[0]);
      break;
    case 2:
      //join_succ_double(orig_idx, succs[0], succs[1]);
      break;
    default:
      return;
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
};
// todo strs_eq_fun ins't absorbed for constant strings
std::pair<bool, bool> fold_tree(CFG &cfg, DomTree &dom) {
  TreeFolder folder(cfg, dom);

  // postorder
  // merge blocks on dominated paths
  for (int i = dom.idx_to_blk.size()-1; i >= 0; i--) {
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