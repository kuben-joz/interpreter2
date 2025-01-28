

#include <llvm-14/llvm/IR/InstVisitor.h>

#include "cfg.h"
#include "dom_tree.h"
#include "init_pass.h"

namespace clean {
class InitCleaner : public llvm::InstVisitor<InitCleaner> {
public:
  bool change_glob = false;
  bool change_structure = false;
  bool is_past_ret = false;
  CFG &cfg;
  DomTree &dom;
  std::vector<bool> removed;
  std::vector<llvm::Instruction *> inst_to_del;

  InitCleaner(CFG &cfg, DomTree &dom)
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
      change_glob = true;
      change_structure = true;
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
        change_glob = true;
        change_structure = true;
        dom.blk_to_idx.erase(dom.idx_to_blk[succ]);
        dom.idx_to_blk[succ]->eraseFromParent();
        dom.idx_to_blk[succ] = nullptr;
      }
    }
  }

  void clean_hanging_dom(int idx) {
    if (!is_past_ret) {
      return;
    }
    is_past_ret = false;
    if (dom.cfg_succs[idx].empty()) {
      return;
    }
    change_structure = true;
    change_glob = true;
    clean_hanging_dom_rec(idx);
  }

  void visitInstruction(llvm::Instruction &inst) {
    if (is_past_ret) {
      inst_to_del.emplace_back(&inst);
      return;
    }
  }

  void visitReturnInst(llvm::ReturnInst &ret) {
    if (is_past_ret) {
      inst_to_del.emplace_back(&ret);
      return;
    }
    is_past_ret = true;
  }

  void visitPHINode(llvm::PHINode &phi) { assert(!is_past_ret); }
};

void transform_rec(int idx, CFG &cfg, DomTree &dom, InitCleaner &cleaner) {
  llvm::BasicBlock *cur_block = dom.idx_to_blk[idx];
  for (auto &inst : cur_block->getInstList()) {
    cleaner.visit(inst);
  }
  cleaner.clean_hanging_dom(idx);
  cleaner.clean_insts();
  for (int succ : dom.dom_succs[idx]) {
    if (!cleaner.removed[succ]) {
      transform_rec(succ, cfg, dom, cleaner);
    }
  }
  cleaner.clean_insts();
}

bool add_void_ret(CFG &cfg) {
  if (!cfg.start_blks.front()->getParent()->getReturnType()->isVoidTy()) {
    return false;
  }
  bool res = false;
  for (auto *blk : cfg.end_blks) {
    if (blk->empty() || !llvm::isa<llvm::ReturnInst>(blk->back())) {
      llvm::ReturnInst::Create(blk->getContext(), blk);
      res = true;
    }
  }
  return res;
}

std::pair<bool, bool> init_clean(CFG &cfg, DomTree &dom) {

  InitCleaner cleaner(cfg, dom);
  bool void_added = add_void_ret(cfg);
  transform_rec(0, cfg, dom, cleaner);

  return std::make_pair(void_added || cleaner.change_glob,
                        cleaner.change_structure);
}
} // namespace clean