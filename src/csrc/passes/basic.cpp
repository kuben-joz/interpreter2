#include "cfg.h"
#include <llvm-14/llvm/IR/BasicBlock.h>
#include <llvm-14/llvm/IR/Instructions.h>
#include <llvm-14/llvm/Support/Casting.h>
#include <vector>

namespace clean {
std::pair<bool, bool> remove_unreachable(CFG &cfg) {
  std::vector<llvm::BasicBlock *> reachable_v = cfg.get_postorder();
  std::set<llvm::BasicBlock *> reachable(reachable_v.begin(),
                                         reachable_v.end());
  std::set<llvm::BasicBlock *> not_reachable;
  for (auto &blk : cfg.func->getBasicBlockList()) {
    if (!reachable.count(&blk)) {
      not_reachable.insert(&blk);
    }
  }
  bool changed = false;
  for (auto *blk : reachable_v) {
    auto it = blk->begin();
    auto end = blk->end();
    while (it != end) {
      if (auto *phi = llvm::dyn_cast<llvm::PHINode>(&(*it))) {
        std::vector<llvm::BasicBlock *> phi_blk_rem;
        for (auto *blk1 : phi->blocks()) {
          if (not_reachable.count(blk1)) {
            phi_blk_rem.emplace_back(blk1);
          }
        }
        for (auto *blk1 : phi_blk_rem) {
          int idx = phi->getBasicBlockIndex(blk1);
          assert(idx >= 0);
          phi->removeIncomingValue(idx);
        }
      } else {
        break;
      }
      it++;
    }
  }

  for (auto *blk : not_reachable) {
    blk->eraseFromParent();
    changed = true;
  }
  return {changed, changed};
}

} // namespace clean