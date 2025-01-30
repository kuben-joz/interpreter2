#include "cfg.h"
#include <llvm-14/llvm/IR/BasicBlock.h>
#include <vector>

namespace clean {
std::pair<bool, bool> remove_unreachable(CFG &cfg) {
  std::vector<llvm::BasicBlock *> reachable_v = cfg.get_postorder();
  std::set<llvm::BasicBlock *> reachable(reachable_v.begin(),
                                         reachable_v.end());
  std::vector<llvm::BasicBlock *> not_reachable;
  for (auto &blk : cfg.func->getBasicBlockList()) {
    if (!reachable.count(&blk)) {
      not_reachable.emplace_back(&blk);
    }
  }
  bool changed = false;
  for (auto *blk : not_reachable) {
    blk->eraseFromParent();
    changed = true;
  }
  return {changed, changed};
}

} // namespace clean