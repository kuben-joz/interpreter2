#pragma once

#include <llvm-14/llvm/IR/InstVisitor.h>

#include "cfg.h"
#include "dom_tree.h"

namespace clean {
// todo strs_eq_fun ins't absorbed for constant strings
std::pair<bool, bool> init_clean(CFG &cfg, DomTree &dom);
} // namespace clean