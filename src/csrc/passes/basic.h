#pragma once
#include "cfg.h"
#include <llvm-14/llvm/IR/BasicBlock.h>

namespace clean {

std::pair<bool, bool> remove_unreachable(CFG &cfg);
}