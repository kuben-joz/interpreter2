#pragma once

#include "cfg.h"
#include "dom_tree.h"
#include "pass_util.h"
#include <llvm-14/llvm/IR/Function.h>
#include <utility>

// todo same for strings
namespace clean {
std::pair<bool, bool> val_prop(CFG &cfg, DomTree &dom, llvm::Function* str_eq_fn, StringCMP &str_cmp);
}