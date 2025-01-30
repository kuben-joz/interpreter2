#pragma once

#include "cfg.h"
#include "dom_tree.h"

namespace clean {
std::pair<bool, bool> run_gcse(CFG &cfg, DomTree &dom);
} // namespace clean