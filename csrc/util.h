#pragma once

#include <memory>
#include "skel.h"

namespace treeparse {
  std::unique_ptr<ast::Program> build_prog(std::string s_in);
} // namespace treeparse