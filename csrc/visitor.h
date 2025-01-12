#pragma once

#include "skel.h"

class Visitor {
  public:
  virtual ~Visitor() = default;
  virtual void visit_stmt(const ast::Stmt& stmt) = 0;
  
};