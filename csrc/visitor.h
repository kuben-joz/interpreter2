#pragma once

#include "syntax.h"

class Visitor {
public:
  virtual ~Visitor() = default;

  virtual void visit_prog(ast::Program &prog) = 0;

  virtual void visit_fndef(ast::FnDef &fn) = 0;

  virtual void visit_blk(ast::BStmt &stmt) = 0;
  virtual void visit_decl(ast::DeclStmt &stmt) = 0;
  virtual void visit_ass(ast::AssStmt &stmt) = 0;
  virtual void visit_incdec(ast::IncDecStmt &stmt) = 0;
  virtual void visit_ret(ast::RetStmt &stmt) = 0;
  virtual void visit_cond(ast::CondStmt &stmt) = 0;
  virtual void visit_while(ast::WhileStmt &stmt) = 0;
  virtual void visit_expr(ast::ExprStmt &stmt) = 0;

  virtual void visit_var(ast::VarExpr &expr) = 0;
  virtual void visit_int(ast::LIntExpr &expr) = 0;
  virtual void visit_bool(ast::LBoolExpr &expr) = 0;
  virtual void visit_string(ast::LStringExpr &expr) = 0;
  virtual void visit_fnapp(ast::FunAppExpr &expr) = 0;
  virtual void visit_neg(ast::NegExpr &expr) = 0;
  virtual void visit_not(ast::NotExpr &expr) = 0;
  virtual void visit_mul(ast::MulExpr &expr) = 0;
  virtual void visit_add(ast::AddExpr &expr) = 0;
  virtual void visit_rel(ast::RelExpr &expr) = 0;
  virtual void visit_log(ast::LogExpr &expr) = 0;
};

/*
class Visitortemplate : public Visitor {
private:


public:
  void visit_prog(ast::Program &prog) override {}

  void visit_fndef(ast::FnDef &fn) override {}

  void visit_blk(ast::BStmt &stmt) override {}
  void visit_decl(ast::DeclStmt &stmt) override {}
  void visit_ass(ast::AssStmt &stmt) override {}
  void visit_incdec(ast::IncDecStmt &stmt) override {}
  void visit_ret(ast::RetStmt &stmt) override {}
  void visit_cond(ast::CondStmt &stmt) override {}
  void visit_while(ast::WhileStmt &stmt) override {}
  void visit_expr(ast::ExprStmt &stmt) override {}

  void visit_var(ast::VarExpr &var) override {}
  void visit_int(ast::LIntExpr &i) override {}
  void visit_bool(ast::LBoolExpr &b) override {}
  void visit_string(ast::LStringExpr &s) override {}
  void visit_fnapp(ast::FunAppExpr &fn) override {}
  void visit_neg(ast::NegExpr &neg) override {}
  void visit_not(ast::NotExpr &no) override {}
  void visit_mul(ast::MulExpr &mul) override {}
  void visit_add(ast::AddExpr &add) override {}
  void visit_rel(ast::RelExpr &rel) override {}
  void visit_log(ast::LogExpr &log) override {}
};
*/