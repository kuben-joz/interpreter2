#include "skel.h"
#include "visitor.h"
#include <iterator>
#include <memory>

class BStmtCleaner : public Visitor {
private:
  bool is_blk = false;

public:
  void visit_prog(ast::Program &prog) override {
    for (auto &fn : prog.fn_defs) {
      fn->accept(this);
    }
  }

  void visit_fndef(ast::FnDef &fn) override {
    for (auto &stmt : fn.stmts) {
      stmt->accept(this);
    }
    if (fn.stmts.size() == 1 && is_blk) {
      ast::BStmt *child = dynamic_cast<ast::BStmt *>(fn.stmts[0].get());
      std::vector<std::unique_ptr<ast::Stmt>> stmt_children(
          std::move(child->stmts));
      fn.stmts.clear();
      fn.stmts.insert(fn.stmts.end(),
                      std::make_move_iterator(stmt_children.begin()),
                      std::make_move_iterator(stmt_children.end()));
    }
    is_blk = true;
  }

  void visit_blk(ast::BStmt &stmt) override {
    for (auto &stmt : stmt.stmts) {
      stmt->accept(this);
    }
    if (stmt.stmts.size() == 1 && is_blk) {
      ast::BStmt *child = dynamic_cast<ast::BStmt *>(stmt.stmts[0].get());
      std::vector<std::unique_ptr<ast::Stmt>> stmt_children(
          std::move(child->stmts));
      stmt.stmts.clear();
      stmt.stmts.insert(stmt.stmts.end(),
                        std::make_move_iterator(stmt_children.begin()),
                        std::make_move_iterator(stmt_children.end()));
    }
    is_blk = true;
  }
  void visit_decl(ast::DeclStmt &stmt) override { is_blk = false; }
  void visit_ass(ast::AssStmt &stmt) override { is_blk = false; }
  void visit_incdec(ast::IncDecStmt &stmt) override { is_blk = false; }
  void visit_ret(ast::RetStmt &stmt) override { is_blk = false; }
  void visit_cond(ast::CondStmt &stmt) override {
    stmt.if_stmt->accept(this);
    if (stmt.else_stmt) { // todo check this is how it works for nullptr
      stmt.else_stmt->accept(this);
    }
    is_blk = false;
  }
  void visit_while(ast::WhileStmt &stmt) override {
    stmt.body->accept(this);
    is_blk = false;
  }
  void visit_expr(ast::ExprStmt &stmt) override { is_blk = false; }

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