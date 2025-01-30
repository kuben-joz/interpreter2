#pragma once

#include "visitor.h"
#include <memory>
#include <string>
#include <type_traits>
#include <vector>

namespace ast {

enum Type { INT, STR, BOOL, VOID };

enum MulOp { TIMES, DIV, MOD };

enum AddOp { PLUS, MINUS };

enum RelOp { LTH, LE, GTH, GE, EQU, NE };

enum LogOp { AND, OR };

struct metadata {
  int line = -1;
  int col = -1;
};

struct param {
  Type type;
  std::string ident;
  param(Type type, const std::string &ident) : type(type), ident(ident) {}
};

class AST {
public:
  const metadata meta;
  AST(metadata meta);
  virtual ~AST() = default;
  virtual void accept(Visitor *visitor) = 0;
};
// static_assert(std::is_polymorphic_v<AST>);

class Program : public AST {
public:
  std::vector<std::unique_ptr<FnDef>> fn_defs;
  Program(std::vector<std::unique_ptr<FnDef>> fn_defs, metadata meta);
  void accept(Visitor *visitor) override { visitor->visit_prog(*this); }
};

class FnDef : public AST {
public:
  Type ret_type;
  std::string ident;
  std::vector<param> params;
  std::vector<std::unique_ptr<Stmt>> stmts;
  FnDef(Type ret_type, const std::string &ident, std::vector<param> params,
        std::vector<std::unique_ptr<Stmt>> stmts, metadata meta);
  void accept(Visitor *visitor) override { visitor->visit_fndef(*this); }
};

class Stmt : public AST {
public:
  Stmt(metadata meta);
  virtual ~Stmt() = default;
  virtual void accept(Visitor *visitor) = 0;
};
// static_assert(std::is_polymorphic_v<Stmt>);

class Expr : public AST {
public:
  Expr(metadata meta);
  virtual ~Expr() = default;
  virtual void accept(Visitor *visitor) = 0;
};
// static_assert(std::is_polymorphic_v<Expr>);

class BStmt : public Stmt {
public:
  std::vector<std::unique_ptr<Stmt>> stmts;
  BStmt(std::vector<std::unique_ptr<Stmt>> stmts, metadata meta);
  void accept(Visitor *visitor) override { visitor->visit_blk(*this); }
};

class DeclStmt : public Stmt {
public:
  Type type;
  std::string ident;
  std::unique_ptr<Expr> init_val; // nullptr if none
  DeclStmt(Type type, const std::string &ident, std::unique_ptr<Expr> init_val,
           metadata meta);
  void accept(Visitor *visitor) override { visitor->visit_decl(*this); }
};

class AssStmt : public Stmt {
public:
  std::string ident;
  std::unique_ptr<Expr> ass_expr;
  AssStmt(const std::string &ident, std::unique_ptr<Expr> ass_expr,
          metadata meta);
  void accept(Visitor *visitor) override { visitor->visit_ass(*this); }
};

class IncDecStmt : public Stmt {
public:
  std::string ident;
  bool is_inc;
  IncDecStmt(const std::string &ident, bool is_inc, metadata meta);
  void accept(Visitor *visitor) override { visitor->visit_incdec(*this); }
};

class RetStmt : public Stmt {
public:
  std::unique_ptr<Expr> ret_expr;
  RetStmt(std::unique_ptr<Expr> ret_expr, metadata meta);
  void accept(Visitor *visitor) override { visitor->visit_ret(*this); }
};

class CondStmt : public Stmt {
public:
  std::unique_ptr<Expr> cond_expr;
  std::unique_ptr<Stmt> if_stmt;
  std::unique_ptr<Stmt> else_stmt;
  CondStmt(std::unique_ptr<Expr> cond_expr, std::unique_ptr<Stmt> if_stmt,
           std::unique_ptr<Stmt> else_stmt, metadata meta);
  void accept(Visitor *visitor) override { visitor->visit_cond(*this); }
};

class WhileStmt : public Stmt {
public:
  std::unique_ptr<Expr> cond;
  std::unique_ptr<Stmt> body;
  WhileStmt(std::unique_ptr<Expr> cond, std::unique_ptr<Stmt> body,
            metadata meta);
  void accept(Visitor *visitor) override { visitor->visit_while(*this); }
};

class ExprStmt : public Stmt {
public:
  std::unique_ptr<Expr> expr;
  ExprStmt(std::unique_ptr<Expr> expr, metadata data);
  void accept(Visitor *visitor) override { visitor->visit_expr(*this); }
};

class VarExpr : public Expr {
public:
  std::string ident;
  VarExpr(const std::string &ident, metadata meta);
  void accept(Visitor *visitor) override { visitor->visit_var(*this); }
};

class LIntExpr : public Expr {
public:
  int32_t val;
  LIntExpr(int32_t val, metadata meta);
  void accept(Visitor *visitor) override { visitor->visit_int(*this); }
};

class LBoolExpr : public Expr {
public:
  bool val;
  LBoolExpr(bool val, metadata meta);
  void accept(Visitor *visitor) override { visitor->visit_bool(*this); }
};

class LStringExpr : public Expr {
public:
  std::string val;
  LStringExpr(const std::string &val, metadata meta);
  void accept(Visitor *visitor) override { visitor->visit_string(*this); }
};

class FunAppExpr : public Expr {
public:
  std::string ident;
  std::vector<std::unique_ptr<Expr>> args;
  FunAppExpr(const std::string &ident, std::vector<std::unique_ptr<Expr>> args,
             metadata meta);
  void accept(Visitor *visitor) override { visitor->visit_fnapp(*this); }
};

class NegExpr : public Expr {
public:
  std::unique_ptr<Expr> sub_expr;
  NegExpr(std::unique_ptr<Expr> sub_expr, metadata meta);
  void accept(Visitor *visitor) override { visitor->visit_neg(*this); }
};

class NotExpr : public Expr {
public:
  std::unique_ptr<Expr> sub_expr;
  NotExpr(std::unique_ptr<Expr> sub_expr, metadata meta);
  void accept(Visitor *visitor) override { visitor->visit_not(*this); }
};

class MulExpr : public Expr {
public:
  MulOp op;
  std::unique_ptr<Expr> l_sub_expr;
  std::unique_ptr<Expr> r_sub_expr;
  MulExpr(MulOp op, std::unique_ptr<Expr> l_sub_expr,
          std::unique_ptr<Expr> r_sub_expr, metadata meta);
  void accept(Visitor *visitor) override { visitor->visit_mul(*this); }
};

class AddExpr : public Expr {
public:
  AddOp op;
  std::unique_ptr<Expr> l_sub_expr;
  std::unique_ptr<Expr> r_sub_expr;
  AddExpr(AddOp op, std::unique_ptr<Expr> l_sub_expr,
          std::unique_ptr<Expr> r_sub_expr, metadata meta);
  void accept(Visitor *visitor) override { visitor->visit_add(*this); }
};

class RelExpr : public Expr {
public:
  RelOp op;
  std::unique_ptr<Expr> l_sub_expr;
  std::unique_ptr<Expr> r_sub_expr;
  RelExpr(RelOp op, std::unique_ptr<Expr> l_sub_expr,
          std::unique_ptr<Expr> r_sub_expr, metadata meta);
  void accept(Visitor *visitor) override { visitor->visit_rel(*this); }
};

class LogExpr : public Expr {
public:
  LogOp op;
  std::unique_ptr<Expr> l_sub_expr;
  std::unique_ptr<Expr> r_sub_expr;
  LogExpr(LogOp op, std::unique_ptr<Expr> l_sub_expr,
          std::unique_ptr<Expr> r_sub_expr, metadata meta);
  void accept(Visitor *visitor) override { visitor->visit_log(*this); }
};

} // namespace ast