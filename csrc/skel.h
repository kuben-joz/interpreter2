#pragma once

#include <memory>
#include <string>
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

class AST;

class Program;

class FnDef;

class Stmt;
// class EmptyStmt; // not used
class BStmt;
class DeclStmt;
class AssStmt;
class IncDecStmt;
class RetStmt;
class CondStmt;
class WhileStmt;
class ExprStmt;

class Expr;
class VarExpr;
class LIntExpr;
class LBoolExpr;
class LStringExpr;
class FunAppExpr;
class NegExpr;
class NotExpr;
class MulExpr;
class AddExpr;
class RelExpr;
class LogExpr;

class AST {
public:
  const metadata meta;
  AST(metadata meta);
  virtual ~AST() = default;
};

class Program : public AST {
public:
  std::vector<std::unique_ptr<FnDef>> fn_defs;
  Program(std::vector<std::unique_ptr<FnDef>> fn_defs, metadata meta);
};

class FnDef : public AST {
public:
  Type ret_type;
  std::string ident;
  std::vector<param> params;
  std::vector<std::unique_ptr<Stmt>> stmts;
  FnDef(Type ret_type, const std::string &ident, std::vector<param> params,
        std::vector<std::unique_ptr<Stmt>> stmts, metadata meta);
};

class Stmt : public AST {
public:
  Stmt(metadata meta);
  virtual ~Stmt() = default;
};

class Expr : public AST {
public:
  Expr(metadata meta);
  virtual ~Expr() = default;
};

class BStmt : public Stmt {
public:
  std::vector<std::unique_ptr<Stmt>> stmts;
  BStmt(std::vector<std::unique_ptr<Stmt>> stmts, metadata meta);
};

class DeclStmt : public Stmt {
public:
  Type type;
  std::string ident;
  std::unique_ptr<Expr> init_val; // nullptr if none
  DeclStmt(Type type, const std::string &ident, std::unique_ptr<Expr> init_val,
           metadata meta);
};

class AssStmt : public Stmt {
public:
  std::string ident;
  std::unique_ptr<Expr> ass_expr;
  AssStmt(const std::string &ident, std::unique_ptr<Expr> ass_expr,
          metadata meta);
};

class IncDecStmt : public Stmt {
public:
  std::string ident;
  bool is_inc;
  IncDecStmt(const std::string &ident, bool is_inc, metadata meta);
};

class RetStmt : public Stmt {
public:
  std::unique_ptr<Expr> ret_expr;
  RetStmt(std::unique_ptr<Expr> ret_expr, metadata meta);
};

class CondStmt : public Stmt {
public:
  std::unique_ptr<Expr> cond_expr;
  std::unique_ptr<Stmt> if_stmt;
  std::unique_ptr<Stmt> else_stmt;
  CondStmt(std::unique_ptr<Expr> cond_expr, std::unique_ptr<Stmt> if_stmt,
           std::unique_ptr<Stmt> else_stmt, metadata meta);
};

class WhileStmt : public Stmt {
public:
  std::unique_ptr<Expr> cond;
  std::unique_ptr<Stmt> body;
  WhileStmt(std::unique_ptr<Expr> cond, std::unique_ptr<Stmt> body,
            metadata meta);
};

class ExprStmt : public Stmt {
public:
  std::unique_ptr<Expr> expr;
  ExprStmt(std::unique_ptr<Expr> expr, metadata data);
};

class VarExpr : public Expr {
public:
  std::string ident;
  VarExpr(const std::string &ident, metadata meta);
};

class LIntExpr : public Expr {
public:
  int32_t val;
  LIntExpr(int32_t val, metadata meta);
};

class LBoolExpr : public Expr {
public:
  bool val;
  LBoolExpr(bool val, metadata meta);
};

class LStringExpr : public Expr {
public:
  std::string val;
  LStringExpr(const std::string &val, metadata meta);
};

class FunAppExpr : public Expr {
public:
  std::string ident;
  std::vector<std::unique_ptr<Expr>> args;
  FunAppExpr(const std::string &ident, std::vector<std::unique_ptr<Expr>> args,
             metadata meta);
};

class NegExpr : public Expr {
public:
  std::unique_ptr<Expr> sub_expr;
  NegExpr(std::unique_ptr<Expr> sub_expr, metadata meta);
};

class NotExpr : public Expr {
public:
  std::unique_ptr<Expr> sub_expr;
  NotExpr(std::unique_ptr<Expr> sub_expr, metadata meta);
};

class MulExpr : public Expr {
public:
  MulOp op;
  std::unique_ptr<Expr> l_sub_expr;
  std::unique_ptr<Expr> r_sub_expr;
  MulExpr(MulOp op, std::unique_ptr<Expr> l_sub_expr,
          std::unique_ptr<Expr> r_sub_expr, metadata meta);
};

class AddExpr : public Expr {
public:
  AddOp op;
  std::unique_ptr<Expr> l_sub_expr;
  std::unique_ptr<Expr> r_sub_expr;
  AddExpr(AddOp op, std::unique_ptr<Expr> l_sub_expr,
          std::unique_ptr<Expr> r_sub_expr, metadata meta);
};

class RelExpr : public Expr {
public:
  RelOp op;
  std::unique_ptr<Expr> l_sub_expr;
  std::unique_ptr<Expr> r_sub_expr;
  RelExpr(RelOp op, std::unique_ptr<Expr> l_sub_expr,
          std::unique_ptr<Expr> r_sub_expr, metadata meta);
};

class LogExpr : public Expr {
public:
  LogOp op;
  std::unique_ptr<Expr> l_sub_expr;
  std::unique_ptr<Expr> r_sub_expr;
  LogExpr(LogOp op, std::unique_ptr<Expr> l_sub_expr,
          std::unique_ptr<Expr> r_sub_expr, metadata meta);
};

} // namespace ast