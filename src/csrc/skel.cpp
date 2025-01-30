#include <memory>
#include <vector>

#include "skel.h"

namespace ast {

AST::AST(metadata meta) : meta(meta) {}

Program::Program(std::vector<std::unique_ptr<FnDef>> fn_defs, metadata meta)
    : AST(meta), fn_defs(std::move(fn_defs)) {}

FnDef::FnDef(Type ret_type, const std::string &ident, std::vector<param> params,
             std::vector<std::unique_ptr<Stmt>> stmts, metadata meta)
    : AST(meta), ret_type(ret_type), ident(ident), params(params),
      stmts(std::move(stmts)) {}

Stmt::Stmt(metadata meta) : AST(meta) {}

Expr::Expr(metadata meta) : AST(meta) {}

BStmt::BStmt(std::vector<std::unique_ptr<Stmt>> stmts, metadata meta)
    : Stmt(meta), stmts(std::move(stmts)) {}

DeclStmt::DeclStmt(Type type, const std::string &ident,
                   std::unique_ptr<Expr> init_val, metadata meta)
    : Stmt(meta), type(type), ident(ident), init_val(std::move(init_val)) {}

AssStmt::AssStmt(const std::string &ident, std::unique_ptr<Expr> ass_expr,
                 metadata meta)
    : Stmt(meta), ident(ident), ass_expr(std::move(ass_expr)) {}

IncDecStmt::IncDecStmt(const std::string &ident, bool is_inc, metadata meta)
    : Stmt(meta), ident(ident), is_inc(is_inc) {}

RetStmt::RetStmt(std::unique_ptr<Expr> ret_expr, metadata meta)
    : Stmt(meta), ret_expr(std::move(ret_expr)) {}

CondStmt::CondStmt(std::unique_ptr<Expr> cond_expr,
                   std::unique_ptr<Stmt> if_stmt,
                   std::unique_ptr<Stmt> else_stmt, metadata meta)
    : Stmt(meta), cond_expr(std::move(cond_expr)), if_stmt(std::move(if_stmt)),
      else_stmt(std::move(else_stmt)) {}

WhileStmt::WhileStmt(std::unique_ptr<Expr> cond, std::unique_ptr<Stmt> body,
                     metadata meta)
    : Stmt(meta), cond(std::move(cond)), body(std::move(body)) {}

ExprStmt::ExprStmt(std::unique_ptr<Expr> expr, metadata data)
    : Stmt(data), expr(std::move(expr)) {}

VarExpr::VarExpr(const std::string &ident, metadata meta)
    : Expr(meta), ident(ident) {}

LIntExpr::LIntExpr(int32_t val, metadata meta) : Expr(meta), val(val) {}

LBoolExpr::LBoolExpr(bool val, metadata meta) : Expr(meta), val(val) {}

LStringExpr::LStringExpr(const std::string &val, metadata meta)
    : Expr(meta), val(val) {}

FunAppExpr::FunAppExpr(const std::string &ident,
                       std::vector<std::unique_ptr<Expr>> args, metadata meta)
    : Expr(meta), ident(ident), args(std::move(args)) {}

NegExpr::NegExpr(std::unique_ptr<Expr> sub_expr, metadata meta)
    : Expr(meta), sub_expr(std::move(sub_expr)) {}

NotExpr::NotExpr(std::unique_ptr<Expr> sub_expr, metadata meta)
    : Expr(meta), sub_expr(std::move(sub_expr)) {}

MulExpr::MulExpr(MulOp op, std::unique_ptr<Expr> l_sub_expr,
                 std::unique_ptr<Expr> r_sub_expr, metadata meta)
    : Expr(meta), op(op), l_sub_expr(std::move(l_sub_expr)),
      r_sub_expr(std::move(r_sub_expr)) {}

AddExpr::AddExpr(AddOp op, std::unique_ptr<Expr> l_sub_expr,
                 std::unique_ptr<Expr> r_sub_expr, metadata meta)
    : Expr(meta), op(op), l_sub_expr(std::move(l_sub_expr)),
      r_sub_expr(std::move(r_sub_expr)) {}

RelExpr::RelExpr(RelOp op, std::unique_ptr<Expr> l_sub_expr,
                 std::unique_ptr<Expr> r_sub_expr, metadata meta)
    : Expr(meta), op(op), l_sub_expr(std::move(l_sub_expr)),
      r_sub_expr(std::move(r_sub_expr)) {}

LogExpr::LogExpr(LogOp op, std::unique_ptr<Expr> l_sub_expr,
                 std::unique_ptr<Expr> r_sub_expr, metadata meta)
    : Expr(meta), op(op), l_sub_expr(std::move(l_sub_expr)),
      r_sub_expr(std::move(r_sub_expr)) {}

} // namespace ast
