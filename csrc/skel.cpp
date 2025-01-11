#include <memory>
#include <sstream>
#include <vector>

#include "skel.h"

namespace ast {

AST::AST(metadata meta) : meta(meta) {}

Program::Program(std::vector<std::unique_ptr<FnDef>> fn_defs, metadata meta)
    : fn_defs(std::move(fn_defs)), AST(meta) {}

FnDef::FnDef(Type ret_type, const std::string &ident, std::vector<param> params,
             std::vector<std::unique_ptr<Stmt>> stmts, metadata meta)
    : ret_type(ret_type), ident(ident), params(params), stmts(std::move(stmts)),
      AST(meta) {}

Stmt::Stmt(metadata meta) : AST(meta) {}

Expr::Expr(metadata meta) : AST(meta) {}

BStmt::BStmt(std::vector<std::unique_ptr<Stmt>> stmts, metadata meta)
    : stmts(std::move(stmts)), Stmt(meta) {}

DeclStmt::DeclStmt(Type type, const std::string &ident,
                   std::unique_ptr<Expr> init_val, metadata meta)
    : ident(ident), init_val(std::move(init_val)), Stmt(meta) {}

AssStmt::AssStmt(const std::string &ident, std::unique_ptr<Expr> ass_expr,
                 metadata meta)
    : ident(ident), ass_expr(std::move(ass_expr)), Stmt(meta) {}

IncDecStmt::IncDecStmt(const std::string &ident, bool is_inc, metadata meta)
    : ident(ident), is_inc(is_inc), Stmt(meta) {}

RetStmt::RetStmt(std::unique_ptr<Expr> ret_expr, metadata meta)
    : ret_expr(std::move(ret_expr)), Stmt(meta) {}

CondStmt::CondStmt(std::unique_ptr<Expr> cond_expr,
                   std::unique_ptr<Stmt> if_stmt,
                   std::unique_ptr<Stmt> else_stmt, metadata meta)
    : cond_expr(std::move(cond_expr)), if_stmt(std::move(if_stmt)),
      else_stmt(std::move(else_stmt)), Stmt(meta) {}

WhileStmt::WhileStmt(std::unique_ptr<Expr> cond, std::unique_ptr<Stmt> body,
                     metadata meta)
    : cond(std::move(cond)), body(std::move(body)), Stmt(meta) {}

ExprStmt::ExprStmt(std::unique_ptr<Expr> expr, metadata data)
    : expr(std::move(expr)), Stmt(data) {}

VarExpr::VarExpr(const std::string &ident, metadata meta)
    : ident(ident), Expr(meta) {}

LIntExpr::LIntExpr(int32_t val, metadata meta) : val(val), Expr(meta) {}

LBoolExpr::LBoolExpr(bool val, metadata meta) : val(val), Expr(meta) {}

LStringExpr::LStringExpr(const std::string &val, metadata meta)
    : val(val), Expr(meta) {}

FunAppExpr::FunAppExpr(const std::string &ident,
                       std::vector<std::unique_ptr<Expr>> args, metadata meta)
    : ident(ident), args(std::move(args)), Expr(meta) {}

NegExpr::NegExpr(std::unique_ptr<Expr> sub_expr, metadata meta)
    : sub_expr(std::move(sub_expr)), Expr(meta) {}

NotExpr::NotExpr(std::unique_ptr<Expr> sub_expr, metadata meta)
    : sub_expr(std::move(sub_expr)), Expr(meta) {}

MulExpr::MulExpr(MulOp op, std::unique_ptr<Expr> l_sub_expr,
                 std::unique_ptr<Expr> r_sub_expr, metadata meta)
    : op(op), l_sub_expr(std::move(l_sub_expr)),
      r_sub_expr(std::move(r_sub_expr)), Expr(meta) {}

AddExpr::AddExpr(AddOp op, std::unique_ptr<Expr> l_sub_expr,
                 std::unique_ptr<Expr> r_sub_expr, metadata meta)
    : op(op), l_sub_expr(std::move(l_sub_expr)),
      r_sub_expr(std::move(r_sub_expr)), Expr(meta) {}

RelExpr::RelExpr(RelOp op, std::unique_ptr<Expr> l_sub_expr,
                 std::unique_ptr<Expr> r_sub_expr, metadata meta)
    : op(op), l_sub_expr(std::move(l_sub_expr)),
      r_sub_expr(std::move(r_sub_expr)), Expr(meta) {}

LogExpr::LogExpr(LogOp op, std::unique_ptr<Expr> l_sub_expr,
                 std::unique_ptr<Expr> r_sub_expr, metadata meta)
    : op(op), l_sub_expr(std::move(l_sub_expr)),
      r_sub_expr(std::move(r_sub_expr)), Expr(meta) {}

} // namespace ast
