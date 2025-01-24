#include <cassert>
#include <cctype>
#include <cstring>
#include <iostream>
#include <iterator>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

#include "skel.h"

namespace treeparse {

ast::metadata build_meta(std::stringstream &ss) {
  ast::metadata res;
  std::string s; // skip string
  char c;        // skip char
  ss >> s;
  assert(s == "(Just");
  ss >> c;
  assert(c == '(');
  ss >> res.line;
  ss >> c;
  assert(c == ',');
  ss >> res.col;
  ss >> c;
  assert(c == ')');
  ss >> c;
  assert(c == ')');

  assert(res.col >= 0);
  assert(res.line >= 0);
  return res;
}

std::string read_string(std::stringstream &ss_in) {
  std::stringstream ss_out;
  char c;
  ss_in >> c;
  assert(c == '"');
  bool escaped = false;
  c = ss_in.get();
  while (c != '"' || escaped) {
    if (escaped) {
      char c_conv;
      switch (c) {
      case 'a':
        c_conv = '\a';
        break;
      case 'b':
        c_conv = '\b';
        break;
      case 't':
        c_conv = '\t';
        break;
      case 'n':
        c_conv = '\n';
        break;
      case 'v':
        c_conv = '\v';
        break;
      case 'f':
        c_conv = '\f';
        break;
      case 'r':
        c_conv = '\r';
        break;
      default:
        c_conv = c;
      }
      ss_out << c_conv;
      escaped = false;
    } else if (c == '\\') {
      escaped = true;
    } else {
      ss_out << c;
    }
    c = ss_in.get();
  }
  return ss_out.str();
}

ast::Type get_type(std::stringstream &ss) {
  char c;
  ss >> c;
  assert(c == '(');
  std::string s;
  ss >> s;
  build_meta(ss);
  ss >> c;
  assert(c == ')');
  if (s == "Int") {
    return ast::INT;
  } else if (s == "Str") {
    return ast::STR;
  } else if (s == "Bool") {
    return ast::BOOL;
  } else if (s == "Void") {
    return ast::VOID;
  } else {
    throw std::runtime_error("Invlaid type read when parsing tree");
  }
}

std::string get_ident(std::stringstream &ss) {
  std::string s;
  ss >> s;
  assert(s == "(Ident");
  std::string res = read_string(ss);
  char c;
  ss >> c;
  assert(c == ')');
  return res;
}

ast::MulOp get_mul_op(std::stringstream &ss) {
  std::string s;
  ss >> s;
  ast::MulOp res;
  if (s == "(Times") {
    res = ast::TIMES;
  } else if (s == "(Div") {
    res = ast::DIV;
  } else if (s == "(Mod") {
    res = ast::MOD;
  } else {
    throw std::runtime_error("unkown mul op");
  }
  build_meta(ss);
  char c;
  ss >> c;
  assert(c == ')');
  return res;
}

ast::AddOp get_add_op(std::stringstream &ss) {
  std::string s;
  ss >> s;
  ast::AddOp res;
  if (s == "(Plus") {
    res = ast::PLUS;
  } else if (s == "(Minus") {
    res = ast::MINUS;
  } else {
    throw std::runtime_error("unkown add op");
  }
  build_meta(ss);
  char c;
  ss >> c;
  assert(c == ')');
  return res;
}

ast::RelOp get_rel_op(std::stringstream &ss) {
  std::string s;
  ss >> s;
  ast::RelOp res;
  if (s == "(LTH") {
    res = ast::LTH;
  } else if (s == "(LE") {
    res = ast::LE;
  } else if (s == "(GTH") {
    res = ast::GTH;
  } else if (s == "(GE") {
    res = ast::GE;
  } else if (s == "(EQU") {
    res = ast::EQU;
  } else if (s == "(NE") {
    res = ast::NE;
  } else {
    throw std::runtime_error("unkown rel op");
  }
  build_meta(ss);
  char c;
  ss >> c;
  assert(c == ')');
  return res;
}

std::vector<std::unique_ptr<ast::Stmt>> build_block(std::stringstream &ss);

std::unique_ptr<ast::Expr> build_expr(std::stringstream &ss) {
  char c;
  ss >> c;
  assert(c == '(');
  std::string s;
  ss >> s;
  ast::metadata meta = build_meta(ss);
  std::unique_ptr<ast::Expr> res;
  if (s == "EVar") {
    std::string ident = get_ident(ss);
    res = std::make_unique<ast::VarExpr>(ident, meta);
  } else if (s == "ELitInt") {
    int32_t val;
    ss >> val;
    res = std::make_unique<ast::LIntExpr>(val, meta);
  } else if (s == "ELitTrue") {
    bool val = true;
    res = std::make_unique<ast::LBoolExpr>(val, meta);
  } else if (s == "ELitFalse") {
    bool val = false;
    res = std::make_unique<ast::LBoolExpr>(val, meta);
  } else if (s == "EString") {
    std::string val = read_string(ss);
    res = std::make_unique<ast::LStringExpr>(val, meta);
  } else if (s == "EApp") {
    std::string ident = get_ident(ss);
    std::vector<std::unique_ptr<ast::Expr>> args;
    char delim;
    ss >> delim;
    assert(delim == '[');
    delim = ss.peek();
    if (delim == ']')
      ss.get();
    while (delim != ']') {
      args.emplace_back(build_expr(ss));
      ss >> delim;
    }
    res = std::make_unique<ast::FunAppExpr>(ident, std::move(args), meta);

  } else if (s == "Neg") {
    res = std::make_unique<ast::NegExpr>(build_expr(ss), meta);
  } else if (s == "Not") {
    res = std::make_unique<ast::NotExpr>(build_expr(ss), meta);
  } else if (s == "EMul") {
    std::unique_ptr<ast::Expr> LHS = build_expr(ss);
    ast::MulOp op = get_mul_op(ss);
    std::unique_ptr<ast::Expr> RHS = build_expr(ss);
    res = std::make_unique<ast::MulExpr>(op, std::move(LHS), std::move(RHS),
                                         meta);
  } else if (s == "EAdd") {
    std::unique_ptr<ast::Expr> LHS = build_expr(ss);
    ast::AddOp op = get_add_op(ss);
    std::unique_ptr<ast::Expr> RHS = build_expr(ss);
    res = std::make_unique<ast::AddExpr>(op, std::move(LHS), std::move(RHS),
                                         meta);
  } else if (s == "ERel") {
    std::unique_ptr<ast::Expr> LHS = build_expr(ss);
    ast::RelOp op = get_rel_op(ss);
    std::unique_ptr<ast::Expr> RHS = build_expr(ss);
    res = std::make_unique<ast::RelExpr>(op, std::move(LHS), std::move(RHS),
                                         meta);
  } else if (s == "EAnd" || s == "EOr") {
    std::unique_ptr<ast::Expr> LHS = build_expr(ss);
    ast::LogOp op = s == "EAnd" ? ast::AND : ast::OR;
    std::unique_ptr<ast::Expr> RHS = build_expr(ss);
    res = std::make_unique<ast::LogExpr>(op, std::move(LHS), std::move(RHS),
                                         meta);
  } else {
    throw std::runtime_error("unknow expression");
  }
  ss >> c;
  assert(c == ')');
  return res;
}

// return vecotor in case of empty statement or declarations with multiple
// variables
std::vector<std::unique_ptr<ast::Stmt>> build_stmt(std::stringstream &ss) {
  std::string s;
  ss >> s;
  ast::metadata meta = build_meta(ss);
  std::vector<std::unique_ptr<ast::Stmt>> res;
  if (s == "Empty") {
    // Ignore empty statements
    // return std::make_unique<ast::EmptyStmt>(meta);
  } else if (s == "BStmt") {
    res.emplace_back(std::make_unique<ast::BStmt>(build_block(ss), meta));
  } else if (s == "Decl") { // split all declarations
    ast::Type type = get_type(ss);
    char delim;
    ss >> delim;
    assert(delim == '[');
    delim = ss.peek();
    if (delim == ']')
      ss.get();
    while (delim != ']') {
      ss >> s;
      meta = build_meta(ss);
      std::string ident = get_ident(ss);
      std::unique_ptr<ast::Expr> init_val(nullptr);
      if (s == "Init")
        init_val = std::move(build_expr(ss));
      res.emplace_back(std::make_unique<ast::DeclStmt>(
          type, ident, std::move(init_val), meta));
      ss >> delim;
    }
  } else if (s == "Ass") {
    std::string ident = get_ident(ss);
    std::unique_ptr<ast::Expr> ass_expr = build_expr(ss);
    res.emplace_back(
        std::make_unique<ast::AssStmt>(ident, std::move(ass_expr), meta));
  } else if (s == "Incr" || s == "Decr") {
    bool is_inc = s == "Incr";
    std::string ident = get_ident(ss);
    res.emplace_back(std::make_unique<ast::IncDecStmt>(ident, is_inc, meta));
  } else if (s == "Ret" || s == "RetNone") {
    std::unique_ptr<ast::Expr> ret_expr(nullptr);
    if (s == "Ret")
      ret_expr = build_expr(ss);
    res.emplace_back(std::make_unique<ast::RetStmt>(std::move(ret_expr), meta));
  } else if (s == "Cond" || s == "CondElse" || s == "While") {
    std::unique_ptr<ast::Expr> expr = build_expr(ss);
    char c;
    ss >> c;
    assert(c == '(');
    std::unique_ptr<ast::Stmt> if_stmt =
        std::make_unique<ast::BStmt>(build_stmt(ss), meta);
    if (s == "CondElse") {
      std::unique_ptr<ast::Stmt> else_stmt =
          std::make_unique<ast::BStmt>(build_stmt(ss), meta);
      res.emplace_back(std::make_unique<ast::CondStmt>(
          std::move(expr), std::move(if_stmt), std::move(else_stmt), meta));
    } else if (s == "Cond") {
      res.emplace_back(std::make_unique<ast::CondStmt>(
          std::move(expr), std::move(if_stmt), nullptr, meta));
    } else {
      res.emplace_back(std::make_unique<ast::WhileStmt>(
          std::move(expr), std::move(if_stmt), meta));
    }
  } else if (s == "SExp") {
    std::unique_ptr<ast::Expr> expr = build_expr(ss);
    res.emplace_back(std::make_unique<ast::ExprStmt>(std::move(expr), meta));
  } else {
    throw std::runtime_error("unkown statement");
  }
  return res;
}

std::vector<std::unique_ptr<ast::Stmt>> build_block(std::stringstream &ss) {
  std::string s;
  ss >> s;
  assert(s == "(FunBlock");
  build_meta(ss);
  std::vector<std::unique_ptr<ast::Stmt>> res;
  char delim;
  ss >> delim;
  assert(delim == '[');
  delim = ss.peek();
  if (delim == ']')
    ss.get();
  while (delim != ']') {

    std::vector<std::unique_ptr<ast::Stmt>> temp_stmts = build_stmt(ss);
    res.insert(res.end(), std::make_move_iterator(temp_stmts.begin()),
               std::make_move_iterator(temp_stmts.end()));
    ss >> delim;
  }
  char c;
  ss >> c;
  assert(c == ')');
  return res;
}

std::unique_ptr<ast::FnDef> build_func(std::stringstream &ss) {
  std::string s;
  ss >> s;
  assert(s == "FunDef");
  ast::metadata meta = build_meta(ss);
  ast::Type ret_type = get_type(ss);
  std::string ident = get_ident(ss);
  std::vector<ast::param> params;
  char delim;
  ss >> delim;
  assert(delim == '[');
  delim = ss.peek();
  if (delim == ']')
    ss.get();
  while (delim != ']') {
    ss >> s;
    assert(s == "ArgVal");
    build_meta(ss);
    ast::Type param_type = get_type(ss);
    std::string param_ident = get_ident(ss);
    params.emplace_back(param_type, param_ident);
    ss >> delim;
  }
  std::vector<std::unique_ptr<ast::Stmt>> stmts = build_block(ss);
  return std::make_unique<ast::FnDef>(ret_type, ident, std::move(params),
                                      std::move(stmts), meta);
}

std::vector<std::unique_ptr<ast::FnDef>> build_funcs(std::stringstream &ss) {
  std::vector<std::unique_ptr<ast::FnDef>> res;
  char delim;
  ss >> delim;
  assert(delim == '[');
  delim = ss.peek();
  if (delim == ']')
    ss.get();
  while (delim != ']') {
    res.emplace_back(build_func(ss));
    ss >> delim;
  }
  return res;
}

std::unique_ptr<ast::Program> build_prog(std::string s_in) {
  std::stringstream ss(s_in);
  std::string s;
  ss >> s;
  assert(s == "ProgramS");
  ast::metadata meta = build_meta(ss);
  std::vector<std::unique_ptr<ast::FnDef>> funcs = build_funcs(ss);
  return std::make_unique<ast::Program>(std::move(funcs), meta);
}

} // namespace treeparse