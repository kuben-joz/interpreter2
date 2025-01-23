#pragma once
// todo remove and split into cpp and h

#include <llvm-14/llvm/IR/BasicBlock.h>
#include <llvm-14/llvm/IR/DerivedTypes.h>
#include <llvm-14/llvm/IR/Instructions.h>
#include <llvm-14/llvm/IR/Module.h>
#include <llvm-14/llvm/IR/Value.h>

#include <cassert>
#include <cstddef>
#include <map>
#include <memory>
#include <stdexcept>
#include <utility>
#include <vector>

#include "skel.h"
#include "visitor.h"
#include "llvm/ADT/APSInt.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Type.h"

class IRGen : public Visitor {
public:
  std::unique_ptr<llvm::LLVMContext> context;
  std::unique_ptr<llvm::Module> module;
  std::unique_ptr<llvm::IRBuilder<>> builder;

private:
  struct var {
    ast::Type type;
    llvm::AllocaInst *ptr;

    var(ast::Type typ, llvm::AllocaInst *alloc) : type(typ), ptr(alloc) {}
  };

  struct func {
    ast::Type ret_type;
    llvm::Function *ptr;

    func(ast::Type typ, llvm::Function *fn) : ret_type(typ), ptr(fn) {}
  };

  llvm::IntegerType *int_type = nullptr;
  llvm::IntegerType *bool_type = nullptr;
  std::map<std::string, std::vector<var>> vars;
  std::map<std::string, func> funcs;
  std::set<std::string>
      to_pop; // variables in current block that should be popped out

  llvm::Value *ret_val = nullptr;
  ast::Type ret_type = ast::VOID;

  llvm::BasicBlock *jmp_true = nullptr;
  llvm::BasicBlock *jmp_false = nullptr;
  llvm::BasicBlock *jmp_end = nullptr;
  std::vector<std::pair<llvm::Value *, llvm::BasicBlock *>> jmp_end_vals;

  llvm::BasicBlock *start_blk = nullptr; // for adding allocas

  // todo true jump and false jump

  llvm::Type *convert_type(ast::Type t) {
    llvm::Type *res = nullptr;
    switch (t) {
    case ast::VOID:
      res = llvm::Type::getVoidTy(*context);
      break;
    case ast::INT:
      res = int_type;
      break;
    case ast::BOOL:
      res = bool_type;
      break;
    case ast::STR:
      assert(false && "not implemented type for string");
      break;
    default:
      assert(false && "unknown type");
    }
    return res;
  }

public:
  IRGen() {
    context = std::make_unique<llvm::LLVMContext>();
    module = std::make_unique<llvm::Module>("my init module", *context);
    builder = std::make_unique<llvm::IRBuilder<>>(*context);
    int_type = llvm::IntegerType::getInt32Ty(*context);
    bool_type = llvm::IntegerType::getInt1Ty(*context);
  }

  void visit_prog(ast::Program &prog) override {
    assert(!ret_val);
    assert(ret_type == ast::VOID);
    assert(!jmp_true);
    assert(!jmp_false);
    assert(!jmp_end);
    assert(jmp_end_vals.empty());

    // todo add prototype functions for puts etc.
    for (auto &fn : prog.fn_defs) {
      std::vector<llvm::Type *> param_types;
      llvm::Type *ret_type = convert_type(fn->ret_type);
      for (auto &param : fn->params) {
        param_types.emplace_back(convert_type(param.type));
      }
      llvm::FunctionType *fn_typ =
          llvm::FunctionType::get(ret_type, param_types, false);
      llvm::Function *res_fn = llvm::Function::Create(
          fn_typ, llvm::Function::ExternalLinkage, fn->ident,
          module.get()); // todo check linkage is good
      int idx = 0;
      for (auto &arg : res_fn->args()) {
        const std::string &ident = fn->params[idx++].ident;
        arg.setName(ident);
      }
      funcs[fn->ident] = func(fn->ret_type, res_fn);
    }
    // first time just gets function signature, second time actually generates
    // code for them
    for (auto &fn : prog.fn_defs) {
      fn->accept(this);
    }
  }

  void visit_fndef(ast::FnDef &fn)
      override { // todo create stores for args, eliminated in mem2reg
    assert(!start_blk);
    assert(!ret_val);
    assert(ret_type == ast::VOID);
    assert(!jmp_true);
    assert(!jmp_false);
    assert(!jmp_end);
    assert(jmp_end_vals.empty());
    assert(to_pop.empty());
    assert(vars.empty());
    assert(funcs.count(fn.ident) && "Function doesn't exist");

    llvm::Function *cur_fn = funcs[fn.ident].ptr;
    start_blk = llvm::BasicBlock::Create(*context, "start", cur_fn);
    builder->SetInsertPoint(start_blk);
    int idx = 0;
    for (auto &arg : cur_fn->args()) {
      std::string arg_id = fn.params[idx].ident;
      ast::Type arg_typ = fn.params[idx++].type;
      to_pop.insert(arg_id);
      llvm::AllocaInst *alloc =
          builder->CreateAlloca(convert_type(arg_typ), nullptr, arg_id);
      // store argument for mem2reg purposes
      builder->CreateStore(&arg, alloc);
      vars[arg_id].emplace_back(arg_typ, alloc);
    }
    for (auto &stmt : fn.stmts) {
      stmt->accept(this);
    }
    assert(to_pop.size() == vars.size());
    to_pop.clear();
    vars.clear();
    start_blk = nullptr;
  }

  void visit_blk(ast::BStmt &stmt) override {
    assert(!ret_val);
    assert(ret_type == ast::VOID);
    assert(!jmp_true);
    assert(!jmp_false);
    assert(!jmp_end);
    assert(jmp_end_vals.empty());
    std::set<std::string> to_pop_bak(std::move(to_pop));
    to_pop.clear();
    for (auto &sub_stmt : stmt.stmts) {
      sub_stmt->accept(this);
    }
    for (const std::string &id : to_pop) {
      auto cur_pop = vars.find(id);
      assert(cur_pop != vars.end());
      assert(cur_pop->second.size());
      cur_pop->second.pop_back();
      if (cur_pop->second.size() == 0) {
        vars.erase(cur_pop);
      }
    }
    to_pop = std::move(to_pop_bak);
  }

  // LLVM convention is to add all alloca at beginning of first block
  void visit_decl(ast::DeclStmt &stmt) override {
    assert(!ret_val);
    assert(ret_type == ast::VOID);
    assert(!jmp_true);
    assert(!jmp_false);
    assert(!jmp_end);
    assert(jmp_end_vals.empty());

    assert(!to_pop.count(stmt.ident) &&
           "Variable already delcared in this context");
    if (stmt.init_val) {
      stmt.init_val->accept(this);
      assert(ret_val);
      assert(ret_type == stmt.type);
    } else {
      ret_type = stmt.type;
      assert(ret_type != ast::VOID && "Declaration of void variable");
      switch (ret_type) {
      case ast::INT:
        ret_val = llvm::ConstantInt::getSigned(convert_type(ret_type), 0);
        break;
      case ast::BOOL:
        ret_val = llvm::ConstantInt::getFalse(bool_type);
        break;
      case ast::STR:
        assert(false && "string declaration not yet implemented");
        break;
      }
    }
    llvm::BasicBlock *bb_bak = builder->GetInsertBlock();
    builder->SetInsertPoint(&(start_blk->front()));
    llvm::AllocaInst *alloc =
        builder->CreateAlloca(convert_type(stmt.type), nullptr, stmt.ident);
    builder->SetInsertPoint(bb_bak);
    builder->CreateStore(ret_val,
                         alloc); // we always have default value for variable
    vars[stmt.ident].emplace_back(stmt.type, alloc);
    to_pop.insert(stmt.ident);
    ret_val = nullptr;
    ret_type = ast::VOID;
  }

  void visit_ass(ast::AssStmt &stmt)
      override { // todo make sure it calcualtes val before assingment for int
                 // a = 5; {int a = a;}, chekc this is well tested in frontend
    assert(!ret_val);
    assert(ret_type == ast::VOID);
    assert(!jmp_true);
    assert(!jmp_false);
    assert(!jmp_end);
    assert(jmp_end_vals.empty());
    // calculate new val
    stmt.ass_expr->accept(this);
    assert(ret_val);
    auto ident_v = vars.find(stmt.ident);
    assert(ident_v != vars.end() && "Ident not found in values");
    assert(!ident_v->second.empty() &&
           "ident found in values but values are empty");
    assert(ret_type != ident_v->second.back().type);
    // store new value
    builder->CreateStore(ret_val, ident_v->second.back().ptr);
    ret_val = nullptr;
    ret_type = ast::VOID;
  }

  void visit_incdec(ast::IncDecStmt &stmt) override { // todo add load and store
    assert(!ret_val);
    assert(ret_type == ast::VOID);
    assert(!jmp_true);
    assert(!jmp_false);
    assert(!jmp_end);
    assert(jmp_end_vals.empty());
    // load prev value
    auto vec = vars.find(stmt.ident);
    assert(vec != vars.end() && "val not found");
    assert(vec->second.size() != 0 && "Val registered but vector is empty");
    assert(vec->second.back().ptr && "Val was declared but is nullptr");

    assert(vec->second.back().type == ast::INT && "can only incdec an INT");
    llvm::AllocaInst *alloc = vec->second.back().ptr;
    llvm::Value *val =
        builder->CreateLoad(alloc->getAllocatedType(), alloc, "to_inc");
    // add or substract one
    int i = stmt.is_inc ? 1 : -1;
    llvm::Value *i_val = llvm::ConstantInt::getSigned(int_type, i);
    llvm::Value *res_val = builder->CreateAdd(val, i_val);
    // store new result
    builder->CreateStore(res_val, alloc);
  }

  void visit_ret(ast::RetStmt &stmt) override {
    assert(!ret_val);
    assert(ret_type == ast::VOID);
    assert(!jmp_true);
    assert(!jmp_false);
    assert(!jmp_end);
    assert(jmp_end_vals.empty());

    if (stmt.ret_expr) {
      stmt.ret_expr->accept(this);
      assert(ret_val);
      builder->CreateRet(ret_val);
    } else {
      builder->CreateRetVoid();
    }
    ret_val = nullptr;
    ret_type = ast::VOID;
  }

  void visit_cond(ast::CondStmt &stmt) override {
    assert(!ret_val);
    assert(ret_type == ast::VOID);
    assert(!jmp_true);
    assert(!jmp_false);
    assert(!jmp_end);
    assert(jmp_end_vals.empty());

    llvm::BasicBlock *jmp_if = llvm::BasicBlock::Create(*context, "jmp_if");
    llvm::BasicBlock *jmp_after =
        llvm::BasicBlock::Create(*context, "jmp_after");
    llvm::BasicBlock *jmp_else = nullptr;
    if (stmt.else_stmt) {
      jmp_else = llvm::BasicBlock::Create(*context, "jmp_else");
    } else {
      jmp_else = jmp_after;
    }
    stmt.cond_expr->accept(this);
    assert(ret_val);
    assert(ret_type != ast::VOID);
    builder->CreateCondBr(ret_val, jmp_if, jmp_else);
    ret_val = nullptr;
    ret_type = ast::VOID;
    // if cond
    llvm::Function *cur_func = builder->GetInsertBlock()->getParent();
    jmp_if->insertInto(cur_func);
    builder->SetInsertPoint(jmp_if);
    std::set<std::string> to_pop_bak(std::move(to_pop));
    to_pop.clear();
    stmt.if_stmt->accept(this);
    builder->CreateBr(jmp_after);
    for (const std::string &id : to_pop) {
      auto cur_pop = vars.find(id);
      assert(cur_pop != vars.end());
      assert(cur_pop->second.size());
      cur_pop->second.pop_back();
      if (cur_pop->second.size() == 0) {
        vars.erase(cur_pop);
      }
    }
    // else cond
    if (stmt.else_stmt) {
      to_pop.clear();
      jmp_else->insertInto(cur_func);
      builder->SetInsertPoint(jmp_else);
      stmt.else_stmt->accept(this);
      builder->CreateBr(jmp_after);
      for (const std::string &id : to_pop) {
        auto cur_pop = vars.find(id);
        assert(cur_pop != vars.end());
        assert(cur_pop->second.size());
        cur_pop->second.pop_back();
        if (cur_pop->second.size() == 0) {
          vars.erase(cur_pop);
        }
      }
    }
    // after if/else
    to_pop = std::move(to_pop_bak);
    jmp_after->insertInto(cur_func);
    builder->SetInsertPoint(jmp_after);
  }

  void visit_while(ast::WhileStmt &stmt)
      override { // do it in the do while structure for conversion to assembly
    assert(!ret_val);
    assert(ret_type == ast::VOID);
    assert(!jmp_true);
    assert(!jmp_false);
    assert(!jmp_end);
    assert(jmp_end_vals.empty());

    llvm::BasicBlock *jmp_cond = llvm::BasicBlock::Create(*context);
    llvm::BasicBlock *jmp_body = llvm::BasicBlock::Create(*context);
    llvm::BasicBlock *jmp_after = llvm::BasicBlock::Create(*context);
    builder->CreateBr(jmp_cond);
    // body
    llvm::Function *cur_func = builder->GetInsertBlock()->getParent();
    jmp_body->insertInto(cur_func);
    builder->SetInsertPoint(jmp_body);
    std::set<std::string> to_pop_bak(std::move(to_pop));
    to_pop.clear();
    stmt.body->accept(this);
    builder->CreateBr(jmp_cond);
    for (const std::string &id : to_pop) {
      auto cur_pop = vars.find(id);
      assert(cur_pop != vars.end());
      assert(cur_pop->second.size());
      cur_pop->second.pop_back();
      if (cur_pop->second.size() == 0) {
        vars.erase(cur_pop);
      }
    }
    to_pop = std::move(to_pop_bak);
    // condition
    jmp_cond->insertInto(cur_func);
    builder->SetInsertPoint(jmp_cond);
    stmt.cond->accept(this);
    assert(ret_val);
    assert(ret_type != ast::VOID);
    builder->CreateCondBr(ret_val, jmp_body, jmp_after);
    ret_val = nullptr;
    ret_type = ast::VOID;
    // after
    jmp_after->insertInto(cur_func);
    builder->SetInsertPoint(jmp_after);
  }

  void visit_expr(ast::ExprStmt &stmt) override {
    assert(!ret_val);
    assert(ret_type == ast::VOID);
    assert(!jmp_true);
    assert(!jmp_false);
    assert(!jmp_end);
    assert(jmp_end_vals.empty());
    stmt.expr->accept(this);
    ret_val = nullptr;
    ret_type = ast::VOID;
  }

  void visit_var(ast::VarExpr &var) override {
    assert(!ret_val);
    assert(ret_type == ast::VOID);
    auto vec = vars.find(var.ident);
    assert(vec != vars.end() && "val not found");
    assert(vec->second.size() != 0 && "Val registered but vector is empty");
    assert(vec->second.back().ptr && "Val was declared but is nullptr");
    llvm::AllocaInst *alloc = vec->second.back().ptr;
    ret_val = builder->CreateLoad(alloc->getAllocatedType(), alloc, "loadvar");
    ret_type = vec->second.back().type;
  }

  void visit_int(ast::LIntExpr &i) override {
    assert(!ret_val);
    assert(ret_type == ast::VOID);
    // todo check canonilization as it's stored as unsigned int
    ret_val = llvm::ConstantInt::getSigned(int_type, i.val);
    ret_type = ast::INT;
  }

  void visit_bool(ast::LBoolExpr &b) override {
    assert(!ret_val);
    assert(ret_type == ast::VOID);
    ret_val = llvm::ConstantInt::getBool(*context, b.val);
    ret_type = ast::STR;
  }

  void visit_string(ast::LStringExpr &s) override {
    assert(!ret_val);
    assert(ret_type == ast::VOID);
    // todo change this to string implementation like others
    ret_val = builder->CreateGlobalString(s.val);
    ret_type = ast::STR;
  }

  void visit_fnapp(ast::FunAppExpr &fn) override {
    assert(!ret_val);
    assert(ret_type == ast::VOID);
    std::vector<llvm::Value *> args;
    for (auto &arg_exp : fn.args) {
      arg_exp->accept(this);
      assert(ret_val);
      args.push_back(ret_val);
      ret_val = nullptr;
    }
    assert(funcs.count(fn.ident) && "Called function doesn't exist");
    auto &func_node = funcs[fn.ident];
    llvm::Function *call_fn = func_node.ptr;
    assert(call_fn->arg_size() == args.size() &&
           "wrong number of args for call");

    ret_val = builder->CreateCall(call_fn, args, "fn call");
    ret_type = func_node.ret_type;
  }

  void visit_neg(ast::NegExpr &neg) override {
    assert(!ret_val);
    assert(ret_type == ast::VOID);
    neg.sub_expr->accept(this);
    assert(ret_val);
    // todo check ret_val not null
    ret_val = builder->CreateNeg(ret_val);
  }
  void visit_not(ast::NotExpr &no) override { // todo swap for short_circut?
    assert(!ret_val);
    std::swap(jmp_true, jmp_false);
    no.sub_expr->accept(this);
    if (ret_val) {
      ret_val = builder->CreateNot(
          ret_val); // todo check this works with short circuit
    }
    std::swap(jmp_true, jmp_false);
  }

  void visit_mul(ast::MulExpr &mul) override {
    assert(!ret_val);
    assert(ret_type == ast::VOID);
    mul.l_sub_expr->accept(this);
    assert(ret_val);
    llvm::Value *l_val = ret_val;
    ret_val = nullptr;
    mul.r_sub_expr->accept(this);
    assert(ret_val);
    llvm::Value *r_val = ret_val;
    ret_val = nullptr;
    switch (mul.op) {
    case ast::TIMES:
      ret_val = builder->CreateMul(l_val, r_val);
      break;
    case ast::DIV:
      ret_val = builder->CreateSDiv(l_val, r_val);
      break;
    case ast::MOD:
      ret_val = builder->CreateSRem(l_val, r_val);
      break;
    default:
      throw std::runtime_error("unkown multiplication op");
    }
  }

  void visit_add(ast::AddExpr &add) override {
    assert(!ret_val);
    assert(ret_type == ast::VOID);
    add.l_sub_expr->accept(this);
    assert(ret_val);
    assert(ret_type != ast::VOID);
    llvm::Value *l_val = ret_val;
    ast::Type l_type = ret_type;
    ret_val = nullptr;
    ret_type = ast::VOID;
    add.r_sub_expr->accept(this);
    assert(ret_val);
    assert(ret_type != ast::VOID);
    llvm::Value *r_val = ret_val;
    ast::Type r_type = ret_type;
    ret_val = nullptr;
    assert(l_type == r_type);
    if(add.op == ast::PLUS) {
      if(l_type == ast::INT) {
        ret_val = builder->CreateAdd(l_val, r_val);
        ret_type = l_type;
      }
      else {
        assert(l_type == ast::STR);
        assert(false && "string addition not implemented");
      }
    }
    else {
      assert(add.op == ast::MINUS && "unkown add operation");
      assert(l_type == ast::INT);
      ret_val = builder->CreateSub(l_val, r_val);
      ret_type = l_type;
    }
  }

  void visit_rel(ast::RelExpr &rel) override {
    assert(!ret_val);
    assert(ret_type == ast::VOID);
    rel.l_sub_expr->accept(this);
    assert(ret_val);
    llvm::Value *l_val = ret_val;
    ret_val = nullptr;
    rel.r_sub_expr->accept(this);
    assert(ret_val);
    llvm::Value *r_val = ret_val;
    ret_val = nullptr;
    switch (rel.op) {
    case ast::LTH:
      ret_val = builder->CreateICmpSLT(l_val, r_val);
      break;
    case ast::LE:
      ret_val = builder->CreateICmpSLE(l_val, r_val);
      break;
    case ast::GTH:
      ret_val = builder->CreateICmpSGT(l_val, r_val);
      break;
    case ast::GE:
      ret_val = builder->CreateICmpSGE(l_val, r_val);
      break;
    case ast::EQU:
      ret_val = builder->CreateICmpEQ(l_val, r_val);
      break;
    case ast::NE:
      ret_val = builder->CreateICmpNE(l_val, r_val);
      break;
    default:
      throw std::runtime_error("unkown rel op");
    }
    ret_type = ast::BOOL;
  }

  void visit_log(ast::LogExpr &log) override {
    assert(!ret_val);
    assert(ret_type == ast::VOID);
    llvm::BasicBlock *jmp_true_bak = nullptr;
    llvm::BasicBlock *jmp_false_bak = nullptr;
    llvm::BasicBlock *jmp_end_bak = nullptr;
    std::vector<std::pair<llvm::Value *, llvm::BasicBlock *>> jmp_end_vals_bak;
    bool is_rhs =
        false; // if we are in the right side of the tree (including root)
    if (!jmp_end) {
      is_rhs = true;
      assert(!jmp_true);
      assert(!jmp_false);
      assert(jmp_end_vals.empty());
      jmp_true = llvm::BasicBlock::Create(*context, "jmp_true");
      jmp_false = llvm::BasicBlock::Create(*context, "jmp_false");
      if (log.op == ast::OR) {
        jmp_end = jmp_true;
      } else { // log.op == ast::AND
        assert(log.op == ast::AND);
        jmp_end = jmp_false;
      }
    } else { // jmp_end
      assert(jmp_true);
      assert(jmp_false);
      if (log.op == ast::OR) {
        jmp_false_bak = llvm::BasicBlock::Create(*context, "jmp_false");
        std::swap(jmp_false, jmp_false_bak);
      } else { // log.op == ast::AND
        assert(log.op == ast::AND);
        jmp_true_bak = llvm::BasicBlock::Create(*context, "jmp_true");
        std::swap(jmp_true, jmp_true_bak);
      }
    }
    assert(jmp_true != jmp_false);

    // LHS
    log.l_sub_expr->accept(this);
    if (ret_val) { // lhs isn't a logical binary statement
      builder->CreateCondBr(ret_val, jmp_true, jmp_false);
      if (jmp_true == jmp_end) {
        assert(log.op == ast::OR);
        jmp_end_vals.emplace_back(llvm::ConstantInt::getTrue(*context),
                                  builder->GetInsertBlock());
      } else if (jmp_false == jmp_end) { // jmp_false = jmp_end
        // assert(jmp_false == jmp_end); //todo check case usch as (a || b) && c
        assert(log.op == ast::AND);
        jmp_end_vals.emplace_back(llvm::ConstantInt::getFalse(*context),
                                  builder->GetInsertBlock());
      }
      ret_val = nullptr;
      ret_type = ast::VOID;
    }

    // RHS
    assert(!ret_val);
    llvm::Function *cur_func = builder->GetInsertBlock()->getParent();
    if (log.op == ast::OR) {
      jmp_false->insertInto(cur_func);
      builder->SetInsertPoint(jmp_false);
    } else { // log.op == ast::AND
      jmp_true->insertInto(cur_func);
      builder->SetInsertPoint(jmp_true);
    }

    if (is_rhs) {
      // all null for level below to do their own thing and return the final
      // value
      std::swap(jmp_true, jmp_true_bak);
      std::swap(jmp_false, jmp_false_bak);
      std::swap(jmp_end, jmp_end_bak);
      std::swap(jmp_end_vals, jmp_end_vals_bak);
      assert(!jmp_true);
      assert(!jmp_false);
      assert(!jmp_end);
      assert(jmp_end_vals.empty());
    } else {
      if (log.op == ast::OR) {
        std::swap(jmp_false, jmp_false_bak);
      } else { // log.op == ast::AND
        std::swap(jmp_true, jmp_true_bak);
      }
    }

    log.r_sub_expr->accept(this);
    assert(!is_rhs || ret_val); // is_rhs implies ret_val
    if (is_rhs && ret_val) {
      assert(!jmp_true);
      assert(!jmp_false);
      assert(!jmp_end);
      assert(jmp_end_vals.empty());
      builder->CreateBr(jmp_end_bak);
      jmp_end_vals_bak.emplace_back(ret_val, builder->GetInsertBlock());
      jmp_end_bak->insertInto(cur_func);
      builder->SetInsertPoint(jmp_end_bak);
      assert(jmp_end_vals_bak.size() > 1);
      llvm::PHINode *phi =
          builder->CreatePHI(bool_type, jmp_end_vals_bak.size());
      for (auto &v_b : jmp_end_vals_bak) {
        phi->addIncoming(v_b.first, v_b.second);
      }
      ret_val = phi;
      ret_type = ast::BOOL;
    } else if (ret_val) {
      builder->CreateCondBr(ret_val, jmp_true, jmp_false);
      if (jmp_true == jmp_end) {
        jmp_end_vals.emplace_back(llvm::ConstantInt::getTrue(*context),
                                  builder->GetInsertBlock());
      } else if (jmp_false == jmp_end) { // jmp_false == jmp_end
        jmp_end_vals.emplace_back(llvm::ConstantInt::getFalse(*context),
                                  builder->GetInsertBlock());
      }
      ret_val = nullptr;
      ret_type = ast::VOID;
    }
  }
};