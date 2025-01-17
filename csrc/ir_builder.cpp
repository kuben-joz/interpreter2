#include "skel.h"
#include "visitor.h"
#include "llvm/ADT/APSInt.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Type.h"

#include <cassert>
#include <cstddef>
#include <llvm-14/llvm/IR/BasicBlock.h>
#include <llvm-14/llvm/IR/Instructions.h>
#include <llvm-14/llvm/IR/Value.h>
#include <map>
#include <memory>
#include <stdexcept>

class IRGen : public Visitor {
private:
  std::unique_ptr<llvm::LLVMContext> context;
  std::unique_ptr<llvm::IRBuilder<>> builder;
  std::unique_ptr<llvm::Module> module;
  std::map<std::string, std::vector<llvm::Value *>> vals; // named values
  std::set<std::string>
      to_pop; // variables in current block that should be popped out

  llvm::Value *ret_val = nullptr;

  llvm::BasicBlock *true_jmp = nullptr;
  llvm::BasicBlock *false_jmp = nullptr;

  // todo true jump and false jump

public:
  void visit_prog(ast::Program &prog) override {
    //todo init module, context, etc.
  }

  void visit_fndef(ast::FnDef &fn) override {
    //todo params

  }

  void visit_blk(ast::BStmt &stmt) override {
    std::set<std::string> to_pop_bak(std::move(to_pop));
    to_pop.clear();
    for(auto &sub_stmt : stmt.stmts) {
      sub_stmt->accept(this);
    }
    for(const std::string &id : to_pop) {
      auto cur_pop = vals.find(id);
      assert(cur_pop->second.size());
      cur_pop->second.pop_back();
      if(cur_pop->second.size() == 0) {
        vals.erase(cur_pop);
      }
    }
    to_pop = std::move(to_pop_bak);
  }
  void visit_decl(ast::DeclStmt &stmt) override {

  }
  void visit_ass(ast::AssStmt &stmt)
      override { // todo make sure it calcualtes val before assingment for int a
                 // = 5; {int a = a;}, chekc this is well tested in frontend
  }
  void visit_incdec(ast::IncDecStmt &stmt) override {}
  void visit_ret(ast::RetStmt &stmt) override {}
  void visit_cond(ast::CondStmt &stmt) override {}
  void visit_while(ast::WhileStmt &stmt) override {}
  void visit_expr(ast::ExprStmt &stmt) override {}

  void visit_var(ast::VarExpr &var) override {
    auto vec = vals.find(var.ident);
    if (vec == vals.end()) {
      throw std::runtime_error("Val not found");
    }
    if (vec->second.size() == 0) {
      throw std::runtime_error("Val registered but vector is empty");
    }
    ret_val = vec->second.back();
  }
  void visit_int(ast::LIntExpr &i) override {
    ret_val = llvm::ConstantInt::get(*context, llvm::APSInt(i.val));
  }
  void visit_bool(ast::LBoolExpr &b) override {
    ret_val = llvm::ConstantInt::get(*context, llvm::APSInt(b.val));
  }
  void visit_string(ast::LStringExpr &s) override {
    ret_val = builder->CreateGlobalString(s.val);
  }
  void visit_fnapp(ast::FunAppExpr &fn) override {}
  void visit_neg(ast::NegExpr &neg) override {
    assert(!ret_val);
    neg.sub_expr->accept(this);
    assert(ret_val);
    // todo check ret_val not null
    ret_val = builder->CreateNeg(ret_val);
  }
  void visit_not(ast::NotExpr &no) override { // todo swap for short_circut?
    assert(!ret_val);

    std::swap(true_jmp, false_jmp);
    no.sub_expr->accept(this);
    assert(ret_val);
    ret_val = builder->CreateNot(ret_val);
  }
  void visit_mul(ast::MulExpr &mul) override {
    assert(!ret_val);
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
    add.l_sub_expr->accept(this);
    assert(ret_val);
    llvm::Value *l_val = ret_val;
    ret_val = nullptr;
    add.r_sub_expr->accept(this);
    assert(ret_val);
    llvm::Value *r_val = ret_val;
    ret_val = nullptr;
    switch (add.op) {
    case ast::PLUS:
      ret_val = builder->CreateAdd(l_val, r_val);
      break;
    case ast::MINUS:
      ret_val = builder->CreateSub(l_val, r_val);
      break;
    default:
      throw std::runtime_error("unkown add op");
    }
  }
  void visit_rel(ast::RelExpr &rel) override {
    assert(!ret_val);
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
  }
  void visit_log(ast::LogExpr &log)
      override { // todo add backpatching, even for assingment
    if (log.op == ast::OR) {
      llvm::BasicBlock *true_jmp_bak = true_jmp;
      llvm::BasicBlock *false_jmp_bak = false_jmp;
      if (!true_jmp) {
        true_jmp = llvm::BasicBlock::Create(*context, "btrue");
      }
      if (!false_jmp) {
        false_jmp = llvm::BasicBlock::Create(*context, "bfalse");
      }
      llvm::BasicBlock *cur_block = builder->GetInsertBlock();
      llvm::Function *cur_func = cur_block->getParent();
      assert(!ret_val);
      log.l_sub_expr->accept(this);
      assert(ret_val);
      llvm::Value *l_val = ret_val;
      ret_val = nullptr;
      llvm::PHINode *phi =
          builder->CreatePHI(Type * Ty, unsigned int NumReservedValues);
    }

    // fisnih phi here
  }

  // void visit_log(ast::LogExpr &log)
  //     override { // todo add backpatching, even for assingment
  //   assert(!ret_val);
  //   log.l_sub_expr->accept(this);
  //   assert(ret_val);
  //   llvm::Value *l_val = ret_val;
  //   ret_val = nullptr;
  //   llvm::Function *cur_func = builder->GetInsertBlock()->getParent();
  //   assert(cur_func);
  //   llvm::BasicBlock *cont_blk = llvm::BasicBlock::Create(*context, "cont");
  //   llvm::BasicBlock *short_crc_blk =
  //       llvm::BasicBlock::Create(*context, "short");
  //   //cur_func->insert(cur_func->end(), cont_blk); // this only works on
  //   newer llvms I think cont_blk->insertInto(cur_func,nullptr); // insert
  //   block at the end builder->SetInsertPoint(cont_blk);
  //   log.r_sub_expr->accept(this);
  //   assert(ret_val);
  //   llvm::Value *r_val = ret_val;
  //   ret_val = nullptr;
  //   // we can return phi as value
  //
  //  log.r_sub_expr->accept(this);
  //  assert(ret_val);
  //  llvm::Value *r_val = ret_val;
  //  ret_val = nullptr;
  //  switch (log.op) {
  //    // todo
  //  }
  //}
};