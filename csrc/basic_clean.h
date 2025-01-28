#pragma once
#include "cfg.h"
#include "dom_tree.h"
#include <cassert>
#include <llvm-14/llvm/IR/BasicBlock.h>
#include <llvm-14/llvm/IR/Constant.h>
#include <llvm-14/llvm/IR/Constants.h>
#include <llvm-14/llvm/IR/InstVisitor.h>
#include <llvm-14/llvm/IR/Instruction.h>
#include <llvm-14/llvm/IR/Type.h>
#include <llvm-14/llvm/Support/Casting.h>
#include <utility>

// todo
// strcmp of same global string reduced to true or false for different
// phi node collapse if same phi node psuhed through two different directions

namespace clean {
class Cleaner : public llvm::InstVisitor<Cleaner> {
public:
  bool change_glob = false;
  bool change_structure = false;
  bool is_past_ret = false;
  CFG &cfg;
  DomTree &dom;
  std::vector<bool> removed;
  std::vector<llvm::Instruction *> inst_to_del;

  Cleaner(CFG &cfg, DomTree &dom)
      : cfg(cfg), dom(dom), removed(dom.blk_to_idx.size(), false) {}



  void join_succ(int orig_idx) {
    int num_succ = 0;
    int succ_idx = -1;
    for(int succ : dom.cfg_succs[orig_idx]) {
      if(removed[succ]) {
        continue;
      }
      num_succ++;
      succ_idx = succ;
    }
    assert(succ_idx >= 0 || num_succ == 0);
    if(num_succ != 1) {
      return;
    }
    int num_preds = 0;
    for(int pred : dom.cfg_preds[succ_idx]) {
      if(removed[pred]) {
        continue;
      }
    }
    assert(num_preds > 0);
    if(num_preds != 1) {
      return;
    }
    change_glob = true;
    change_structure = true;

  }

  void clean_insts() {
    if(inst_to_del.empty()) {
      return;
    }
    change_glob = true;
    for(auto *inst : inst_to_del) {
      inst->eraseFromParent();
    }
    inst_to_del.clear();
  }

  void remove_phis(int idx_orig, int idx_dest) {
    llvm::BasicBlock *orig_blk = dom.idx_to_blk[idx_orig];
    llvm::BasicBlock *dest_blk = dom.idx_to_blk[idx_dest];
    std::vector<std::pair<llvm::PHINode *, int>> phis;
    int num_vals = -1;
    for (auto &inst_ref : dest_blk->getInstList()) {
      if (auto phi = llvm::dyn_cast<llvm::PHINode>(&inst_ref)) {
        int phi_blk_idx = phi->getBasicBlockIndex(orig_blk);
        num_vals = phi->getNumIncomingValues();
        assert(num_vals > 1 && phi_blk_idx >= 0);
        if (phi_blk_idx < 0 || num_vals <= 1) {
          break;
        }
        phis.emplace_back(phi, phi_blk_idx);
      } else {
        break;
      }
    }
    for (auto &p_i : phis) {
      p_i.first->removeIncomingValue(p_i.second);
    }
  }

  void clean_past_ret_rec(int idx) {
    // preorder
    for (int succ : dom.dom_succ[idx]) {
      removed[succ] = true;
    }
    for (int succ : dom.cfg_succs[idx]) {
      if (!removed[succ]) {
        remove_phis(idx, succ);
      }
    }
    // go down
    for (int succ : dom.dom_succ[idx]) {
      clean_past_ret_rec(succ);
    }
    // postorder
    for (int succ : dom.dom_succ[idx]) {
      if (dom.idx_to_blk[succ]) {
        dom.idx_to_blk[succ]->eraseFromParent();
        dom.idx_to_blk[succ] = nullptr;
      }
    }
  }

  void clean_past_ret(int idx) {
    if(!is_past_ret) {
      return;
    }
    is_past_ret = false;
    if (dom.cfg_succs[idx].empty()) {
      return;
    }
    change_structure = true;
    change_glob = true;
    clean_past_ret_rec(idx);
  }

  void visitInstruction(llvm::Instruction &inst) {
    if (is_past_ret) {
      // todo check this is removed with the traversal instead
      // llvm::BasicBlock *blk = inst.getParent();
      // for (auto *user : inst.users()) {
      //  if (auto *phi = llvm::dyn_cast<llvm::PHINode>(user)) {
      //    int phi_blk_idx = phi->getBasicBlockIndex(blk);
      //    if (phi_blk_idx >= 0) {
      //      phi->removeIncomingValue(phi_blk_idx, true);
      //    }
      //  }
      //}
      inst_to_del.emplace_back(&inst);
      return;
    }
  }

  void visitBinaryOperator(llvm::BinaryOperator &mul) {
    if (is_past_ret) {
      inst_to_del.emplace_back(&mul);
      return;
    }
    assert(mul.getNumOperands() == 2);
    if (mul.getNumOperands() != 2) {
      return;
    }
    llvm::Value *l_v = mul.getOperand(0);
    llvm::Value *r_v = mul.getOperand(1);
    llvm::ConstantInt *l_const = llvm::dyn_cast<llvm::ConstantInt>(l_v);
    llvm::ConstantInt *r_const = llvm::dyn_cast<llvm::ConstantInt>(r_v);
    if (l_const && r_const) {
      int64_t l = l_const->getSExtValue() % (int64_t)l_const->getBitWidth();
      int64_t r = r_const->getSExtValue() % (int64_t)r_const->getBitWidth();
      bool changed = true;
      int64_t res = 0;
      switch (mul.getOpcode()) {
      case llvm::Instruction::Add:
        res = l + r;
        break;
      case llvm::Instruction::Sub:
        res = l - r;
        break;
      case llvm::Instruction::Mul:
        res = l * r;
        break;
      case llvm::Instruction::SDiv:
        res = l / r;
        break;
      case llvm::Instruction::SRem:
        res = l % r;
        break;
      case llvm::Instruction::And:
        res = l & r;
        break;
      case llvm::Instruction::Or:
        res = l | r;
        break;
      case llvm::Instruction::Xor:
        res = l ^ r;
        break;
      default:
        changed = false;
        break;
      }
      if (changed) {
        change_glob = true;
        res = res % (int64_t)l_const->getBitWidth();
        llvm::Value *new_v = llvm::ConstantInt::getSigned(l_v->getType(), res);
        mul.replaceAllUsesWith(new_v);
        inst_to_del.emplace_back(&mul);
      }
    }
  }

  void visitICmpInst(llvm::ICmpInst &icmp) {
    if (is_past_ret) {
      inst_to_del.emplace_back(&icmp);
      return;
    }
    assert(icmp.getNumOperands() == 2);
    if (icmp.getNumOperands() != 2) {
      return;
    }
    llvm::Value *l_v = icmp.getOperand(0);
    llvm::Value *r_v = icmp.getOperand(1);
    llvm::ConstantInt *l_const = llvm::dyn_cast<llvm::ConstantInt>(l_v);
    llvm::ConstantInt *r_const = llvm::dyn_cast<llvm::ConstantInt>(r_v);
    if (l_const && r_const) {
      int64_t l = l_const->getSExtValue() % (int64_t)l_const->getBitWidth();
      int64_t r = r_const->getSExtValue() % (int64_t)r_const->getBitWidth();
      bool changed = true;
      bool res = 0;
      switch (icmp.getPredicate()) {
      case llvm::CmpInst::ICMP_EQ:
        res = l == r;
        break;
      case llvm::CmpInst::ICMP_NE:
        res = l != r;
        break;
      case llvm::CmpInst::ICMP_SGT:
        res = l > r;
        break;
      case llvm::CmpInst::ICMP_SGE:
        res = l >= r;
        break;
      case llvm::CmpInst::ICMP_SLT:
        res = l < r;
        break;
      case llvm::CmpInst::ICMP_SLE:
        res = l <= r;
        break;
      default:
        changed = false;
        break;
      }
      if (changed) {
        change_glob = true;
        llvm::Value *new_v = llvm::ConstantInt::getBool(
            llvm::Type::getInt1Ty(icmp.getContext()), res);
        icmp.replaceAllUsesWith(new_v);
        inst_to_del.emplace_back(&icmp);
      }
    }
  }

  void visitReturnInst(llvm::ReturnInst &ret) {
    if (is_past_ret) {
      inst_to_del.emplace_back(&ret);
      return;
    }
    is_past_ret = true;
  }

  void visitBranchInst(llvm::BranchInst &br) {
    if (is_past_ret) {
      inst_to_del.emplace_back(br);
      return;
    }
    // if next has only 1 pred then merge
    // if (br.isConditional()) {
    //  // check if condition is known and if so then change to br,
    //  assert(br.getNumSuccessors() == 2);;
    //  br.getCondition();
    //} else {
    //  assert(br.getNumSuccessors() == 1);
    //  strm << "label ";
    //  print_blk_name(br.getSuccessor(0), false);
    //  strm << '\n';
    //}
  }

  // replace with val is only one
  void visitPHINode(llvm::PHINode &phi) {
    assert(!is_past_ret);
    int num_paths = phi.getNumIncomingValues();
    assert(num_paths); // todo check if there is situation where this isn't the
                       // case
    if (num_paths == 0) {
        return;
    }
    if(num_paths == 1) {
      
    }
    for (int i = 0; i < num_paths; i++) {
      phi.getIncomingValue(i);
      phi.getIncomingBlock(i);
    }
  }

  void reset() {
    change_glob = false;
    change_structure = false;
    is_past_ret = false;
    inst_to_del.clear();
  }
};

void transform_rec(int idx, CFG &cfg, DomTree &dom, Cleaner &cleaner) {
  llvm::BasicBlock *cur_block = dom.idx_to_blk[idx];
  for(auto &inst : cur_block->getInstList()) {
    cleaner.visit(inst);
  }
  cleaner.clean_past_ret(idx);
  cleaner.clean_insts();
  for(int succ : dom.dom_succ[idx]) {
    if(cleaner.removed[succ]) {
      continue;
    }
    transform_rec(succ, cfg, dom, cleaner);
  }
  cleaner.join_succ(idx);
  cleaner.clean_insts();
}

std::pair<bool, bool> transform(CFG &cfg, DomTree &dom) {
  Cleaner cleaner(cfg, dom);
  do {
    cleaner.reset();
    transform_rec(0, cfg, dom, cleaner);
  } while (cleaner.change_glob && !cleaner.change_structure);
  return std::make_pair(cleaner.change_glob, cleaner.change_structure);
}
} // namespace clean