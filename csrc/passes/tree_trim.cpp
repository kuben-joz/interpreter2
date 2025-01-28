#pragma once
#include "cfg.h"
#include "dom_tree.h"
#include <cassert>
#include <llvm-14/llvm/IR/BasicBlock.h>
#include <llvm-14/llvm/IR/Constant.h>
#include <llvm-14/llvm/IR/Constants.h>
#include <llvm-14/llvm/IR/Function.h>
#include <llvm-14/llvm/IR/InstVisitor.h>
#include <llvm-14/llvm/IR/Instruction.h>
#include <llvm-14/llvm/IR/Instructions.h>
#include <llvm-14/llvm/IR/Type.h>
#include <llvm-14/llvm/IR/Value.h>
#include <llvm-14/llvm/Support/Casting.h>
#include <utility>

namespace clean {
class TreeTrimmer : public llvm::InstVisitor<TreeTrimmer> {
public:
  bool change_glob = false;
  bool change_structure = false;
  bool is_past_ret = false;
  CFG &cfg;
  DomTree &dom;
  std::vector<bool> removed;
  std::vector<llvm::Instruction *> inst_to_del;

  TreeTrimmer(CFG &cfg, DomTree &dom)
      : cfg(cfg), dom(dom), removed(dom.blk_to_idx.size(), false) {}

  void join_succ_single(int orig_idx, int succ_idx) {
    int num_preds = 0;
    for (int pred : dom.cfg_preds[succ_idx]) {
      if (!removed[pred]) {
        num_preds++;
      }
    }
    assert(num_preds > 0);
    if (num_preds != 1) {
      return;
    }
    change_glob = true;
    change_structure = true;
    llvm::BasicBlock *orig = dom.idx_to_blk[orig_idx];
    assert(orig);
    llvm::BasicBlock *join = dom.idx_to_blk[succ_idx];
    assert(join);
    assert(!orig->empty());
    assert(!join->empty());
    assert(llvm::isa<llvm::BranchInst>(orig->back()));
    inst_to_del.emplace_back(&orig->back());
    auto it = join->begin();
    auto end = join->end();
    while (it != end) {
      if (auto *phi = llvm::dyn_cast<llvm::PHINode>(it)) {
        assert(phi->getNumIncomingValues() == 1);
        llvm::Value *v = phi->getIncomingValue(0);
        phi->replaceAllUsesWith(v);
        inst_to_del.emplace_back(phi);
      } else {
        break;
      }
      it++;
    }
    std::vector<llvm::Instruction *> to_move;
    while (it != end) {
      to_move.emplace_back(&it);
      it++;
    }
    for (auto *instr : to_move) {
      instr->removeFromParent();
      instr->insertAfter(&orig->back());
    }
    for (int succ_succ : dom.cfg_succs[succ_idx]) {
      if (!removed[succ_succ]) {
        llvm::BasicBlock *cur_succ = dom.idx_to_blk[succ_succ];
        for (auto &inst : cur_succ->getInstList()) {
          if (auto *phi = llvm::dyn_cast<llvm::PHINode>(&inst)) {
            int blk_idx = phi->getBasicBlockIndex(join);
            assert(blk_idx >= 0);
            phi->setIncomingBlock(blk_idx, orig);
          } else {
            break;
          }
        }
      }
    }
    removed[succ_idx] = true;
    dom.blk_to_idx.erase(join);
    join->eraseFromParent();
    dom.idx_to_blk[succ_idx] = nullptr;
  }

  void join_succ_double(int orig_idx) {
    assert(!removed[orig_idx]);
    llvm::BasicBlock *orig = dom.idx_to_blk[orig_idx];
    assert(orig);

    std::vector<int> succlst;
    for (int succ : dom.cfg_succs[orig_idx]) {
      if (!removed[succ]) {
        succlst.emplace_back(succ);
      }
    }
    assert(succlst.size() == 2);

    // check all have one predecessor and one successor
    for (int succ : succlst) {
      int num_preds = 0;
      for (int pred : dom.cfg_preds[succ]) {
        if (!removed[pred]) {
          num_preds++;
        }
      }
      assert(num_preds > 0);
      if (num_preds > 1) {
        return;
      }
      // todo maybe add to merge if they return the same thing or if all phi
      // nodes agree
    }

    // check both have one branch
    std::vector<llvm::BasicBlock *> succ_succs;
    for (int succ : succlst) {
      assert(dom.idx_to_blk[succ]);
      llvm::BasicBlock *succ_blk = dom.idx_to_blk[succ];
      assert(!succ_blk->empty());
      if (auto *br = llvm::dyn_cast<llvm::BranchInst>(&succ_blk->front())) {
        if (br->isConditional()) {
          return;
        } else {
          llvm::BasicBlock *succ_succ = br->getSuccessor(0);
          assert(dom.blk_to_idx.count(succ_succ));
          assert(!succ_succ->empty());
          succ_succs.emplace_back(succ_succ);
        }
      } else {
        return;
      }
    }
    assert(succ_succs.size() == 2);
    // don't get rid of phi node info
    llvm::BranchInst *orig_br = llvm::dyn_cast<llvm::BranchInst>(&orig->back());
    assert(orig_br);
    assert(orig_br->isConditional());
    if (succ_succs[0] == succ_succs[1]) {
      if (llvm::isa<llvm::PHINode>(succ_succs[0]->front())) {
        return;
      } else {
        inst_to_del.emplace_back(orig_br);
        llvm::BranchInst::Create(succ_succs[0], orig);
      }
    } else { // jump to different places
      for (int i = 0; i < 2; i++) {
        llvm::BasicBlock *succ = dom.idx_to_blk[succlst[i]];
        llvm::BasicBlock *succ_succ = succ_succs[i];
        for (auto &inst : succ_succ->getInstList()) {
          if (auto *phi = llvm::dyn_cast<llvm::PHINode>(&inst)) {
          }
        }
      }
    }
  }

  // if we are the only successor and we don't have any other successors we join
  // his isntructions into ours
  void join_succ(int orig_idx) {
    int num_succ = 0;
    int succ_idx = -1;
    for (int succ : dom.cfg_succs[orig_idx]) {
      if (!removed[succ]) {
        num_succ++;
        succ_idx = succ;
      }
    }
    assert(succ_idx >= 0 || num_succ == 0);
    if (num_succ == 1) {
      join_succ_single(orig_idx, succ_idx);
    } else {
      join_succ_double(orig_idx);
    }
  }

  void clean_insts() {
    if (inst_to_del.empty()) {
      return;
    }
    change_glob = true;
    for (auto *inst : inst_to_del) {
      inst->eraseFromParent();
    }
    inst_to_del.clear();
  }

  void remove_phis(int idx_orig, int idx_dest) {
    llvm::BasicBlock *orig_blk = dom.idx_to_blk[idx_orig];
    llvm::BasicBlock *dest_blk = dom.idx_to_blk[idx_dest];
    std::vector<std::pair<llvm::PHINode *, int>> phis;
    for (auto &inst_ref : dest_blk->getInstList()) {
      if (auto phi = llvm::dyn_cast<llvm::PHINode>(&inst_ref)) {
        int phi_blk_idx = phi->getBasicBlockIndex(orig_blk);
        int num_vals = phi->getNumIncomingValues();
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

  void clean_hanging_dom_rec(int idx) {
    // preorder
    for (int succ : dom.dom_succs[idx]) {
      removed[succ] = true;
    }
    for (int succ : dom.cfg_succs[idx]) {
      if (!removed[succ]) {
        remove_phis(idx, succ);
      }
    }
    // go down
    for (int succ : dom.dom_succs[idx]) {
      clean_hanging_dom_rec(succ);
    }
    // postorder
    for (int succ : dom.dom_succs[idx]) {
      if (dom.idx_to_blk[succ]) {
        dom.blk_to_idx.erase(dom.idx_to_blk[succ]);
        dom.idx_to_blk[succ]->eraseFromParent();
        dom.idx_to_blk[succ] = nullptr;
      }
    }
  }

  void clean_hanging_dom(int idx) {
    if (!is_past_ret) {
      return;
    }
    is_past_ret = false;
    if (dom.cfg_succs[idx].empty()) {
      return;
    }
    change_structure = true;
    change_glob = true;
    clean_hanging_dom_rec(idx);
  }

  void visitInstruction(llvm::Instruction &inst) {
    if (is_past_ret) {
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
      int32_t l = l_const->getSExtValue();
      int32_t r = r_const->getSExtValue();
      bool changed = true;
      int32_t res = 0;
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
      int32_t l = l_const->getSExtValue();
      int32_t r = r_const->getSExtValue();
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
    if (br.isConditional()) {
      llvm::Value *cond = br.getCondition();
      if (auto const_i = llvm::dyn_cast<llvm::ConstantInt>(cond)) {
        assert(const_i->getBitWidth() == 1);
        int64_t cond_v = const_i->getZExtValue();
        llvm::BasicBlock *keep = br.getSuccessor(0);
        llvm::BasicBlock *skip = br.getSuccessor(1);
        llvm::BasicBlock *cur = br.getParent();
        if (!cond_v) {
          std::swap(keep, skip);
        }
        assert(dom.blk_to_idx.count(cur));
        int cur_idx = dom.blk_to_idx[cur];
        assert(dom.blk_to_idx.count(skip));
        int skip_idx = dom.blk_to_idx[skip];
        assert(dom.blk_to_idx.count(keep));
        int keep_idx = dom.blk_to_idx[keep];
        change_glob = true;
        change_structure = true;
        for (int succ : dom.dom_succs[cur_idx]) {
          if (succ == skip_idx) {
            assert(!removed[skip_idx]);
            removed[skip_idx] = true;
            clean_hanging_dom_rec(skip_idx);
            dom.blk_to_idx.erase(dom.idx_to_blk[skip_idx]);
            skip->removeFromParent();
            dom.idx_to_blk[skip_idx] = nullptr;
            break;
          }
        }
        if (!removed[skip_idx]) {
          remove_phis(cur_idx, skip_idx);
        }
        // swap to single br
        inst_to_del.emplace_back(&br);
        llvm::BranchInst::Create(keep, cur);
      }
    } else { // br to one
      return;
    }
  }

  // todo if all values are the same also for global strings
  // todo constant OR all using itself!!!!!!!!!!!!!!!!!!!
  // check all local string or all alloced
  // in one check pointers in other check value
  // split it out with isptrtype, then for strings if all are global
  void visitPHINode(llvm::PHINode &phi) {
    assert(!is_past_ret);
    int num_paths = phi.getNumIncomingValues();
    assert(num_paths); // todo check if there is situation where this isn't the
                       // case
    if (num_paths == 0) {
      return;
    }
    if (num_paths == 1) {
      if (auto const_i =
              llvm::dyn_cast<llvm::ConstantInt>(phi.getIncomingValue(0))) {
        int32_t val = const_i->getSExtValue();
        change_glob = true;
        llvm::Value *new_v =
            llvm::ConstantInt::getSigned(const_i->getType(), val);
        phi.replaceAllUsesWith(new_v);
        inst_to_del.emplace_back(phi);
        return;
      } else if (auto *const_exp = llvm::dyn_cast<llvm::ConstantExpr>(
                     phi.getIncomingValue(0))) {
        phi.replaceAllUsesWith(const_exp);
        inst_to_del.emplace_back(phi);
      }
    } else {
      if (llvm::isa<llvm::ConstantInt>(phi.getIncomingValue(0))) {
        std::vector<llvm::ConstantInt *> vals;
        for (auto &v : phi.incoming_values()) {
          if (auto const_i = llvm::dyn_cast<llvm::ConstantInt>(&v)) {
            vals.emplace_back(const_i);
          } else if (auto *phi2 = llvm::dyn_cast<llvm::PHINode>(&v)) {
            if (&phi == phi2) {
              continue;
            } else {
              return; // todo maybe add deeper
            }
          } else {
            return;
          }
        }
        int32_t val = vals.front()->getSExtValue();
        for (auto v : vals) {
          if (val != (int32_t)v->getSExtValue()) {
            return;
          }
        }
        change_glob = true;
        llvm::Value *new_v =
            llvm::ConstantInt::getSigned(vals.front()->getType(), val);
        phi.replaceAllUsesWith(new_v);
        inst_to_del.emplace_back(phi);
        return;
      } else if (llvm::isa<llvm::ConstantExpr>(phi.getIncomingValue(0))) {
        std::vector<llvm::ConstantInt *> vals;
        for (auto &v : phi.incoming_values()) {
          if (auto const_i = llvm::dyn_cast<llvm::ConstantInt>(&v)) {
            vals.emplace_back(const_i);
          } else {
            return;
          }
        }
      }
    }
  }

};


// todo strs_eq_fun ins't absorbed for constant strings
std::pair<bool, bool> transform(CFG &cfg, DomTree &dom,
                                llvm::Function *strs_eq_fn) {

  TreeTrimmer cleaner(cfg, dom);

}
} // namespace clean