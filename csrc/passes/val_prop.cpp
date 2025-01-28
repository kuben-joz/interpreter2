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

// todo
// strcmp of same global string reduced to true or false for different
// phi node collapse if same phi node psuhed through two different directions
// maybe cleaned uncalled funcitons without side effects

namespace clean {
class ValProp : public llvm::InstVisitor<ValProp> {
public:
  bool change_glob = false;
  bool change_structure = false;
  CFG &cfg;
  DomTree &dom;
  llvm::Function *str_eq_fn;
  std::vector<llvm::Instruction *> inst_to_del;

  ValProp(CFG &cfg, DomTree &dom, llvm::Function *str_eq_fn)
      : cfg(cfg), dom(dom), str_eq_fn(str_eq_fn) {}

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

  void visitBinaryOperator(llvm::BinaryOperator &mul) {
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

  void visitPHINode(llvm::PHINode &phi) {
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
            if (!vals.empty() &&
                vals.back()->getSExtValue() != const_i->getSExtValue()) {
              break;
            }
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
        assert(!vals.empty());
        if (vals.empty()) {
          return;
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
      }
    }
  }
};

// todo strs_eq_fun ins't absorbed for constant strings
std::pair<bool, bool> val_prop(CFG &cfg, DomTree &dom,
                               llvm::Function *strs_eq_fn) {

  ValProp propagator(cfg, dom, strs_eq_fn);
  for(int idx = 0; idx < dom.idx_to_blk.size(); idx++) {
    llvm::BasicBlock *blk = dom.idx_to_blk[idx];
    for(auto &inst : blk->getInstList()) {
      propagator.visit(inst);
    }
    propagator.clean_insts();
  }
  return std::make_pair(propagator.change_glob, propagator.change_structure);
}
} // namespace clean