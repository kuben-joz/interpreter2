#include "cfg.h"
#include "dom_tree.h"
#include "pass_util.h"
#include <cassert>
#include <iostream>
#include <llvm-14/llvm/IR/BasicBlock.h>
#include <llvm-14/llvm/IR/Constant.h>
#include <llvm-14/llvm/IR/Constants.h>
#include <llvm-14/llvm/IR/Function.h>
#include <llvm-14/llvm/IR/IRBuilder.h>
#include <llvm-14/llvm/IR/InstVisitor.h>
#include <llvm-14/llvm/IR/Instruction.h>
#include <llvm-14/llvm/IR/Instructions.h>
#include <llvm-14/llvm/IR/Operator.h>
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
  StringCMP &str_cmp;
  llvm::Function *str_eq_fn;
  llvm::IRBuilder<> *builder;
  std::vector<llvm::Instruction *> inst_to_del;

  ValProp(CFG &cfg, DomTree &dom, llvm::Function *str_eq_fn, StringCMP &str_cmp,
          llvm::IRBuilder<> *builder)
      : cfg(cfg), dom(dom), str_eq_fn(str_eq_fn), str_cmp(str_cmp),
        builder(builder) {}

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

  void visitCallInst(llvm::CallInst &call) {
    if (call.getCalledFunction() == str_eq_fn) {
      std::pair<bool, bool> res =
          str_cmp.cmp_strings(call.getArgOperand(0), call.getArgOperand(1));
      if (res.first) {
        inst_to_del.emplace_back(&call);
        llvm::Value *new_v =
            llvm::ConstantInt::getBool(call.getType(), res.second);
        call.replaceAllUsesWith(new_v);
        change_glob = true;
      }
    }
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
        if (r == 0) {
#ifndef NDEBUG
          std::cerr << "division by zero detected in constant prop\n";
#endif
          changed = false;
          break;
        }
        res = l / r;
        break;
      case llvm::Instruction::SRem:
        if (r == 0) {
#ifndef NDEBUG
          std::cerr << "modulo zero detected in constant prop\n";
#endif
          changed = false;
          break;
        }
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
        llvm::Value *new_v = llvm::ConstantInt::getSigned(mul.getType(), res);
        mul.replaceAllUsesWith(new_v);
        inst_to_del.emplace_back(&mul);
      }
    } else if (l_const || r_const) {
      int32_t l = -2;
      int32_t r = -2;
      bool is_l = false;
      bool is_r = false;
      if (l_const) {
        is_l = true;
        l = l_const->getSExtValue();
      }
      if (r_const) {
        is_r = true;
        r = r_const->getSExtValue();
      }
      bool is_const = false;
      int32_t res = 0;
      llvm::Value *res_ptr = nullptr;
      switch (mul.getOpcode()) {
      case llvm::Instruction::Add:
        if (is_r && r == 0) {
          res_ptr = l_v;
        } else if (is_l && l == 0) {
          res_ptr = r_v;
        }
        break;
      case llvm::Instruction::Sub:
        if (is_r && r == 0) {
          res_ptr = l_v;
        }
        break;
      case llvm::Instruction::Mul:
        if (is_r) {
          if (r == 0) {
            is_const = true;
            res = 0;
          } else if (r == 1) {
            res_ptr = l_v;
          } else if (r == -1) {
            builder->SetInsertPoint(&mul);
            res_ptr = builder->CreateSub(
                llvm::ConstantInt::getSigned(mul.getType(), 0), l_v);
          }
        } else if (is_l) {
          if (l == 0) {
            is_const = true;
            res = 0;
          } else if (l == 1) {
            res_ptr = r_v;
          } else if (l == -1) {
            builder->SetInsertPoint(&mul);
            res_ptr = builder->CreateSub(
                llvm::ConstantInt::getSigned(mul.getType(), 0), r_v);
          }
        }
        break;
      case llvm::Instruction::SDiv:
        if (is_r) {
          if (r == 0) {
#ifndef NDEBUG
            std::cerr << "division by zero detected in constant prop\n";
#endif
            is_const = false;
            break;
          } else if (r == 1) {
            res_ptr = l_v;
          }
        }
        break;
      case llvm::Instruction::SRem:
        if (is_r) {
          if (r == 0) {
#ifndef NDEBUG
            std::cerr << "modulo zero detected in constant prop\n";
#endif
            is_const = false;
            break;
          } else if (r == 1) {
            is_const = true;
            res = 0;
          }
        }
        break;
      case llvm::Instruction::And:
        break;
      case llvm::Instruction::Or:
        break;
      case llvm::Instruction::Xor:
        break;
      default:
        is_const = false;
        break;
      }
      if (is_const) {
        change_glob = true;
        llvm::Value *new_v = llvm::ConstantInt::getSigned(mul.getType(), res);
        mul.replaceAllUsesWith(new_v);
        inst_to_del.emplace_back(&mul);
      } else if (res_ptr) {
        change_glob = true;
        mul.replaceAllUsesWith(res_ptr);
        inst_to_del.emplace_back(&mul);
      }
    } else {
      bool changed = false;
      int32_t res = 0;
      llvm::Value *res_ptr = nullptr;
      switch (mul.getOpcode()) {
      case llvm::Instruction::Add:
        break;
      case llvm::Instruction::Sub:
        if (l_v == r_v) {
          changed = true;
          res = 0;
        }
        break;
      case llvm::Instruction::Mul:
        break;
      case llvm::Instruction::SDiv:
        break;
      case llvm::Instruction::SRem:
        break;
      case llvm::Instruction::And:
        if (l_v == r_v) {
          res_ptr = l_v;
        }
        break;
      case llvm::Instruction::Or:
        if (l_v == r_v) {
          res_ptr = l_v;
        }
        break;
      case llvm::Instruction::Xor:
        if (l_v == r_v) {
          changed = true;
          res = 0;
        }
        break;
      default:
        changed = false;
        break;
      }
      if (changed) {
        change_glob = true;
        llvm::Value *new_v = llvm::ConstantInt::getSigned(mul.getType(), res);
        mul.replaceAllUsesWith(new_v);
        inst_to_del.emplace_back(&mul);
      } else if (res_ptr) {
        change_glob = true;
        mul.replaceAllUsesWith(res_ptr);
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
        llvm::Value *new_v = llvm::ConstantInt::getBool(icmp.getType(), res);
        icmp.replaceAllUsesWith(new_v);
        inst_to_del.emplace_back(&icmp);
      }
    } else {
      bool changed = false;
      bool res = 0;
      switch (icmp.getPredicate()) {
      case llvm::CmpInst::ICMP_EQ:
        if (l_v == r_v) {
          res = 1;
          changed = true;
        }
        break;
      case llvm::CmpInst::ICMP_NE:
        if (l_v == r_v) {
          res = 0;
          changed = true;
        }
        break;
      case llvm::CmpInst::ICMP_SGT:
        if (l_v == r_v) {
          res = 0;
          changed = true;
        }
        break;
      case llvm::CmpInst::ICMP_SGE:
        break;
      case llvm::CmpInst::ICMP_SLT:
        if (l_v == r_v) {
          res = 0;
          changed = true;
        }
        break;
      case llvm::CmpInst::ICMP_SLE:
        break;
      default:
        changed = false;
        break;
      }
      if (changed) {
        change_glob = true;
        llvm::Value *new_v = llvm::ConstantInt::getBool(icmp.getType(), res);
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
      llvm::Value *new_v = phi.getIncomingValue(0);
      assert(new_v != &phi);
      if (new_v == &phi) {
        return;
      }
      assert(phi.hasConstantValue());
      phi.replaceAllUsesWith(new_v);
      inst_to_del.emplace_back(&phi);
      change_glob = true;
    } else {
      if (phi.getType()->isIntegerTy()) {
        // same vals have same pointer
        llvm::Value *prev = nullptr;
        for (auto &use : phi.incoming_values()) {
          llvm::Value *val = use.get();
          if (val == &phi) {
            continue;
          }
          if(!prev) {
            prev = val;
          }
          else if(prev != val) {
            return;
          }
        }
        assert(prev);
        assert(phi.hasConstantValue());
        inst_to_del.emplace_back(&phi);
        phi.replaceAllUsesWith(prev);
        change_glob = true;
      } 
      //else if (str_cmp.is_string_type(phi.getType())) {
      //  std::vector<llvm::Value *> vals;
      //  for (auto &v : phi.incoming_values()) {
      //    if (auto *phi2 = llvm::dyn_cast<llvm::PHINode>(&v)) {
      //      if (&phi == phi2) {
      //        continue;
      //      } else {
      //        return; // todo maybe add deeper
      //      }
      //    } else {
      //      assert(str_cmp.is_string(v));
      //      if (!str_cmp.is_string(v)) {
      //        return;
      //      }
      //      vals.emplace_back(v);
      //    }
      //  }
      //  assert(!vals.empty());
      //  if (vals.empty()) {
      //    return;
      //  }
      //  llvm::Value *new_v = vals.front();
      //  inst_to_del.emplace_back(&phi);
      //  phi.replaceAllUsesWith(new_v);
      //  change_glob = true;
      //}
    }
  };

  // todo strs_eq_fun ins't absorbed for constant strings
};
std::pair<bool, bool> val_prop(CFG &cfg, DomTree &dom,
                               llvm::Function *strs_eq_fn, StringCMP &str_cmp,
                               llvm::IRBuilder<> *builder) {

  ValProp propagator(cfg, dom, strs_eq_fn, str_cmp, builder);
  for (int idx = 0; idx < dom.idx_to_blk.size(); idx++) {
    llvm::BasicBlock *blk = dom.idx_to_blk[idx];
    for (auto &inst : blk->getInstList()) {
      propagator.visit(inst);
    }
    propagator.clean_insts();
  }
  return std::make_pair(propagator.change_glob, propagator.change_structure);
}
} // namespace clean