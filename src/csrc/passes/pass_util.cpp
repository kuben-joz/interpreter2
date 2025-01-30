#include <llvm-14/llvm/IR/Constants.h>
#include <llvm-14/llvm/IR/DerivedTypes.h>
#include <llvm-14/llvm/IR/Type.h>
#include <llvm-14/llvm/IR/Value.h>
#include <llvm-14/llvm/Support/Casting.h>
#include <map>
#include <utility>

#include "pass_util.h"

// can_compare, true/false
std::pair<bool, bool> StringCMP::cmp_strings(llvm::Value *v1, llvm::Value *v2) {
  if (v1 == v2) {
    return std::make_pair(true, true);
  }
  if (auto *const_exp1 = llvm::dyn_cast<llvm::ConstantExpr>(v1)) {
    auto *const_exp2 = llvm::dyn_cast<llvm::ConstantExpr>(v2);
    if (!const_exp2) {
      return std::make_pair(false, false);
    }
    llvm::Constant *glob1 = const_exp1->getOperand(0);
    llvm::Value *str1 = glob1->getOperand(0);
    llvm::Constant *glob2 = const_exp2->getOperand(0);
    llvm::Value *str2 = glob2->getOperand(0);
    auto it = glob_str_cache.find(std::make_pair(str1, str2));
    if (it != glob_str_cache.end()) {
      return std::make_pair(true, it->second);
    }
    if (auto *str1_v = llvm::dyn_cast<llvm::ConstantDataSequential>(str1)) {
      if (auto *str2_v = llvm::dyn_cast<llvm::ConstantDataSequential>(str2)) {
        std::string s1(str1_v->getAsString());
        std::string s2(str2_v->getAsString());
        glob_str_cache[std::make_pair(str1, str2)] = s1 == s2;
        return std::make_pair(true, s1 == s2);
      } else {
        return std::make_pair(true, false);
      }
    } else if (llvm::isa<llvm::ConstantData>(str1)) {
      return std::make_pair(true,
                            !llvm::isa<llvm::ConstantDataSequential>(str2));
    } else {
      return std::make_pair(false, false);
    }
  }
  return std::make_pair(false, false);
}

bool StringCMP::is_string_type(llvm::Type *typ) {
  if (auto p_typ = llvm::dyn_cast<llvm::PointerType>(typ)) {
    llvm::Type *v_typ = typ->getPointerElementType();
    if (v_typ->isIntegerTy() && v_typ->getIntegerBitWidth() == 8) {
      return true;
    }
  }
  return false;
}

bool StringCMP::is_string(llvm::Value *val) {
  llvm::Type *typ = val->getType();
  return is_string_type(typ);
}

// bit width 0 if not ocnstant
std::pair<int, int64_t> get_const_int(llvm::Value *val) {
  if (auto *int_val = llvm::dyn_cast<llvm::ConstantInt>(val)) {
    return std::make_pair(int_val->getBitWidth(), int_val->getZExtValue());
  } else {
    return std::make_pair(0, 0);
  }
}
