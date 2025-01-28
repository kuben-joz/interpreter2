#include <llvm-14/llvm/IR/Constants.h>
#include <llvm-14/llvm/IR/DerivedTypes.h>
#include <llvm-14/llvm/IR/Value.h>
#include <llvm-14/llvm/Support/Casting.h>
#include <map>
#include <utility>

struct StringCMP {

  std::map<std::pair<llvm::Value *, llvm::Value *>, bool> glob_str_cache;

  // can_compare, true/false
  std::pair<bool, bool> cmp_strings(llvm::Value *v1, llvm::Value *v2) {
    if (v1 == v2) {
      return std::make_pair(true, true);
    }
    if (auto *const_exp1 = llvm::dyn_cast<llvm::ConstantExpr>(v1)) {
      auto *const_exp2 = llvm::dyn_cast<llvm::ConstantExpr>(v2);
      if (!const_exp2) {
        return std::make_pair(false, false);
      }
      llvm::Value *str1 = const_exp1->getOperand(0)->getOperand(0);
      llvm::Value *str2 = const_exp2->getOperand(0)->getOperand(0);
      auto it = glob_str_cache.find(std::make_pair(str1, str2));
      if (it != glob_str_cache.end()) {
        return std::make_pair(true, it->second);
      }
      if (auto *str1_v = llvm::dyn_cast<llvm::ConstantDataSequential>(str1)) {
        auto *str2_v = llvm::dyn_cast<llvm::ConstantDataSequential>(str2);
        if (!str2) {
          return std::make_pair(true, false);
        }
        std::string s1(str1_v->getAsString());
        std::string s2(str2_v->getAsString());
        glob_str_cache[std::make_pair(str1, str2)] = s1 == s2;
        return std::make_pair(true, s1 == s2);
      } else if (llvm::isa<llvm::ConstantData>(str1)) {
        return std::make_pair(true,
                              !llvm::isa<llvm::ConstantDataSequential>(str2));
      } else {
        return std::make_pair(false, false);
      }
    }
    return std::make_pair(false, false);
  }

  bool is_string(llvm::Value *val) {
    llvm::Type *typ = val->getType();
    if (auto p_typ = llvm::dyn_cast<llvm::PointerType>(typ)) {
      llvm::Type *v_typ = typ->getPointerElementType();
      if (v_typ->isIntegerTy() && v_typ->getIntegerBitWidth() == 8) {
        return true;
      }
      return false;
    } else {
      return llvm::isa<llvm::ConstantExpr>(val);
    }
  }
};