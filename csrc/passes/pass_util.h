#pragma once

#include <llvm-14/llvm/IR/Constants.h>
#include <llvm-14/llvm/IR/Value.h>
#include <map>

struct StringCMP {

  std::map<std::pair<llvm::Value *, llvm::Value *>, bool> glob_str_cache;

  // can_compare, true/false
  std::pair<bool, bool> cmp_strings(llvm::Value *v1, llvm::Value *v2);
  bool is_string_type(llvm::Type *typ);
  bool is_string(llvm::Value *v1);
};
