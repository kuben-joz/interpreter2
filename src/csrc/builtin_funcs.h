#pragma once
#include "skel.h"
#include <vector>
// printInt void 1 int
// printString void 1 string
// error void 0
// readInt int 0
// readString string 0

struct ProtoFunc {
  const std::string name;
  const ast::Type ret_type;
  const std::vector<ast::Type> param_types;

  ProtoFunc(const std::string &name, ast::Type ret_type,
            std::vector<ast::Type> param_types)
      : name(name), ret_type(ret_type), param_types(param_types) {}
};

extern ProtoFunc builtin_func_sigs[5];

// declare i1 @strs_eq(i8* %s1, i8* %s2)
// declare i8* @merge_strs(i8* %s1, i8* %s2)
extern ProtoFunc string_eq_sig;

extern ProtoFunc merge_strings_sig;
