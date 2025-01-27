#pragma once
#include <llvm-14/llvm/IR/InstVisitor.h>
#include <map>

namespace output {

class LLVMPrinter : llvm::InstVisitor<LLVMPrinter> {
  public:
    std::map<llvm::Value*, std::string> vals;
    unsigned long long val_n = 0;
    unsigned long long block_n = 0;


};



}