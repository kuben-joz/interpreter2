#include <iostream>
#include <llvm-14/llvm/IR/Function.h>
#include <llvm-14/llvm/IR/InstVisitor.h>
#include <llvm-14/llvm/IR/Module.h>
#include <llvm-14/llvm/IR/Value.h>
#include <map>


namespace printer {


std::string preamble = "declare void @printInt(i32)\n\ndeclare void @printString(i8*)\n\ndeclare void @error()\n\ndeclare i32 @readInt()\n\ndeclare i8* @readString()\n\ndeclare i1 @strs_eq(i8*, i8*)\n\ndeclare i8* @merge_strs(i8* %0, i8* %1)\n\n";

class LLVMPrinter : llvm::InstVisitor<LLVMPrinter> {
  public:
    std::map<llvm::Value*, std::string> vals;
    unsigned long long val_n = 0;
    unsigned long long block_n = 0;


};

std::map<llvm::Function *, std::string> func_to_string;

void print(llvm::Module* module, std::set<llvm::Function*> extern_funcs) {
  std::cout << preamble;
  for(auto &fn_ref : module->functions()) {
    llvm::Function *fn = &fn_ref;
    std::string fn_name(fn->getName());
    if(fn_name != "main" && !extern_funcs.count(fn)) {
      fn_name += ".1"; //avoid collisions
    }
    func_to_string[fn] = fn_name;
    
  }
}




}