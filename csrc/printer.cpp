#include <iostream>
#include <llvm-14/llvm/IR/BasicBlock.h>
#include <llvm-14/llvm/IR/Constant.h>
#include <llvm-14/llvm/IR/Constants.h>
#include <llvm-14/llvm/IR/DerivedTypes.h>
#include <llvm-14/llvm/IR/Function.h>
#include <llvm-14/llvm/IR/InstVisitor.h>
#include <llvm-14/llvm/IR/InstrTypes.h>
#include <llvm-14/llvm/IR/Instruction.h>
#include <llvm-14/llvm/IR/Instructions.h>
#include <llvm-14/llvm/IR/Module.h>
#include <llvm-14/llvm/IR/Type.h>
#include <llvm-14/llvm/IR/Value.h>
#include <llvm-14/llvm/Support/Casting.h>
#include <map>
#include <ostream>
#include <sstream>
#include <string>

namespace printer {

std::string preamble =
    "declare void @printInt(i32)\n\ndeclare void @printString(i8*)\n\ndeclare "
    "void @error()\n\ndeclare i32 @readInt()\n\ndeclare i8* "
    "@readString()\n\ndeclare i1 @strs_eq(i8*, i8*)\n\ndeclare i8* "
    "@merge_strs(i8*, i8*)\n\n";

class LLVMPrinter : public llvm::InstVisitor<LLVMPrinter> {
public:
  std::map<llvm::Function *, std::string> func_to_string;
  std::map<llvm::Value *, std::string> val_to_string;
  std::map<llvm::BasicBlock *, std::string> blk_to_string;
  std::map<llvm::Type *, std::string> type_cache;
  std::map<llvm::Type *, std::string> align_cache;
  std::ostream &strm;

  LLVMPrinter(std::ostream &strm) : strm(strm) {}

private:
  unsigned long long reg_n = 0;
  std::string next_reg_name = "%r0";
  unsigned long long blk_n = 0;
  std::string next_blk_name = "blk_0";
  unsigned long long glob_n = 0;
  std::string next_glob_name = "glob_0";

  void type_rec(llvm::Type *typ, std::stringstream &res) {
    if (auto *ptrtyp = llvm::dyn_cast<llvm::PointerType>(typ)) {
      type_rec(typ->getPointerElementType(), res);
      res << '*';
    } else if (auto *ityp = llvm::dyn_cast<llvm::IntegerType>(typ)) {
      res << "i" << std::to_string(ityp->getBitWidth());
    } else {
      res << "void";
    }
  }

public:
  void print_type(llvm::Type *typ) {
    auto it = type_cache.find(typ);
    if (it != type_cache.end()) {
      strm << it->second;
    } else {
      std::stringstream ress;
      type_rec(typ, ress);
      std::string res(ress.str());
      type_cache[typ] = res;
      strm << res;
    }
  }

  void print_align(llvm::Type *typ) {
    auto it = align_cache.find(typ);
    if(it == align_cache.end()) {
      if(llvm::isa<llvm::PointerType>(typ)) {
        std::string cache_v = ", align 8";
        strm << cache_v;
        align_cache[typ] = cache_v;
      }
      else if(auto *ityp = llvm::dyn_cast<llvm::IntegerType>(typ)) {
        std::string cache_v = ", align " + std::to_string((ityp->getBitWidth()+7) / 8);
        strm << cache_v;
        align_cache[typ] = cache_v;
      }
      else {
        align_cache[typ] = "";
      }
    }
    else {
      strm << it->second;
    }
  }

  void print_val(llvm::Value *val, bool strict) {
    if(auto const_exp = llvm::dyn_cast<llvm::ConstantExpr>(val)) {
      const_exp->getNumOperands();

      return;
    }
    else if(auto const_int = llvm::dyn_cast<llvm::ConstantInt>(val)) {
      strm << const_int->getValue();
    }
    else if(auto const_exp = llvm::dyn_cast<llvm::Constant>(val)) {
      assert(false && "missing implementation for constant");
      return;
    }
    auto it = val_to_string.find(val);
    if (it == val_to_string.end()) {
      assert(!strict && "value unkown, can't print");
      val_to_string[val] = next_reg_name;
      strm << next_reg_name;
      next_reg_name = "%r" + std::to_string(++reg_n);
    } else {
      strm << it->second;
    }
  }

  void print_blk_name(llvm::BasicBlock *blk, bool strict) {
    auto it = blk_to_string.find(blk);
    if (it == blk_to_string.end()) {
      assert(!strict && "can't find block to write");
      blk_to_string[blk] = next_blk_name;
      strm << "%" << next_blk_name;
      next_blk_name = "blk_" + std::to_string(++blk_n);
    } else {
      strm << "%" << it->second;
    }
  }
  // ------ Visitors Start ------------------------------------
  void visitInstruction(llvm::Instruction &inst) {
    std::cerr << blk_to_string[inst.getParent()] << "unhandled here"
              << std::endl;
    // assert(false && "Unhandled instruction");
  }

  void visitAllocaInst(llvm::AllocaInst &alloc) { 
    strm << "  ";
    print_val(&alloc, false);
    strm << " = alloca ";
    llvm::Type *typ = alloc.getAllocatedType();
    print_type(typ);
    print_align(typ);
    strm << '\n';
}

  void visitCallInst(llvm::CallInst &call) { 
    strm << "  ";
    print_val(&call, false);
    strm << " = call ";
    llvm::Function *fn = call.getCalledFunction();
    print_type(fn->getReturnType());
    strm << " @" << func_to_string[fn] << '(';
    int n_params = fn->arg_size();
    int i = 0;
    for(auto &arg_use : call.args()) {
      llvm::Value *arg = arg_use.get();
      print_type(arg_use->getType());
      strm << " ";
      if(auto getelemptr = llvm::dyn_cast<llvm::ConstantExpr>(arg)) {
        strm << "getelemptr";
      }
      else {
        print_val(arg, false);
      }
      if(i < n_params - 1) {
        strm << ", ";
      }
      i++;
    }
    strm << ")\n";
    
   }

  void visitReturnInst(llvm::ReturnInst &ret) { strm << "ret\n"; }

  void visitICmpInst(llvm::ICmpInst &icmp) { strm << "icmp\n"; }

  void visitBranchInst(llvm::BranchInst &br) { strm << "br\n"; }

  void visitBinaryOperator(llvm::BinaryOperator &mul) { strm << "mul\n"; }

  void visitPHINode(llvm::PHINode &phi) { strm << "phi\n"; }

  void visitStoreInst(llvm::StoreInst &store) { strm << "store\n"; }

  void visitLoadInst(llvm::LoadInst &load) { strm << "load\n"; }

  // ------ Visitors End ------------------------------------

  void print_params(llvm::Function *fn) {
    int num_params = fn->arg_size();
    int i = 0;
    for (auto &arg_ref : fn->args()) {
      llvm::Value *arg = &arg_ref;
      print_type(arg->getType());
      strm << " ";
      print_val(arg, false);
      if (i < num_params - 1) {
        strm << ", ";
      }
      i++;
    }
  }

  void print_blk(llvm::BasicBlock *blk) {
    auto it = blk_to_string.find(blk);
    if (it == blk_to_string.end()) {
      blk_to_string[blk] = next_blk_name;
      strm << next_blk_name << ":\n";
      next_blk_name = "blk_" + std::to_string(++blk_n);
    } else {
      strm << it->second << ":\n";
    }
    for (auto &inst_ref : blk->getInstList()) {
      this->visit(inst_ref);
    }
  }

  void print_func(const std::string &name, llvm::Function *fn) {
    reg_n = 0;
    next_reg_name = "%r0";
    blk_n = 0;
    next_blk_name = "blk_0";
    strm << "define internal ";
    print_type(fn->getReturnType());
    strm << " @" << name << '(';
    print_params(fn);
    strm << ") {\n";
    for (auto &blk_ref : fn->getBasicBlockList()) {
      print_blk(&blk_ref);
    }
    strm << "}\n\n";
  }

  void print_func(llvm::Function *fn) { print_func(func_to_string[fn], fn); }
};

void print(llvm::Module *module, std::set<llvm::Function *> extern_funcs) {

  auto &strm = std::cout;
  std::stringstream ss; // todo change to this
  strm << preamble;
  LLVMPrinter printer(strm);
  std::map<llvm::Function *, std::string> &func_to_string =
      printer.func_to_string;
  for (auto &fn_ref : module->functions()) {
    llvm::Function *fn = &fn_ref;
    std::string fn_name(fn->getName());
    bool is_extern = extern_funcs.count(fn);
    if (fn_name != "main" && !is_extern) {
      fn_name += ".1"; // avoid collisions
    }
    func_to_string[fn] = fn_name;
    if (is_extern) {
      continue;
    }
  }
  for (auto &fn_ref : module->functions()) {
    llvm::Function *fn = &fn_ref;
    bool is_extern = extern_funcs.count(fn);
    if (is_extern) {
      continue;
    }
    std::string fn_name(fn->getName());
    printer.print_func(fn_name, fn);
  }
}

} // namespace printer