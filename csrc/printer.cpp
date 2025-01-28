#include <cstdint>
#include <iostream>
#include <llvm-14/llvm/ADT/StringRef.h>
#include <llvm-14/llvm/IR/BasicBlock.h>
#include <llvm-14/llvm/IR/Constant.h>
#include <llvm-14/llvm/IR/Constants.h>
#include <llvm-14/llvm/IR/DerivedTypes.h>
#include <llvm-14/llvm/IR/Function.h>
#include <llvm-14/llvm/IR/GlobalValue.h>
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
  std::map<std::string, unsigned long long> str_to_glob;
  std::ostream &strm;

  LLVMPrinter(std::ostream &strm) : strm(strm) {}

private:
  unsigned long long reg_n = 0;
  std::string next_reg_name = "%r0";
  unsigned long long blk_n = 0;
  std::string next_blk_name = "blk_0";
  unsigned long long glob_n = 0;

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
    if (it == align_cache.end()) {
      if (llvm::isa<llvm::PointerType>(typ)) {
        std::string cache_v = ", align 8";
        strm << cache_v;
        align_cache[typ] = cache_v;
      } else if (auto *ityp = llvm::dyn_cast<llvm::IntegerType>(typ)) {
        std::string cache_v =
            ", align " + std::to_string((ityp->getBitWidth() + 7) / 8);
        strm << cache_v;
        align_cache[typ] = cache_v;
      } else {
        align_cache[typ] = "";
      }
    } else {
      strm << it->second;
    }
  }

  void print_val(llvm::Value *val, bool strict) {
    if (auto *const_exp = llvm::dyn_cast<llvm::ConstantExpr>(val)) {
      llvm::Constant *glob = const_exp->getOperand(0);
      llvm::Value *str_ptr = glob->getOperand(0);
      if (auto *str_v = llvm::dyn_cast<llvm::ConstantDataSequential>(str_ptr)) {
        std::string str(str_v->getAsString());
        auto it = str_to_glob.find(str);
        unsigned long long str_idx;
        if (it == str_to_glob.end()) {
          str_idx = ++glob_n;
          str_to_glob[str] = glob_n;
        } else {
          str_idx = it->second;
        }
        strm << "getelementptr inbounds ([" << str.length() << " x i8], ["
             << str.length() << " x i8]* @str_" << str_idx << ", i32 0, i32 0)";
      } else if (llvm::isa<llvm::ConstantData>(str_ptr)) {
        auto it = str_to_glob.find("");
        unsigned long long str_idx;
        if (it == str_to_glob.end()) {
          str_idx = ++glob_n;
          str_to_glob[""] = glob_n;
        } else {
          str_idx = it->second;
        }
        strm << "getelementptr inbounds ([1 x i8], [1 x i8]* @str_" << str_idx
             << ", i32 0, i32 0)";
      } else {
        strm << "!!!!!!!!! not getelemptr !!!!!!!!!!!!";
        // assert(false && "error when writing global str getelemptr");
      }
      return;
    } else if (auto const_int = llvm::dyn_cast<llvm::ConstantInt>(val)) {
      if (const_int->getBitWidth() == 1) {
        if (const_int->getSExtValue()) {
          strm << "true";
        } else {
          strm << "false";
        }
      } else {
        strm << const_int->getSExtValue() % (int64_t)INT32_MAX;
      }
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
    std::cerr << blk_to_string[inst.getParent()]
              << "!!!!!!!!!!1unhandled here!!!!!!!!!!!!!!!!" << std::endl;
    assert(false && "Unhandled instruction");
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
    for (auto &arg_use : call.args()) {
      llvm::Value *arg = arg_use.get();
      print_type(arg_use->getType());
      strm << " ";

      print_val(arg, false);

      if (i < n_params - 1) {
        strm << ", ";
      }
      i++;
    }
    strm << ")\n";
  }

  void visitReturnInst(llvm::ReturnInst &ret) {
    strm << "  ret ";
    llvm::Value *ret_v = ret.getReturnValue();
    if (ret_v) {
      print_type(ret_v->getType());
      strm << ' ';
      print_val(ret_v, false);
      strm << '\n';
    } else {
      strm << "void\n";
    }
  }

  // https://llvm.org/doxygen/classllvm_1_1CmpInst.html#a2be3583dac92a031fa1458d4d992c78b
  void visitICmpInst(llvm::ICmpInst &icmp) {
    strm << "  ";
    print_val(&icmp, false);
    strm << " = icmp ";

    switch (icmp.getPredicate()) {
    case llvm::CmpInst::ICMP_EQ:
      strm << "eq ";
      break;
    case llvm::CmpInst::ICMP_NE:
      strm << "ne ";
      break;
    case llvm::CmpInst::ICMP_SGT:
      strm << "sgt ";
      break;
    case llvm::CmpInst::ICMP_SGE:
      strm << "sge ";
      break;
    case llvm::CmpInst::ICMP_SLT:
      strm << "slt ";
      break;
    case llvm::CmpInst::ICMP_SLE:
      strm << "sle ";
      break;
    default:
      strm << "!!!predicate print not implemented!!!! ";
      assert(false && "!!!predicate print not implemented!!!! ");
    }
    assert(icmp.getNumOperands() == 2);
    llvm::Value *l_v = icmp.getOperand(0);
    llvm::Value *r_v = icmp.getOperand(1);
    print_type(l_v->getType());
    strm << ' ';
    print_val(l_v, false);
    strm << ", ";
    print_val(r_v, false);
    strm << '\n';
  }

  void visitBranchInst(llvm::BranchInst &br) {
    strm << "  br ";
    if (br.isConditional()) {
      assert(br.getNumSuccessors() == 2);
      strm << "i1 ";
      print_val(br.getCondition(), false);
      strm << ", label ";
      print_blk_name(br.getSuccessor(0), false);
      strm << ", label ";
      print_blk_name(br.getSuccessor(1), false);
      strm << '\n';
    } else {
      assert(br.getNumSuccessors() == 1);
      strm << "label ";
      print_blk_name(br.getSuccessor(0), false);
      strm << '\n';
    }
  }

  void visitBinaryOperator(llvm::BinaryOperator &mul) {
    strm << "  ";
    print_val(&mul, false);
    strm << " = ";
    switch (mul.getOpcode()) {
    case llvm::Instruction::Add:
      strm << "add ";
      break;
    case llvm::Instruction::Sub:
      strm << "sub ";
      break;
    case llvm::Instruction::Mul:
      strm << "mul ";
      break;
    case llvm::Instruction::UDiv:
      strm << "udiv ";
      break;
    case llvm::Instruction::SDiv:
      strm << "sdiv ";
      break;
    case llvm::Instruction::URem:
      strm << "urem ";
      break;
    case llvm::Instruction::SRem:
      strm << "srem ";
      break;
    case llvm::Instruction::Shl:
      strm << "shl ";
      break;
    case llvm::Instruction::LShr:
      strm << "lshr ";
      break;
    case llvm::Instruction::AShr:
      strm << "ashr ";
      break;
    case llvm::Instruction::And:
      strm << "and ";
      break;
    case llvm::Instruction::Or:
      strm << "or ";
      break;
    case llvm::Instruction::Xor:
      strm << "xor ";
      break;
    default:
      strm << "!!!binary op print not implemented!!!! ";
      assert(false && "!!!binary op print not implemented!!!! ");
      break;
    }
    assert(mul.getNumOperands() == 2);
    llvm::Value *l_v = mul.getOperand(0);
    llvm::Value *r_v = mul.getOperand(1);
    print_type(l_v->getType());
    strm << " ";
    print_val(l_v, false);
    strm << ", ";
    print_val(r_v, false);
    strm << '\n';
  }

  void visitPHINode(llvm::PHINode &phi) {
    strm << "  ";
    print_val(&phi, false);
    strm << ' ';
    print_type(phi.getType());
    strm << " [";
    int num_paths = phi.getNumIncomingValues();
    for (int i = 0; i < num_paths; i++) {
      strm << "[ ";
      print_val(phi.getIncomingValue(i), false);
      strm << ", ";
      print_blk_name(phi.getIncomingBlock(i), false);
      strm << " ]";
      if (i < num_paths - 1) {
        strm << ", ";
      }
    }
    strm << '\n';
  }

  void visitStoreInst(llvm::StoreInst &store) {
    strm << "  store ";
    llvm::Value *val = store.getValueOperand();
    print_type(val->getType());
    strm << ' ';
    print_val(val, false);
    strm << ", ";
    llvm::Value *ptr = store.getPointerOperand();
    print_type(ptr->getType());
    strm << ' ';
    print_val(ptr, false);
    print_align(val->getType());
    strm << '\n';
  }

  void visitLoadInst(llvm::LoadInst &load) {
    strm << "  ";
    print_val(&load, false);
    strm << " = load ";
    print_type(load.getType());
    strm << ", ";
    llvm::Value *ptr = load.getPointerOperand();
    print_type(ptr->getType());
    strm << ' ';
    print_val(ptr, false);
    print_align(load.getType());
    strm << '\n';
  }

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
    strm << '\n';
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
  for (auto it : printer.str_to_glob) {
    strm << "@str_" << it.second << " = private constant [";
    if (it.first.length() > 0) {
      strm << it.first.length() << " x i8] ";
      strm << "c\"";
      for (char c : it.first) {
        if (c < 16) {
          strm << "\\0" << std::hex << (int)c << std::dec;
        } else {
          strm << '\\' << std::hex << (int)c << std::dec;
        }
      }
      strm << "\", align 1; \"" << it.first.substr(0, it.first.length() - 1)
           << "\"\n";
    } else {
      strm  << " 1 x i8] " << "zeroinitializer, align 1; empty string\n";
    }
  }
}

} // namespace printer