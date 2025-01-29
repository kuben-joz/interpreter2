#include <cstddef>
#include <llvm-14/llvm/IR/BasicBlock.h>
#include <llvm-14/llvm/IR/InstVisitor.h>
#include <llvm-14/llvm/IR/InstrTypes.h>
#include <llvm-14/llvm/IR/Instruction.h>
#include <llvm-14/llvm/IR/Instructions.h>
#include <tuple>
#include <unordered_map>

#include "cfg.h"
#include "dom_tree.h"
#include "init_pass.h"
#include "skel.h"

using namespace llvm;

namespace clean {

// {REF}
// https://stackoverflow.com/questions/2590677/how-do-i-combine-hash-values-in-c0x
template <class T> inline void hash_combine(std::size_t &seed, const T &v) {
  std::hash<T> hasher;
  seed ^= hasher(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}

class GCSE : public InstVisitor<GCSE> {
public:
  bool change_glob = false;
  bool change_structure = false;
  bool was_phi = false;
  CFG &cfg;
  DomTree &dom;
  std::vector<bool> removed;
  std::vector<Instruction *> inst_to_del;
  // this is kinda a FIND UNION

  enum OPS {
    EQ,
    NE,
    GT,
    LT,
    LE,
    ADD,
    SUB,
    MUL,
    UDIV,
    SDIV,
    UREM,
    SREM,
    SHL,
    LSHR,
    ASHR,
    AND,
    OR,
    XOR
  };

  struct val {
    int64_t const_val;
    ast::Type type;
    llvm::Value *ptr_val;

    val(int64_t const_val, ast::Type type)
        : const_val(const_val), type(type), ptr_val(nullptr) {}

    val(Value *ptr_val, ast::Type type)
        : const_val(0), type(type), ptr_val(ptr_val) {}

    std::size_t operator()(const val &x) const {
      std::hash<int64_t> hasher;
      size_t seed = hasher(x.const_val);
      seed ^= hasher(x.type) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
      seed ^=
          hasher((intptr_t)x.ptr_val) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
      return seed;
    }

    bool operator==(const val &other) {
      return (const_val == other.const_val) && (type == other.type) &&
             (ptr_val == other.ptr_val);
    }
  };

  struct pairhash {
  public:
    std::size_t operator()(const std::pair<int, int> &x) const {
      std::hash<int> hasher;
      size_t seed = hasher(x.first);
      seed ^= hasher(x.second) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
      return seed;
    }
  };

  // ICMP or binaryop

  std::vector<int> parent;
  std::vector<val> vals;
  std::map<llvm::CmpInst::Predicate,
           std::unordered_map<std::pair<int, int>, int, pairhash>>
      icmp_refs;
  std::map<llvm::Instruction::BinaryOps,
           std::unordered_map<std::pair<int, int>, int, pairhash>>
      bin_op_refs;

  std::unordered_map<llvm::Value *, int> val_to_int;

  GCSE(CFG &cfg, DomTree &dom)
      : cfg(cfg), dom(dom), removed(dom.blk_to_idx.size(), false) {}

  void clean_insts() {
    val v(1, ast::VOID);
    val v2(v);
    if (inst_to_del.empty()) {
      return;
    }
    change_glob = true;
    for (auto *inst : inst_to_del) {
      inst->eraseFromParent();
    }
    inst_to_del.clear();
  }

  void visitInstruction(Instruction &inst) {
    assert("false" && "unhandled instruction");
  }

  void visitICmpInst(ICmpInst &icmp) {

    llvm::Value *l_v = icmp.getOperand(0);
    llvm::Value *r_v = icmp.getOperand(1);
    switch (icmp.getPredicate()) {
    case llvm::CmpInst::ICMP_EQ:

      break;
    case llvm::CmpInst::ICMP_NE:

      break;
    case llvm::CmpInst::ICMP_SGT:

      break;
    case llvm::CmpInst::ICMP_SGE:

      break;
    case llvm::CmpInst::ICMP_SLT:

      break;
    case llvm::CmpInst::ICMP_SLE:

      break;
    default:
      break;
    }
  }

  void visitAllocaInst(AllocaInst &alloca) {}

  void visitLoadInst(LoadInst &load) {}

  void visitStoreInst(StoreInst *store) {}

  void visitPHINode(PHINode &phi) { was_phi = true; }

  void visitCallInst(CallInst &call) {}

  void visitReturnInst(ReturnInst &ret) {}

  void visitBranchInst(BranchInst &br) {}

  void visitBinaryOperator(BinaryOperator &mul) {
    switch (mul.getOpcode()) {
    case llvm::Instruction::Add:

      break;
    case llvm::Instruction::Sub:

      break;
    case llvm::Instruction::Mul:

      break;
    case llvm::Instruction::UDiv:

      break;
    case llvm::Instruction::SDiv:

      break;
    case llvm::Instruction::URem:

      break;
    case llvm::Instruction::SRem:

      break;
    case llvm::Instruction::Shl:

      break;
    case llvm::Instruction::LShr:

      break;
    case llvm::Instruction::AShr:

      break;
    case llvm::Instruction::And:

      break;
    case llvm::Instruction::Or:

      break;
    case llvm::Instruction::Xor:

      break;
    default:

      break;
    }
  }

  void run_rec(int idx) {
    BasicBlock *blk = dom.idx_to_blk[idx];

    for (int succ : dom.dom_succs[idx]) {
      run_rec(succ);
    }
  }
};

std::pair<bool, bool> run_gcse(CFG &cfg, DomTree &dom) {

  GCSE gcse(cfg, dom);

  return std::make_pair(gcse.change_glob, gcse.change_structure);
}
} // namespace clean