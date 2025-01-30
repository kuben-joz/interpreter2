#include <cstddef>
#include <llvm-14/llvm/IR/BasicBlock.h>
#include <llvm-14/llvm/IR/InstVisitor.h>
#include <llvm-14/llvm/IR/InstrTypes.h>
#include <llvm-14/llvm/IR/Instruction.h>
#include <llvm-14/llvm/IR/Instructions.h>
#include <llvm-14/llvm/IR/Value.h>
#include <tuple>
#include <unordered_map>
#include <utility>
#include <vector>

#include "cfg.h"
#include "dom_tree.h"
#include "init_pass.h"
#include "skel.h"

using namespace llvm;

namespace clean {

// {REF}
// https://stackoverflow.com/questions/2590677/how-do-i-combine-hash-values-in-c0x
// i.e. this is from boost
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

  std::vector<Value *> idx_to_val;
  std::unordered_map<Value *, int> val_to_idx;

  std::map<CmpInst::Predicate,
           std::unordered_map<std::pair<int, int>, int, pairhash>>
      icmp_to_idx;
  std::map<CmpInst::Predicate, std::vector<std::pair<int, int>>> icmp_to_pop;

  std::map<Instruction::BinaryOps,
           std::unordered_map<std::pair<int, int>, int, pairhash>>
      bin_op_to_idx;
  std::map<Instruction::BinaryOps, std::vector<std::pair<int, int>>>
      bin_op_to_pop;

  GCSE(CFG &cfg, DomTree &dom)
      : cfg(cfg), dom(dom), removed(dom.blk_to_idx.size(), false) {}

  std::pair<int, bool> get_or_insert_id(Value *v) {
    int res = idx_to_val.size();
    auto search = val_to_idx.insert({v, res});
    if (search.second) {
      assert(res = idx_to_val.size());
      idx_to_val.emplace_back(res);
      return {res, true};
    } else {
      res = search.first->second;
      return {res, false};
    }
  }

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

  void visitInstruction(Instruction &inst) {
    assert(false && "unhandled instruction");
    val_to_idx[&inst] = idx_to_val.size();
    idx_to_val.emplace_back(&inst);
  }

  void visitICmpInst(ICmpInst &icmp) {
    Value *l_v = icmp.getOperand(0);
    Value *r_v = icmp.getOperand(1);
    auto [l_id, l_new] = get_or_insert_id(l_v);
    auto [r_id, r_new] = get_or_insert_id(r_v);
    const CmpInst::Predicate predicate = icmp.getPredicate();
    auto &inst_map = icmp_to_idx[predicate];
    auto search = inst_map.insert({{l_id, r_id}, (int)idx_to_val.size()});
    if (search.second) {
      auto &cmp_pop = icmp_to_pop[predicate];
      cmp_pop.emplace_back(l_id, r_id);
      if (predicate == CmpInst::ICMP_EQ || predicate == CmpInst::ICMP_NE) {
        auto search_rev =
            inst_map.insert({{r_id, l_id}, (int)idx_to_val.size()});
        assert(search_rev
                   .second); // check we didn't forget to insert other way round
        cmp_pop.emplace_back(r_id, l_id);
      }
      idx_to_val.emplace_back(&icmp);
    } else { // value already present
      Value *repl_inst = idx_to_val[search.first->second];
      inst_to_del.emplace_back(icmp);
      icmp.replaceAllUsesWith(repl_inst);
      change_glob = true;
    }
  }

  // we take care of this in a post mem2reg pass
  void visitAllocaInst(AllocaInst &alloca) {
    val_to_idx[&alloca] = idx_to_val.size();
    idx_to_val.emplace_back(&alloca);
  }

  void visitLoadInst(LoadInst &load) {
    val_to_idx[&load] = idx_to_val.size();
    idx_to_val.emplace_back(&load);
  }

  void visitStoreInst(StoreInst &store) {
    val_to_idx[&store] = idx_to_val.size();
    idx_to_val.emplace_back(&store);
  }

  void visitPHINode(PHINode &phi) {
    assert(false && "todo");
    was_phi = true;
  }

  void visitCallInst(CallInst &call) {
    val_to_idx[&call] = idx_to_val.size();
    idx_to_val.emplace_back(&call);
  }

  void visitReturnInst(ReturnInst &ret) {}

  void visitBranchInst(BranchInst &br) {}

  void visitBinaryOperator(BinaryOperator &mul) {
    Value *l_v = mul.getOperand(0);
    Value *r_v = mul.getOperand(1);
    auto [l_id, new_l] = get_or_insert_id(l_v);
    auto [r_id, new_r] = get_or_insert_id(r_v);
    Instruction::BinaryOps op = mul.getOpcode();

    auto &inst_map = bin_op_to_idx[op];
    auto search = inst_map.insert({{l_id, r_id}, (int)idx_to_val.size()});

    if (search.second) {
      auto &inst_pop = bin_op_to_pop[op];
      const bool is_symm = op == Instruction::Add || op == Instruction::Mul ||
                           op == Instruction::And || op == Instruction::Or ||
                           op == Instruction::Xor;
      if (is_symm) {
        auto search_rev =
            inst_map.insert({{r_id, l_id}, (int)idx_to_val.size()});
        assert(
            search_rev
                .second); // we didn't forget to isnert reverse value somewher
      }
    } else {
      Value *repl_val = idx_to_val[search.first->second];
      inst_to_del.emplace_back(mul);
      mul.replaceAllUsesWith(repl_val);
      change_glob = true;
    }
  }

  void run_rec(int idx) {
    BasicBlock *blk = dom.idx_to_blk[idx];
    std::map<Instruction::BinaryOps, int> bin_op_pop_nums;
    for (auto &[op, to_pop] : bin_op_to_pop) {
      bin_op_pop_nums[op] = to_pop.size();
    }
    std::map<CmpInst::Predicate, int> icmp_pop_nums;
    for (auto &[pred, to_pop] : icmp_to_pop) {
      icmp_pop_nums[pred] = to_pop.size();
    }
    assert(false && "todo");

    clean_insts();
    int init_num_vals = idx_to_val.size();

    for (int succ : dom.dom_succs[idx]) {

      run_rec(succ);
      for (auto &[op, to_pop] : bin_op_to_pop) {
        std::unordered_map<std::pair<int, int>, int, pairhash> &pop_map =
            bin_op_to_idx[op];
        const int pop_size = bin_op_pop_nums[op];
        while (to_pop.size() != pop_size) {
          pop_map.erase(to_pop.back());
          to_pop.pop_back();
        }
      }
      for (auto &[pred, to_pop] : icmp_to_pop) {
        std::unordered_map<std::pair<int, int>, int, pairhash> &pop_map =
            icmp_to_idx[pred];
        const int pop_size = icmp_pop_nums[pred];
        while (to_pop.size() != pop_size) {
          pop_map.erase(to_pop.back());
          to_pop.pop_back();
        }
      }
      idx_to_val.resize(init_num_vals);
    }
  }
};

std::pair<bool, bool> run_gcse(CFG &cfg, DomTree &dom) {

  GCSE gcse(cfg, dom);

  return {gcse.change_glob, gcse.change_structure};
}
} // namespace clean