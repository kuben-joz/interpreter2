#include <cstddef>
#include <cstdint>
#include <llvm-14/llvm/IR/BasicBlock.h>
#include <llvm-14/llvm/IR/InstVisitor.h>
#include <llvm-14/llvm/IR/InstrTypes.h>
#include <llvm-14/llvm/IR/Instruction.h>
#include <llvm-14/llvm/IR/Instructions.h>
#include <llvm-14/llvm/IR/Type.h>
#include <llvm-14/llvm/IR/Value.h>
#include <llvm-14/llvm/Support/Casting.h>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include "cfg.h"
#include "dom_tree.h"
#include "gcse.h"
#include "skel.h"
#include "util.h"

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
  CFG &cfg;
  DomTree &dom;
  std::vector<bool> removed;
  std::vector<Instruction *> inst_to_del;

  struct pairhash_int {
  public:
    std::size_t operator()(const std::pair<int, int> &x) const {
      std::hash<int> hasher;
      size_t seed = hasher(x.first);
      seed ^= hasher(x.second) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
      return seed;
    }
  };

  struct pairhash_ptr {
  public:
    std::size_t operator()(const std::pair<uintptr_t, uintptr_t> &x) const {
      std::hash<uintptr_t> hasher;
      size_t seed = hasher(x.first);
      seed ^= hasher(x.second) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
      return seed;
    }
  };

  // {block, val}
  std::map<Type *, std::unordered_map<std::pair<uintptr_t, uintptr_t>, int,
                                      pairhash_ptr>>
      phi_v_to_idx;
  std::vector<Type *> phi_types;
  std::map<Type *, int> phi_type_counters;
  std::vector<DynamicBitset> phi_bits;
  std::vector<PHINode *> phis;

  std::vector<Value *> idx_to_val;
  std::unordered_map<Value *, int> val_to_idx;

  std::map<CmpInst::Predicate,
           std::unordered_map<std::pair<int, int>, int, pairhash_int>>
      icmp_to_idx;
  std::map<CmpInst::Predicate, std::vector<std::pair<int, int>>> icmp_to_pop;

  std::map<Instruction::BinaryOps,
           std::unordered_map<std::pair<int, int>, int, pairhash_int>>
      bin_op_to_idx;
  std::map<Instruction::BinaryOps, std::vector<std::pair<int, int>>>
      bin_op_to_pop;

  GCSE(CFG &cfg, DomTree &dom)
      : cfg(cfg), dom(dom), removed(dom.blk_to_idx.size(), false) {}

  std::pair<int, bool> get_or_insert_id(Value *v) {
    int res = idx_to_val.size();
    auto search = val_to_idx.insert({v, res});
    if (search.second) {
      assert(res == idx_to_val.size());
      idx_to_val.emplace_back(v);
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
    int insert_val = idx_to_val.size();
    auto search = inst_map.insert({{l_id, r_id}, insert_val});
    if (search.second) {
      auto &cmp_pop = icmp_to_pop[predicate];
      cmp_pop.emplace_back(l_id, r_id);
      if (predicate == CmpInst::ICMP_EQ || predicate == CmpInst::ICMP_NE) {
        auto search_rev = inst_map.insert({{r_id, l_id}, insert_val});
        assert(search_rev
                   .second); // check we didn't forget to insert other way round
        cmp_pop.emplace_back(r_id, l_id);
      }
      idx_to_val.emplace_back(&icmp);
    } else { // value already present
      Value *repl_inst = idx_to_val[search.first->second];
      inst_to_del.emplace_back(&icmp);
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
    phis.emplace_back(&phi);
    Type *typ = phi.getType();
    phi_types.emplace_back(typ);
    int counter = phi_type_counters[typ];
    auto &val_map = phi_v_to_idx[typ];
    int num_vals = phi.getNumIncomingValues();
    int sz = (counter + 63) / 64;
    sz *= 64;
    phi_bits.emplace_back(sz);
    DynamicBitset &bitset = phi_bits.back();
    for (int i = 0; i < num_vals; i++) {
      Value *val = phi.getIncomingValue(i);
      BasicBlock *blk = phi.getIncomingBlock(i);
      uintptr_t vval = (uintptr_t)val;
      uintptr_t vblk = (uintptr_t)blk;
      auto [it, new_val] = val_map.insert({{vval, vblk}, counter});
      if (new_val) {
        while (counter >= sz) {
          sz += 64;
        }
        bitset.expand(sz);
        counter++;
      }
      bitset.set(it->second);
    }
    phi_type_counters[typ] = counter;
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
    int insert_val = idx_to_val.size();
    auto search = inst_map.insert({{l_id, r_id}, insert_val});

    if (search.second) {
      auto &inst_pop = bin_op_to_pop[op];
      inst_pop.emplace_back(l_id, r_id);
      const bool is_symm = op == Instruction::Add || op == Instruction::Mul ||
                           op == Instruction::And || op == Instruction::Or ||
                           op == Instruction::Xor;
      if (l_id != r_id && is_symm) {
        auto search_rev = inst_map.insert({{r_id, l_id}, insert_val});
        // we didn't forget to isnert reverse value somewher
        assert(search_rev.second);
        inst_pop.emplace_back(r_id, l_id);
      }
      idx_to_val.emplace_back(&mul);
    } else {
      Value *repl_val = idx_to_val[search.first->second];
      inst_to_del.emplace_back(&mul);
      mul.replaceAllUsesWith(repl_val);
      change_glob = true;
    }
  }

  void run_rec(int idx) {
    BasicBlock *blk = dom.idx_to_blk[idx];
    assert(!blk->empty());
    if (blk->empty()) {
      return;
    }
    std::map<Instruction::BinaryOps, int> bin_op_pop_nums;
    for (auto &[op, to_pop] : bin_op_to_pop) {
      bin_op_pop_nums[op] = to_pop.size();
    }
    std::map<CmpInst::Predicate, int> icmp_pop_nums;
    for (auto &[pred, to_pop] : icmp_to_pop) {
      icmp_pop_nums[pred] = to_pop.size();
    }
    int init_num_vals = idx_to_val.size();

    for (auto &phi : blk->phis()) {
      visit(phi);
    }
    assert(phi_bits.size() == phi_types.size());
    assert(phi_type_counters.size() == phi_v_to_idx.size());
    assert(phis.size() == phi_types.size());
    int num_phis = phi_types.size();
    for (int i = 0; i < num_phis; i++) {
      if (!phis[i]) {
        continue;
      }
      for (int j = i + 1; j < num_phis; j++) {
        if (!phis[j]) {
          continue;
        }
        if (phi_types[i] != phi_types[j]) {
          continue;
        }
        if (phi_bits[i] == phi_bits[j]) {
          inst_to_del.emplace_back(phis[j]);
          phis[j]->replaceAllUsesWith(phis[i]);
          phis[j] = nullptr;
          change_glob = true;
        }
      }
    }
    phi_v_to_idx.clear();
    phi_types.clear();
    phi_type_counters.clear();
    phi_bits.clear();
    PHINode *last_phi = phis.empty() ? nullptr : phis.back();
    phis.clear();
    if (last_phi == &blk->back()) {
      clean_insts();
      return;
    }
    auto it = blk->getFirstNonPHI()->getIterator();
    auto end = blk->end();
    while (it != end) {
      visit(*it);
      it++;
    }

    clean_insts();

    for (int succ : dom.dom_succs[idx]) {
      run_rec(succ);
    }

    for (auto &[op, to_pop] : bin_op_to_pop) {
      std::unordered_map<std::pair<int, int>, int, pairhash_int> &pop_map =
          bin_op_to_idx[op];
      const int pop_size = bin_op_pop_nums[op];
      while (to_pop.size() != pop_size) {
        pop_map.erase(to_pop.back());
        to_pop.pop_back();
      }
    }
    for (auto &[pred, to_pop] : icmp_to_pop) {
      std::unordered_map<std::pair<int, int>, int, pairhash_int> &pop_map =
          icmp_to_idx[pred];
      const int pop_size = icmp_pop_nums[pred];
      while (to_pop.size() != pop_size) {
        pop_map.erase(to_pop.back());
        to_pop.pop_back();
      }
    }
    while (idx_to_val.size() != init_num_vals) {
      val_to_idx.erase(idx_to_val.back());
      idx_to_val.pop_back();
    }
  }
};

std::pair<bool, bool> run_gcse(CFG &cfg, DomTree &dom) {

  GCSE gcse(cfg, dom);
  gcse.run_rec(0);
  return {gcse.change_glob, gcse.change_structure};
}
} // namespace clean