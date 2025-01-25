#include "mem2reg.h"
#include <cstddef>

namespace mem2reg {
struct var {
  llvm::AllocaInst *alloc;
  // todo maybe chagne to ints for var to int
  std::set<llvm::BasicBlock *> store_blks;
  std::vector<llvm::Value *> repl_val_stack;
  std::vector<llvm::Instruction *> replaced_insts;
  var(llvm::AllocaInst *alloc) : alloc(alloc) {}
};

void add_alloca(llvm::AllocaInst *alloc,
                std::vector<std::unique_ptr<var>> &vars,
                std::map<llvm::Instruction *, var *> &inst_to_var) {
  std::vector<llvm::Instruction *> insts;
  std::unique_ptr<var> v = std::make_unique<var>(alloc);
  for (auto *use : alloc->users()) {
    if (auto *load_use = llvm::dyn_cast<llvm::LoadInst>(use)) {
      insts.emplace_back(load_use);
    } else if (auto *store_use = llvm::dyn_cast<llvm::StoreInst>(use)) {
      if (store_use->getOperand(1) == v->alloc) {
        insts.emplace_back(store_use);
        v->store_blks.insert(store_use->getParent());
      } else {
        assert(false && "I don't think we ever store alloca pointers");
        return;
      }
    } else {
      assert(false && "I don't think any alloca pointers have other uses");
      return;
    }
  }
  vars.emplace_back(std::move(v));
  for (auto inst : insts) {
    inst_to_var[inst] = vars.back().get();
  }
}

// Search(X) in paper
void rename_rec(const int blk_idx,
                std::map<llvm::Instruction *, var *> &inst_to_var, CFG &cfg,
                DomTree &dom_tree) {
  llvm::BasicBlock *blk = dom_tree.idx_to_blk[blk_idx];
  for (auto &inst_ref : blk->getInstList()) {
    llvm::Instruction *inst = &inst_ref;
    if (llvm::isa<llvm::LoadInst>(inst)) {
      auto var_it = inst_to_var.find(inst);
      if (var_it == inst_to_var.end()) {
        continue;
      }
      if (var_it->second->repl_val_stack.empty()) {
        // todo, how do we deal with this???
        assert(false && "Edge case todo");
      } else {
        // This is equivalent to the C(V) renaming in the paper
        // We take last store and teleport it to everywhere the next load is used
        // note to self, assinging to next store works as intended
        assert(!var_it->second->repl_val_stack.empty());
        inst->replaceAllUsesWith(var_it->second->repl_val_stack.back());
      }
    } else if (llvm::isa<llvm::StoreInst>(inst)) {
      auto var_it = inst_to_var.find(inst);
      if (var_it == inst_to_var.end()) {
        continue;
      }
      // The value we stored is instead remembered at top of stack and used until next phi or store
      llvm::Value *new_repl_val = inst->getOperand(0);
      var_it->second->repl_val_stack.emplace_back(new_repl_val);

    } else if (llvm::isa<llvm::PHINode>(inst)) {
      auto var_it = inst_to_var.find(inst);
      if (var_it == inst_to_var.end()) {
        continue;
      }
      llvm::Value *new_repl_val = inst;
      var_it->second->repl_val_stack.emplace_back(new_repl_val);
    }
  } // end of first loop

  // all children, not just dominated
  // note to self, this is for something like while loops to have phis both ways
  for (llvm::BasicBlock *cfg_child : cfg.succ[blk]) {
    for (auto &inst_ref : cfg_child->getInstList()) {
      if (auto *phi_inst = llvm::dyn_cast<llvm::PHINode>(&inst_ref)) {
        auto var_it = inst_to_var.find(phi_inst);
        if (var_it == inst_to_var.end()) {
          continue;
        }
        assert(!var_it->second->repl_val_stack.empty());
        phi_inst->addIncoming(var_it->second->repl_val_stack.back(), blk);
      } else { // nothing between phi nodes in block
        break;
      }
    }
  } // end of second loop

  // recurse down dominator tree
  for (int dom_child : dom_tree.dom_succ[blk_idx]) {
    rename_rec(dom_child, inst_to_var, cfg, dom_tree);
  } // return from dfs

  for (auto &inst_ref : blk->getInstList()) {
    llvm::Instruction *inst = &inst_ref;
    if (llvm::isa<llvm::LoadInst>(inst)) {
      auto var_it = inst_to_var.find(inst);
      if (var_it == inst_to_var.end()) {
        continue;
      }
      // todo check if empty stack here is a problem
      // we don't pop since we haven't pushed
      var_it->second->replaced_insts.emplace_back(inst);
    } else if (llvm::isa<llvm::StoreInst>(inst)) {
      auto var_it = inst_to_var.find(inst);
      if (var_it == inst_to_var.end()) {
        continue;
      }
      var_it->second->repl_val_stack.pop_back();
      var_it->second->replaced_insts.emplace_back(inst);

    } else if (llvm::isa<llvm::PHINode>(inst)) {
      auto var_it = inst_to_var.find(inst);
      if (var_it == inst_to_var.end()) {
        continue;
      }
      var_it->second->repl_val_stack.pop_back();
      // we dont remove phi nodes we added
    }
  } // end of loop after return
}

void transform(CFG &cfg, DomTree &dom) {
  std::vector<std::unique_ptr<var>> vars;
  std::map<llvm::Instruction *, var *> inst_to_var;
  assert(cfg.start_blks.size() == 1 &&
         "Dom tree for reverse cfg not implemented, need exactly one start "
         "block");
  for (auto &inst_ref : cfg.start_blks.front()->getInstList()) {
    llvm::Instruction *inst = &inst_ref;
    if (auto *alloc = llvm::dyn_cast<llvm::AllocaInst>(inst)) {
      add_alloca(alloc, vars, inst_to_var);
    } else {
      // all allocs are at beginning of first block
      // todo make a test for this
      break;
    }
  }
  for (auto &v : vars) {
    std::vector<llvm::BasicBlock *> it_dom_front =
        dom.it_dom_front(v->store_blks);
    for (auto *blk : it_dom_front) {
      // todo maybe assert blk not empty
      assert(!blk->empty());
      llvm::PHINode *phi = nullptr;
      if (blk->empty()) { // todo check if, when and why this case can occur
        phi = llvm::PHINode::Create(v->alloc->getAllocatedType(),
                                    cfg.pred[blk].size(), "allocphi", blk);
      } else {
        phi = llvm::PHINode::Create(v->alloc->getAllocatedType(),
                                    cfg.pred[blk].size(), "allocphi",
                                    &blk->front());
      }
      assert(phi);
      inst_to_var[phi] = v.get();
    }
  }

  // block zero is always entry node in dom tree
  rename_rec(0, inst_to_var, cfg, dom);

  // remove unneeded isntructions
  for (auto &v : vars) {
    assert(v->repl_val_stack.empty());
    v->alloc->eraseFromParent();
    for (auto *inst : v->replaced_insts) {
      inst->eraseFromParent();
    }
  }
}
} // namespace mem2reg