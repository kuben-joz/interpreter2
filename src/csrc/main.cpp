#include <fstream>
#include <iostream>
#include <llvm-14/llvm/IR/BasicBlock.h>
#include <llvm-14/llvm/IR/Function.h>
#include <llvm-14/llvm/IR/IRBuilder.h>
#include <llvm-14/llvm/IR/LLVMContext.h>
#include <llvm-14/llvm/IR/Module.h>
#include <llvm-14/llvm/Support/raw_ostream.h>
#include <memory>
#include <sstream>
#include <string>

#include "cfg.h"
#include "dom_tree.h"
#include "gcse.h"
#include "init_pass.h"
#include "ir_builder.h"
#include "mem2reg.h"
#include "pass_util.h"
#include "printer.h"
#include "skel.h"
#include "tree_trim.h"
#include "treeparse.h"
#include "util.h"
#include "val_prop.h"

void draw_cfg(llvm::Module *module, CFG &cfg, int i) {
  std::ofstream cfg_f("debug/" + std::to_string(i) + "-cfg.out");
  int num_blks = cfg.succ.size();
  cfg_f << num_blks << '\n';
  int blk_id = 0;
  std::map<llvm::BasicBlock *, int> blk_to_idx;
  for (auto &succ : cfg.succ) {
    blk_to_idx[succ.first] = blk_id++;
  }
  for (auto &succ : cfg.succ) {
    std::string blk_addr;
    llvm::raw_string_ostream ss(blk_addr);
    int id_l = blk_to_idx[succ.first];
    succ.first->printAsOperand(ss, false, module);
    std::string l_name = ss.str();
    for (auto &s : succ.second) {
      std::string blk_addr;
      llvm::raw_string_ostream ss(blk_addr);
      s->printAsOperand(ss, false, module);
      int id_r = blk_to_idx[s];
      std::string r_name = ss.str();
      cfg_f << l_name << ' ' << r_name << '\n';
    }
  }
}

void draw_rev_cfg(llvm::Module *module, CFG &cfg, int i) {
  std::ofstream cfg_f("debug/" + std::to_string(i) + "-cfg.out");
  int num_blks = cfg.succ.size();
  cfg_f << num_blks << '\n';
  int blk_id = 0;
  std::map<llvm::BasicBlock *, int> blk_to_idx;
  for (auto &succ : cfg.pred) {
    blk_to_idx[succ.first] = blk_id++;
  }
  for (auto &succ : cfg.pred) {
    std::string blk_addr;
    llvm::raw_string_ostream ss(blk_addr);
    int id_l = blk_to_idx[succ.first];
    succ.first->printAsOperand(ss, false, module);
    std::string l_name = ss.str();
    for (auto &s : succ.second) {
      std::string blk_addr;
      llvm::raw_string_ostream ss(blk_addr);
      s->printAsOperand(ss, false, module);
      int id_r = blk_to_idx[s];
      std::string r_name = ss.str();
      cfg_f << l_name << ' ' << r_name << '\n';
    }
  }
}

int main() {
  // int code = strtol(argv[1], nullptr, 0);
  std::string res;
  getline(std::cin, res);
  std::unique_ptr<ast::Program> prog_ast = treeparse::build_prog(res);
  std::unique_ptr<llvm::LLVMContext> context =
      std::make_unique<llvm::LLVMContext>();
  std::unique_ptr<llvm::Module> module =
      std::make_unique<llvm::Module>("my init module", *context);
  std::unique_ptr<llvm::IRBuilder<>> builder =
      std::make_unique<llvm::IRBuilder<>>(*context);
  IRGen visitor(context.get(), module.get(), builder.get());
  prog_ast->accept(&visitor);
  std::set<llvm::Function *> extern_funcs = std::move(visitor.extern_funcs);
  llvm::Function *strs_eq_fn = visitor.str_eq_fn;
  int i = 0;
  for (auto &fn_ref : module->getFunctionList()) {
    StringCMP str_cmp;
    llvm::Function *fn = &fn_ref;
    if (extern_funcs.count(fn)) {
      continue;
    }
    CFG cfg(fn);
    DomTree dom(cfg);
    std::pair<bool, bool> res = clean::init_clean(cfg, dom);

    cfg = CFG(fn);
    dom = DomTree(cfg);
    dom.dom_frontier();
    mem2reg::transform(cfg, dom);
    cfg = CFG(fn);
    dom = DomTree(cfg);
    res = clean::val_prop(cfg, dom, strs_eq_fn, str_cmp, builder.get());
    assert(!res.second);

    cfg = CFG(fn);
    dom = DomTree(cfg);
    res = clean::trim_tree(cfg, dom);

    cfg = CFG(fn);
    dom = DomTree(cfg);
    clean::run_gcse(cfg, dom);
    i++;
  }
  printer::print(module.get(), extern_funcs);
  //module->dump();
  return 0;
}

// join lbocks
// ret removal
// also value propagation