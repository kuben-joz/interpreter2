#include <fstream>
#include <iostream>
#include <llvm-14/llvm/IR/BasicBlock.h>
#include <llvm-14/llvm/IR/Function.h>
#include <llvm-14/llvm/IR/Module.h>
#include <llvm-14/llvm/Support/raw_ostream.h>
#include <memory>
#include <sstream>
#include <string>

#include "cfg.h"
#include "dom_tree.h"
#include "ir_builder.h"
#include "mem2reg.h"
#include "skel.h"
#include "treeparse.h"

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
  IRGen visitor;
  prog_ast->accept(&visitor);
  std::unique_ptr<llvm::Module> module = std::move(visitor.module);
  int i = 0;
  for (auto &fn_ref : module->getFunctionList()) {
    llvm::Function *fn = &fn_ref;
    CFG cfg(fn);
    //  draw_cfg(module.get(), cfg, i);
    //  draw_rev_cfg(module.get(), cfg, 1000+i);
    DomTree dom(cfg);
    mem2reg::transform(cfg, dom);
    i++;
  }
  module->dump();
  return 0;
}