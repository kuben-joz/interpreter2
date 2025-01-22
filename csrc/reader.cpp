#include <fstream>
#include <iostream>
#include <memory>
#include <sstream>

#include "ir_builder.h"
#include "skel.h"
#include "util.h"

int main(int argc, char *argv[]) {
  // int code = strtol(argv[1], nullptr, 0);
  std::string res;
  getline(std::cin, res);
  std::unique_ptr<ast::Program> prog_ast = treeparse::build_prog(res);
  IRGen visitor;
  prog_ast->accept(&visitor);
  std::cout << visitor.module->getIFuncList().size() << std::endl;
  visitor.module->dump();
  return 0;
}