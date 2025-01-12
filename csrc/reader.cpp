#include <iostream>
#include <sstream>

#include "skel.h"
#include "util.h"

int main(int argc, char *argv[]) {
  // int code = strtol(argv[1], nullptr, 0);
  std::string res;
  getline(std::cin, res);
  treeparse::build_prog(res);
  return 0;
}