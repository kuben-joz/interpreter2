#pragma once

#include <llvm-14/llvm/IR/Module.h>

namespace printer {
void print(llvm::Module *module, std::set<llvm::Function *> extern_funcs);
}