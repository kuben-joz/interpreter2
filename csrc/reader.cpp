#include <iostream>
#include <sstream>

#include "skel.h"
#include "util.h"

int main(int argc, char *argv[]) {
    //int code = strtol(argv[1], nullptr, 0);
    std::string res;
    std::cin >> res; // ignore OK/ERROR line
    //getline(std::cin, res);
    //if(code) {
    //    std::cout << res << std::endl;
    //    return code;
    //}
    treeparse::build_prog(res);

    return 0;
}