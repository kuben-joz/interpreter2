#include <iostream>
#include <sstream>

int main(int argc, char *argv[]) {
    int code = strtol(argv[1], nullptr, 0);
    std::string res;
    std::cin >> res; // ignore OK/ERROR line
    getline(std::cin, res);
    if(code) {
        std::cout << res << std::endl;
        return code;
    }
    std::stringstream ss(res);
    

    return 0;
}