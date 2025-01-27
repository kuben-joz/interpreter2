#include "builtin_funcs.h"

ProtoFunc builtin_func_sigs[5] = {
    ProtoFunc("printInt", ast::VOID, std::vector<ast::Type>({ast::INT})),
    ProtoFunc("printString", ast::VOID, std::vector<ast::Type>({ast::STR})),
    ProtoFunc("error", ast::VOID, std::vector<ast::Type>({})),
    ProtoFunc("readInt", ast::INT, std::vector<ast::Type>({})),
    ProtoFunc("readString", ast::STR, std::vector<ast::Type>({})),
};

ProtoFunc string_eq_sig("strs_eq", ast::BOOL,
                        std::vector<ast::Type>({ast::STR, ast::STR}));

ProtoFunc merge_strings_sig("merge_strs", ast::STR,
                         std::vector<ast::Type>({ast::STR, ast::STR}));