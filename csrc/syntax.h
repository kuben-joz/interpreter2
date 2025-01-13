#pragma once

namespace ast {
class AST;

class Program;

class FnDef;

class Stmt;
// class EmptyStmt; // not used
class BStmt;
class DeclStmt;
class AssStmt;
class IncDecStmt;
class RetStmt;
class CondStmt;
class WhileStmt;
class ExprStmt;

class Expr;
class VarExpr;
class LIntExpr;
class LBoolExpr;
class LStringExpr;
class FunAppExpr;
class NegExpr;
class NotExpr;
class MulExpr;
class AddExpr;
class RelExpr;
class LogExpr;
} // namespace ast