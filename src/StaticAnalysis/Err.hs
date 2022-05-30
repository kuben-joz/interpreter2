{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module StaticAnalysis.Err where

import Parsing.AbsMacchiato ( BNFC'Position, Expr, MulOp, AddOp, RelOp, Expr' (Neg, Not) )
import StaticAnalysis.MacchiatoTypes
import Mem.SymbolTable (SymTable(loc))

type ErrLoc = BNFC'Position

errMsgStart:: ErrLoc -> String
errMsgStart (Just(line, col)) = "Exception caught on line " ++ show line ++ ", collumn " ++ show col ++ ": "
errMsgStart Nothing = "Exception caught: "

data StaticException =
      UnknownVar ErrLoc String
    | VarRedef ErrLoc String
    | NoMain
    -- expected but found
    | TypeMismatch ErrLoc MFType MFType
    | RefMismatch ErrLoc MFType MFType
    | IncompatibleTypeOp ErrLoc Expr MFType
    | IncompatibleTypeOpMul ErrLoc MulOp MFType MFType
    | IncompatibleTypeOpAdd ErrLoc AddOp MFType MFType
    | IncompatibleTypeOpRel ErrLoc RelOp MFType MFType
    | IncompatibleTypeOpAnd ErrLoc MFType MFType
    | IncompatibleTypeOpOr ErrLoc MFType MFType
    | InvalidKeyword ErrLoc
    | FuncNameCollision ErrLoc String
    | NoReturn ErrLoc String
    | AssToUndeclaredVar ErrLoc String
    | UseOfUndeclaredVar ErrLoc String
    | CallToUnderclaredFun ErrLoc String
    -- pos, name of func, its type, parameters given
    | IncompatibleFunParams ErrLoc String MFType [MFType]
    -- actual, how far accessed
    | ArrTooShallow ErrLoc Int Int
    | IncompPrintParam ErrLoc MFType
    -- expected actual
    | BadRetType ErrLoc MFType MFType
    | RefFuncAsVar ErrLoc String
    | ContInvalidPos ErrLoc
    | BrkInvalidPos ErrLoc
    | ForbiddenId ErrLoc String
    | IfElseTypeMistmatch ErrLoc MFType MFType
    | VarAsFunc ErrLoc String


instance Show StaticException where
    show (VarRedef loc id) = (errMsgStart loc) ++ "Variable " ++ show id ++ " redefinition"
    show (UnknownVar loc  id) = (errMsgStart loc) ++"Unkown variable " ++ show id
    show NoMain = (errMsgStart Nothing) ++"No main function found";
    show (TypeMismatch loc t1 t2) = (errMsgStart loc) ++"Type mismatch between " ++ show t1 ++  " and " ++ show t2;
    show (RefMismatch loc t1 t2) = (errMsgStart loc) ++"Referential mistach between " ++ show t1 ++ " and " ++ show t2
    show (IncompatibleTypeOp  loc op t) = 
      case op of
        (Not _ _) -> (errMsgStart loc) ++"Incompatible type " ++ show t ++ " for logical negation";
        (Neg _ _) -> (errMsgStart loc) ++"Incompatible type " ++ show t ++ " for integer negation";
    show (IncompatibleTypeOpMul loc op t1 t2) = (errMsgStart loc) ++" Incompatible types for operation, " ++ show t1 ++ show op ++ show t2;
    show (IncompatibleTypeOpAdd loc op t1 t2) = (errMsgStart loc) ++" Incompatible types for operation, " ++ show t1 ++ show op ++ show t2;
    show (IncompatibleTypeOpRel loc op t1 t2) = (errMsgStart loc) ++" Incompatible types for operation, " ++ show t1 ++ show op ++ show t2;
    show (IncompatibleTypeOpAnd loc t1 t2)= (errMsgStart loc) ++" Incompatible types for operations, " ++ show t1 ++ " && " ++ show t2;
    show (IncompatibleTypeOpOr loc t1 t2)= (errMsgStart loc) ++" Incompatible types for operations, " ++ show t1 ++ " || " ++ show t2;
    show (InvalidKeyword loc) = (errMsgStart loc) ++ "Invalid keyword";
    show (FuncNameCollision  loc name) = (errMsgStart loc) ++" Function name collison for " ++ show name;
    show (NoReturn  loc name) = (errMsgStart loc) ++ " No return in function " ++ show name;
    show (AssToUndeclaredVar  loc id) = (errMsgStart loc) ++"Assingment to undeclared variable " ++ show id;
    show (UseOfUndeclaredVar loc id) = (errMsgStart loc) ++"Use of undeclared variable "  ++ show id ++  " in expression";
    show (CallToUnderclaredFun loc name) = (errMsgStart loc) ++ "Call to undeclared function " ++ show name;
    show (IncompatibleFunParams loc f_name f_type params) = (errMsgStart loc) ++" Incompatible function params for function " ++ show f_name ++ " expected " ++ show f_type ++ " but found " ++ show params;
    show (ArrTooShallow loc size acc_i) = (errMsgStart loc) ++"Array too shallow, access to element at dimension " ++ show acc_i ++ " but this array only has " ++ show size ++ " dimensions";
    show (BadRetType loc expected actual) =(errMsgStart loc) ++ "Bad return type from function, expected " ++ show expected ++ " but got " ++ show actual;
    show (ContInvalidPos loc) = (errMsgStart loc) ++ "Continue outside of while loop"
    show (BrkInvalidPos loc) = (errMsgStart loc) ++ "Break outside of while loop"
    show (IncompPrintParam loc t) = (errMsgStart loc) ++ "Can't print expressions of type: " ++ show t
    show (RefFuncAsVar loc id) =(errMsgStart loc) ++ "Use of function identifier " ++ show id ++ " as a variable"
    show (ForbiddenId loc id) = (errMsgStart loc) ++"Use of forbidden identifier name, " ++ show id
    show (IfElseTypeMistmatch loc t_if t_else) = (errMsgStart loc) ++ "Different return types for \n if -> "
      ++ show t_if ++ "\n else -> " ++ show t_else 
    show (VarAsFunc loc id) = (errMsgStart loc) ++ "Call to " ++ show id ++ " which isn't a function"






