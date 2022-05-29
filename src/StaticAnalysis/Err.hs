module StaticAnalysis.Err where

import Parsing.AbsMacchiato ( BNFC'Position, Expr, MulOp, AddOp, RelOp )
import StaticAnalysis.MacchiatoTypes

type ErrLoc = BNFC'Position

data StaticException =
      UnknownVar ErrLoc String
    | VarRedef ErrLoc String
    | NoMain
    | NotRef   ErrLoc String
    -- expected but found
    | TypeMismatch ErrLoc MFType MFType
    | RefMismatch ErrLoc MFType MFType
    | IncompatibleTypeOp ErrLoc Expr MFType
    | IncompatibleTypeOpMul ErrLoc MulOp MFType MFType
    | IncompatibleTypeOpAdd ErrLoc AddOp MFType MFType
    | IncompatibleTypeOpRel ErrLoc RelOp MFType MFType
    | IncompatibleTypeOpAndOr ErrLoc Expr MFType MFType
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
    | BadRetType ErrLoc MFType MFType
    | RefFuncAsVar ErrLoc String
    | ContInvalidPos ErrLoc
    | BrkInvalidPos ErrLoc
    | ForbiddenId ErrLoc String


instance Show StaticException where
    show (VarRedef _ _) = "Var redef"
    show (UnknownVar _ _) = "Unkown Var"
    show NoMain = "no main";
    show (NotRef _ _) = "not ref";
    show (TypeMismatch loc t1 t2) = "tpye mismatch " ++ show t1 ++ show t2 ++ show loc;
    show RefMismatch {} = "ref mismatch";
    show (IncompatibleTypeOp  loc _ t) = "imcompatible type " ++ show loc ++ show t;
    show IncompatibleTypeOpMul {} = "imcompatible for mul";
    show IncompatibleTypeOpAdd {} = "imcompatbile for add";
    show IncompatibleTypeOpRel {} = "incompabtible for rel";
    show IncompatibleTypeOpAndOr {} = "icompatbile for or or and";
    show (InvalidKeyword  _) = "invalid keyword";
    show (FuncNameCollision  loc name) = show  loc ++ "fonc name collison" ++ name;
    show (NoReturn  loc name) = show loc ++ " no return " ++ name;
    show (AssToUndeclaredVar  _ _) = "assingment to endeclared var";
    show (UseOfUndeclaredVar _ _) = "use of undeclared var in expr";
    show (CallToUnderclaredFun loc name) = show loc ++ "calll to udneclared fun " ++ name;
    show (IncompatibleFunParams loc f_name f_type params) = show loc ++ " incompatbiel func params" ++ show f_name ++ show f_type ++ show params;
    show ArrTooShallow {} = "arr to shaloow";
    show BadRetType {} = "bad ret type";
    show ContInvalidPos {} = "invalid cont position"
    show BrkInvalidPos {} = "break outside of while loop"
    show IncompPrintParam {} = "can't print type"
    show RefFuncAsVar {} = "can't reference function as variable"
    show ForbiddenId {} = "forbidden identifier name "






