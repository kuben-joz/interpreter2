module StaticAnalysis.Err where

import Generated.AbsMacchiato ( BNFC'Position, Expr, MulOp, AddOp, RelOp )
import Control.Exception (NonTermination)
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
    | BadRetType ErrLoc MFType MFType






