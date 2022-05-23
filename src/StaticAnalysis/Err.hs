module StaticAnalysis.Err where

import Generated.AbsMacchiato ( BNFC'Position )
import Control.Exception (NonTermination)
import StaticAnalysis.MacchiatoTypes

type ErrLoc = BNFC'Position

data StaticException =
      UnknownVar ErrLoc String
    | VarRedef ErrLoc String
    | NoMain
    | NotRef   ErrLoc String
    | TypeMismatch ErrLoc MFType MFType
    | IncompatibleTypeOp
    | InvalidKeyword 



