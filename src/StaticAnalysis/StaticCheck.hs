module StaticAnalysis.StaticCheck where

-- this includes all checks that are not type checks but are still static

import qualified Generated.AbsMacchiato as AType
import StaticAnalysis.Traverser
import Util.FieldExtractors
import StaticAnalysis.Err as Err
import Control.Monad.Except


hasMain :: AType.Program -> STraverser
hasMain (AType.ProgramS pos defs)  =
    if foldr isMain True defs then return Nothing else throwError Err.NoMain


isMain :: AType.FnDef' a -> Bool -> Bool
isMain fndef b = undefined
