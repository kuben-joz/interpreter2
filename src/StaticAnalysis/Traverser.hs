module StaticAnalysis.Traverser where


import qualified Mem.SymbolTable as ST
import StaticAnalysis.MacchiatoTypes
import qualified Control.Monad.Except as EM
import qualified Control.Monad.State as SM
import StaticAnalysis.Err (StaticException)
-- import qualified Control.Monad.Reader as 

type Traverser = SM.StateT (ST.SymTable MFType) (EM.Except StaticException)


class Checkable e where
    check :: 












