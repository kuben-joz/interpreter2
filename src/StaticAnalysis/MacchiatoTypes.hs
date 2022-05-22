module StaticAnalysis.MacchiatoTypes where

-- for without arrays but add functionality later
import qualified Generated.AbsMacchiato as ATypes
import qualified Data.List

-- Macchiato Full Type with array dimensions, for now the dim um is always 0
type MFType = (MType, MTypeMods)

data MTypeMods = MTypeMods {dim_num :: Int, has_ref :: Bool}

instance Eq MTypeMods where
    (==) m1@(MTypeMods dim_num1 _) m2@(MTypeMods dim_num2 _) = dim_num1 == dim_num2 

data MType = 
    MBool
    | MString
    | MInt
 --   | MFun {return :: MFType, params :: [MFType]}
    deriving Eq


class MFType


