{-# LANGUAGE RecordWildCards #-}
module Interpretation.MacchiatoVals where

import Mem.SymbolTable
import Parsing.AbsMacchiato

data MArgs =
    MARef Id
    | MAVal Id



data MVal =
      MString String Int
    | MInt Int
    | MBool Bool
    | MFun {env :: [Env], params :: [MArgs], instructions :: Block}
    | MArr {elems :: [Loc], len :: Int, dim_num :: Int}

instance Show MVal where
  show (MString s _) = s
  show (MInt i) = show i
  show (MBool b) = show b
  show (MFun{}) = "fun"
  show (MArr{}) = "arr"

class HasLen a where
  getLen :: a -> Int

instance HasLen MVal where
  getLen (MString _ l) = l
  getLen MArr{..} = len
  getLen _ = undefined
  

getDimNum MArr{..} = dim_num




(!) :: MVal -> MVal
(!) (MBool b) = MBool $ not b
(!) _ = undefined

-- (+), (*), abs, signum, fromInteger, (negate | (-))
instance Num MVal where
  (+) (MString s1 l1) (MString s2 l2) = MString (s1 ++ s2) (l1+l2) 
  (+) (MInt i1) (MInt i2) = MInt $ i1 + i2
  (+) _ _ = undefined
  (*) (MInt i1) (MInt i2) = MInt $ i1 * i2
  (*) _ _ = undefined
  abs (MInt i) = MInt $ abs i
  abs _ = undefined
  signum (MInt i) = MInt $ signum i
  signum _ = undefined
  fromInteger i = MInt $ fromInteger i
  negate (MInt i) = MInt $ -i
  negate _ = undefined


instance Enum MVal where
  toEnum = MInt
  fromEnum (MInt i) = i
  fromEnum _ = undefined


instance Real MVal where
  toRational _ = undefined

instance Ord MVal where
  (<=) (MInt i1) (MInt i2) = i1 <= i2
  (<=) _ _ = undefined

instance Eq MVal where
  (==) (MInt i1) (MInt i2) = i1 == i2
  (==) (MBool b1) (MBool b2) = b1 == b2
  (==) (MString s1 _) (MString s2 _) = s1 == s2
  -- to remeber to do arr pointer checking above
  (==) _ _ = undefined


instance Integral MVal where
  quotRem (MInt i1) (MInt i2) = (MInt (quot i1 i2), MInt (rem i1 i2))
  quotRem _ _ = undefined
  toInteger (MInt i) = toInteger i
  toInteger _ = undefined

fromMVal (MBool b) = b
fromMVal _ = undefined





toDefValue :: Type -> MVal
toDefValue Int{} = MInt 0
toDefValue Str{} = MString "" 0
toDefValue Bool{} = MBool False
toDefValue (Arr _ _ dim_bra) = MArr {elems= [], len=0, dim_num=length dim_bra}
toDefValue _ = undefined

