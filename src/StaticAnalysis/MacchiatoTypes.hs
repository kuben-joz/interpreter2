{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module StaticAnalysis.MacchiatoTypes where

-- for without arrays but add functionality later

import qualified Data.List
import Parsing.AbsMacchiato

-- Macchiato Full Type with array dimensions, for now the dim um is always 0
type MFType = (MType, MTypeMods)

data MTypeMods = MTypeMods {dim_num :: Int, has_ref :: Bool, can_brk_cont :: Bool}

instance Show MTypeMods where
  show (MTypeMods {..}) = show dim_num ++ ", " ++ show has_ref

instance Eq MTypeMods where
  (==) m1@(MTypeMods dim_num1 _ _) m2@(MTypeMods dim_num2 _ _) =
    dim_num1 == dim_num2

class StrictEqual a where
  strictComp :: a -> a -> Bool

instance StrictEqual MTypeMods where
  strictComp m1@(MTypeMods dim_num1 ref1 _) m2@(MTypeMods dim_num2 ref2 _) =
    dim_num1 == dim_num2 && (ref1 == ref2 || not ref1)

instance StrictEqual MFType where
  strictComp m1@(t1, mods1) m2@(t2, mods2) =
    strictComp t1 t2 && strictComp mods1 mods2

instance StrictEqual MType where
  strictComp (MFun r1 ps1) (MFun r2 ps2) =
    foldr (&&) (strictComp r1 r2) (zipWith strictComp ps1 ps2)
  strictComp a b = a == b

instance StrictEqual a => StrictEqual [a] where
  strictComp l1 l2 = foldr (&&) True $ zipWith strictComp l1 l2

instance Show MType where
  show MBool = "bool"
  show MString = "String"
  show MInt = "int"
  show (MFun _ params) = "fun " ++ show params

data MType
  = MBool
  | MString
  | MInt
  | MFun MFType [MFType]
  deriving (Eq)

class Typable a where
  toMFT :: a -> MFType

instance Typable Arg where
  toMFT (ArgVal _ t _) = toMFT t
  toMFT (ArgRef _ t _) = toArgType $ toMFT t
    where
      toArgType (base_t, mods) = (base_t, MTypeMods {dim_num = dim_num mods, has_ref = True, can_brk_cont = False})

instance Typable Type where
  toMFT (Int _) = (MInt, MTypeMods {dim_num = 0, has_ref = False, can_brk_cont = False})
  toMFT (Str _) = (MString, MTypeMods {dim_num = 0, has_ref = False, can_brk_cont = False})
  toMFT (Bool _) = (MBool, MTypeMods {dim_num = 0, has_ref = False, can_brk_cont = False})
  toMFT (Arr _ t bra) = (fst $ toMFT t, MTypeMods {dim_num = length bra, has_ref = False, can_brk_cont = False})

makeReferencable (mtype, MTypeMods {..}) = (mtype, MTypeMods {dim_num = dim_num, has_ref = True, can_brk_cont = can_brk_cont})

instance Typable KeyWord where
  toMFT a = (MInt, MTypeMods {dim_num = 0, has_ref = False, can_brk_cont = False})

instance Typable FnDef where
  toMFT (FunDef _ t _ args _) = (MFun (toMFT t) (map toMFT args), MTypeMods {dim_num = 0, has_ref = True, can_brk_cont = False})

compatibleParent (KeyWordLength _) t = (fst t) == MString || (dim_num $ snd t) > 0
compatibleParent (KeyWordMaxVal _) t = (fst t) == MInt

adjustArrType (t, mods) dim_acc = do
  return $
    ( t,
      MTypeMods
        { dim_num = dim_num mods - length dim_acc,
          has_ref = has_ref mods,
          can_brk_cont = can_brk_cont mods
        }
    )

setBrkCont t = case t of
  Nothing -> Nothing
  Just (t, MTypeMods {..}) -> Just (t, MTypeMods {dim_num = dim_num, has_ref = has_ref, can_brk_cont = True})
