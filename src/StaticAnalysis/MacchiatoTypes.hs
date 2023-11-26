{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module StaticAnalysis.MacchiatoTypes where

import qualified Data.List
import Debug.Trace
import Parsing.AbsMacchiato

type MFType = (MType, MTypeMods)

data MTypeMods = MTypeMods {dim_num :: Int, has_ref :: Bool, can_brk_cont :: Bool}

instance Show MTypeMods where
  show (MTypeMods {..}) = "dimensions: " ++ show dim_num ++ ", can be referenced:" ++ show has_ref

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
    (foldr (&&) (strictComp r1 r2) (zipWith strictComp ps1 ps2)) && (length ps1 == length ps2)
  strictComp a b = a == b

instance StrictEqual a => StrictEqual [a] where
  strictComp l1 l2 = foldr (&&) True $ zipWith strictComp l1 l2

instance Show MType where
  show MBool = "bool"
  show MString = "String"
  show MInt = "int"
  show (MFun _ params) = "fun " ++ show params
  show MVoid = "void"

data MType
  = MBool
  | MString
  | MInt
  | MVoid
  | MFun MFType [MFType]
  deriving (Eq)

class Typable a where
  toMFT :: a -> MFType

instance Typable Arg where
  toMFT (ArgVal _ t _) = toMFT t
    where
      toArgType (base_t, mods) = (base_t, MTypeMods {dim_num = dim_num mods, has_ref = True, can_brk_cont = False})

instance Typable Type where
  toMFT (Int _) = (MInt, MTypeMods {dim_num = 0, has_ref = False, can_brk_cont = False})
  toMFT (Str _) = (MString, MTypeMods {dim_num = 0, has_ref = False, can_brk_cont = False})
  toMFT (Bool _) = (MBool, MTypeMods {dim_num = 0, has_ref = False, can_brk_cont = False})
  toMFT (Void _) = (MVoid, MTypeMods {dim_num = 0, has_ref = False, can_brk_cont = False})

makeReferencable (mtype, MTypeMods {..}) = (mtype, MTypeMods {dim_num = dim_num, has_ref = True, can_brk_cont = can_brk_cont})

instance Typable FnDef where
  toMFT (FunDef _ t _ args _) = (MFun (toMFT t) (map toMFT args), MTypeMods {dim_num = 0, has_ref = True, can_brk_cont = False})
