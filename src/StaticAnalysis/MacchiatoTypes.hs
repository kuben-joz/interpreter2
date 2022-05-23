{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module StaticAnalysis.MacchiatoTypes where

-- for without arrays but add functionality later
import Generated.AbsMacchiato
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
    deriving Eq

    


class TypeDef t where
    castToMT :: t -> MType

instance TypeDef Type where
    castToMT (Int _) = MInt
    castToMT (Str _) = MString
    castToMT (Bool _) = MBool

-- todo Lemat: MTypeMods jest jednoznaczna dla FSM bez licznika
class TypeFulfiller t where
    completeType :: t -> MType -> MFType

instance TypeFulfiller Type where
    completeType (Int _) MInt = (MInt, MTypeMods {dim_num = 0, has_ref = False}) 
    completeType (Str _) MString = (MInt, MTypeMods {dim_num = 0, has_ref = True})
    completeType (Bool _) MBool = (MBool, MTypeMods {dim_num = 0, has_ref = False})
    completeType (Arr _ bra _) typ = (typ, MTypeMods {dim_num = length bra, has_ref = True})
    --todo maybe error for no brackets



