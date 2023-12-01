{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module StaticAnalysis.MacchiatoVals where

import Control.Applicative ((<|>))
import Debug.Trace (trace)
import Mem.SymbolTable
import Parsing.AbsMacchiato
import Text.Printf (vFmt)

data MArgs
  = MARef Id
  | MAVal Id

type MResVal = Maybe MVal

data MVal
  = MString String Int
  | MInt Int
  | MBool Bool
  | MFun {env :: [Env], params :: [MArgs], instructions :: Block}
  | MVoid

instance Show MVal where
  show (MString s _) = s
  show (MInt i) = show i
  show (MBool b) = show b
  show (MFun {}) = "fun"
  show (MVoid) = "void"

class HasLen a where
  getLen :: a -> Int

instance HasLen MVal where
  getLen (MString _ l) = l
  getLen _ = undefined

-- todo change to process the values once we do simplicifaction of code
lt :: MResVal -> MResVal -> MResVal
lt (Just (MInt v1)) (Just (MInt v2)) = Just $ MBool $ v1 < v2
lt a b = voidop a b

lte :: MResVal -> MResVal -> MResVal
lte (Just (MInt v1)) (Just (MInt v2)) = Just $ MBool $ v1 <= v2
lte a b = voidop a b

gt :: MResVal -> MResVal -> MResVal
gt (Just (MInt v1)) (Just (MInt v2)) = Just $ MBool $ v1 > v2
gt a b = voidop a b

gte :: MResVal -> MResVal -> MResVal
gte (Just (MInt v1)) (Just (MInt v2)) = Just $ MBool $ v1 >= v2
gte a b = voidop a b

myand :: MResVal -> MResVal -> MResVal
myand (Just (MBool v1)) (Just (MBool v2)) = Just $ MBool $ v1 && v2
myand a b = voidop a b

myor :: MResVal -> MResVal -> MResVal
myor (Just (MBool v1)) (Just (MBool v2)) = Just $ MBool $ v1 || v2
myor a b = voidop a b

eq :: MResVal -> MResVal -> MResVal
eq Nothing _ = undefined
eq _ Nothing = undefined
eq (Just MVoid) _ = Just MVoid
eq _ (Just MVoid) = Just MVoid
eq (Just v1) (Just v2) = Just $ MBool $ v1 == v2

mynot :: MResVal -> MResVal
mynot (Just MVoid) = Just MVoid
mynot (Just v) = Just $ (!) v
mynot Nothing = undefined

plus :: MResVal -> MResVal -> MResVal
plus Nothing _ = undefined
plus _ Nothing = undefined
plus (Just MVoid) _ = Just MVoid
plus _ (Just MVoid) = Just MVoid
plus (Just v1) (Just v2) = Just $ v1 + v2

minus :: MResVal -> MResVal -> MResVal
minus Nothing _ = undefined
minus _ Nothing = undefined
minus (Just MVoid) _ = Just MVoid
minus _ (Just MVoid) = Just MVoid
minus (Just v1) (Just v2) = Just $ v1 - v2

times :: MResVal -> MResVal -> MResVal
times Nothing _ = undefined
times _ Nothing = undefined
times (Just MVoid) _ = (Just MVoid)
times _ (Just MVoid) = (Just MVoid)
times (Just v1) (Just v2) = Just $ v1 * v2

mydiv :: MResVal -> MResVal -> MResVal
mydiv Nothing _ = undefined
mydiv _ Nothing = undefined
mydiv (Just MVoid) _ = (Just MVoid)
mydiv _ (Just MVoid) = (Just MVoid)
mydiv (Just v1) (Just v2) = Just $ v1 `div` v2

mymod :: MResVal -> MResVal -> MResVal
mymod Nothing _ = undefined
mymod _ Nothing = undefined
mymod (Just MVoid) _ = (Just MVoid)
mymod _ (Just MVoid) = (Just MVoid)
mymod (Just v1) (Just v2) = Just $ v1 `mod` v2

neg :: MResVal -> MResVal
neg (Just MVoid) = Just MVoid
neg (Just v) = Just $ negate v
neg Nothing = undefined

class Negatable a where
  (!) :: a -> a

instance Negatable MVal where
  (!) :: MVal -> MVal
  (!) (MBool b) = MBool $ not b
  (!) _ = undefined

-- (+), (*), abs, signum, fromInteger, (negate | (-))
instance Num MVal where
  (+) (MString s1 l1) (MString s2 l2) = MString (s1 ++ s2) (l1 + l2)
  (+) (MInt i1) (MInt i2) = MInt $ i1 + i2
  (+) _ _ = undefined

  (*) (MInt i1) (MInt i2) = MInt $ i1 * i2
  (*) _ _ = undefined
  abs (MInt i) = MInt $ abs i
  abs _ = undefined
  signum (MInt i) = MInt $ signum i
  signum _ = undefined
  fromInteger i = MInt $ fromInteger i
  negate (MInt i) = MInt $ - i
  negate _ = undefined

instance Enum MVal where
  toEnum = MInt
  fromEnum (MInt i) = i
  fromEnum _ = undefined

instance Enum MResVal where
  toEnum = Just . MInt
  fromEnum (Just (MInt i)) = i
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
  (==) a b = undefined

instance Integral MVal where
  quotRem (MInt i1) (MInt i2) = (MInt (quot i1 i2), MInt (rem i1 i2))
  quotRem _ _ = undefined
  toInteger (MInt i) = toInteger i
  toInteger _ = undefined

fromMVal (MBool b) = b
fromMVal _ = undefined

toDefValue :: Type -> MVal
toDefValue Int {} = MInt 0
toDefValue Str {} = MString "" 0
toDefValue Bool {} = MBool False
toDefValue _ = undefined

voidop :: MResVal -> MResVal -> MResVal
voidop Nothing _ = undefined
voidop _ Nothing = undefined
voidop (Just MVoid) _ = Just MVoid
voidop _ (Just MVoid) = Just MVoid
voidop _ _ = undefined