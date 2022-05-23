{-# LANGUAGE RecordWildCards #-}

module Mem.SymbolTable where

import qualified Data.Map as Map

type Loc = Int
type Id = String

data Value a = Value{val :: a, no_of_ref :: Int}


type Envs = [Env]

type Env = Map.Map Id Loc

type State a = Map.Map Loc (Value a)




data SymTable a = SymTable{proc_envs :: Envs, val_envs :: Envs, state :: State a}


--newEnv :: SymTable a -> SymTable a
--newEnv SymTable {..} = SymTable{proc_envs=proc_envs, val_envs =  : val_envs, ..}


pushEnv :: [Map.Map k a] -> [Map.Map k a]
pushEnv envs = Map.empty : envs


popEnv :: Env -> State a -> State a
popEnv e s = removeRefs (Map.elems e) s


removeRefs :: [Int] -> State a -> State a
removeRefs locs state
  = foldl (flip (Map.update removeRef)) state locs


removeRef :: Value a -> Maybe (Value a)
removeRef Value {val=_, no_of_ref=1} = Nothing
removeRef Value {..} = Just (Value{val=val, no_of_ref=no_of_ref-1})






















