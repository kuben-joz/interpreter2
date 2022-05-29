{-# LANGUAGE RecordWildCards #-}

module Mem.SymbolTable where


import qualified Data.Map as Map

type Loc = Int
type Id = String



type Env = Map.Map Id Loc

type State a = Map.Map Loc a



-- dat is used for any additional data we need, this changes based on wether we are type checking or executing the program
data SymTable s d = SymTable{global_env :: Env, current_env :: [Env], state :: State s, dat :: d, loc :: Int}

refreshState SymTable{..} = SymTable global_env [Map.empty] state dat loc

expandState SymTable{..} = SymTable global_env (Map.empty:current_env) state dat loc

initState d = SymTable Map.empty [] Map.empty d 0

getEnvs SymTable{..} = (global_env, current_env)

getState SymTable{..} = state

getData SymTable{..} = dat


--newEnv :: SymTable a -> SymTable a
--newEnv SymTable {..} = SymTable{proc_envs=proc_envs, val_envs =  : val_envs, ..}

-- based on this https://stackoverflow.com/questions/27663084/how-can-i-decently-add-an-undo-functionality-to-state-monads
-- I also initially had an explicvit stack, but this gives better time complexity as I don't have to manually pop things off SymTable {state}






















