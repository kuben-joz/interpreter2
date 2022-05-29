
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module StaticAnalysis.Traverser where


import Mem.SymbolTable
import StaticAnalysis.MacchiatoTypes
import Control.Monad.Except
import Control.Monad.State
import Data.Maybe
import qualified Parsing.AbsMacchiato as AType
import qualified StaticAnalysis.Err as Err
import qualified Data.Map as M
import Foreign (deRefStablePtr)
import GHC.IO.Encoding (getLocaleEncoding)
import Control.Monad.Reader
-- import qualified Control.Monad.Reader as 

type Traverser = StateT (SymTable MFType ()) (Except Err.StaticException)

-- our static traverser
type STraverser = Traverser (Maybe MFType)



pushPop f = do
    s <- get
    put $ refreshState s
    res <- f
    put s
    return res

--pushPopEnv f env = do 
 --   SymTable{..} <- get


push f = do
    s <- get
    put $ expandState s
    res <- f
    put s
    return res



-- todo check if check for prev value everywhere
addKeyVal key val' = do
    let val = makeReferencable val'
    SymTable{..} <- get
    let (prev_val, new_env) = M.insertLookupWithKey second key loc (head current_env)
    let new_state = M.insert loc val state
    put SymTable{global_env=global_env, current_env=new_env : tail current_env , state = new_state, dat= dat, loc= loc+1}
    return prev_val




addKeyLoc key loc = do
    SymTable{..} <- get
    let (prev_val, new_env) = M.insertLookupWithKey second key loc (head current_env)
    put SymTable{global_env=global_env, current_env= new_env : tail current_env,..}
    return prev_val


getLoc key = do
    (g_env, c_envs) <- gets getEnvs
    let main_search = listToMaybe (mapMaybe (M.lookup key) c_envs)
    case main_search of
        Nothing -> return $ M.lookup key g_env
        _       -> return main_search


getVal key = do
    state <- gets getState
    loc <- getLoc key
    return $ (`M.lookup` state) =<< loc
    
second a b c = b
















