{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Interpretation.Traverser where
import Control.Monad.Except
import Control.Monad.State
import Mem.SymbolTable
import Interpretation.MacchiatoVals
import Interpretation.Err
import qualified Data.Map as M
import Data.Maybe

type Traverser a = StateT(SymTable  MVal (MVal, Bool)) (ExceptT RuntimeException IO) a
type ITraverser = Traverser (Maybe MVal)


initProg :: ITraverser
initProg = do
    -- The MBool space is for the return value, bool is for wether we should continue
    put $ initState (MBool False, False)
    return Nothing

addKeyVal k v = do
    s@SymTable{..} <- get
    let v_loc = loc
    put SymTable{global_env=M.insert k loc global_env,current_env= current_env, 
    state= M.insert loc v state, dat=dat, loc=loc+1}
    return loc

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

-- this could be more efficent I admit
getLocVal key = do
    loc <- getLoc key
    val <- getVal key
    return (loc, val)            








