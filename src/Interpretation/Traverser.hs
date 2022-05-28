{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Interpretation.Traverser where
import Control.Monad.Except
import Control.Monad.State ( MonadState(put, get), StateT, gets )
import Mem.SymbolTable
import Interpretation.MacchiatoVals
import Interpretation.Err
import qualified Data.Map as M
import Data.Maybe
import Parsing.AbsMacchiato

type Traverser a = StateT(SymTable  MVal (MVal, Bool)) (ExceptT RuntimeException IO) a
type ITraverser = Traverser (Maybe MVal)


initProg :: ITraverser
initProg = do
    -- The MBool space is for the return value, bool is for wether we should continue
    put $ initState (MBool False, False)
    return Nothing

addToGlobal id params blk = do
    s@SymTable{..} <- get
    let fun = MFun{env=[], params=params, instructions=blk}
    put SymTable{global_env=M.insert id loc global_env, current_env =current_env,
    state = M.insert loc fun state, dat=dat,loc=loc+1}
    return Nothing

addKeyVal k v = do
    s@SymTable{..} <- get
    let v_loc = loc
    put SymTable{global_env= global_env,current_env=M.insert k loc (head current_env): tail current_env,
    state= M.insert loc v state, dat=dat, loc=loc+1}
    return loc


addLocVal v = do
    s@SymTable{..} <- get
    let v_loc = loc
    put SymTable{global_env= global_env,current_env=current_env,
    state= M.insert loc v state, dat=dat, loc=loc+1}
    return v_loc

    -- todoput SymTable{global}

addKeyLoc k l = do
    s@SymTable{..} <- get
    put $ SymTable{global_env=global_env,
    current_env=M.insert k l (head current_env):tail current_env,
    state=state, dat=dat,loc=loc}


getLoc key = do
    (g_env, c_envs) <- gets getEnvs
    let main_search = listToMaybe (mapMaybe (M.lookup key) c_envs)
    case main_search of
        Nothing -> return $ M.lookup key g_env
        _       -> return main_search


getVal key = do
    loc <- getLoc key
    getVal' loc


getVal' :: Maybe Loc -> ITraverser
getVal' Nothing = do
    return Nothing
getVal' m_loc = do
    s <- gets getState
    return $ M.lookup (fromJust m_loc) s



getLocVal key = do
    loc <- getLoc key
    val <- getVal' loc
    return (loc, val)

--todo I think I have to give both the values and env at the same time, or I could return env
-- inserts are (id, Maybe Loc)
pushPop new_env inserts f = do
    s <- get
    put $ SymTable (global_env s) (M.empty:new_env) (state s) (dat s) (loc s)
    mapM_ addParamVal inserts
    ret <- f
    s2 <- get
    put $ SymTable (global_env s2) (current_env s) (state s2) (dat s2) (loc s)
    return ret


addParamVal :: (Id, Either Loc MVal) -> ITraverser
addParamVal (id, Left loc) = do
    addKeyLoc id loc
    return Nothing
addParamVal (id, Right val) = do
    addKeyVal id val
    return Nothing









