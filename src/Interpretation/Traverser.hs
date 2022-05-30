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
import GHC.IO.Handle
import System.IO
import Debug.Trace

data ReturnState = RetState{cont :: Bool, brk :: Bool}

type ISymTable = SymTable MVal ReturnState

type Traverser a = StateT(ISymTable) (ExceptT RuntimeException IO) a
type ITraverser = Traverser (Maybe MVal)


--initProg :: ITraverser
initProg = initState (RetState False False)
    -- The MBool space is for the return value, bool is for wether we should continue
 --   put $ initState (RetState False False)
  --  return Nothing

addToGlobal id params blk = do
    s@SymTable{..} <- get
    let fun = MFun{env=[], params=params, instructions=blk}
    put SymTable{global_env=M.insert id loc global_env, current_env =current_env,
    state = M.insert loc fun state, dat=dat,loc=loc+1}
    return Nothing

addKeyVal :: Id -> MVal -> Traverser Loc
addKeyVal k v = do
    s@SymTable{..} <- get
    put SymTable{global_env= global_env,current_env=M.insert k loc (head current_env): tail current_env,
    state= M.insert loc v state, dat=dat, loc=loc+1}
    return loc

addOrModifyKeyVal k v = do
    s@SymTable{..} <- get
    loc_temp <- getLoc k
    case loc_temp of
        Nothing -> addKeyVal k v
        Just v_loc -> do
            put SymTable{global_env= global_env,current_env=M.insert k v_loc (head current_env): tail current_env,
            state= M.insert v_loc v state, dat=dat, loc=loc}
            return loc 


addLocVal v = do
    s@SymTable{..} <- get
    let v_loc = loc
    put SymTable{global_env= global_env,current_env=current_env,
    state= M.insert loc v state, dat=dat, loc=loc+1}
    return v_loc



addLocVal' l v = do
    s@SymTable{..} <- get
    put SymTable{global_env= global_env,current_env=current_env,
    state= M.insert l v state, dat=dat, loc=loc}
    return Nothing

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

getEnv::Traverser [Env]
getEnv = do
    (_, env) <- gets getEnvs
    return env
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

pushPop' f = do
    s <- get
    put $ SymTable (global_env s) (M.empty:current_env s) (state s) (dat s) (loc s)
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

brkOrContCalled :: Traverser Bool
brkOrContCalled = do 
    RetState{..} <- gets getData
    return $ brk || cont

getAndSet :: (ISymTable -> a) -> (a -> ISymTable) -> Traverser a
getAndSet getter setter = do
    res <- gets getter
    put $ setter res
    return res


getAndSetCont:: Bool -> Traverser Bool
getAndSetCont b = do
    SymTable{..} <- get
    let RetState{..} = dat
    let ret = cont
    put $ SymTable {global_env=global_env, current_env=current_env, 
    state=state, dat=RetState{cont=b, brk=brk}, loc=loc}
    return ret

getAndSetBrk:: Bool -> Traverser Bool
getAndSetBrk b = do
    SymTable{..} <- get
    let RetState{..} = dat
    let ret = brk
    put $ SymTable {global_env=global_env, current_env=current_env, 
    state=state, dat=RetState{cont=cont, brk=b}, loc=loc}
    return ret

getAndSetContBrk:: Bool -> Bool -> Traverser (Bool, Bool)
getAndSetContBrk b_cont b_brk = do
    SymTable{..} <- get
    let RetState{..} = dat
    let ret = (cont, brk)
    put $ SymTable {global_env=global_env, current_env=current_env, 
    state=state, dat=RetState{cont=b_cont, brk=b_brk}, loc=loc}
    return ret 

{-
brkOrContCalled :: Traverser (Maybe MControlStmts)
brkOrContCalled = do 
    rs <- gets getData
    return $ retStateToControlStmt rs
    

retStateToControlStmt r =
    case r of
        RetState True True -> undefined
        RetState True _ -> Just MCont
        RetState _ True -> Just MBrk
        _               -> Nothing
        -}

printState :: ITraverser
printState = do
    state <- gets getState
    trace (show state) (return Nothing)

printEnv :: ITraverser
printEnv = do
    envs <- gets getEnvs
    trace (show envs) (return Nothing)










