{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module StaticAnalysis.CFGOptim where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Data.Bool (bool)
import Data.Maybe (catMaybes, fromJust, isNothing, listToMaybe)
import Debug.Trace
import GHC.Float (expFloat)
import Mem.SymbolTable (Id, Loc, SymTable (current_env), getData)
import Parsing.AbsMacchiato as AType
import StaticAnalysis.CFGTraverser
import StaticAnalysis.Err (StaticException (NoReturnCont))
import qualified StaticAnalysis.Err as Err
import StaticAnalysis.MacchiatoTypes (MType)
import qualified StaticAnalysis.MacchiatoTypes as MTypes
import StaticAnalysis.MacchiatoVals
import StaticAnalysis.Params
import StaticAnalysis.TypeCheck (nothingGuard)
import System.IO
import System.Posix.Internals (st_mtime)

data IncDec = Inc | Dec

class Interpretable a where
  interpret :: a -> ITraverser

startInterpret :: Interpretable a => a -> IO (Either StaticException MResVal)
startInterpret prog = runExceptT $ evalStateT (interpret prog) (initProg)

instance Interpretable Program where
  interpret (ProgramS loc fndefs) = do
    mapM_ initGlobal fndefs
    mapM_ interpret fndefs
    return Nothing

instance Interpretable FnDef where
  interpret fn@(FunDef funloc ftype (Ident id) args blk) = do
    fn_def_m@(Just MFun {..}) <- getVal id
    let ftype' = MTypes.toMFT ftype
    case ftype' of
      (MTypes.MVoid, _) -> return $ Just MVoid
      _ -> pushPop'' (funloc, id) (interpret instructions)

instance Interpretable Block where
  -- we push the stack before getting here so we don't do it here
  interpret (FunBlock f_loc (stmt : stmts)) = do
    res <- interpret stmt
    case res of
      Nothing -> interpret (FunBlock f_loc stmts)
      _ -> return res
  interpret (FunBlock _ []) = do return Nothing

--StackInfo {..} <- gets getData
--case res of
-- Nothing -> throwError $ Err.NoReturnCont calls
-- _ -> return res

interpretStatements ::
  Interpretable (Stmt' a) =>
  [Stmt' a] ->
  StateT ISymTable (ExceptT StaticException IO) (Maybe MVal)
interpretStatements [] = return Nothing
interpretStatements s_arr@(BStmt {} : stmts) = nestedInterpret s_arr
interpretStatements s_arr@(Cond {} : stmts) = nestedInterpret s_arr
interpretStatements s_arr@(CondElse {} : stmts) = nestedInterpret s_arr
interpretStatements s_arr@(While {} : stmts) = nestedInterpret s_arr
interpretStatements (stmt : stmts) = do
  res <- interpret stmt
  case res of
    Nothing -> interpretStatements stmts
    _ -> return res

nestedInterpret (stmt : stmts) = do
  res <- interpret stmt
  case res of
    Nothing -> interpretStatements stmts
    _ -> return res
nestedInterpret [] = do return Nothing

instance Interpretable Stmt where
  interpret Empty {} = return Nothing
  interpret (BStmt _ block) = do
    pushPop' $ interpret block
  interpret (Decl _ t items) = do
    let def_val = toDefValue t
    mapM_ (addItem def_val) items
    return Nothing
  interpret (Ass loc (Ident id) expr) = do
    res <- interpret expr -- todo remove this to remove things like bounds chekcing on ints
    nothingGuard res (Err.InternalError loc)
    addOrModifyKeyVal id (fromJust res)
    return Nothing
  interpret (Ret _ expr) = do
    interpret expr
  -- return $ Just MVoid todo check above works ok
  interpret (RetNone _) = do
    return $ Just MVoid
  interpret (Cond _ expr stmt) = do
    inf <- isInfiniteWhile
    bool_m <- interpret expr
    if inf
      then do
        case bool_m of
          Just (MBool False) ->
            return Nothing
          _ -> do
            res <- interpret stmt
            if isNothing res
              then return Nothing
              else return $ Just MVoid
      else do
        case bool_m of
          Just (MBool True) -> interpret stmt
          _ -> return Nothing
  interpret (CondElse _ expr stmt_t stmt_f) = do
    inf <- isInfiniteWhile
    bool_m <- interpret expr
    if inf
      then do
        case bool_m of
          Just (MBool False) -> interpret stmt_f
          Just (MBool True) -> interpret stmt_t
          -- todo not sure why alternative doesnt work here
          _ -> do
            res1 <- interpret stmt_f
            res2 <- interpret stmt_t
            case (res1, res2) of
              (Nothing, _) -> return res2
              (_, Nothing) -> return res1
              ((Just v1), (Just v2)) -> return $ Just MVoid
      else do
        case bool_m of
          Just (MBool False) -> interpret stmt_f
          Just (MBool True) -> interpret stmt_t
          _ -> do
            res1 <- interpret stmt_f
            res2 <- interpret stmt_t
            if isNothing res1
              then return Nothing
              else return res2
  interpret stmt'@(While loc expr stmt) = do
    -- infinite loop?
    bool_m <- pushPop'''' $ interpret expr
    case bool_m of
      Just (MBool True) -> do
        res <- (pushPop''' $ interpret stmt)
        nothingGuard res (Err.NoReturnInf loc)
        return res
      _ -> return Nothing
  interpret (SExp _ (EApp _ (Ident "error") _)) = do
    -- this is special because it always results in program ending
    return $ Just MVoid
  interpret (SExp loc (EApp _ (Ident _) exprs)) = do
    -- this is special because it always results in program ending
    res <- mapM interpret exprs -- todo remove to remove type checkign like int too big
    mapM_ (`nothingGuard` Err.InternalError loc) res
    return Nothing
  interpret (SExp loc expr) = do
    res <- interpret expr -- todo remove to remove type checkign like int too big
    nothingGuard res (Err.InternalError loc)
    return Nothing
  interpret (Incr _ (Ident id)) = do
    return Nothing
  interpret (Decr _ (Ident id)) = do
    return Nothing

instance Interpretable Expr where
  interpret (EVar _ (Ident id)) = do
    checkwhile <- isCheckingWhile
    if checkwhile
      then return $ Just MVoid
      else do
        res <- getVal id
        case res of
          Nothing -> return $ Just MVoid
          _ -> return res
  --return $ Just MVoid -- todo this if we don't want to keep track of variables
  interpret (ELitInt pos val) = do
    intRangeGuard val pos
    return $ Just (MInt (fromInteger val))
  interpret ELitTrue {} = do return $ Just (MBool True)
  interpret ELitFalse {} = do return $ Just (MBool False)
  interpret (EApp loc (Ident "printInt") exprs) = do
    -- I think this is unreachable because of the case for Sexp Sapp above
    res <- mapM interpret exprs -- todo remove to remove type checkign like int too big
    mapM_ (`nothingGuard` Err.InternalError loc) res
    return $ Just MVoid
  interpret (EApp loc (Ident "printString") exprs) = do
    -- I think this is unreachable because of the case for Sexp Sapp above
    res <- mapM interpret exprs -- todo remove to remove type checkign like int too big
    mapM_ (`nothingGuard` Err.InternalError loc) res
    return $ Just MVoid
  interpret (EApp loc (Ident "readInt") exprs) = do
    return $ Just MVoid
  interpret (EApp loc (Ident "readString") exprs) = do
    return $ Just MVoid
  interpret (EApp loc (Ident id) exprs) = do
    res <- mapM interpret exprs -- todo remove to remove type checkign like int too big
    mapM_ (`nothingGuard` Err.InternalError loc) res
    StackInfo {..} <- gets getData
    if max_depth == length calls
      then return $ Just MVoid
      else do
        fn_def_m <- getVal id
        let fn_def@(MFun {..}) = fromJust fn_def_m
        inserts <- mapM getInsert (zip params exprs)
        pushPop env inserts (loc, id) (interpret instructions)
  interpret (EString _ s) = do return $ Just (MString s (length s))
  interpret (Neg _ expr) = do
    int_j <- interpret expr
    return $ neg int_j
  interpret (Not _ expr) = do
    bool_j <- interpret expr
    return $ mynot bool_j
  interpret (EMul _ expl Times {} expr) = do
    l <- interpret expl
    r <- interpret expr
    return $ l `times` r
  interpret (EMul loc expl Div {} expr) = do
    l <- interpret expl
    r <- interpret expr
    zeroGuard r (Err.DivByZero loc)
    return $ l `mydiv` r
  interpret (EMul loc expl Mod {} expr) = do
    l <- interpret expl
    r <- interpret expr
    zeroGuard r (Err.ModZero loc)
    return $ l `mymod` r
  interpret (EAdd _ expl Plus {} expr) = do
    l <- interpret expl
    r <- interpret expr
    return $ l `plus` r
  interpret (EAdd _ expl Minus {} expr) = do
    l <- interpret expl
    r <- interpret expr
    return $ l `minus` r
  interpret (ERel _ expl LTH {} expr) = do
    l <- interpret expl
    r <- interpret expr
    return $ lt l r
  interpret (ERel _ expl LE {} expr) = do
    l <- interpret expl
    r <- interpret expr
    return $ lte l r
  interpret (ERel _ expl GTH {} expr) = do
    l <- interpret expl
    r <- interpret expr
    return $ gt l r
  interpret (ERel _ expl GE {} expr) = do
    l <- interpret expl
    r <- interpret expr
    return $ gte l r
  interpret (ERel _ expl EQU {} expr) = do
    l_loc_m <- tryFindLoc expl
    r_loc_m <- tryFindLoc expr
    l <- interpret expl
    r <- interpret expr
    return $ eq l r
  interpret (ERel pos expl (NE pos') expr) = do
    l <- interpret expl
    r <- interpret expr
    return . mynot $ eq l r
  interpret (EAnd _ expl expr) = do
    l <- interpret expl
    r <- interpret expr
    return $ myand l r
  interpret (EOr _ expl expr) = do
    l <- interpret expl
    r <- interpret expr
    return $ myor l r

getInsert :: (MArgs, Expr) -> Traverser (Id, Either Loc MResVal)
getInsert ((MARef id), expr) = do
  loc_m <- tryFindLoc expr
  return $ (id, Left (fromJust loc_m))
getInsert ((MAVal id), expr) = do
  val_m <- interpret expr
  return $ (id, Right val_m)

tryFindLoc :: Expr -> Traverser (Maybe Loc)
tryFindLoc _ = return Nothing

initGlobal :: FnDef' BNFC'Position -> ITraverser
initGlobal (FunDef _ _ (Ident id) args blk) = do
  params <- mapM getArg args
  addToGlobal id params blk
  return Nothing

getArg (ArgVal _ _ (Ident id)) = do
  return $ MAVal id

intRangeGuard :: MonadError StaticException m => Integer -> Err.ErrLoc -> m (Maybe a)
intRangeGuard val pos = do
  case compare val int_min of
    LT -> throwError $ Err.IntTooSmall pos val
    _ -> case compare val int_max of
      GT -> throwError $ Err.IntTooLarge pos val
      _ -> return Nothing

-- constructArray (acc:accs) _

zeroGuard (Just (MInt 0)) err = throwError err
zeroGuard _ _ = return Nothing

-- zeroGuard _ _ = error "problem with typecheck caused problem during interpretation"

leqZeroGuard val err = do
  case compare val 1 of
    LT -> throwError err
    _ -> return Nothing

ltZeroGuard val err = do
  case compare val 0 of
    LT -> throwError err
    _ -> return Nothing

notNothingGuard Nothing _ = do return ()
notNothingGuard _ err = do throwError error

addItem def_val (NoInit _ (Ident id)) = do
  addKeyVal id def_val
addItem _ (Init _ (Ident id) expr) = do
  val <- interpret expr
  addKeyVal id (fromJust val)

{-
class Convertable a where
  convert :: a -> ITraverser

instance Convertable FnDef where
    convert (FunDef _ t (Ident id) args blk)
    -}