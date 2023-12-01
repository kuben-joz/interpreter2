{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module StaticAnalysis.TypeCheck where

-- this includes all checks related to types

import Control.Monad.Except
import Control.Monad.State (MonadState (get, put), evalStateT)
import Data.Bifoldable (Bifoldable)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, listToMaybe, mapMaybe)
import Debug.Trace
import Mem.SymbolTable
import qualified Mem.SymbolTable as ST
import qualified Parsing.AbsMacchiato as AType
import StaticAnalysis.Err as Err
  ( ErrLoc,
    StaticException
      ( AssToUndeclaredVar,
        BadRetType,
        CallToUnderclaredFun,
        ForbiddenId,
        FuncNameCollision,
        IfElseTypeMistmatch,
        IncompatibleFunParams,
        IncompatibleTypeOp,
        IncompatibleTypeOpAdd,
        IncompatibleTypeOpAnd,
        IncompatibleTypeOpMul,
        IncompatibleTypeOpOr,
        IncompatibleTypeOpRel,
        InvalidKeyword,
        NoMain,
        NoReturn,
        RefFuncAsVar,
        RefMismatch,
        TypeMismatch,
        UseOfUndeclaredVar,
        VarAsFunc,
        VoidVar, VoidExplicitRet
      ),
  )
import StaticAnalysis.MacchiatoTypes
import StaticAnalysis.TCTraverser
import Util.FieldExtractors

forbiddenIds =
  [ "while",
    "return",
    "if",
    "else",
    -- "new", only after adding arrays
    "int",
    "string",
    "boolean",
    "void",
    "true",
    "false"
  ]

builtInFuncsTypes =
  [ makeType'' (MFun (makeType'' MVoid) [makeType'' MInt]),
    makeType'' (MFun (makeType'' MVoid) [makeType'' MString]),
    makeType'' (MFun (makeType'' MInt) []),
    makeType'' (MFun (makeType'' MString) []),
    makeType'' (MFun (makeType'' MVoid) [])
  ]

builtInFuncsNames =
  [ "printInt",
    "printString",
    "readInt",
    "readString",
    "error"
  ]

startTypeCheck :: AType.Program -> Either StaticException (Maybe MFType)
startTypeCheck prog = runExcept $ evalStateT (check Nothing prog) (initState ())

class Checkable e where
  check :: (Maybe MFType) -> e -> STraverser

makeMain :: MFType
makeMain = makeType (MFun (makeType'' MInt) []) 0 True

-- catch exception here
-- todo add built in functions to initenv
instance Checkable AType.Program where
  check _ prog@(AType.ProgramS pos defs) = do
    main <- getMain defs
    assertMain main
    mapM_ addBuiltInFuncs $ zip builtInFuncsNames builtInFuncsTypes
    mapM_ initEnv defs
    mapM_ (pushPop . check Nothing) defs
    return Nothing

instance Checkable AType.FnDef where
  check _ fn@(AType.FunDef pos t id'@(AType.Ident id) args blk) = do
    checkIllegalId id' pos
    mapM_ (check Nothing) args
    --      return $ fromMaybe (throwError $ Err.NoReturn pos id) (check (Just $ toMFT t) blk)
    res_type <- check (Just $ toMFT t) blk
    nonVoidGuard res_type (toMFT t) (Err.NoReturn pos id)
    return Nothing

instance Checkable AType.Arg where
  check _ arg@(AType.ArgVal pos t id'@(AType.Ident id)) = do
    checkIllegalId id' pos
    key_val <- addKeyVal id (toMFT arg)
    notNothingGuard key_val (Err.FuncNameCollision pos id)
    voidGuard (toMFT t) (Err.VoidVar pos)
    return Nothing

instance Checkable AType.Block where
  check t blk@(AType.FunBlock pos stmts) = do
    rets <- mapM (check t) stmts
    return $ listToMaybe $ catMaybes rets

instance Checkable AType.Stmt where
  check _ (AType.Empty pos) = do return Nothing
  check t (AType.BStmt pos blk) = do
    push $ check t blk
  check _ inst@(AType.Decl loc t1 items) = do
    let t1' = toMFT t1
    voidGuard t1' (Err.VoidVar loc)
    mapM_ (check $ Just $ toMFT t1) items
    return Nothing
  check _ (AType.Ass loc id'@(AType.Ident id) expr) = do
    checkIllegalId id' loc
    id_temp <- getVal id
    nothingGuard id_temp (Err.AssToUndeclaredVar loc id)
    let id_type = fromJust id_temp
    expr_temp <- check Nothing expr
    let expr_type = fromJust expr_temp
    assertType id_type expr_type loc
    return Nothing
  check _ (AType.Incr loc id'@(AType.Ident id)) = do
    checkIllegalId id' loc
    id_temp <- getVal id
    nothingGuard id_temp (Err.AssToUndeclaredVar loc id)
    let id_type = fromJust id_temp
    let expr_type = makeType'' MInt
    assertType id_type expr_type loc
    return Nothing
  check _ (AType.Decr loc id'@(AType.Ident id)) = do
    checkIllegalId id' loc
    id_temp <- getVal id
    nothingGuard id_temp (Err.AssToUndeclaredVar loc id)
    let id_type = fromJust id_temp
    let expr_type = makeType'' MInt
    assertType id_type expr_type loc
    return Nothing
  check (Just t) (AType.Ret pos expr) = do
    voidGuard t (Err.VoidExplicitRet pos)
    expr_temp <- check Nothing expr
    let expr_type = fromJust expr_temp
    if expr_type == t
      then return $ Just t
      else throwError $ Err.BadRetType pos t expr_type
  check (Just t) (AType.RetNone pos) = do
    let expr_type = makeType'' MVoid
    if expr_type == t
      then return $ Just t
      else throwError $ Err.BadRetType pos t expr_type
  check t (AType.Cond loc expr stmt) = do
    expr_temp <- check Nothing expr
    let expr_type = fromJust expr_temp
    assertType (makeType'' MBool) expr_type loc
    push $ check t stmt -- todo here check if always true
  check t (AType.CondElse loc expr stmt_if stmt_else) = do
    if_res <- check t (AType.Cond loc expr stmt_if)
    else_res <- push $ check t stmt_else
    case (if_res, else_res) of
      (Nothing, Nothing) -> return Nothing
      (Just _, Nothing) -> return if_res
      (Nothing, Just _) -> return else_res
      (Just a, Just b) -> assertType' a b (Err.IfElseTypeMistmatch loc a b)
  check t (AType.While loc expr stmt) = do
    expr_temp <- check Nothing expr
    let expr_type = fromJust expr_temp
    assertType (makeType'' MBool) expr_type loc
    push $ check t stmt
  check _ (AType.SExp loc expr) = do
    check Nothing expr
    return Nothing

instance Checkable AType.Item where
  check (Just t) (AType.NoInit pos id'@(AType.Ident id)) = do
    checkIllegalId id' pos
    prev_val <- addKeyVal id t
    notNothingGuard prev_val (Err.FuncNameCollision pos id)
    return Nothing
  check (Just t) (AType.Init pos id'@(AType.Ident id) expr) = do
    checkIllegalId id' pos
    expr_t <- check Nothing expr
    assertType t (fromJust expr_t) pos
    prev_val <- addKeyVal id t
    notNothingGuard prev_val (Err.FuncNameCollision pos id)
    return Nothing

instance Checkable AType.Expr where
  check _ (AType.EVar pos id'@(AType.Ident id)) = do
    checkIllegalId id' pos
    var_type <- getVal id
    nothingGuard var_type (Err.UseOfUndeclaredVar pos id)
    case var_type of
      Just (MFun {}, _) -> throwError $ Err.RefFuncAsVar pos id
      _ -> return var_type
  check _ (AType.EApp pos id'@(AType.Ident id) exps) = do
    checkIllegalId id' pos
    exp_ts <- mapM (check Nothing) exps
    let exp_ts2 = catMaybes exp_ts
    f_t <- getVal id
    nothingGuard f_t (Err.CallToUnderclaredFun pos id)
    let f_t1 = fromJust f_t
    case f_t1 of
      (MFun t' ts', _) -> do
        notEqGuard (length ts') (length exp_ts2) (Err.IncompatibleFunParams pos id f_t1 exp_ts2)
        if strictComp ts' exp_ts2
          then return $ Just t'
          else throwError $ Err.IncompatibleFunParams pos id f_t1 exp_ts2
      _ -> throwError $ Err.VarAsFunc pos id
  check _ (AType.ELitInt _ _) = do return $ Just (makeType'' MInt)
  check _ (AType.ELitTrue _) = do return $ Just (makeType'' MBool)
  check _ (AType.ELitFalse _) = do return $ Just (makeType'' MBool)
  check _ (AType.EString _ _) = do return $ Just (makeType'' MString)
  check _ op@(AType.Neg pos expr) = do oneOpCheck expr MInt 0 pos op
  check _ op@(AType.Not pos expr) = do oneOpCheck expr MBool 0 pos op
  check _ op@(AType.EMul pos exp1 opi exp2) = do
    t1t <- check Nothing exp1
    t2t <- check Nothing exp2
    let t1 = fromJust t1t
    let t2 = fromJust t2t
    notEqGuard t1 t2 $ Err.IncompatibleTypeOpMul pos opi t1 t2
    check (Just t1) opi
  check _ op@(AType.EAdd pos exp1 opi exp2) = do
    t1t <- check Nothing exp1
    t2t <- check Nothing exp2
    let t1 = fromJust t1t
    let t2 = fromJust t2t
    notEqGuard t1 t2 $ Err.IncompatibleTypeOpAdd pos opi t1 t2
    check (Just t1) opi
  check _ op@(AType.ERel pos exp1 opi exp2) = do
    t1t <- check Nothing exp1
    t2t <- check Nothing exp2
    let t1 = fromJust t1t
    let t2 = fromJust t2t
    notEqGuard t1 t2 $ Err.IncompatibleTypeOpRel pos opi t1 t2
    check (Just t1) opi
  check _ op@(AType.EAnd pos exp1 exp2) = do
    t1t <- check Nothing exp1
    t2t <- check Nothing exp2
    let t1 = fromJust t1t
    let t2 = fromJust t2t
    notEqGuard t1 t2 $ Err.IncompatibleTypeOpAnd pos t1 t2
    if fst t1 /= MBool
      then throwError $ Err.IncompatibleTypeOpAnd pos t1 t2
      else return $ Just t1
  check _ op@(AType.EOr pos exp1 exp2) = do
    t1t <- check Nothing exp1
    t2t <- check Nothing exp2
    let t1 = fromJust t1t
    let t2 = fromJust t2t
    notEqGuard t1 t2 $ Err.IncompatibleTypeOpOr pos t1 t2
    if fst t1 /= MBool
      then throwError $ Err.IncompatibleTypeOpOr pos t1 t2
      else return $ Just t1

instance Checkable AType.AddOp where
  check t op@(AType.Plus pos) = do
    let t1@(tt, m) = fromJust t
    case tt of
      MString -> return $ Just t1
      MInt -> return $ Just t1
      _ -> throwError $ Err.IncompatibleTypeOpAdd pos op t1 t1
  check t op = do
    let t1@(tt, m) = fromJust t
    case tt of
      MInt -> return $ Just t1
      _ -> throwError $ Err.IncompatibleTypeOpAdd (AType.hasPosition op) op t1 t1

instance Checkable AType.MulOp where
  check t op = do
    let t1@(tt, m) = fromJust t
    case tt of
      MInt -> return $ Just t1
      _ -> throwError $ Err.IncompatibleTypeOpMul (AType.hasPosition op) op t1 t1

instance Checkable AType.RelOp where
  check t op@(AType.EQU pos) = do return $ Just (makeType'' MBool)
  check t op@(AType.NE pos) = do return $ Just (makeType'' MBool)
  check t op = do
    let t1@(tt, m) = fromJust t
    case tt of
      MInt -> return $ Just (makeType'' MBool)
      _ -> throwError $ Err.IncompatibleTypeOpRel (AType.hasPosition op) op t1 t1

oneOpCheck expr target_t lim pos op = do
  expr_temp <- check Nothing expr
  let expr_t = fromJust expr_temp
  if fst expr_t == target_t
    then return $ Just expr_t
    else throwError $ Err.IncompatibleTypeOp pos op expr_t

checkIllegalId (AType.Ident id) pos = do
  if elem id forbiddenIds
    then throwError $ Err.ForbiddenId pos id
    else return Nothing

getMain :: [AType.FnDef] -> Traverser AType.FnDef
getMain fns = do
  let l = mapMaybe isMain fns
  case l of
    [] -> throwError Err.NoMain
    (mf : fs) -> return mf
  where
    isMain fn
      | getId fn == "main" = Just fn
      | otherwise = Nothing

assertMain :: AType.FnDef -> STraverser
assertMain fn = assertType makeMain (toMFT fn) (AType.hasPosition fn)

addBuiltInFuncs :: ([Char], MFType) -> STraverser
addBuiltInFuncs (funcId, funcType) = do
  s@SymTable {..} <- get
  put $
    SymTable
      { global_env = (M.insert funcId loc global_env),
        current_env = current_env,
        state = (M.insert loc funcType state),
        dat = dat,
        loc = loc + 1
      }
  return Nothing

initEnv :: AType.FnDef -> STraverser
initEnv f@(AType.FunDef pos ret_type id'@(AType.Ident id) args blk) = do
  checkIllegalId id' pos
  s@SymTable {..} <- get
  if M.member id global_env
    then throwError $ Err.FuncNameCollision pos id
    else
      put $
        SymTable
          { global_env = (M.insert id loc global_env),
            current_env = current_env,
            state = (M.insert loc (toMFT f) state),
            dat = dat,
            loc = loc + 1
          }
  return Nothing

--storeProcType :: Typable a => Id -> a -> SymTable MFType d -> SymTable MFType d
--storeProcType id fndef s@SymTable{..} =
--   SymTable{proc_env = M.insert id loc proc_env, val_env=val_env ,
--   state = M.insert loc (toMFT fndef) state , dat = dat , loc = loc + 1}

assertType :: MFType -> MFType -> ErrLoc -> STraverser
assertType expected actual loc =
  if expected == actual
    then return $ Just expected
    else throwError $ TypeMismatch loc expected actual

assertTypeAndRef :: MFType -> MFType -> ErrLoc -> STraverser
assertTypeAndRef expected@(_, emod) act@(_, amod) loc = do
  assertType expected act loc
  if has_ref emod == has_ref amod
    then return $ Just act
    else throwError $ Err.RefMismatch loc expected act

assertType' :: MFType -> MFType -> StaticException -> STraverser
assertType' expected actual err =
  if expected == actual
    then return $ Just expected
    else throwError err

nothingGuard Nothing err = do throwError err
nothingGuard _ _ = do return ()

notNothingGuard Nothing _ = do return ()
notNothingGuard _ err = do throwError err

funGuard ((MFun _ _), _) err = do throwError err
funGuard _ _ = do return ()

voidGuard (MVoid, _) err = do throwError err
voidGuard _ _ = do return ()

nonVoidGuard Nothing (MVoid, _) _ = do return ()
nonVoidGuard (Just (MVoid, _)) (MVoid, _) _ = do return ()
nonVoidGuard _ (MVoid, _) err = do throwError err
nonVoidGuard Nothing _ err = do throwError err
nonVoidGuard _ _ _ = do return ()

notEqGuard t1 t2 err = do
  if t1 == t2
    then return Nothing
    else throwError err

makeType :: MType -> Int -> Bool -> MFType
makeType t dim ref = (t, MTypeMods dim ref False)

makeType' :: MType -> Int -> MFType
makeType' t dim = makeType t dim False

makeType'' :: MType -> MFType
makeType'' t = makeType t 0 False