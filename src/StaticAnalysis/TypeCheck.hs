{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
module StaticAnalysis.TypeCheck where

-- this includes all checks related to types

import qualified Mem.SymbolTable as ST


import qualified Parsing.AbsMacchiato as AType
import Util.FieldExtractors
import StaticAnalysis.Traverser
import StaticAnalysis.Err as Err
import Mem.SymbolTable
import StaticAnalysis.MacchiatoTypes
import qualified Data.Map as M
import Control.Monad.Except
import Control.Monad.State
import Data.Maybe


startTypeCheck :: AType.Program -> Either StaticException (Maybe MFType)
startTypeCheck prog = runExcept $ evalStateT (check Nothing prog) (initState ())

class Checkable e where
    check :: (Maybe MFType) -> e -> STraverser

makeMain :: MFType
makeMain = makeType'' (MFun (makeType'' MInt) [])

-- catch exception here
instance Checkable AType.Program where
    check _ prog@(AType.ProgramS pos defs) = do
        main <- getMain defs
        assertMain main
        mapM_ initEnv defs
        mapM_ (pushPop . check Nothing) defs
        return Nothing


instance Checkable AType.FnDef where
    check _ fn@(AType.FunDef pos t (AType.UIdent id) args blk) = do
        mapM_ (check Nothing) args
  --      return $ fromMaybe (throwError $ Err.NoReturn pos id) (check (Just $ toMFT t) blk)
        res_type <- check (Just $ toMFT t) blk
        nothingGuard res_type (Err.NoReturn pos id)
        -- todo check this should be ret nothing and not res_type
        return Nothing

instance Checkable AType.Arg where
    check _ arg@(AType.ArgVal pos t (AType.UIdent id)) = do
        key_val <- addKeyVal id (toMFT arg)
        notNothingGuard key_val (Err.FuncNameCollision pos id)
        return Nothing
    check _ arg@(AType.ArgRef pos t (AType.UIdent id)) = do
        key_val <- addKeyVal id (toMFT arg)
        notNothingGuard key_val (Err.FuncNameCollision pos id)
        return Nothing



instance Checkable AType.Block where
    check t blk@(AType.FunBlock pos stmts) = do
        rets <- mapM (check t) stmts
        return $ listToMaybe $ catMaybes rets


instance Checkable AType.Stmt where
    check _ (AType.Empty pos) = do return Nothing
    check t (AType.BStmt pos blk) = do push $ check t blk
    check _ (AType.FunStmt loc fn@(AType.FunDef pos t (AType.UIdent id) args blk)) = do
        prev_func <- addKeyVal id (toMFT fn)
        notNothingGuard prev_func (Err.FuncNameCollision pos id)
        push $ check Nothing fn
        return Nothing
    check _ (AType.Decl loc t1 items) = do
        mapM_ (check $ Just $ toMFT t1) items
        return Nothing
    check _ (AType.Ass loc (AType.UIdent id) expr) = do
        id_temp <- getVal id
        nothingGuard id_temp (Err.AssToUndeclaredVar loc id)
        let id_type = fromJust id_temp
        expr_temp <- check Nothing expr
        let expr_type = fromJust expr_temp
        if id_type == expr_type then return Nothing
        else throwError $ Err.TypeMismatch loc id_type expr_type
    check t (AType.ArrAss loc (AType.UIdent id) dimAcc expr) = do
        mapM_ (check t) dimAcc
        arr_type <- getVal id
        (adj_arr_type, arr_mods)  <- adjustArrType (fromJust arr_type) dimAcc
        case compare (dim_num arr_mods) 0 of
            LT -> do throwError $ Err.ArrTooShallow loc (dim_num arr_mods) (length dimAcc)
            _ -> do
                expr_temp <- check Nothing expr
                let expr_type = fromJust expr_temp
                assertType (adj_arr_type, arr_mods) expr_type loc
                return Nothing
    check (Just t) (AType.Ret pos expr) = do
        expr_temp <- check Nothing expr
        let expr_type = fromJust expr_temp
        if expr_type == t then return $ Just t
        else throwError $ Err.BadRetType pos t expr_type
    check t (AType.Cond loc expr stmt) = do
        expr_temp <- check Nothing expr
        let expr_type = fromJust expr_temp
        assertType (makeType'' MBool) expr_type loc
        push $ check t stmt
    check t (AType.CondElse loc expr stmt_if stmt_else) = do
        check t (AType.Cond loc expr stmt_if)
        push $ check t stmt_else
    check t (AType.While loc expr stmt) = do
        expr_temp <- check Nothing expr
        let expr_type = fromJust expr_temp
        assertType (makeType'' MBool) expr_type loc
        push $ check (setBrkCont t) stmt
    check _ (AType.SExp loc expr) = do check Nothing expr
    check _ (AType.Print loc print_params) = do
        mapM_ (check Nothing) print_params
        return Nothing
-- todo add chekcign for this
    check (Just (t, MTypeMods{..})) (AType.Break pos) = do 
        if can_brk_cont then return Nothing else 
            throwError $ Err.BrkInvalidPos pos
    check (Just (t, MTypeMods{..})) (AType.Cont pos) = do
        if can_brk_cont then return Nothing else 
            throwError $ Err.ContInvalidPos pos

instance Checkable AType.Item where
    check (Just t) (AType.NoInit pos (AType.UIdent id)) = do
        prev_val <- addKeyVal id t
        notNothingGuard prev_val (Err.FuncNameCollision pos id)
        return Nothing
    check (Just t) (AType.Init pos (AType.UIdent id) expr) = do
        expr_t <- check Nothing expr
        assertType t (fromJust expr_t) pos
        prev_val <- addKeyVal id t
        notNothingGuard prev_val (Err.FuncNameCollision pos id)
        return Nothing

instance Checkable AType.PrintParam where
    check _ (AType.FunPrintParam loc expr) = do
        expr_temp <- check Nothing expr
        let expr_t = fromJust expr_temp
        arrGuard expr_t (Err.IncompPrintParam loc expr_t)
        funGuard expr_t (Err.IncompPrintParam loc expr_t)
        return Nothing

-- todo check arr acc always has ref even if value as I am implementing it as locs

instance Checkable AType.Expr where
    check _ (AType.EVar pos (AType.UIdent id)) = do
        var_type <- getVal id
        nothingGuard var_type (Err.UseOfUndeclaredVar pos id)
        case var_type of
            Just (MFun{}, _) -> throwError $ Err.RefFuncAsVar pos id
            _                -> return var_type
    check _ (AType.ENewArr pos t dim_acc dim_bra) = do
        mapM_ (check Nothing) dim_acc
        return $ Just (fst $ toMFT t, MTypeMods{dim_num=length dim_acc+length dim_bra, has_ref = False})
    check _ (AType.EArrAcc pos (AType.UIdent id) dim_acc) = do
        mapM_ (check Nothing) dim_acc
        arr_t <- getVal id
        nothingGuard arr_t (Err.UseOfUndeclaredVar pos id)
        let arr_t1 = fromJust arr_t
        arr_t2 <- adjustArrType arr_t1 dim_acc
        case compare (dim_num (snd arr_t2)) 0 of
            LT -> do throwError $ Err.ArrTooShallow pos (dim_num (snd arr_t2)) (length dim_acc)
            _  -> do return $ Just (fst arr_t1, snd arr_t2)
    check _ (AType.EKeyWord pos (AType.UIdent id) keyword) = do
        ident_t <- getVal id
        nothingGuard ident_t (Err.UseOfUndeclaredVar pos id)
        if compatibleParent keyword (fromJust ident_t) then return $ Just (toMFT keyword)
        else throwError $ Err.InvalidKeyword (AType.hasPosition keyword)
    check _ (AType.EArrKeyWord pos (AType.UIdent id) dim_acc keyword) = do
        mapM_ (check $ Just (makeType'' MInt)) dim_acc
        arr_t <- getVal id
        nothingGuard arr_t (Err.UseOfUndeclaredVar pos id)
        let arr_t1 = fromJust arr_t
        arr_t2 <- adjustArrType arr_t1 dim_acc
        case compare (dim_num (snd arr_t2)) 0 of
            LT -> do throwError $ Err.ArrTooShallow pos (dim_num (snd arr_t2)) (length dim_acc)
            _  -> do
                if compatibleParent keyword arr_t2 then return $ Just (toMFT keyword)
                else throwError $ Err.InvalidKeyword pos
    check _ (AType.EApp pos (AType.UIdent id) exps) = do
        exp_ts <- mapM (check Nothing) exps
        let exp_ts2 = catMaybes exp_ts
        f_t <- getVal id
        nothingGuard f_t (Err.CallToUnderclaredFun pos id)
        let Just f_t1@(t, mods) = f_t
        case t of
            (MFun t' ts') -> if strictComp ts' exp_ts2 then return $ Just t'
                           else throwError $ Err.IncompatibleFunParams pos id f_t1 exp_ts2
    check _ (AType.ELitInt _ _) = do return $ Just (makeType'' MInt)
    check _ (AType.ELitTrue _ ) = do return $ Just (makeType'' MBool)
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
        arrGuard t1 $ Err.IncompatibleTypeOpAndOr pos op t1 t2
        check (Just t1) opi
    check _ op@(AType.EAdd pos exp1 opi exp2) = do
        t1t <- check Nothing exp1
        t2t <- check Nothing exp2
        let t1 = fromJust t1t
        let t2 = fromJust t2t
        notEqGuard t1 t2 $ Err.IncompatibleTypeOpAdd pos opi t1 t2
        arrGuard t1 $ Err.IncompatibleTypeOpAndOr pos op t1 t2
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
        notEqGuard t1 t2 $ Err.IncompatibleTypeOpAndOr pos op t1 t2
        arrGuard t1 $ Err.IncompatibleTypeOpAndOr pos op t1 t2
        if fst t1 /= MBool then throwError $ Err.IncompatibleTypeOpAndOr pos op t1 t2
        else return $ Just t1
    check _ op@(AType.EOr pos exp1 exp2) = do
        t1t <- check Nothing exp1
        t2t <- check Nothing exp2
        let t1 = fromJust t1t
        let t2 = fromJust t2t
        notEqGuard t1 t2 $ Err.IncompatibleTypeOpAndOr pos op t1 t2
        arrGuard t1 $ Err.IncompatibleTypeOpAndOr pos op t1 t2
        if fst t1 /= MBool then throwError $ Err.IncompatibleTypeOpAndOr pos op t1 t2
        else return $ Just t1


-- todo check arr guards

instance Checkable AType.AddOp where
    check t op@(AType.Plus pos) = do
        let t1@(tt, m) = fromJust t
        case tt of
            MString -> return $ Just t1
            MInt    -> return $ Just t1
            _       -> throwError $ Err.IncompatibleTypeOpAdd pos op t1 t1
    check t op = do
        let t1@(tt, m) = fromJust t
        case tt of
            MInt    -> return $ Just t1
            _       -> throwError $ Err.IncompatibleTypeOpAdd (AType.hasPosition op) op t1 t1

instance Checkable AType.MulOp where
    check t op = do
        let t1@(tt, m) = fromJust t
        case tt of
            MInt -> return $ Just t1
            _   -> throwError $ Err.IncompatibleTypeOpMul (AType.hasPosition op) op t1 t1
{-
    check t op@(AType.Div pos) = do
        let t1@(tt, m) = fromJust t
        case tt of
            MInt -> return Nothing
            _   -> throwError $ Err.IncompatibleTypeOpMul pos op t1 t1
    check t op@(AType.Mod pos) = do
        let t1@(tt, m) = fromJust t
        case tt of
            MInt -> return $ Just MInt
            _   -> throwError $ Err.IncompatibleTypeOpMul pos op t1 t1

 -}

instance Checkable AType.RelOp where
    check t op@(AType.EQU pos) = do return $ Just (makeType'' MBool)
    check t op@(AType.NE pos) = do return $ Just (makeType'' MBool)
    check t op = do
        let t1@(tt, m) = fromJust t
        arrGuard t1 (Err.IncompatibleTypeOpRel (AType.hasPosition op) op t1 t1)
        case tt of
            MInt -> return $ Just t1
            _   -> throwError $ Err.IncompatibleTypeOpRel (AType.hasPosition op) op t1 t1






oneOpCheck expr target_t lim pos op = do
        expr_temp <- check Nothing expr
        let expr_t = fromJust expr_temp
    --    if fst expr_t == target_t && (dim_num (snd expr_t) <= lim) then
        if fst expr_t == target_t && (dim_num (snd expr_t) <= lim) then
            return $ Just expr_t
        else
            throwError $ Err.IncompatibleTypeOp pos op expr_t





instance Checkable AType.DimAcc where
    check _ (AType.EDimAcc loc expr) = do
        expr_temp <- check Nothing expr
        let expr_t = fromJust expr_temp
        assertType (makeType'' MInt) expr_t loc


getMain :: [AType.FnDef] -> Traverser AType.FnDef
getMain fns = do
    let l = mapMaybe isMain fns
    case l of
        [] -> throwError Err.NoMain
        (mf:fs) -> return mf
    where
        isMain fn   | getId fn == "main" = Just fn
                    | otherwise = Nothing


assertMain :: AType.FnDef -> STraverser
assertMain fn = assertType makeMain (toMFT fn) (AType.hasPosition fn)

-- todo change noMain
initEnv :: AType.FnDef -> STraverser
initEnv f@(AType.FunDef pos ret_type (AType.UIdent id) args blk) = do
    s@SymTable{..} <- get
    if M.member id global_env then throwError $ Err.FuncNameCollision pos id
    else
        put $ SymTable{global_env=(M.insert id loc global_env), current_env=current_env,
        state=(M.insert loc (toMFT f) state), dat=dat, loc= loc+1}
    return Nothing

--storeProcType :: Typable a => Id -> a -> SymTable MFType d -> SymTable MFType d
--storeProcType id fndef s@SymTable{..} =
 --   SymTable{proc_env = M.insert id loc proc_env, val_env=val_env ,
 --   state = M.insert loc (toMFT fndef) state , dat = dat , loc = loc + 1}



assertType :: MFType -> MFType -> ErrLoc -> STraverser
assertType expected actual loc =
    if expected == actual then return $ Just expected
    else throwError $ TypeMismatch loc expected actual
assertType _ _ _ = error "Illegal state in assert type"


assertTypeAndRef :: MFType -> MFType -> ErrLoc -> STraverser
assertTypeAndRef expected@(_,emod) act@(_,amod) loc = do
    assertType expected act loc
    if has_ref emod == has_ref amod then return $ Just act
    else throwError $ Err.RefMismatch loc expected act
assertTypeAndRef _ _ _ = error "Illegal state in assert type"


nothingGuard Nothing err = do throwError err
nothingGuard _       _   = do return ()

notNothingGuard Nothing _ = do return ()
notNothingGuard _       err   = do throwError err

funGuard ((MFun _ _), _) err = do throwError err
funGuard _               _   = do return ()

arrGuard (_, MTypeMods{..}) err = do
    case dim_num of
        0 -> return ()
        _ -> throwError err

notEqGuard t1 t2 err = do
    if t1 == t2 then return Nothing
    else throwError err




makeType :: MType -> Int -> Bool -> MFType
makeType t dim ref = (t, MTypeMods dim ref False)
makeType' :: MType -> Int -> MFType
makeType' t dim = makeType t dim False
makeType'' :: MType -> MFType
makeType'' t = makeType t 0 False