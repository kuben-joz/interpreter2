{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Interpretation.Interpret where

import Interpretation.MacchiatoVals
import Parsing.AbsMacchiato
import qualified Interpretation.Err as Err
import Interpretation.Traverser
import Data.Maybe (listToMaybe, catMaybes, fromJust)
import Mem.SymbolTable (Loc, SymTable, Id)
import Control.Monad.Except
import GHC.Float (expFloat)



class Interpretable a where
    interpret ::  a -> ITraverser


instance Interpretable Program where
    interpret (ProgramS loc fndefs) = do
        initProg
        mapM_ initGlobal fndefs
        interpret (EApp loc (UIdent "main") [])



instance Interpretable FnDef where
    interpret (FunDef _ _ (UIdent id) args blk) = do
        --todo this is we need to do a separate function for external
        params <- mapM getArg args
        let mfun = MFun{env = [], params = params, instructions = blk}
        addKeyVal id mfun
        return Nothing


instance Interpretable Block where
    interpret = undefined

instance Interpretable Expr where
    interpret (EVar _ (UIdent id)) = do getVal id
    interpret (ENewArr loc t dim_accs dim_bra) = do
        res_arr <- constructArray t dim_accs dim_bra
        return $ Just res_arr
    interpret (EArrAcc _ (UIdent id) dim_accs) = do
        arr <- getVal id
        getArrVal (fromJust arr) dim_accs
        --todo check I didn't forbid arr assingment
    interpret (EKeyWord _ (UIdent id) keyw) = do
        val_m <- getVal id
        let val = fromJust val_m
        case keyw of
            (KeyWordLength _) -> return $ Just(MInt (getLen val))
            (KeyWordMaxVal _ ) -> return $ Just (MInt maxBound)
            (KeyWordMinVal _ ) -> return $ Just (MInt minBound)
            (KeyWordDimNum _) -> return $ Just (MInt (getDimNum val))
    interpret (EArrKeyWord loc (UIdent id) dimaccs keyw) = do
        val_m <- interpret (EArrAcc loc (UIdent id) dimaccs)
        let val = fromJust val_m
        case keyw of
            (KeyWordLength _) -> return $ Just(MInt (getLen val))
            (KeyWordMaxVal _ ) -> return $ Just (MInt maxBound)
            (KeyWordMinVal _ ) -> return $ Just (MInt minBound)
            (KeyWordDimNum _) -> return $ Just (MInt (getDimNum val))
    interpret (ELitInt pos val) = do
        intRangeGuard val pos
        return $ Just (MInt (fromInteger val))
    interpret ELitTrue{} = do return $ Just (MBool True)
    interpret ELitFalse{} = do return $ Just (MBool False)
    interpret (EApp _ (UIdent id) exprs) = do
        fn_def_m <- getVal id
        let fn_def@(MFun{..}) = fromJust fn_def_m
        inserts <- mapM getInsert (zip params exprs)
        pushPop env inserts (interpret instructions)
    interpret (EString _ s) = do return $ Just (MString s (length s))
    interpret (Neg _ expr) = do
        int_j <- interpret expr
        return $ Just (-(fromJust int_j))
    interpret (Not _ expr) = do
        bool_j <- interpret expr
        return $ Just ((!)(fromJust bool_j))
    interpret (EMul _ expl Times{} expr) = do
        l <- interpret expl
        r <- interpret expr
        return . Just $ (fromJust l) * (fromJust r)
    interpret (EMul loc expl Div{} expr) = do
        l <- interpret expl
        r <- interpret expr
        zeroGuard (fromJust r) (Err.DivByZero loc)
        return . Just $ (fromJust l) `div` (fromJust r)
    interpret (EAdd _ expl Plus{} expr) = do
        l <- interpret expl
        r <- interpret expr
        return . Just $ (fromJust l) + (fromJust r)
    interpret (EAdd _ expl Minus{} expr) = do
        l <- interpret expl
        r <- interpret expr
        return . Just $ (fromJust l) - (fromJust r)
    interpret (ERel _ expl LTH{} expr) = do
        l <- interpret expl
        r <- interpret expr
        return . Just . MBool $ (fromJust l) < (fromJust r)
    interpret (ERel _ expl LE{} expr) = do
        l <- interpret expl
        r <- interpret expr
        return . Just . MBool $ (fromJust l) <= (fromJust r)
    interpret (ERel _ expl GTH{} expr) = do
        l <- interpret expl
        r <- interpret expr
        return . Just . MBool $ (fromJust l) > (fromJust r)
    interpret (ERel _ expl GE{} expr) = do
        l <- interpret expl
        r <- interpret expr
        return . Just . MBool $ (fromJust l) >= (fromJust r)
-- todo add array loc checking
    interpret (ERel _ expl EQU{} expr) = do
        l_loc_m <- tryFindLoc expl
        r_loc_m <- tryFindLoc expr
        case (l_loc_m, r_loc_m) of
            (Just l_loc, Just r_loc) -> return . Just . MBool $ l_loc == r_loc
            _                        -> do
                l <- interpret expl
                r <- interpret expr
                return . Just . MBool $ (fromJust l) == (fromJust r)
    interpret (ERel pos expl (NE pos') expr) = do
        eq_res <- interpret (ERel pos expl (EQU pos') expr)
        return $ (Just.(!).fromJust) eq_res
    interpret (EAnd _ expl expr) = do
        l <- interpret expl
        r <- interpret expr
        return . Just . MBool $ (fromMVal . fromJust) l && (fromMVal . fromJust) r
    interpret (EOr _ expl expr) = do
        l <- interpret expl
        r <- interpret expr
        return . Just . MBool $ (fromMVal . fromJust) l || (fromMVal . fromJust) r


tryFindLoc :: Expr -> Traverser (Maybe Loc)
tryFindLoc (EVar pos (UIdent id)) = do
    (loc_m, val_m) <- getLocVal id
    case (loc_m, val_m) of
        (Nothing, _)            -> return Nothing
        (Just loc, Just MArr{}) -> return $ Just loc
        _                       -> return Nothing
tryFindLoc _ = do return Nothing
-- todo same for arr access


constructArray t (as@(EDimAcc pos expr):accs) bs = do
    l_m <- interpret expr
    let l = (fromEnum.fromJust) l_m
    leqZeroGuard l (Err.ArrDimLEQZero pos l)
    let d = length as + length bs
    res_elems <- mapM (insertArr (toDefValue t) accs) [d-1 | x <- [1..l]]
    return $ MArr{elems = res_elems, len=l, dim_num=d}
-- According to the grammar it can't be ampty, have to declare at least one dimension
--constructArray t [] (bra:bras) = do

--todo check that I am passign the right dim_level maybe -1 form what it is
-- todo check I give arrays hasref at arracc

insertArr def_val [] 0 = do
    addLocVal def_val
insertArr _ [] dim_num = do
    addLocVal MArr{elems=[], len = 0, dim_num= dim_num}
    {-
insertArr def_val (acc@(EDimAcc pos expr):[]) 1 = do
    l_m <- interpret expr
    let l = (fromEnum.fromJust) l_m
    leqZeroGuard l (Err.ArrDimLEQZero pos l)
    res_elems <- mapM addLocVal [def_val | x <- [1..l]]
    addLocVal MArr{elems= res_elems, len=l, dim_num= 1}

insertArr def_val (acc@(EDimAcc pos expr):[]) dim_num = do
    l_m <- interpret expr
    let l = (fromEnum.fromJust) l_m
    leqZeroGuard l (Err.ArrDimLEQZero pos l)
    res_elems <- mapM addLocVal [MArr{elems = [], len=0, dim_num=dim_num-1} | x <- [1..l]]
    addLocVal MArr{elems=res_elems, len=l, dim_num=dim_num}
    -}
{- this is degenerate case I think
insertArr def_val (acc@(EDimAcc pos expr):accs) 0 = do
    l_m <- interpret expr
    let l = (fromEnum.fromJust) l_m
    leqZeroGuard l (Err.ArrDimLEQZero pos l)
    res_elems <- mapM (insertArr def_val accs
-}
insertArr def_val (acc@(EDimAcc pos expr):accs) dim_num = do
    l_m <- interpret expr
    let l = (fromEnum.fromJust) l_m
    leqZeroGuard l (Err.ArrDimLEQZero pos l)
    res_elems <- mapM (insertArr def_val accs) [dim_num-1 | x <- [1..l]]
    addLocVal MArr{elems=res_elems, len=l, dim_num=dim_num}



getArrVal :: MVal -> [DimAcc] -> ITraverser
getArrVal (MArr{..}) ((EDimAcc loc expr):[]) = do
    i_m <- interpret expr
    let i = (fromEnum . fromJust) i_m
    if i >= len then throwError $ Err.ArrOutOfBounds loc i len
    else
        getVal' $ Just (elems !! i)
getArrVal (MArr{..}) ((EDimAcc loc expr):accs) = do
    i_m <- interpret expr
    let i = (fromEnum . fromJust) i_m
    if i >= len then throwError $ Err.ArrOutOfBounds loc i len
    else do
        next_arr_m <- getVal' $ Just (elems !! i)
        getArrVal (fromJust next_arr_m) accs
       -- getVal' $ Just (elems !! i)



initGlobal (FunDef _ _ (UIdent id) args blk) = do
    params <- mapM getArg args
    addToGlobal id params blk

getArg (ArgVal _ _ (UIdent id)) = do
    return $ MAVal id
getArg (ArgRef _ _ (UIdent id)) = do
    return $ MARef id

getInsert :: (MArgs, Expr) -> Traverser (Id, Either Loc MVal)
getInsert ((MARef id), expr) = do
    loc_m <- tryFindLoc expr
    return $ (id, Left (fromJust loc_m))
getInsert ((MAVal id), expr) = do
    val_m <- interpret expr
    return $ (id, Right (fromJust val_m))



-- todo check if the symbol was GT
intRangeGuard val pos = do
    case compare val (toInteger (minBound :: Int)) of
        LT -> throwError $ Err.IntTooSmall pos (toInteger val)
        _  -> case compare val (toInteger (minBound :: Int)) of
            GT -> throwError $ Err.IntTooLarge pos (toInteger val)
            _  -> return Nothing
-- constructArray (acc:accs) _ = 

zeroGuard (MInt 0) err = throwError err
zeroGuard MInt{} _ = return Nothing
zeroGuard _ _ = error "problem with typecheck caused problem during interpretation"

leqZeroGuard val err = do
    case compare val 1 of
        LT -> throwError err
        _  -> return Nothing

{-
class Convertable a where
  convert :: a -> ITraverser

instance Convertable FnDef where
    convert (FunDef _ t (UIdent id) args blk)
    -}