{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
import Interpretation.MacchiatoVals
import Parsing.AbsMacchiato
import qualified Interpretation.Err as Err
import Interpretation.Traverser
import Data.Maybe (listToMaybe, catMaybes, fromJust)
import Mem.SymbolTable (Loc)
import Control.Monad.Except
import GHC.Float (expFloat)



class Interpretable a where
    interpret ::  a -> ITraverser


instance Interpretable Program where
    interpret (ProgramS loc fndefs) = do
        initProg
        mapM_ interpret fndefs
        interpret (EApp loc (UIdent "main") [])



instance Interpretable FnDef where
    interpret (FunDef _ _ (UIdent id) args blk) = do
        params <- mapM getArg args
        let mfun = MFun{env = [], params = params, instructions = blk}
        addKeyVal id mfun
        return Nothing

instance Interpretable Expr where
    interpret (EVar _ (UIdent id)) = do getVal id
    interpret (ENewArr loc t dim_accs dim_bra) = do
        return constructArray
    interpret (ELitInt pos val) = do 
        intRangeGuard val pos
        return $ Just (MInt (fromInteger val))
    interpret ELitTrue{} = do return $ Just (MBool True)
    interpret ELitFalse{} = do return $ Just (MBool False)
    interpret (EApp _ _ _) = do return Nothing
    interpret (EString _ s) = do return $ Just (MString s)
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


class Referenceable a where
    getRef :: a -> Traverser (Loc)



getArg (ArgVal _ _ (UIdent id)) = do
    return $ MAVal id
getArg (ArgRef _ _ (UIdent id)) = do
    return $ MARef id

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



{-
class Convertable a where
  convert :: a -> ITraverser

instance Convertable FnDef where
    convert (FunDef _ t (UIdent id) args blk)
    -}