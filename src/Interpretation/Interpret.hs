{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Interpretation.Interpret where

import Control.Monad.Except
import Control.Monad.State
import Data.Maybe (catMaybes, fromJust, listToMaybe)
import GHC.Float (expFloat)
import qualified Interpretation.Err as Err
import Interpretation.MacchiatoVals
import Interpretation.Traverser
import Mem.SymbolTable (Id, Loc, SymTable (current_env))
import Parsing.AbsMacchiato
import System.IO

class Interpretable a where
  interpret :: a -> ITraverser

--startInterpret :: Program -> IO (Either Err.RuntimeException (Maybe MVal))
startInterpret :: Interpretable a => a -> IO (Either Err.RuntimeException (Maybe MVal))
startInterpret prog = runExceptT $ evalStateT (interpret prog) (initProg)

instance Interpretable Program where
  interpret (ProgramS loc fndefs) = do
    mapM_ initGlobal fndefs
    res_val <- interpret (EApp loc (UIdent "main") [])
    let res_message =  "\nProgram terminated with exit code: " ++ (show (fromJust res_val))
    liftIO $ putStrLn res_message
    return res_val

instance Interpretable FnDef where
  interpret (FunDef _ _ (UIdent id) args blk) = do
    --todo this is we need to do a separate function for external
    params <- mapM getArg args
    current_env <- getEnv
    let mfun = MFun {env = current_env, params = params, instructions = blk}
    addKeyVal id mfun
    return Nothing

instance Interpretable Block where
  -- we push the stack before getting here so we don't do it here
  interpret (FunBlock _ stmts) = interpretStatements stmts

interpretStatements [] = return Nothing
interpretStatements s_arr@(BStmt {} : stmts) = nestedInterpret s_arr
interpretStatements ((Ret _ expr) : _) = do
  interpret expr
interpretStatements s_arr@(Cond {} : stmts) = nestedInterpret s_arr
interpretStatements s_arr@(CondElse {} : stmts) = nestedInterpret s_arr
interpretStatements s_arr@(While {} : stmts) = nestedInterpret s_arr
interpretStatements (stmt@(Break _) : _) = do
  interpret stmt
  return Nothing
interpretStatements (stmt@(Cont _) : _) = do
  interpret stmt
  return Nothing
interpretStatements (stmt : stmts) = do
  interpret stmt
  interpretStatements stmts

nestedInterpret (stmt : stmts) = do
  res <- interpret stmt
  brk_or_cont <- brkOrContCalled
  -- debug could do a check on res being nothing here
  -- todo I dont think I need this
  if brk_or_cont
    then return res
    else case res of
      Nothing -> interpretStatements stmts
      _ -> return res
nestedInterpret [] = undefined

instance Interpretable Stmt where
  interpret Empty {} = return Nothing
  --todo this seems to call stmt instead of blick
  interpret (BStmt _ block) = do
    pushPop' $ interpret block
  interpret (FunStmt _ fun_def) = do
    interpret fun_def
  interpret (Decl _ t items) = do
    let def_val = toDefValue t
    mapM_ (addItem def_val) items
    return Nothing
  interpret (Ass _ (UIdent id) expr) = do
    val <- interpret expr
    addOrModifyKeyVal id (fromJust val)
    return Nothing
  interpret (ArrAss _ (UIdent id) dimaccs expr) = do
    val <- interpret expr
    arr <- getVal id
    loc <- getArrLoc (fromJust arr) dimaccs
    addLocVal' loc (fromJust val)
  interpret (Ret _ expr) = do
    interpret expr
  interpret (Cond _ expr stmt) = do
    bool_m <- interpret expr
    if fromMVal $ fromJust bool_m
      then interpret stmt
      else return Nothing
  interpret (CondElse _ expr stmt_t stmt_f) = do
    bool_m <- interpret expr
    if fromMVal $ fromJust bool_m
      then interpret stmt_t
      else interpret stmt_f
  interpret stmt'@(While _ expr stmt) = do
    bool_m <- interpret expr
    if not $ (fromMVal . fromJust) bool_m
      then return Nothing
      else do
        res <- interpret stmt
        case res of
          Just a -> return res
          Nothing -> checkBrkCont stmt'
  interpret (SExp _ expr) = do interpret expr
  interpret (Print _ params) = do
    mapM_ interpret params
    liftIO $ hFlush stdout
    return Nothing
  interpret Cont {} = do
    getAndSetCont True
    return Nothing
  interpret Break {} = do
    getAndSetBrk True
    return Nothing

addItem def_val (NoInit _ (UIdent id)) = do
  addKeyVal id def_val
addItem _ (Init _ (UIdent id) expr) = do
  val <- interpret expr
  addKeyVal id (fromJust val)

checkBrkCont stmt = do
  res@(cont, brk) <- getAndSetContBrk False False
  case res of
    (True, True) -> undefined
    (True, _) -> interpret stmt
    (_, True) -> return Nothing
    (_, _) -> undefined

instance Interpretable PrintParam where
  interpret (FunPrintParam _ expr) = do
    res <- interpret expr
    liftIO $ putStr (show.fromJust $ res)
    return Nothing


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
      (KeyWordLength _) -> return $ Just (MInt (getLen val))
      (KeyWordMaxVal _) -> return $ Just (MInt maxBound)
      (KeyWordMinVal _) -> return $ Just (MInt minBound)
      (KeyWordDimNum _) -> return $ Just (MInt (getDimNum val))
  interpret (EArrKeyWord loc (UIdent id) dimaccs keyw) = do
    val_m <- interpret (EArrAcc loc (UIdent id) dimaccs)
    let val = fromJust val_m
    case keyw of
      (KeyWordLength _) -> return $ Just (MInt (getLen val))
      (KeyWordMaxVal _) -> return $ Just (MInt maxBound)
      (KeyWordMinVal _) -> return $ Just (MInt minBound)
      (KeyWordDimNum _) -> return $ Just (MInt (getDimNum val))
  interpret (ELitInt pos val) = do
    intRangeGuard val pos
    return $ Just (MInt (fromInteger val))
  interpret ELitTrue {} = do return $ Just (MBool True)
  interpret ELitFalse {} = do return $ Just (MBool False)
  interpret (EApp _ (UIdent id) exprs) = do
    fn_def_m <- getVal id
    let fn_def@(MFun {..}) = fromJust fn_def_m
    inserts <- mapM getInsert (zip params exprs)
    pushPop env inserts (interpret instructions)
  interpret (EString _ s) = do return $ Just (MString s (length s))
  interpret (Neg _ expr) = do
    int_j <- interpret expr
    return $ Just (- (fromJust int_j))
  interpret (Not _ expr) = do
    bool_j <- interpret expr
    return $ Just ((!) (fromJust bool_j))
  interpret (EMul _ expl Times {} expr) = do
    l <- interpret expl
    r <- interpret expr
    return . Just $ (fromJust l) * (fromJust r)
  interpret (EMul loc expl Div {} expr) = do
    l <- interpret expl
    r <- interpret expr
    zeroGuard (fromJust r) (Err.DivByZero loc)
    return . Just $ (fromJust l) `div` (fromJust r)
  interpret (EMul loc expl Mod {} expr) = do
    l <- interpret expl
    r <- interpret expr
    zeroGuard (fromJust r) (Err.ModZero loc)
    return . Just $ (fromJust l) `mod` (fromJust r)
  interpret (EAdd _ expl Plus {} expr) = do
    l <- interpret expl
    r <- interpret expr
    return . Just $ (fromJust l) + (fromJust r)
  interpret (EAdd _ expl Minus {} expr) = do
    l <- interpret expl
    r <- interpret expr
    return . Just $ (fromJust l) - (fromJust r)
  interpret (ERel _ expl LTH {} expr) = do
    l <- interpret expl
    r <- interpret expr
    return . Just . MBool $ (fromJust l) < (fromJust r)
  interpret (ERel _ expl LE {} expr) = do
    l <- interpret expl
    r <- interpret expr
    return . Just . MBool $ (fromJust l) <= (fromJust r)
  interpret (ERel _ expl GTH {} expr) = do
    l <- interpret expl
    r <- interpret expr
    return . Just . MBool $ (fromJust l) > (fromJust r)
  interpret (ERel _ expl GE {} expr) = do
    l <- interpret expl
    r <- interpret expr
    return . Just . MBool $ (fromJust l) >= (fromJust r)
  -- todo add array loc checking
  interpret (ERel _ expl EQU {} expr) = do
    l_loc_m <- tryFindLoc expl
    r_loc_m <- tryFindLoc expr
    l <- interpret expl
    r <- interpret expr
    case (l, r) of
      (Just (MArr {}), _) -> return . Just . MBool $ l_loc_m == r_loc_m
      _ -> do return . Just . MBool $ (fromJust l) == (fromJust r)
  interpret (ERel pos expl (NE pos') expr) = do
    eq_res <- interpret (ERel pos expl (EQU pos') expr)
    return $ (Just . (!) . fromJust) eq_res
  interpret (EAnd _ expl expr) = do
    l <- interpret expl
    r <- interpret expr
    return . Just . MBool $ (fromMVal . fromJust) l && (fromMVal . fromJust) r
  interpret (EOr _ expl expr) = do
    l <- interpret expl
    r <- interpret expr
    return . Just . MBool $ (fromMVal . fromJust) l || (fromMVal . fromJust) r


getInsert :: (MArgs, Expr) -> Traverser (Id, Either Loc MVal)
getInsert ((MARef id), expr) = do
  loc_m <- tryFindLoc expr
  return $ (id, Left (fromJust loc_m))
getInsert ((MAVal id), expr) = do
  val_m <- interpret expr
  return $ (id, Right (fromJust val_m))

tryFindLoc :: Expr -> Traverser (Maybe Loc)
tryFindLoc (EVar pos (UIdent id)) = do
  p@(loc_m, val_m) <- getLocVal id
  return loc_m
tryFindLoc _ = return Nothing

-- todo same for arr access

constructArray t (as@(EDimAcc pos expr) : accs) bs = do
  l_m <- interpret expr
  let l = (fromEnum . fromJust) l_m
  leqZeroGuard l (Err.ArrDimLTZero pos l)
  let d = length as + length bs
  res_elems <- mapM (insertArr (toDefValue t) accs) [d -1 | x <- [1 .. l]]
  return $ MArr {elems = res_elems, len = l, dim_num = d}
-- According to the grammar it can't be ampty, have to declare at least one dimension
constructArray t [] bs = undefined

--todo check that I am passign the right dim_level maybe -1 form what it is
-- todo check I give arrays hasref at arracc

insertArr def_val [] 0 = do
  addLocVal def_val
insertArr _ [] dim_num = do
  addLocVal MArr {elems = [], len = 0, dim_num = dim_num}
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
insertArr def_val (acc@(EDimAcc pos expr) : accs) dim_num = do
  l_m <- interpret expr
  let l = (fromEnum . fromJust) l_m
  leqZeroGuard l (Err.ArrDimLTZero pos l)
  res_elems <- mapM (insertArr def_val accs) [dim_num -1 | x <- [1 .. l]]
  addLocVal MArr {elems = res_elems, len = l, dim_num = dim_num}

getArr :: MVal -> [DimAcc] -> ([Loc] -> Int -> Traverser a) -> Traverser a
getArr (MArr {..}) ((EDimAcc loc expr) : []) f = do
  i_m <- interpret expr
  let i = (fromEnum . fromJust) i_m
  if i >= len
    then throwError $ Err.ArrOutOfBounds loc i len
    else f elems i
getArr (MArr {..}) ((EDimAcc loc expr) : accs) f = do
  i_m <- interpret expr
  let i = (fromEnum . fromJust) i_m
  if i >= len
    then throwError $ Err.ArrOutOfBounds loc i len
    else do
      next_arr_m <- getVal' $ Just (elems !! i)
      getArr (fromJust next_arr_m) accs f
-- getVal' $ Just (elems !! i)
getArr _ _ _ = undefined

getArrVal mval dimaccs = getArr mval dimaccs getter
  where
    getter elems i = getVal' $ Just (elems !! i)

getArrLoc mval dimaccs = getArr mval dimaccs getter
  where
    getter elems i = return (elems !! i)

initGlobal (FunDef _ _ (UIdent id) args blk) = do
  params <- mapM getArg args
  addToGlobal id params blk

getArg (ArgVal _ _ (UIdent id)) = do
  return $ MAVal id
getArg (ArgRef _ _ (UIdent id)) = do
  return $ MARef id


-- todo check if the symbol was GT
intRangeGuard val pos = do
  case compare val (toInteger (minBound :: Int)) of
    LT -> throwError $ Err.IntTooSmall pos val
    _ -> case compare val (toInteger (maxBound :: Int)) of
      GT -> throwError $ Err.IntTooLarge pos val
      _ -> return Nothing

-- constructArray (acc:accs) _ =

zeroGuard (MInt 0) err = throwError err
zeroGuard MInt {} _ = return Nothing
zeroGuard _ _ = error "problem with typecheck caused problem during interpretation"

leqZeroGuard val err = do
  case compare val 1 of
    LT -> throwError err
    _ -> return Nothing

{-
class Convertable a where
  convert :: a -> ITraverser

instance Convertable FnDef where
    convert (FunDef _ t (UIdent id) args blk)
    -}