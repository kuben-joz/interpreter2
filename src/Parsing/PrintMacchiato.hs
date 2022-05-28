-- File generated by the BNF Converter (bnfc 2.9.4).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for PrintMacchiato.

module Parsing.PrintMacchiato where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified AbsMacchiato

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print AbsMacchiato.UIdent where
  prt _ (AbsMacchiato.UIdent i) = doc $ showString i
instance Print (AbsMacchiato.Program' a) where
  prt i = \case
    AbsMacchiato.ProgramS _ fndefs -> prPrec i 0 (concatD [prt 0 fndefs])

instance Print (AbsMacchiato.FnDef' a) where
  prt i = \case
    AbsMacchiato.FunDef _ type_ uident args block -> prPrec i 0 (concatD [prt 0 type_, prt 0 uident, doc (showString "("), prt 0 args, doc (showString ")"), prt 0 block])

instance Print [AbsMacchiato.FnDef' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (AbsMacchiato.Arg' a) where
  prt i = \case
    AbsMacchiato.ArgVal _ type_ uident -> prPrec i 0 (concatD [prt 0 type_, prt 0 uident])
    AbsMacchiato.ArgRef _ type_ uident -> prPrec i 0 (concatD [prt 0 type_, doc (showString "&"), prt 0 uident])

instance Print [AbsMacchiato.Arg' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (AbsMacchiato.Block' a) where
  prt i = \case
    AbsMacchiato.FunBlock _ stmts -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stmts, doc (showString "}")])

instance Print [AbsMacchiato.Stmt' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (AbsMacchiato.Stmt' a) where
  prt i = \case
    AbsMacchiato.Empty _ -> prPrec i 0 (concatD [doc (showString ";")])
    AbsMacchiato.BStmt _ block -> prPrec i 0 (concatD [prt 0 block])
    AbsMacchiato.FunStmt _ fndef -> prPrec i 0 (concatD [prt 0 fndef])
    AbsMacchiato.Decl _ type_ items -> prPrec i 0 (concatD [prt 0 type_, prt 0 items, doc (showString ";")])
    AbsMacchiato.Ass _ uident expr -> prPrec i 0 (concatD [prt 0 uident, doc (showString "="), prt 0 expr, doc (showString ";")])
    AbsMacchiato.ArrAss _ uident dimaccs expr -> prPrec i 0 (concatD [prt 0 uident, prt 0 dimaccs, doc (showString "="), prt 0 expr, doc (showString ";")])
    AbsMacchiato.Ret _ expr -> prPrec i 0 (concatD [doc (showString "return"), prt 0 expr, doc (showString ";")])
    AbsMacchiato.Cond _ expr stmt -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt])
    AbsMacchiato.CondElse _ expr stmt1 stmt2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt1, doc (showString "else"), prt 0 stmt2])
    AbsMacchiato.While _ expr stmt -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt])
    AbsMacchiato.SExp _ expr -> prPrec i 0 (concatD [prt 0 expr, doc (showString ";")])
    AbsMacchiato.Print _ printparams -> prPrec i 0 (concatD [doc (showString "print"), prt 0 printparams, doc (showString ";")])
    AbsMacchiato.Break _ -> prPrec i 0 (concatD [doc (showString "break"), doc (showString ";")])
    AbsMacchiato.Cont _ -> prPrec i 0 (concatD [doc (showString "continue"), doc (showString ";")])

instance Print (AbsMacchiato.Item' a) where
  prt i = \case
    AbsMacchiato.NoInit _ uident -> prPrec i 0 (concatD [prt 0 uident])
    AbsMacchiato.Init _ uident expr -> prPrec i 0 (concatD [prt 0 uident, doc (showString "="), prt 0 expr])

instance Print [AbsMacchiato.Item' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (AbsMacchiato.PrintParam' a) where
  prt i = \case
    AbsMacchiato.FunPrintParam _ expr -> prPrec i 0 (concatD [doc (showString "<<"), prt 0 expr])

instance Print [AbsMacchiato.PrintParam' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (AbsMacchiato.Type' a) where
  prt i = \case
    AbsMacchiato.Int _ -> prPrec i 0 (concatD [doc (showString "int")])
    AbsMacchiato.Str _ -> prPrec i 0 (concatD [doc (showString "string")])
    AbsMacchiato.Bool _ -> prPrec i 0 (concatD [doc (showString "bool")])
    AbsMacchiato.Arr _ type_ dimbras -> prPrec i 0 (concatD [prt 0 type_, doc (showString "[]"), prt 0 dimbras])

instance Print (AbsMacchiato.DimBra' a) where
  prt i = \case
    AbsMacchiato.ModDimBra _ -> prPrec i 0 (concatD [doc (showString "[]")])

instance Print [AbsMacchiato.DimBra' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [AbsMacchiato.Type' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (AbsMacchiato.Expr' a) where
  prt i = \case
    AbsMacchiato.EVar _ uident -> prPrec i 6 (concatD [prt 0 uident])
    AbsMacchiato.ENewArr _ type_ dimaccs dimbras -> prPrec i 6 (concatD [doc (showString "new"), prt 0 type_, prt 0 dimaccs, prt 0 dimbras])
    AbsMacchiato.EArrAcc _ uident dimaccs -> prPrec i 6 (concatD [prt 0 uident, prt 0 dimaccs])
    AbsMacchiato.EKeyWord _ uident keyword -> prPrec i 6 (concatD [prt 0 uident, doc (showString "::"), prt 0 keyword])
    AbsMacchiato.EArrKeyWord _ uident dimaccs keyword -> prPrec i 6 (concatD [prt 0 uident, prt 0 dimaccs, doc (showString "::"), prt 0 keyword])
    AbsMacchiato.ELitInt _ n -> prPrec i 6 (concatD [prt 0 n])
    AbsMacchiato.ELitTrue _ -> prPrec i 6 (concatD [doc (showString "true")])
    AbsMacchiato.ELitFalse _ -> prPrec i 6 (concatD [doc (showString "false")])
    AbsMacchiato.EApp _ uident exprs -> prPrec i 6 (concatD [prt 0 uident, doc (showString "("), prt 0 exprs, doc (showString ")")])
    AbsMacchiato.EString _ str -> prPrec i 6 (concatD [printString str])
    AbsMacchiato.Neg _ expr -> prPrec i 5 (concatD [doc (showString "-"), prt 6 expr])
    AbsMacchiato.Not _ expr -> prPrec i 5 (concatD [doc (showString "!"), prt 6 expr])
    AbsMacchiato.EMul _ expr1 mulop expr2 -> prPrec i 4 (concatD [prt 4 expr1, prt 0 mulop, prt 5 expr2])
    AbsMacchiato.EAdd _ expr1 addop expr2 -> prPrec i 3 (concatD [prt 3 expr1, prt 0 addop, prt 4 expr2])
    AbsMacchiato.ERel _ expr1 relop expr2 -> prPrec i 2 (concatD [prt 2 expr1, prt 0 relop, prt 3 expr2])
    AbsMacchiato.EAnd _ expr1 expr2 -> prPrec i 1 (concatD [prt 2 expr1, doc (showString "&&"), prt 1 expr2])
    AbsMacchiato.EOr _ expr1 expr2 -> prPrec i 0 (concatD [prt 1 expr1, doc (showString "||"), prt 0 expr2])

instance Print (AbsMacchiato.DimAcc' a) where
  prt i = \case
    AbsMacchiato.EDimAcc _ expr -> prPrec i 0 (concatD [doc (showString "["), prt 6 expr, doc (showString "]")])

instance Print [AbsMacchiato.DimAcc' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (AbsMacchiato.KeyWord' a) where
  prt i = \case
    AbsMacchiato.KeyWordLength _ -> prPrec i 0 (concatD [doc (showString "LENGTH")])
    AbsMacchiato.KeyWordMaxVal _ -> prPrec i 0 (concatD [doc (showString "MAX_VAL")])
    AbsMacchiato.KeyWordMinVal _ -> prPrec i 0 (concatD [doc (showString "MIN_VAL")])
    AbsMacchiato.KeyWordDimNum _ -> prPrec i 0 (concatD [doc (showString "DIM_NUM")])

instance Print [AbsMacchiato.Expr' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (AbsMacchiato.AddOp' a) where
  prt i = \case
    AbsMacchiato.Plus _ -> prPrec i 0 (concatD [doc (showString "+")])
    AbsMacchiato.Minus _ -> prPrec i 0 (concatD [doc (showString "-")])

instance Print (AbsMacchiato.MulOp' a) where
  prt i = \case
    AbsMacchiato.Times _ -> prPrec i 0 (concatD [doc (showString "*")])
    AbsMacchiato.Div _ -> prPrec i 0 (concatD [doc (showString "/")])
    AbsMacchiato.Mod _ -> prPrec i 0 (concatD [doc (showString "%")])

instance Print (AbsMacchiato.RelOp' a) where
  prt i = \case
    AbsMacchiato.LTH _ -> prPrec i 0 (concatD [doc (showString "<")])
    AbsMacchiato.LE _ -> prPrec i 0 (concatD [doc (showString "<=")])
    AbsMacchiato.GTH _ -> prPrec i 0 (concatD [doc (showString ">")])
    AbsMacchiato.GE _ -> prPrec i 0 (concatD [doc (showString ">=")])
    AbsMacchiato.EQU _ -> prPrec i 0 (concatD [doc (showString "==")])
    AbsMacchiato.NE _ -> prPrec i 0 (concatD [doc (showString "!=")])
