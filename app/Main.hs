{-# LANGUAGE LambdaCase #-}

module Main where
import Parsing.ParMacchiato
import StaticAnalysis.TypeCheck
import Interpretation.Interpreter
import           Control.Monad      ((<=<))
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)
import           Control.Arrow           (ArrowChoice (left))


-- myLexer :: String -> [Token]
-- myLexer = tokens

-- Entrypoints

-- pProgram :: [Token] -> Err AbsMacchiato.Program
-- pProgram = fmap snd . pProgram_internal
-- {-# LINE 1 "templates/GenericTemplate.hs" #-}

main :: IO ()
main = getArgs >>= \case
  [f]        -> interpret <=< readFile $ f
  []         -> getContents >>= interpret
  _          -> exitFailure


