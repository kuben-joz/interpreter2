{-# LANGUAGE LambdaCase #-}

module Main where
import Parsing.ParMacchiato
import StaticAnalysis.TypeCheck
import Interpretation.Interpreter
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)
import           Control.Arrow           (ArrowChoice (left))

main :: IO ()
main = do 
  args <- getArgs
  startProg args

startProg [f] = do
  contents <- readFile f
  interpret contents

startProg [] = do
  contents <- getContents
  interpret contents

startProg _ = do
  putStrLn "Please provide only the filename or nothing at all as program param"
  exitFailure


