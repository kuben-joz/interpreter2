{-# LANGUAGE LambdaCase #-}

module Main where

import Data.IntMap (showTree)
import Parsing.ParMacchiato
import Parsing.PrintMacchiato (printTree)
import StaticAnalysis.CFGOptim
import StaticAnalysis.TypeCheck
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPrint, stderr)

main :: IO ()
main = do
  args <- getArgs
  startProg args

startProg [f] = do
  contents <- readFile f
  check''' contents
startProg [] = do
  contents <- getContents
  check''' contents
startProg _ = do
  putStrLn "Please provide only the filename or nothing at all as program param"
  exitFailure

check''' :: String -> IO ()
check''' s = do
  case (pProgram . myLexer) s of
    Left err -> printErr err
    Right prog -> check'' prog

check'' prog = do
  case startTypeCheck prog of
    Left ex -> print ex
    Right _ -> check' prog

check' prog = do
  res_val <- startInterpret prog
  case res_val of
    Left ex -> print ex
    Right _ -> printTree' prog

--return (liftIO)

printTree' prog = do
  putStrLn "OK"
  print prog
  putStrLn $ printTree prog

printErr err_msg = do
  putStrLn "ERROR"
  putStrLn err_msg
