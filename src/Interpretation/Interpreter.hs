{-# LANGUAGE LambdaCase #-}

module Interpretation.Interpreter where
import Parsing.ParMacchiato
import StaticAnalysis.TypeCheck
import           Control.Monad      ((<=<))
import           System.Environment (getArgs)
import           Control.Arrow           (ArrowChoice (left))
import           System.Exit             (exitFailure, exitSuccess)
import           System.IO               (hPrint, stderr)
import Control.Monad.Except
import Data.Either
import GHC.RTS.Flags (RTSFlags(profilingFlags))
import qualified StaticAnalysis.Traverser


exit :: IO (Either String a) -> IO ()
exit computation = computation >>= \case
  Left err -> hPrint stderr err >> exitFailure
  Right _  -> exitSuccess


interpret :: String -> IO ()
interpret s = do 
    case  ( pProgram . myLexer) s of
        Left err -> hPrint stderr err
        Right prog -> check' prog
                    


check' prog = do
  case startTypeCheck prog of
    Left ex -> print ex
    Right _ -> print "success"
  --return (liftIO)


 -- check' = either Left (left show . check)