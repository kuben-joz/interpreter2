module Interpretation.Err where
import Parsing.AbsMacchiato
import Interpretation.MacchiatoVals

type ErrLoc = BNFC'Position


data RuntimeException = 
      DivByZero ErrLoc
    | ModZero ErrLoc
    -- add minBound
    | IntTooSmall ErrLoc Integer
    -- add maxBound in print
    | IntTooLarge ErrLoc Integer