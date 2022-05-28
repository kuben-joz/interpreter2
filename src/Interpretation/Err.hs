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
    | ArrDimLEQZero ErrLoc Int
    -- location, what was given, what the array has
    | ArrOutOfBounds ErrLoc Int Int