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

--todo maybe add precedence brakcets
instance Show RuntimeException where
  showsPrec _ (DivByZero loc) = shows "Div by zero at ". shows loc
  showsPrec _ (ModZero loc) = shows "Modulo zero at ". shows loc
  showsPrec _ (IntTooSmall loc i) = shows loc . shows ":Value of ". shows i .
    shows " is smaller than Int's min of " . shows (minBound :: Int)
  showsPrec _ (IntTooLarge loc i) = shows loc . shows ":Value of ". shows i .
    shows " is larger than Int's max of " . shows (maxBound :: Int)
  showsPrec _ (ArrDimLEQZero loc val) = shows loc . 
    shows ": arr access with non-positive value of " . shows val
  showsPrec _ (ArrOutOfBounds loc i_a i_max) = shows loc . shows ": access out of bounds at index ".
    shows i_a . shows " for array of size " . shows i_max