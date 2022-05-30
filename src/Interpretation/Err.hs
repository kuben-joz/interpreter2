module Interpretation.Err where
import Parsing.AbsMacchiato
import Interpretation.MacchiatoVals

type ErrLoc = BNFC'Position

errMsgStart:: ErrLoc -> String
errMsgStart (Just(line, col)) = "Exception caught on line " ++ show line ++ ", collumn " ++ show col ++ ": "
errMsgStart Nothing = "Exception caught: "

data RuntimeException = 
      DivByZero ErrLoc
    | ModZero ErrLoc
    -- add minBound
    | IntTooSmall ErrLoc Integer
    -- add maxBound in print
    | IntTooLarge ErrLoc Integer
    | ArrDimLTZero ErrLoc Int
    -- location, what was given, what the array has
    | ArrOutOfBounds ErrLoc Int Int

--todo maybe add precedence brakcets
instance Show RuntimeException where
  show (DivByZero loc) = (errMsgStart loc) ++ "Division by zero"
  show (ModZero loc) = (errMsgStart loc)++ "Modulo zero"
  show (IntTooSmall loc i) = (errMsgStart loc)++ "Value of "++ show i ++
    " is smaller than Int's min of " ++ show (minBound :: Int)
  show (IntTooLarge loc i) = (errMsgStart loc)++ "Value of "++ show i ++
    " is larger than Int's max of " ++ show (maxBound :: Int)
  show (ArrDimLTZero loc val) = (errMsgStart loc)++ "Array access with non-positive value of " ++ show val
  show (ArrOutOfBounds loc i_a i_max) = (errMsgStart loc)++ "Access out of bounds at index "++
    show i_a ++ " for array of size " ++ show i_max