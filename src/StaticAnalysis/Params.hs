module StaticAnalysis.Params where

{--
used in CFGTraverser.hs
how deeply in functions calls is an expression simplified
if we have int a = f(3) and search_depth > 1 then it will check if f(3) can
be statically determined

possible todo
we could add a check if all movement from main is accounted for
    and if it fits in depth then even otherwise incorrect funcitons pass
--}
search_depth :: Int
search_depth = 1

{--
used in CFGOptim.hs
Specifies max int value
--}
int_max :: Integer
int_max = toInteger (maxBound :: Int)

-- can be potetnially smaller/larger than 2^63-1
-- int_max = 2147483647
{--
used in CFGOptim.hs
Specifies min int value
--}
int_min :: Integer
int_min = toInteger (minBound :: Int)

-- can be potetnially smaller/larger than -2^63
-- int_min = -2147483648
