module Octal (showOct, readOct) where

{-
For the appropriate amount of challenge here, you should only
use functionality present in Prelude. Try not to use
Data.List, Data.Char, Data.Bits, or Numeric.

Try and use seq, $!, or BangPatterns appropriately to ensure
that the solution is efficient.

Handling invalid input is not necessary.
-}

showOct :: (Integral a, Show a) => a -> String
showOct = doShowOct ""

doShowOct :: (Integral a, Show a) => String -> a -> String
doShowOct ""  0 = "0"
doShowOct str 0 = str
doShowOct str a = doShowOct (show (a `mod` 8) ++ str) (a `div` 8)



readOct :: (Integral a) => String -> a
readOct = doReadOct 0

doReadOct :: (Integral a) => a -> String -> a
doReadOct a "" = a
doReadOct a (x:xs) = doReadOct (a*8 + value x) xs
    where value '0' = 0
          value '1' = 1
          value '2' = 2
          value '3' = 3
          value '4' = 4
          value '5' = 5
          value '6' = 6
          value '7' = 7
          value _   = error "bad literal in octal"