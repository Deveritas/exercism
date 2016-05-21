module Beer (sing, verse) where

import Data.Char (toUpper)

sing :: Integer -> Integer -> String
sing start stop = concatMap (\l -> verse l ++ "\n") $ reverse [stop..start]

verse :: Integer -> String
verse 2 = line1 "2 bottles" ++ line2 "1 bottle"
verse 1 = line1 "1 bottle"  ++ "Take it down and pass it around, no more bottles of beer on the wall.\n"
verse 0 = line1 "no more bottles" ++ "Go to the store and buy some more, 99 bottles of beer on the wall.\n"
verse count = line1 (bottleCount count) ++ line2 (bottleCount (count-1))

bottleCount :: Integer -> String
bottleCount 1 = "1 bottle"
bottleCount 0 = "no more bottles"
bottleCount count = show count ++ " bottles"

line1 :: String -> String
line1 prefix@(first:rest) = (toUpper first : rest) ++ " of beer on the wall, " ++ prefix ++ " of beer.\n"
line1 [] = error ""

line2 :: String -> String
line2 prefix = "Take one down and pass it around, " ++ prefix ++ " of beer on the wall.\n"