module Atbash (encode) where

import Data.Char (isAlphaNum, isNumber, isAscii, toLower, ord, chr)

encode :: String -> String
encode = spaces.filterAscii

spaces :: String -> String
spaces txt = snd $ foldl addSpaces start txt
    where start = (0::Integer, "")
          addSpaces (step, text) letter = if step == 5 then (1, text ++ [' ', letter])
                                                       else (step+1, text ++ [letter])

filterAscii :: String -> String
filterAscii input = map (swapChar.toLower) $ filter (\c -> isAscii c && isAlphaNum c) input

swapChar :: Char -> Char
swapChar c
 | isNumber c = c
 | otherwise = chr $ z + a - ord c
    where a = ord 'a'
          z = ord 'z'