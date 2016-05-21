module Bob (responseFor) where

import Data.Char (isLetter, isUpper)

responseFor :: String -> String
responseFor text
    | isEmpty text = "Fine. Be that way!"
    | isYelling text && hasText text= "Whoa, chill out!"
    | isQuestion text = "Sure."
    | otherwise = "Whatever."

isYelling :: String -> Bool
isYelling [] = False
isYelling cs = foldr (\c -> (&&)(isUpper c || not (isLetter c))) True cs && hasText cs

hasText :: String -> Bool
hasText = foldr ((||) . isLetter) False

isQuestion :: String -> Bool
isQuestion [] = False
isQuestion (c:cs) = c == '?' && isEmpty cs || isQuestion cs

isEmpty :: String -> Bool
isEmpty [] = True
isEmpty cs = foldr (\c -> (&&)(c `elem` "\n\r \t\v\xA0\x2002")) True cs
