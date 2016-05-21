module Anagram where

import Data.Char (toUpper)

anagramsFor :: String -> [String] -> [String]
anagramsFor source = filter (testAnagram source)

testAnagram :: String -> String -> Bool
testAnagram a b = _testAnagram (map toUpper a) (map toUpper b)

_testAnagram :: String -> String -> Bool
_testAnagram [] [] = True
_testAnagram a b
    | a == b = False
    | otherwise = testMatch a b where
        testMatch [] [] = True
        testMatch (l:ls) r = l `elem` r && testMatch ls (stripLetter l r)
        testMatch _ _ = False

stripLetter :: Char -> String -> String
stripLetter letter (x:xs) = if x == letter then xs else x : stripLetter letter xs
stripLetter _ [] = []