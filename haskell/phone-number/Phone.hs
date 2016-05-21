module Phone (number, areaCode, prettyPrint) where

import Data.Char (isNumber)

number :: String -> String
areaCode :: String -> String
prettyPrint :: String -> String

errorNumber :: String
errorNumber = "0000000000"

number = verifyLength . stripLetters

stripLetters :: String -> String
stripLetters [] = []
stripLetters (x:xs) = if isNumber x then x : stripLetters xs else stripLetters xs

verifyLength :: String -> String
verifyLength [] = errorNumber
verifyLength input@(x:xs)
    | length input < 10 = errorNumber
    | length input == 11 = if x == '1' then xs else errorNumber
    | length input > 11 = errorNumber
    | otherwise = input

areaCode input = getCode $ number input where
    getCode :: String -> String
    getCode [a,b,c,_,_,_,_,_,_,_] = [a,b,c]
    getCode _ = error "bad input"

prettyPrint input = split $ number input where
    split [a,b,c,d,e,f,g,h,i,j] = ['(',a,b,c,')',' ',d,e,f,'-',g,h,i,j]
    split _ = error "bad input"