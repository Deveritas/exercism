module OCR (convert) where

import Data.List (transpose, intercalate)

type Letter = [String]

convert :: String -> String
convert =  intercalate "," . map (map analyze . toLetters) . toLines

toLines :: String -> [[String]]
toLines =  chopVertical . lines

toLetters :: [String] -> [Letter]
toLetters = transpose . map chopHorizontal

chopVertical ::[a] -> [[a]]
chopVertical (a : b : c : d : rest) = [a,b,c,d] : chopVertical rest
chopVertical _ = []

chopHorizontal :: [a] -> [[a]]
chopHorizontal (a : b : c : rest) = [a,b,c] : chopHorizontal rest
chopHorizontal _ = []

analyze :: [String] -> Char
analyze [[' ','_',' '], ['|',' ','|'], ['|','_','|'], [' ',' ',' ']] = '0'
analyze [[' ',' ',' '], [' ',' ','|'], [' ',' ','|'], [' ',' ',' ']] = '1'
analyze [[' ','_',' '], [' ','_','|'], ['|','_',' '], [' ',' ',' ']] = '2'
analyze [[' ','_',' '], [' ','_','|'], [' ','_','|'], [' ',' ',' ']] = '3'
analyze [[' ',' ',' '], ['|','_','|'], [' ',' ','|'], [' ',' ',' ']] = '4'
analyze [[' ','_',' '], ['|','_',' '], [' ','_','|'], [' ',' ',' ']] = '5'
analyze [[' ','_',' '], ['|','_',' '], ['|','_','|'], [' ',' ',' ']] = '6'
analyze [[' ','_',' '], [' ',' ','|'], [' ',' ','|'], [' ',' ',' ']] = '7'
analyze [[' ','_',' '], ['|','_','|'], ['|','_','|'], [' ',' ',' ']] = '8'
analyze [[' ','_',' '], ['|','_','|'], [' ','_','|'], [' ',' ',' ']] = '9'
analyze _ = '?'
