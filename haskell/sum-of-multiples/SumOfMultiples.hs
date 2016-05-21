module SumOfMultiples where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples multiples bound = sum $ filter (filterValid multiples) [1..bound-1]

filterValid :: [Integer] -> Integer -> Bool
filterValid filters i = any (\c -> i `mod` c == 0) filters
