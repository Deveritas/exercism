module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors = try 2

try :: Integer -> Integer -> [Integer]
try test num
    | test > num = []
    | num `rem` test == 0 = test : try 2 (num `div` test)
    | otherwise = try (test+1) num