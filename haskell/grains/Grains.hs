module Grains (square, total) where

square :: Integer -> Integer
square n = if n <= 1 then 1 else 2 * square (n-1)

total :: Integer
total = sum $ map square [1..64]
