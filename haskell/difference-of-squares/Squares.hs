module Squares (sumOfSquares, squareOfSums, difference) where

sumOfSquares :: (Integral a) => a -> a
sumOfSquares n = sum [x^(2::Integer) | x <- [1..n]]

squareOfSums :: (Integral a) => a -> a
squareOfSums n = sum [1..n] ^ (2::Integer)

difference :: (Integral a) => a -> a
difference n = squareOfSums n - sumOfSquares n