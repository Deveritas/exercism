module Hamming where

distance :: (Eq a) => [a] -> [a] -> Integer
distance (l:ls) (r:rs) = if l == r then distance ls rs else 1 + distance ls rs
distance _ _ = 0