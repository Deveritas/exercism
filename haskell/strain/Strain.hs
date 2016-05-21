module Strain where

keep :: (a -> Bool) -> [a] -> [a]
discard :: (a -> Bool) -> [a] -> [a]

keep _ [] = []
keep predicate (x:xs) = if predicate x then x : keep predicate xs else keep predicate xs

discard _ [] = []
discard predicate (x:xs) = if predicate x then discard predicate xs else x : discard predicate xs

