module Sublist (Sublist(Equal, Superlist, Sublist, Unequal), checkSublist) where

data Sublist = Equal | Superlist | Sublist | Unequal deriving (Eq, Show)

isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

tails :: [a] -> [[a]]
tails [] = [[]]
tails xxs@(_:xs) = xxs : tails xs

isInfixOf :: (Eq a) => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

equal :: (Eq a) => [a] -> [a] -> Bool
equal [] [] = True
equal (x:xs) (y:ys) = x == y && equal xs ys
equal _ _ = False

checkSublist :: (Eq a) => [a] -> [a] -> Sublist
checkSublist l r
    | equal l r = Equal
    | l `isInfixOf` r = Sublist
    | r `isInfixOf` l = Superlist
    | otherwise = Unequal
