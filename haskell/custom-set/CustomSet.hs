module CustomSet (CustomSet, fromList, empty, delete, difference, isDisjointFrom, null, intersection, member, insert, size, isSubsetOf, toList, union) where

import           Data.List (sort)
import           Prelude   hiding (null)

data CustomSet a = Set [a]

instance (Eq a, Ord a) => Eq (CustomSet a) where
    (==) (Set a) (Set b) = sort a == sort b

instance (Show a, Ord a) => Show (CustomSet a) where
    show (Set ary) = "fromList " ++ show (sort ary)

type Set a = CustomSet a


fromList :: (Eq a) => [a] -> Set a
fromList = foldr insert empty

empty :: Set a
empty = Set []


delete :: (Eq a) => a -> Set a -> Set a
delete item (Set ary) = Set $ filter (/=item) ary

insert :: (Eq a) => a -> Set a -> Set a
insert item (Set ary) = Set $ item : filter (/=item) ary

member :: (Eq a) => a -> Set a -> Bool
member item (Set ary) = item `elem` ary


difference :: (Eq a) => Set a -> Set a -> Set a
difference (Set a) b = Set $ filter (not . (`member` b)) a

union :: (Eq a) => Set a -> Set a -> Set a
union a (Set b) = foldr insert a b

intersection :: (Eq a) => Set a -> Set a -> Set a
intersection (Set a) b = Set $ filter (`member` b) a

isDisjointFrom :: (Eq a) => Set a -> Set a -> Bool
isDisjointFrom (Set a) b = not $ any (`member` b) a

isSubsetOf :: (Eq a) => Set a -> Set a -> Bool
isSubsetOf (Set a) b = all (`member` b) a


null :: Set a -> Bool
null (Set []) = True
null (Set _ ) = False

size :: Set a -> Int
size (Set ary) = length ary


toList :: (Ord a) => Set a -> [a]
toList (Set ary) = sort ary

