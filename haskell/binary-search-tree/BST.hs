module BST (bstLeft, bstRight, bstValue, empty, singleton, insert, fromList, toList) where

data Tree a = Empty | Node {
    value :: a,
    left :: Tree a,
    right :: Tree a
} deriving (Show, Read, Eq)

empty :: Tree a
empty = Empty

singleton :: a -> Tree a
singleton a = Node {value = a, left = Empty, right = Empty}

bstValue :: Tree a -> Maybe a
bstValue Empty = Nothing
bstValue tree = Just $ value tree

bstLeft :: Tree a -> Maybe (Tree a)
bstLeft Empty = Nothing
bstLeft Node {left = l} = Just l

bstRight :: Tree a -> Maybe (Tree a)
bstRight Empty = Nothing
bstRight Node {right = r} = Just r

insert :: (Ord a) => a -> Tree a -> Tree a
insert val root
    | root == Empty = singleton val
    | val <= value root = root {left = insert val (left root)}
    | val > value root = root {right = insert val (right root)}
    | otherwise = error "This shouldn't be reached"

fromList :: (Ord a) => [a] -> Tree a
fromList = foldl (flip insert) Empty

toList :: Tree a -> [a]
toList Empty = []
toList (Node v l r) = toList l ++ v : toList r