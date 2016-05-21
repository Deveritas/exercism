module LinkedList where

data List a = Cons a (List a) | Nil

isNil :: List a -> Bool
isNil (Cons _ _) = False
isNil Nil = True

nil :: List a
nil = Nil

datum :: List a -> a
datum (Cons a _) = a
datum _ = error "Tried to read from empty list"

next :: List a -> List a
next (Cons _ t) = t
next _ = error "Tried to next empty list"

toList :: List a -> [a]
toList Nil = []
toList (Cons a t) = a : toList t

fromList :: [a] -> List a
fromList = foldr Cons Nil

reverseLinkedList :: List a -> List a
reverseLinkedList = fromList . reverse . toList

new :: a -> List a -> List a
new = Cons
