module DNA (toRNA) where

type DNA = String
type RNA = String

toRNA :: DNA -> RNA
toRNA [] = []
toRNA ('A':xs) = 'U' : toRNA xs
toRNA ('C':xs) = 'G' : toRNA xs
toRNA ('G':xs) = 'C' : toRNA xs
toRNA ('T':xs) = 'A' : toRNA xs
toRNA (char:_) = error "Bad mutation in DNA : " ++ [char]