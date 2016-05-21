module Series (largestProduct) where

import Data.Char (ord)

largestProduct :: Int -> String -> Maybe Int
largestProduct i string = if i <= length string then Just $ scanString i string 0 else Nothing

scanString :: Int -> String -> Int -> Int
scanString 0 _ _ = 1
scanString i string@(_:xs) m = if i > length string then m else scanString i xs $ max m $ test i string
scanString _ [] m = m

test :: Int -> String -> Int
test 1 (c:_) = ord c - ord '0'
test i (c:cs) = (ord c - ord '0') * test (i-1) cs
test _ _ = error "i to large or negative"