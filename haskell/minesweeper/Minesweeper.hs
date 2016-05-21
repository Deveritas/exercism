module Minesweeper (annotate) where

import Data.List (transpose)
import Data.Char (intToDigit)
import Data.Maybe (fromMaybe)

annotate :: [String] -> [String]
annotate = map (map (fromCode . toCode)) . transpose . map (map concat . slice3) . transpose . map buildSubstrings . addFirstLastLine

addFirstLastLine :: [String] -> [String]
addFirstLastLine [] = []
addFirstLastLine lanes@(first:_) = createEmpty first : lanes ++ createLast lanes
    where createEmpty line = zipWith (const . const ' ') line ([1..]::[Integer])
          createLast [line] = [createEmpty line]
          createLast (_:ls) = createLast ls
          createLast _ = error "should not be reached"

toCode :: String -> Maybe Int
toCode [_, _, _, _, '*', _, _, _, _] = Nothing
toCode square = Just $ foldr (\c -> if c == '*' then (+1) else id) 0 square

fromCode :: Maybe Int -> Char
fromCode i = swapZero $ fromMaybe '*' $ intToDigit <$> i
    where swapZero '0' = ' '
          swapZero c   = c

slice3 :: [a] -> [[a]]
slice3 [x, y, z] = [[x, y, z]]
slice3 (x:rest@(y:z:_)) = [x, y, z] : slice3 rest
slice3 _ = error "slice3 requires list of three elements"

buildSubstrings :: String -> [String]
buildSubstrings [] = ["   "]
buildSubstrings [x] = [[' ', x, ' ']]
buildSubstrings list@(x:y:_) = [' ', x, y] : _buildSubstrings list

_buildSubstrings :: String -> [String]
_buildSubstrings [x, y] = [[x, y, ' ']]
_buildSubstrings (x:other@(y:z:_)) = [x, y, z] : _buildSubstrings other
_buildSubstrings _ = error "_buildSubstrings: Last call should be x:y:[]"