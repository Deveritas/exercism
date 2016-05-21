module Matrix ( Matrix, row, column, rows, cols, shape, transpose, reshape, flatten, fromString, fromList) where

import qualified Data.Vector as V
import qualified Data.List as L

data Matrix a = Matrix (V.Vector (V.Vector a)) deriving (Eq, Show)

row :: Int -> Matrix a -> V.Vector a
row idx (Matrix rowsV) = rowsV V.! idx

column :: Int -> Matrix a -> V.Vector a
column idx (Matrix rowsV) = V.map (V.! idx) rowsV

shape :: Matrix a -> (Int, Int)
shape matrix = (rows matrix, cols matrix)

rows :: Matrix a -> Int
rows (Matrix rowsV) = length rowsV

cols :: Matrix a -> Int
cols (Matrix rowsV) = if null rowsV then 0 else length $ rowsV V.! 0

transpose :: Matrix a -> Matrix a
transpose (Matrix rowsV) = Matrix $ V.fromList $ L.map V.fromList $ L.transpose $ L.map V.toList $ V.toList rowsV

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (_, columns) = fromList . sliceFor columns . V.toList . flatten

sliceFor :: Int -> [a] -> [[a]]
sliceFor _ [] = []
sliceFor i line = take i line : sliceFor i (drop i line)

flatten :: Matrix a -> V.Vector a
flatten (Matrix rowsV) = V.concat $ V.toList rowsV

fromString :: (Read a) => String -> Matrix a
fromString string = fromList $ L.map parseLine $ lines string

fromList :: [[a]] -> Matrix a
fromList = Matrix . V.fromList . L.map V.fromList


parseLine :: (Read a) => String -> [a]
parseLine line = case reads line of [] -> []
                                    (piece, rest):_ -> piece : parseLine rest