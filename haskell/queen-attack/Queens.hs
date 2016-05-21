module Queens (boardString, canAttack) where

import Data.Maybe (fromJust, isNothing)
import Data.List (intersperse)

type Position = (Int, Int)
type Board = String

boardString :: Maybe Position -> Maybe Position -> Board
boardString white black
    | isNothing white && isNothing black = toBoard emptyBoard
    | isNothing white = toBoard $ setBlack emptyBoard
    | isNothing black = toBoard $ setWhite emptyBoard
    | otherwise = toBoard $ setWhite $ setBlack emptyBoard
    where setBlack = setCharAt (fromJust black) 'B'
          setWhite = setCharAt (fromJust white) 'W'

emptyBoard :: [String]
emptyBoard = replicate 8 $ replicate 8 '_'

setCharAt :: Position -> Char -> [String] -> [String]
setCharAt (0,y) c (l:ls) = setCharAtLine y c l : ls
setCharAt _ _ [] = error "Out of bounds position"
setCharAt (x, y) c (l:ls) = l : setCharAt (x-1, y) c ls

setCharAtLine :: Int -> Char -> String -> String
setCharAtLine 0 c (_:ls) = c : ls
setCharAtLine _ _ [] = error "Out of bounds position"
setCharAtLine x c (l:ls) = l : setCharAtLine (x-1) c ls

toBoard :: [String] -> Board
toBoard = unlines . map (intersperse ' ')

canAttack :: Position -> Position -> Bool
canAttack (wx, wy) (bx, by)
    | onAxis = True
    | onDiagonal = True
    | otherwise = False
    where onAxis = wx == bx || wy == by
          onDiagonal = wx-bx == wy-by || wx-bx == by-wy