module Garden (garden, defaultGarden, lookupPlants, Plant(..)) where

import           Data.List (sort, transpose)
import qualified Data.Map  as Map

type Child = String
type Plants = String
type Garden = Map.Map Child [Plant]

data Plant = Clover | Grass | Radishes | Violets deriving (Eq, Enum, Show)


defaultGarden :: Plants -> Garden
defaultGarden = garden ["Alice", "Bob", "Charlie", "David", "Eve", "Fred", "Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry"]

garden :: [Child] -> Plants -> Garden
garden children = Map.fromList . zip (sort children) . map (map toPlant . concat) . transpose . map (segment 2) . lines

segment :: Int -> [a] -> [[a]]
segment _ [] = []
segment size text = take size text : segment size (drop size text)

toPlant :: Char -> Plant
toPlant c
    | c == 'C' = Clover
    | c == 'G' = Grass
    | c == 'R' = Radishes
    | c == 'V' = Violets
    | otherwise = error "Only support CGRV"

lookupPlants :: Child -> Garden -> [Plant]
lookupPlants = flip (Map.!)
