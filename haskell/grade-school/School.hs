module School where

import qualified Data.Map as Map
import Data.List (sort, sortOn)
import Data.Maybe (fromMaybe)
import Control.Arrow (second)

data School = School (Map.Map Int [String])

sorted :: School -> [(Int, [String])]
sorted (School school) = map (second sort) $ sortOn fst $ Map.toList school

add :: Int -> String -> School -> School
add i name (School grades) = School $ Map.insertWith (++) i [name] grades

empty :: School
empty = School Map.empty

grade :: Int -> School -> [String]
grade i (School school) = fromMaybe [] $ Map.lookup i school