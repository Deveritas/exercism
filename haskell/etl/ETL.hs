module ETL where

import Data.Char (toLower)
import qualified Data.Map as Map

transform :: Map.Map Int [String] -> Map.Map String Int
transform = Map.fromList . concatMap trans . Map.toList
    where trans (s, chars) = map (\c -> (map toLower c, s)) chars