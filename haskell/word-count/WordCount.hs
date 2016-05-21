module WordCount (wordCount) where

import           Data.Char       (isAlphaNum, toLower)
import qualified Data.List.Split as Split
import qualified Data.Map        as Map
import           Data.Maybe      (fromMaybe)

wordCount :: String -> Map.Map String Int
wordCount = count . map (map toLower) . filter (/= "") . Split.splitWhen (not.isAlphaNum)

count :: [String] -> Map.Map String Int
count = foldr (\word counts -> Map.insert word (1 + fromMaybe 0 (Map.lookup word counts)) counts) Map.empty
