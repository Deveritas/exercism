module DNA (count, nucleotideCounts) where
import Data.Map (Map, fromList)
import Debug.Trace (trace)

count :: Char -> String -> Integer
count a
    | a `elem` "ACGT" = doCount a
    | otherwise = error ("invalid nucleotide " ++ show a)


doCount :: Char -> String -> Integer
doCount _ [] = 0
doCount n (d:ds)
    | d `elem` "ACGT" =  if d == n then 1 + doCount n ds else doCount n ds
    | otherwise = error ("invalid nucleotide " ++ show d)

nucleotideCounts :: String -> Map Char Integer
nucleotideCounts dna = fromList $ map (\c -> (,) c $! doCount c dna) "ACGT"