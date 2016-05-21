module Raindrops where

convert :: Int -> String
convert num
    | pling && plang && plong = "PlingPlangPlong"
    | pling && plang = "PlingPlang"
    | pling && plong = "PlingPlong"
    | plang && plong = "PlangPlong"
    | pling = "Pling"
    | plang = "Plang"
    | plong = "Plong"
    | otherwise = show num
    where pling = num `mod` 3 == 0
          plang = num `mod` 5 == 0
          plong = num `mod` 7 == 0