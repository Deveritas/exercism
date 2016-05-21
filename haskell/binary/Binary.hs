module Binary (toDecimal) where

toDecimal :: String -> Int
toDecimal = foldl (\t c -> case c of '0' -> (*2)t
                                     '1' -> (+1)(t*2)
                                     _   ->  0) 0
