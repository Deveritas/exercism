module House where

import Data.List (tails)

segments :: [String]
segments = map concat $ tail $ reverse $ tails $ reverse [ "house that Jack built.\n"
           , "malt\nthat lay in the "
           , "rat\nthat ate the "
           , "cat\nthat killed the "
           , "dog\nthat worried the "
           , "cow with the crumpled horn\nthat tossed the "
           , "maiden all forlorn\nthat milked the "
           , "man all tattered and torn\nthat kissed the "
           , "priest all shaven and shorn\nthat married the "
           , "rooster that crowed in the morn\nthat woke the "
           , "farmer sowing his corn\nthat kept the "
           , "horse and the hound and the horn\nthat belonged to the "
           ]

rhyme :: String
rhyme = unlines $ map (\v -> "This is the "++v) segments