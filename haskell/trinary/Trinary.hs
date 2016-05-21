module Trinary (showTri, readTri) where

import qualified Data.Map   as Map
import           Data.Maybe (fromMaybe)

showTri :: (Integral a, Show a) => a -> String
showTri = either (const "0") id . doShowBase 3 ""

doShowBase :: (Integral a, Show a) => a -> String -> a -> Either String String
doShowBase _ ""  0 = Right "0"
doShowBase _ str 0 = Right str
doShowBase base str a | isValid = doShowBase base (intValues Map.! (a `mod` base) : str) (a `div` base)
    where isValid = base `Map.member` intValues
doShowBase base _ _ = Left $ "Cannot handle base greater than 64. Given " ++ show base

intValues :: (Integral a) => Map.Map a Char
intValues = Map.fromList $ zip [0..9] ['0'..'9'] ++ zip [10..35] ['a'..'z'] ++ zip [36..61] ['A'..'Z'] ++ [(62, '+'), (63, '/')]


readTri :: (Integral a) => String -> a
readTri = either (const 0) id . doReadBase 3 0

doReadBase :: (Integral a) => a-> a -> String -> Either String a
doReadBase _ acc "" = Right acc
doReadBase base acc (x:xs) | isValid = doReadBase base (acc*base + charValues Map.! x) xs
    where isValid = fromMaybe False $ (base >=) <$> Map.lookup x charValues
doReadBase _ _ (x:_) = Left $ "Invalid character in string: " ++ [x]

charValues :: (Integral a) => Map.Map Char a
charValues = Map.fromList $ zip ['0'..'9'] [0..9] ++ zip ['a'..'z'] [10..35] ++ zip ['A'..'Z'] [36..61] ++ [('+', 62), ('/', 63)]
