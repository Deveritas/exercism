module Hexadecimal (hexToInt) where

import Data.Maybe (fromMaybe)
import Data.Ix (inRange)
import Data.Char (toLower, ord)

hexToInt :: String -> Integer
hexToInt = fromMaybe 0 . _hexToInt . map toLower . reverse

_hexToInt :: String -> Maybe Integer
_hexToInt "" = Just 0
_hexToInt (c:bs)
    | ('a', 'f') `inRange` c = step charValue
    | ('0', '9') `inRange` c = step intValue
    where step fn = (+) (toInteger $ fn c) . (*16) <$> _hexToInt bs

_hexToInt _ = Nothing

intValue :: Char -> Int
intValue = flip (-) (ord '0') . ord

charValue :: Char -> Int
charValue = flip (-) (ord 'a') . (+10) . ord

