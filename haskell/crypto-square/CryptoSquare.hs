module CryptoSquare (normalizePlaintext, squareSize, plaintextSegments, ciphertext, normalizeCiphertext) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose)

normalizePlaintext :: String -> String
normalizePlaintext = map toLower . filter isAlphaNum

squareSize :: String -> Int
squareSize = ceiling . (sqrt::Double -> Double) . fromIntegral . length . normalizePlaintext

plaintextSegments :: String -> [String]
plaintextSegments text = segment (normalizePlaintext text) (squareSize text)

segment :: [a] -> Int -> [[a]]
segment [] _ = []
segment text size = take size text : segment (drop size text) size

ciphertext :: String -> String
ciphertext = foldl1 (++) . transpose . plaintextSegments

normalizeCiphertext :: String -> String
normalizeCiphertext = foldl1 (\a b -> a ++ " " ++ b) . transpose . plaintextSegments