module Luhn (checkDigit, addends, checksum, isValid, create) where

checkDigit :: (Integral a) => a -> a
checkDigit = flip rem 10

splitNumber :: (Integral a) => a -> [a]
splitNumber i
    | i < 10 = [i]
    | otherwise = (i `mod` 10) : splitNumber (i `div` 10)

addends :: (Integral a) => a -> [a]
addends = reverse . dubs False . splitNumber

dubs :: (Integral a) => Bool -> [a] -> [a]
dubs _ [] = []
dubs False (x:xs) = x : dubs True xs
dubs True (x:xs) = converted : dubs False xs
    where converted = if x < 5 then x*2 else (x*2)-9

checksum :: (Integral a) => a -> a
checksum = checkDigit . sum . addends

isValid :: (Integral a) => a -> Bool
isValid = (==0) . checksum

create :: (Integral a) => a -> a
create = _create . (*10)

_create :: (Integral a) => a -> a
_create num = num + (flip mod 10 . (-) 10 . checksum) num