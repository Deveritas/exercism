module Allergies (Allergen(..), isAllergicTo, allergies) where

import Data.Bits ((.&.))

data Allergen = Eggs | Peanuts | Shellfish | Strawberries | Tomatoes | Chocolate  | Pollen | Cats deriving (Enum, Eq, Show)

isAllergicTo :: Allergen -> Integer -> Bool
isAllergicTo allergy value = (2 ^ fromEnum allergy) .&. value /= 0

allergies :: Integer -> [Allergen]
allergies num = filter (`isAllergicTo` num) [Eggs .. Cats]