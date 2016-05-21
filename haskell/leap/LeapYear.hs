{-# LANGUAGE NoImplicitPrelude #-}

module LeapYear (isLeapYear) where

import           Prelude (Bool, Int, mod, not, (&&), (==), (||))

isLeapYear :: Int -> Bool
isLeapYear year = test 4 && (not (test 100) || test 400) where
           test = isDivisible year

isDivisible :: Int -> Int -> Bool
isDivisible num div = num `mod` div == 0
