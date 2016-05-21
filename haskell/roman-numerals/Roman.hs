module Roman where

numerals :: Int -> String
numerals num
    | num >= 1000 = "M" ++ numerals (num - 1000)
    | num >= 900 = "CM" ++ numerals (num - 900)
    | num >= 500 = "D" ++ numerals (num - 500)
    | num >= 400 = "CD" ++ numerals (num - 400)
    | num >= 100 = "C" ++ numerals (num - 100)
    | num >= 90 = "XC" ++ numerals (num - 90)
    | num >= 50 = "L" ++ numerals (num - 50)
    | num >= 40 = "XL" ++ numerals (num - 40)
    | num >= 10 = "X" ++ numerals (num - 10)
    | num >= 9 = "IX" ++ numerals (num - 9)
    | num >= 5 = "V" ++ numerals (num - 5)
    | num >= 4 = "IV" ++ numerals (num - 4)
    | num >= 1 = "I" ++ numerals (num - 1)
    | otherwise = ""