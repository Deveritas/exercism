module Triangle where

data TriangleType = Scalene | Isosceles | Equilateral | Illogical deriving (Eq, Show)

triangleType :: Int -> Int -> Int -> TriangleType
triangleType a b c
    | a + b <= c || a <= b - c || a >= b + c = Illogical
    | a == b && b == c = Equilateral
    | a == b || a == c || b == c = Isosceles
    | otherwise = Scalene