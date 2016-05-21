module Clock (fromHourMin, toString) where

data Clock = Clock Integer Integer deriving (Eq, Show)

instance Num Clock where
    (+) (Clock hr1 min1) (Clock hr2 min2) = fromHourMin (hr1+hr2) (min1+min2)
    (*) _ _ = error "Cannot multiply Clock"
    abs = id
    signum _ = 1
    fromInteger = fromHourMin 0
    negate (Clock hr minute) = Clock (23 - hr) (60 - minute)

fromHourMin :: Integer -> Integer -> Clock
fromHourMin hr minute
    | minute >= 60 = fromHourMin (hr + minute `div` 60) (minute `mod` 60)
    | minute < 0 = fromHourMin (hr - 1) (minute + 60)
    | hr >= 24 = fromHourMin (hr `mod` 24) minute
    | hr < 0 = fromHourMin (hr + 24) minute
    | otherwise = Clock hr minute

toString :: Clock -> String
toString (Clock hr minute) = hour ++ ":" ++ minText
    where hour
            | hr < 10 = '0' : show hr
            | otherwise = show hr
          minText
            | minute < 10 = '0' : show minute
            | otherwise = show minute

