module Gigasecond (fromDay) where

import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime)

giga :: Integer
giga = 10^(9::Integer)

gigaseconds :: NominalDiffTime
gigaseconds = fromIntegral giga

fromDay :: UTCTime -> UTCTime
fromDay = addUTCTime gigaseconds