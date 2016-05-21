module Meetup  where

import           Data.Time.Calendar          (Day, fromGregorian,
                                              gregorianMonthLength)
import           Data.Time.Calendar.WeekDate (fromWeekDate, toWeekDate)

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Bounded, Enum, Show)
data Schedule = First | Second | Third | Fourth | Last | Teenth deriving (Enum)

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day

meetupDay Teenth day year month = (!!0) $ filter ((== day) . getWeekDay) $ map (fromGregorian year month) [13..19]
meetupDay Last   day year month = (!!0) $ filter ((== day) . getWeekDay) $ map (fromGregorian year month) range
    where range = [endDate-6..endDate]
          endDate = gregorianMonthLength year month

meetupDay when day year month = (!!0) $ filter ((== day) . getWeekDay) $ map (fromWeekDate year week) [1..7]
    where week = if shouldStepForward then _week + 1 else _week
          shouldStepForward = getWeekDay (fromGregorian year month 1) > day
          _week = getWeek (fromGregorian year month 1) + fromEnum when


getWeekDay :: Day -> Weekday
getWeekDay = toEnum . (\(_, _, d) -> d - 1) . toWeekDate

getWeek :: Day -> Int
getWeek = toEnum . (\(_, w, _) -> w) . toWeekDate
