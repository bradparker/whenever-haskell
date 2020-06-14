{-# OPTIONS_GHC -Wall #-}

module Main
  ( main,
  )
where

import Brick
  ( (<=>),
    Widget,
    fill,
    hBox,
    padAll,
    padLeftRight,
    simpleMain,
    str,
    vBox,
    withBorderStyle,
  )
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Border.Style (unicode)
import Control.Lens (_1, _2, _3, view)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time (Day)
import qualified Data.Time as Time
import qualified Data.Time.Calendar.MonthDay as Time
import qualified Data.Time.Calendar.WeekDate as Time

data MonthOfYear
  = Janurary
  | Feburary
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  deriving (Eq, Ord, Enum, Show)

data Month
  = Month
      { mYear :: Integer,
        mMonth :: MonthOfYear
      }
  deriving (Eq, Ord, Show)

monthOfDay :: Day -> Month
monthOfDay =
  ( Month
      <$> view _1
      <*> toEnum . subtract 1 . view _2
  )
    . Time.toGregorian

firstOfMonth :: Month -> Day
firstOfMonth (Month y m) =
  Time.fromGregorian y (fromEnum m + 1) 1

lastOfMonth :: Month -> Day
lastOfMonth (Month y m) =
  let monthNumber = fromEnum m + 1
   in Time.fromGregorian
        y
        monthNumber
        (Time.monthLength (Time.isLeapYear y) monthNumber)

monthBounds :: Month -> (Day, Day)
monthBounds = (,) <$> firstOfMonth <*> lastOfMonth

daysInMonth :: Month -> [Day]
daysInMonth = uncurry enumFromTo . monthBounds

data Week
  = Week
      { wYear :: Integer,
        wWeek :: Int
      }
  deriving (Eq, Ord, Show)

weekOfDay :: Day -> Week
weekOfDay = (Week <$> view _1 <*> view _2) . Time.toWeekDate

weeksInMonth :: Month -> Set Week
weeksInMonth = Set.fromList . map weekOfDay . daysInMonth

firstOfWeek :: Week -> Day
firstOfWeek (Week y w) = Time.fromWeekDate y w 1

lastOfWeek :: Week -> Day
lastOfWeek (Week y w) = Time.fromWeekDate y w 7

weekBounds :: Week -> (Day, Day)
weekBounds = (,) <$> firstOfWeek <*> lastOfWeek

daysInWeek :: Week -> [Day]
daysInWeek = uncurry enumFromTo . weekBounds

dayOfMonth :: Day -> Int
dayOfMonth = view _3 . Time.toGregorian

noEvents :: Widget n
noEvents = fill ' '

day :: Day -> Widget n
day d = str (show (dayOfMonth d)) <=> hBorder <=> noEvents

week :: Week -> Widget n
week = hBox . (day <$>) . daysInWeek

month :: Month -> Widget n
month m =
  borderWithLabel
    (padLeftRight 1 (str (show (mMonth m) <> ", " <> show (mYear m))))
    (padAll 1 (vBox (map week (Set.toList (weeksInMonth m)))))

ui :: Month -> Widget ()
ui = withBorderStyle unicode . padAll 1 . month

thisMonth :: IO Month
thisMonth =
  monthOfDay
    . Time.localDay
    . Time.zonedTimeToLocalTime <$> Time.getZonedTime

main :: IO ()
main = (simpleMain . ui) =<< thisMonth
