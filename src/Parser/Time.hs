module Parser.Time
  ( parseDate,
    parseTime,
  )
where

import Control.Monad (msum)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Data.Time
import Data.Time.Calendar.WeekDate (toWeekDate)
import Debug.Trace

strip :: String -> String
strip = f . f
  where
    f = reverse . dropWhile isSpace

-- Define parsing functions for each format for time
parseTime24 :: String -> Maybe TimeOfDay
parseTime24 = parseTimeM True defaultTimeLocale "%-k:%M"

parseTime12 :: String -> Maybe TimeOfDay
parseTime12 = parseTimeM True defaultTimeLocale "%-l%p"

parseTimeHour :: String -> Maybe TimeOfDay
parseTimeHour = parseTimeM True defaultTimeLocale "%-k"

-- -- Define a function that tries each format
-- parseTime :: String -> Maybe TimeOfDay
-- parseTime str = msum $ map ($ str) [parseTime24, parseTime12, parseTimeHour]

-- Define a function that tries each format
parseTime :: String -> Maybe TimeOfDay
parseTime str = trace ("parseTime: " ++ str) $ msum $ map ($ str) [parseTime24, parseTime12, parseTimeHour]

-- Parser for dates

parseDate :: [String] -> IO Day
parseDate strs = do
  today <- utctDay <$> getCurrentTime
  let (_, _, currentWeekday) = toWeekDate today
  let formats =
        [ ("today", return today),
          ("yesterday", return $ addDays (-1) today),
          ("monday", getDayOfWeek today currentWeekday 1),
          ("tuesday", getDayOfWeek today currentWeekday 2),
          ("wednesday", getDayOfWeek today currentWeekday 3),
          ("thursday", getDayOfWeek today currentWeekday 4),
          ("friday", getDayOfWeek today currentWeekday 5),
          ("saturday", getDayOfWeek today currentWeekday 6),
          ("sunday", getDayOfWeek today currentWeekday 7)
        ]
  fromMaybe (return today) $ msum $ map (`lookup` formats) strs

getDayOfWeek :: Day -> Int -> Int -> IO Day
getDayOfWeek today currentWeekday n =
  return $ addDays (fromIntegral $ n - currentWeekday) today
