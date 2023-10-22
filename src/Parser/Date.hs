module Parser.Date
  ( parseDate,
  )
where

import Data.Maybe (fromMaybe)
import Data.Time
import Data.Time.Calendar.WeekDate (toWeekDate)

parseDate :: String -> IO Day
parseDate str = do
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
  fromMaybe (return today) $ lookup str formats

getDayOfWeek :: Day -> Int -> Int -> IO Day
getDayOfWeek today currentWeekday n =
  return $ addDays (fromIntegral $ n - currentWeekday) today
