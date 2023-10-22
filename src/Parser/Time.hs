module Parser.Time
  ( parseWorkTimes,
  )
where

import Control.Monad (msum)
import Data.Char (isSpace)
import Data.List.Split (splitOn)
import Data.Time

strip :: String -> String
strip = f . f
  where
    f = reverse . dropWhile isSpace

-- Define parsing functions for each format
parseTime24 :: String -> Maybe TimeOfDay
parseTime24 = parseTimeM True defaultTimeLocale "%-k:%M"

parseTime12 :: String -> Maybe TimeOfDay
parseTime12 = parseTimeM True defaultTimeLocale "%-l%p"

parseTimeHour :: String -> Maybe TimeOfDay
parseTimeHour = parseTimeM True defaultTimeLocale "%-k"

-- Define a function that tries each format
parseTimeAny :: String -> Maybe TimeOfDay
parseTimeAny str = msum $ map ($ str) [parseTime24, parseTime12, parseTimeHour]

-- Modify parseWorkTimes to handle " to " as a separator
parseWorkTimes :: String -> Maybe (TimeOfDay, Maybe TimeOfDay)
parseWorkTimes str =
  let separators = ["-", " to "]
      results = map (`splitOn` str) separators
   in msum $ map parseWithSeparator results
  where
    parseWithSeparator :: [String] -> Maybe (TimeOfDay, Maybe TimeOfDay)
    parseWithSeparator [start] = do
      startTime <- parseTimeAny (strip start)
      return (startTime, Nothing)
    parseWithSeparator [start, end] = do
      startTime <- parseTimeAny (strip start)
      endTime <- parseTimeAny (strip end)
      return (startTime, Just endTime)
    parseWithSeparator _ = Nothing
