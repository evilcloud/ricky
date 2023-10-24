-- module Interface.Work
--   ( parseWorkArgs,
--   )
-- where

-- import Control.Monad (msum)
-- import Data.List.Split (splitOn)
-- import Data.Maybe (isNothing)
-- import Data.Time
-- import Debug.Trace
-- import qualified Parser.Time as Time

-- parseWorkTimes :: String -> Maybe (TimeOfDay, Maybe TimeOfDay)
-- parseWorkTimes str =
--   let separators = ["-", " to "]
--       results = map (`splitOn` str) separators
--    in msum $ map parseWithSeparator $ traceShow str results
--   where
--     parseWithSeparator :: [String] -> Maybe (TimeOfDay, Maybe TimeOfDay)
--     parseWithSeparator [start] = do
--       startTime <- Time.parseTime start
--       return (startTime, Nothing)
--     parseWithSeparator [start, end] = do
--       startTime <- Time.parseTime start
--       endTime <- Time.parseTime end
--       return (startTime, Just endTime)
--     parseWithSeparator _ = Nothing

-- findTime :: [String] -> IO (Maybe (TimeOfDay, Maybe TimeOfDay))
-- findTime [] = return Nothing
-- findTime (x : xs) = do
--   let time = parseWorkTimes x
--   if isNothing time
--     then findTime xs
--     else return time

-- parseWorkArgs :: [String] -> IO ()
-- parseWorkArgs args = do
--   putStrLn $ "work: " ++ unwords args
--   workDate <- Time.parseDate args
--   workTimes <- findTime args
--   putStrLn $ "date: " ++ show workDate ++ ", result: " ++ maybe "Invalid work times" show workTimes

module Interface.Work
  ( parseWorkArgs,
  )
where

import Control.Monad (msum)
import Data.List.Split (splitOn)
import Data.Maybe (isNothing)
import Data.Time
import Debug.Trace
import qualified Parser.Time as Time

parseWorkTimes :: String -> Maybe (TimeOfDay, Maybe TimeOfDay)
parseWorkTimes str =
  let separators = ["-", " to "]
      results = map (`splitOn` str) separators
   in msum $ map parseWithSeparator $ traceShow str results
  where
    parseWithSeparator :: [String] -> Maybe (TimeOfDay, Maybe TimeOfDay)
    parseWithSeparator [start] = do
      startTime <- Time.parseTime start
      return (startTime, Nothing)
    parseWithSeparator [start, end] = do
      startTime <- Time.parseTime start
      endTime <- Time.parseTime end
      return (startTime, Just endTime)
    parseWithSeparator _ = Nothing

findTime :: [String] -> IO (Maybe (TimeOfDay, Maybe TimeOfDay))
findTime [] = return Nothing
findTime (x : xs) = do
  let time = parseWorkTimes x
  if isNothing time
    then findTime xs
    else return time

parseWorkArgs :: [String] -> IO (Maybe (TimeOfDay, Maybe TimeOfDay))
parseWorkArgs args = do
  putStrLn $ "work: " ++ unwords args
  workDate <- Time.parseDate args
  workTimes <- findTime args
  putStrLn $ "date: " ++ show workDate ++ ", result: " ++ maybe "Invalid work times" show workTimes
  return workTimes
