module Interface.Work
  ( processArgs,
  )
where

-- Add this line
import Data.Constructor (addEndTime, addStartTime, createWorkDay, updateBreakTime)
import Data.Maybe (fromJust, isJust)
import Data.Model (WorkDay (..))
import Data.Time (Day, TimeOfDay)
import Parser.Break (parseBreak)
import Parser.Time (parseWorkTimes)

processArgs :: WorkDay -> [String] -> IO ()
processArgs workDay [] = print workDay -- No more arguments, print the WorkDay
processArgs workDay (arg : args) = do
  newWorkDay <- processArg workDay arg
  processArgs newWorkDay args

processArg :: WorkDay -> String -> IO WorkDay
processArg workDay arg = do
  date <- parseDate arg
  let maybeWorkTimes = parseWorkTimes arg
  let maybeBreak = parseBreak arg
  case (maybeWorkTimes, maybeBreak) of
    (Just workTimes, _) -> updateWorkTimes workDay workTimes
    (_, Just breakTime) -> updateBreak workDay breakTime
    _ -> updateDate workDay date

updateDate :: WorkDay -> Day -> IO WorkDay
updateDate workDay date = return workDay {date = date}

updateWorkTimes :: WorkDay -> (TimeOfDay, Maybe TimeOfDay) -> IO WorkDay
updateWorkTimes workDay (startTime, maybeEndTime) = do
  newWorkDay <- addStartTime workDay startTime
  maybe (return newWorkDay) (addEndTime newWorkDay) maybeEndTime

updateBreak :: WorkDay -> Int -> IO WorkDay
updateBreak workDay breakTime = return $ updateBreakTime workDay breakTime
