module Data.Constructor
  ( createWorkDay,
    addStartTime,
    addEndTime,
    updateBreakTime,
    updateNotes,
  )
where

import Data.List (sortOn)
import Data.Maybe (fromMaybe, isNothing)
import Data.Model (WorkDay (..), WorkTime (..))
import Data.Time (TimeOfDay, getZonedTime, localDay, zonedTimeToLocalTime)
import qualified Interface.Break as Break
import qualified Interface.Work as Work

createWorkDay :: TimeOfDay -> IO WorkDay
createWorkDay startTime = do
  currentZonedTime <- getZonedTime
  let currentLocalTime = zonedTimeToLocalTime currentZonedTime
  let currentDate = localDay currentLocalTime
  return
    WorkDay
      { date = currentDate,
        workTimes = [WorkTime startTime Nothing],
        breakTime = Nothing,
        notes = Nothing
      }

-- Function to handle "break" command
handleBreak :: [String] -> IO (Maybe String, [String])
handleBreak args = do
  let (breakArgs, rest) = getElementsBeforeKeyword args
  maybeBreakTime <- Break.parseBreakArgs breakArgs
  case maybeBreakTime of
    Just breakTime -> return (Just (show breakTime), rest)
    Nothing -> do
      putStrLn "Warning: Invalid break time."
      return (Nothing, rest)

-- Function to handle "work" command
handleWork :: [String] -> IO ([String], [String])
handleWork args = do
  let (workArgs, rest) = getElementsBeforeKeyword args
  return (workArgs, rest)

-- Function to parse arguments
parseArgs :: [String] -> IO (Maybe WorkDay)
parseArgs [] = return Nothing
parseArgs (arg : restArgs) = do
  maybeWorkDay <- parseArgs []
  case maybeWorkDay of
    Nothing -> parseArgs restArgs
    Just workDay -> case arg of
      "break" -> do
        (maybeBreakTime, rest) <- handleBreak restArgs
        case maybeBreakTime of
          Just breakTime -> do
            updatedWorkDay <- parseArgs rest
            return $ Just $ Constructor.updateBreakTime updatedWorkDay (read breakTime)
          Nothing -> parseArgs rest
      _ -> do
        (workArgs, rest) <- handleWork restArgs
        workTimes <- Work.parseWorkArgs [unwords workArgs]
        updatedWorkDay <- foldM Constructor.addWorkTime workDay workTimes
        nextWorkDay <- parseArgs rest
        return $ Just $ mergeWorkDays updatedWorkDay nextWorkDay
