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

addStartTime :: WorkDay -> TimeOfDay -> IO WorkDay
addStartTime workDay startTime
  | not (null (workTimes workDay)) && isNothing (end $ last $ workTimes workDay) = do
      putStrLn "Warning: Can't add a start time because the last work time doesn't have an end time."
      return workDay
  | otherwise = return $ workDay {workTimes = workTimes workDay ++ [newWorkTime]}
  where
    newWorkTime = WorkTime startTime Nothing

addEndTime :: WorkDay -> TimeOfDay -> IO WorkDay
addEndTime workDay endTime
  | any (\wt -> isNothing (end wt) && endTime <= start wt) (workTimes workDay) = do
      putStrLn "Warning: Can't add an end time that's before or at the last start time."
      return workDay
  | otherwise = return $ mergeWorkTimes $ workDay {workTimes = updatedWorkTimes}
  where
    updatedWorkTimes = map updateWorkTime (workTimes workDay)
    updateWorkTime workTime =
      if isNothing (end workTime) || endTime > fromMaybe (start workTime) (end workTime)
        then workTime {end = Just endTime}
        else workTime

updateBreakTime :: WorkDay -> Int -> WorkDay
updateBreakTime workDay newBreakTime = workDay {breakTime = Just newBreakTime}

updateNotes :: WorkDay -> String -> WorkDay
updateNotes workDay newNotes = workDay {notes = Just newNotes}

mergeWorkTimes :: WorkDay -> WorkDay
mergeWorkTimes workDay = workDay {workTimes = mergedWorkTimes}
  where
    sortedWorkTimes = sortOn start (workTimes workDay)
    mergedWorkTimes = foldr mergeWorkTime [] sortedWorkTimes

    mergeWorkTime :: WorkTime -> [WorkTime] -> [WorkTime]
    mergeWorkTime wt [] = [wt]
    mergeWorkTime wt (wt' : wts)
      | maybe False (>= start wt') (end wt) = mergeWorkTime (wt {end = max (end wt) (end wt')}) wts
      | otherwise = wt : wt' : wts
