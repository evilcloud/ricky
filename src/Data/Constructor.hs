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