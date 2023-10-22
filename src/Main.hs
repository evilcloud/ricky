module Main where

import Data.Constructor
import Data.Time
import Parser.Date (parseDate)
import Parser.Time (parseWorkTimes)

main :: IO ()
main = do
  -- Create a new work day starting at 9:00
  workDay <- createWorkDay $ TimeOfDay 9 0 0

  -- Print the initial state of the work day
  print workDay

  -- Add an end time of 12:00 to the work day
  workDayWithFirstEnd <- addEndTime workDay $ TimeOfDay 12 0 0

  -- Print the state of the work day after adding the first end time
  print workDayWithFirstEnd

  -- Update the notes
  let workDayWithFirstTask = updateNotes workDayWithFirstEnd "Worked on project X from 9:00 to 12:00."

  -- Print the state of the work day after updating the notes
  print workDayWithFirstTask

  -- Add a second start time of 11:00 to the work day, which is a crossover time
  workDayWithSecondStart <- addStartTime workDayWithFirstTask $ TimeOfDay 11 0 0

  -- Print the state of the work day after adding the second start time
  print workDayWithSecondStart

  -- Add a second end time of 15:00 to the work day
  workDayWithSecondEnd <- addEndTime workDayWithSecondStart $ TimeOfDay 15 0 0

  -- Print the state of the work day after adding the second end time
  print workDayWithSecondEnd

  -- Update the notes
  let workDayWithSecondTask = updateNotes workDayWithSecondEnd "Worked on project Y from 11:00 to 15:00."

  -- Print the state of the work day after updating the notes
  print workDayWithSecondTask

  -- Update the break time to 60 minutes
  let workDayWithBreak = updateBreakTime workDayWithSecondTask 60

  -- Print the state of the work day after updating the break time
  print workDayWithBreak

  print =<< parseDate "today"
  print =<< parseDate "yesterday"
  print =<< parseDate "monday"
  print =<< parseDate "not a day"

  -- Test the parseWorkTimes function
  print $ parseWorkTimes "9:00"
  print $ parseWorkTimes "9am"
  print $ parseWorkTimes "9"
  print $ parseWorkTimes "9-12"
  print $ parseWorkTimes "9am to 12pm"
  print $ parseWorkTimes "9:00-12:00"
  print $ parseWorkTimes "not a time"