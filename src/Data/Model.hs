module Data.Model
  ( WorkTime (..),
    WorkDay (..),
  )
where

import Data.Time (Day, TimeOfDay)

data WorkTime = WorkTime
  { start :: TimeOfDay,
    end :: Maybe TimeOfDay
  }
  deriving (Show, Eq)

data WorkDay = WorkDay
  { date :: Day,
    workTimes :: [WorkTime],
    breakTime :: Maybe Int,
    notes :: Maybe String
  }
  deriving (Show, Eq)
