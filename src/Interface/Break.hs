module Interfrace.Break
  ( parseBreak,
  )
where

import Data.Time (TimeOfDay (..))
import qualified Parser.Time as Time

-- Parse break time
parseBreak :: String -> Maybe Int
parseBreak str =
  Time.parseTime str >>= \timeOfDay ->
    return $ todHour timeOfDay * 60 + todMin timeOfDay -- Convert hours to minutes
