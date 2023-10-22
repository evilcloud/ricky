module Parser.Break
  ( parseBreak,
  )
where

import Control.Applicative ((<|>))
import Data.List.Split (splitOn)
import Text.Read (readMaybe)

-- Parse duration in "1h" format
parseDurationHours :: String -> Maybe Int
parseDurationHours str = do
  hours <- readMaybe (init str) -- Remove the last character 'h' and parse as number
  return $ hours * 60 -- Convert hours to minutes

-- Parse duration in "1:30" format
parseDurationTime :: String -> Maybe Int
parseDurationTime str = case splitOn ":" str of
  [hoursStr, minutesStr] -> do
    hours <- readMaybe hoursStr
    minutes <- readMaybe minutesStr
    return $ hours * 60 + minutes
  _ -> Nothing

-- Combine the two parsers
parseBreak :: String -> Maybe Int
parseBreak str = parseDurationHours str <|> parseDurationTime str
