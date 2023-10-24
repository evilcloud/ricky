-- module Interface.Break
--   ( parseBreak,
--     parseBreakArgs,
--   )
-- where

-- import Text.Read (readMaybe)

-- -- Parse duration in "1h" format
-- parseDurationHours :: String -> Maybe Int
-- parseDurationHours str = do
--   hours <- readMaybe (init str) -- Remove the last character 'h' and parse as number
--   return $ hours * 60 -- Convert hours to minutes

-- parseBreak :: String -> Maybe Int
-- parseBreak = parseDurationHours

-- parseBreakArgs :: [String] -> IO ()
-- parseBreakArgs args = do
--   putStrLn $ "break: " ++ unwords args
--   let breakTime = parseBreak $ unwords args
--   putStrLn $ "result: " ++ maybe "Invalid break time" show breakTime

module Interface.Break
  ( parseBreak,
    parseBreakArgs,
  )
where

import Text.Read (readMaybe)

parseDurationHours :: String -> Maybe Int
parseDurationHours str = do
  hours <- readMaybe (init str)
  return $ hours * 60

parseBreak :: String -> Maybe Int
parseBreak = parseDurationHours

parseBreakArgs :: [String] -> IO (Maybe Int)
parseBreakArgs args = do
  putStrLn $ "break: " ++ unwords args
  let breakTime = parseBreak $ unwords args
  putStrLn $ "result: " ++ maybe "Invalid break time" show breakTime
  return breakTime
