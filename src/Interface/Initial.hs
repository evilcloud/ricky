-- -- module Interface.Initial where

-- -- import qualified Initial.WorkTime as IW
-- -- import qualified Parser.Break as Break

-- -- parseArgs :: [String] -> IO ()
-- -- parseArgs [] = return ()
-- -- parseArgs (arg : args) = case arg of
-- --   "break" -> do
-- --     let (breakArgs, rest) = getElementsBeforeKeyword args
-- --     parseBreak breakArgs
-- --     parseArgs rest
-- --   "work" -> do
-- --     let (workArgs, rest) = getElementsBeforeKeyword args
-- --     parseWork workArgs
-- --     parseArgs rest
-- --   _ -> do
-- --     putStrLn $ "Invalid command: " ++ arg
-- --     return ()

-- -- parseBreak :: [String] -> IO ()
-- -- parseBreak args = do
-- --   putStrLn $ "break: " ++ unwords args
-- --   let breakTime = Break.parseBreak $ unwords args
-- --   putStrLn $ "result: " ++ maybe "Invalid break time" show breakTime

-- -- parseWork :: [String] -> IO ()
-- -- parseWork args = do
-- --   putStrLn $ "work: " ++ unwords args
-- --   let workTimes = IW.parseWorkTimes $ unwords args
-- --   putStrLn $ "result: " ++ maybe "Invalid work times" show workTimes

-- -- keywords :: [String]
-- -- keywords = ["break", "work"]

-- -- getElementsBeforeKeyword :: [String] -> ([String], [String])
-- -- getElementsBeforeKeyword = span (not . isKeyword)
-- --   where
-- --     isKeyword x = x `elem` keywords

-- module Interface.Initial where

-- import qualified Interface.Break as Break
-- import qualified Interface.Work as Work

-- parseArgs :: [String] -> IO ()
-- parseArgs [] = return ()
-- parseArgs (arg : args) = case arg of
--   "break" -> do
--     let (breakArgs, rest) = getElementsBeforeKeyword args
--     Break.parseBreakArgs breakArgs
--     parseArgs rest
--   _ -> do
--     let (workArgs, rest) = getElementsBeforeKeyword args
--     Work.parseWorkArgs [unwords workArgs]
--     parseArgs rest

-- -- _ -> do
-- --   putStrLn $ "Invalid command: " ++ arg
-- --   return ()

-- keywords :: [String]
-- keywords = ["break", "work"]

-- -- getElementsBeforeKeyword :: [String] -> ([String], [String])
-- -- getElementsBeforeKeyword = span (not . isKeyword)
-- --   where
-- --     isKeyword x = x `elem` keywords

-- -- function above was working fine. Copilot has suggested the improvement, which has not been tested yet.
-- getElementsBeforeKeyword :: [String] -> ([String], [String])
-- getElementsBeforeKeyword = break isKeyword
--   where
--     isKeyword x = x `elem` keywords

module Interface.Initial where

import qualified Data.Constructor as Constructor
import Data.Model (WorkDay) -- Add this line
import Data.Time (TimeOfDay)
import qualified Interface.Break as Break
import qualified Interface.Work as Work

keywords :: [String]
keywords = ["break", "work"]

getElementsBeforeKeyword :: [String] -> ([String], [String])
getElementsBeforeKeyword = break isKeyword
  where
    isKeyword x = x `elem` keywords

parseArgs :: [String] -> IO ()
parseArgs args = do
  workDay <- parseArgs' args
  putStrLn $ "WorkDay: " ++ show workDay
  where
    parseArgs' :: [String] -> IO WorkDay
    parseArgs' [] = Constructor.createWorkDay Nothing
    parseArgs' (arg : restArgs) = do
      workDay <- parseArgs' []
      case arg of
        "break" -> do
          let (breakArgs, rest) = getElementsBeforeKeyword restArgs
          maybeBreakTime <- Break.parseBreakArgs breakArgs
          case maybeBreakTime of
            Just breakTime -> do
              updatedWorkDay <- parseArgs' rest
              return $ Constructor.updateBreakTime updatedWorkDay breakTime
            Nothing -> do
              putStrLn "Warning: Invalid break time."
              parseArgs' rest
        _ -> do
          let (workArgs, rest) = getElementsBeforeKeyword restArgs
          workTimes <- Work.parseWorkArgs [unwords workArgs]
          updatedWorkDay <- foldM Constructor.addWorkTime workDay workTimes
          nextWorkDay <- parseArgs' rest
          return $ mergeWorkDays updatedWorkDay nextWorkDay
