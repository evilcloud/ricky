module Interface.Initial where

import qualified Initial.WorkTime as IW
import qualified Parser.Break as Break

parseArgs :: [String] -> IO ()
parseArgs [] = return ()
parseArgs (arg : args) = case arg of
  "break" -> do
    let (breakArgs, rest) = getElementsBeforeKeyword args
    parseBreak breakArgs
    parseArgs rest
  "work" -> do
    let (workArgs, rest) = getElementsBeforeKeyword args
    parseWork workArgs
    parseArgs rest
  _ -> do
    putStrLn $ "Invalid command: " ++ arg
    return ()

parseBreak :: [String] -> IO ()
parseBreak args = do
  putStrLn $ "break: " ++ unwords args
  let breakTime = Break.parseBreak $ unwords args
  putStrLn $ "result: " ++ maybe "Invalid break time" show breakTime

parseWork :: [String] -> IO ()
parseWork args = do
  putStrLn $ "work: " ++ unwords args
  let workTimes = IW.parseWorkTimes $ unwords args
  putStrLn $ "result: " ++ maybe "Invalid work times" show workTimes

keywords :: [String]
keywords = ["break", "work"]

getElementsBeforeKeyword :: [String] -> ([String], [String])
getElementsBeforeKeyword = span (not . isKeyword)
  where
    isKeyword x = x `elem` keywords
