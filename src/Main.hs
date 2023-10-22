module Main where

import Interface.Initial (parseArgs)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  parseArgs args
