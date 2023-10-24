module Interface.Note (parseNote) where

parseNote :: [String] -> String
parseNote [] = ""
parseNote args = unwords args
