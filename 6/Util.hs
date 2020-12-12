module Util where

import Data.List.Split (splitOn)

getLines :: IO [String]
getLines = fmap lines getContents

readLinesWith :: (String -> a) -> IO [a]
readLinesWith f = (fmap . fmap) f getLines

readLines :: Read a => IO [a]
readLines = readLinesWith read

readGroupsWith :: ([String] -> a) -> IO [a]
readGroupsWith f = fmap f <$> fmap (splitOn [""]) getLines
