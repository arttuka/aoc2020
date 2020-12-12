module Util where

getLines :: IO [String]
getLines = fmap lines getContents

readLinesWith :: (String -> a) -> IO [a]
readLinesWith f = (fmap . fmap) f getLines

readLines :: Read a => IO [a]
readLines = readLinesWith read
