module Util where

getLines :: IO [String]
getLines = fmap lines getContents

readLines :: Read a => IO [a]
readLines = (fmap . fmap) read getLines
