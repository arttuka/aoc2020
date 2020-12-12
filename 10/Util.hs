module Util where

import Data.List.Split (splitOn)
import Data.Map.Strict (Map, fromListWith)

getLines :: IO [String]
getLines = fmap lines getContents

readWith :: ([String] -> a) -> IO a
readWith = (<$> getLines)

readLinesWith :: (String -> a) -> IO [a]
readLinesWith = readWith . fmap

readLines :: Read a => IO [a]
readLines = readLinesWith read

readGroupsWith :: ([String] -> a) -> IO [a]
readGroupsWith f = fmap f . splitOn [""] <$> getLines

frequencies :: Ord a => [a] -> Map a Int
frequencies xs = fromListWith (+) $ zip xs (repeat 1)
