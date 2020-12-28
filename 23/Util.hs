module Util where

import Data.List (elemIndex, splitAt)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

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

insertAt :: Int -> [a] -> [a] -> [a]
insertAt i xs ys = start ++ xs ++ end
  where
    (start, end) = splitAt i ys

elemIndex' :: Eq a => a -> [a] -> Int
elemIndex' = (fromJust .) . elemIndex
