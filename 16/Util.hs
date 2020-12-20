module Util where

import Data.List.Split (splitOn)
import Data.Set (Set, elemAt, size)

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

between :: Int -> Int -> Int -> Bool 
between lo hi x = lo <= x && x <= hi

anyPred :: [a -> Bool] -> a -> Bool
anyPred preds x = any (\f -> f x) preds

isSingleton :: Set a -> Bool
isSingleton = (1 ==) . size

getSingleton :: Set a -> Maybe a
getSingleton s = case size s of
  1 -> Just (elemAt 0 s)
  _ -> Nothing
