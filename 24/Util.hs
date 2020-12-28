{-# LANGUAGE TupleSections #-}
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

addT :: (Int, Int) -> (Int, Int) -> (Int, Int)
addT (a, b) (c, d) = (a + c, b + d)

frequencies :: Ord a => [a] -> Map a Int
frequencies = fromListWith (+) . map (,1)
