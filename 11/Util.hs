module Util where

import Data.Maybe (mapMaybe)
import Data.Vector (Vector, (!?))
import Data.List.Split (splitOn)

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

get :: Vector (Vector a) -> (Int, Int) -> Maybe a
get v (x, y) = (v !? y) >>= (!? x)

getMany :: Vector (Vector a) -> [(Int, Int)] -> [a]
getMany = mapMaybe . get

tAdd :: Num a => (a, a) -> (a, a) -> (a, a)
tAdd (a, b) (c, d) = (a + c, b + d)

consecutiveSame :: Eq a => [a] -> a
consecutiveSame (x:y:ys)
  | x == y    = x
  | otherwise = consecutiveSame (y:ys)
