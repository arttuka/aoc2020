module Util where

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

getMany :: Vector (Vector a) -> [(Int, Int)] -> [Maybe a]
getMany = fmap . get

tAdd :: Num a => (a, a) -> (a, a) -> (a, a)
tAdd (a, b) (c, d) = (a + c, b + d)

iterateTail :: (a -> a) -> a -> [a]
iterateTail = (tail .) . iterate

consecutiveSame :: Eq a => [a] -> a
consecutiveSame (x:y:ys)
  | x == y    = x
  | otherwise = consecutiveSame (y:ys)
