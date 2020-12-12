module Util where

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

tMul :: (Int, Int) -> Int -> (Int, Int)
tMul (x, y) n = (x * n, y * n)

rotate :: Int -> (Int, Int) -> (Int, Int)
rotate 0 pos = pos
rotate n (x, y) = rotate (n - 1) (- y, x)

tAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
tAdd (a, b) (c, d) = (a + c, b + d)
