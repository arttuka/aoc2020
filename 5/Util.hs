module Util where

import Data.List (foldl')

getLines :: IO [String]
getLines = fmap lines getContents

readLinesWith :: (String -> a) -> IO [a]
readLinesWith f = (fmap . fmap) f getLines

readLines :: Read a => IO [a]
readLines = readLinesWith read

bin2dec :: String -> Int
bin2dec = foldl' step 0
  where
    step :: Int -> Char -> Int
    step n c = 2 * n + if c == '1' then 1 else 0

fmapt :: (a -> b) -> (a, a) -> (b, b)
fmapt f (x, y) = (f x, f y)
