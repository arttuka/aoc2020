module Util where

import Data.Int (Int64)
import Data.List (foldl', replicate)
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

bin2dec :: String -> Int64
bin2dec = foldl' step 0
  where
    step :: Int64 -> Char -> Int64
    step n c = 2 * n + if c == '1' then 1 else 0

lpad :: Int -> a -> [a] -> [a]
lpad l x xs = replicate (l - length xs) x ++ xs

dec2bin :: Int64 -> String
dec2bin = lpad 36 '0' . take 36 . step
  where
    step :: Int64 -> String
    step 0 = ""
    step n = (if even n then '0' else '1') : step (n `div` 2)

replace :: Eq a => [a] -> a -> a -> [a]
replace [] _ _     = []
replace (x:xs) s r = (if x == s then r else x) : replace xs s r
