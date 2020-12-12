module Util where

import Data.List.Split (splitOn)
import Data.Set (fromList, member)

getLines :: IO [String]
getLines = fmap lines getContents

readLinesWith :: (String -> a) -> IO [a]
readLinesWith f = (fmap . fmap) f getLines

readLines :: Read a => IO [a]
readLines = readLinesWith read

readGroupsWith :: ([String] -> a) -> IO [a]
readGroupsWith f = fmap f <$> fmap (splitOn [""]) getLines

countWhere :: (a -> Bool) -> [a] -> Int
countWhere pred = length . filter pred

numericChars = fromList "0123456789"
hexChars     = fromList "0123456789abcdef"

isNumeric :: Char -> Bool
isNumeric = (`member` numericChars)

isHex :: Char -> Bool
isHex = (`member` hexChars)

between :: Int -> Int -> Int -> Bool
between low high i = i >= low && i <= high
