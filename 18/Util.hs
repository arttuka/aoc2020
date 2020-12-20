module Util where

import Data.List.Split (splitOn)
import Data.Set (Set, fromList, member)

digits = fromList "0123456789"
ops = fromList "+*"

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

isDigit :: Char -> Bool
isDigit = (`member` digits)

isOp :: Char -> Bool
isOp = (`member` ops)
