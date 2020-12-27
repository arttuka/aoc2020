module Util where

import Data.List (foldr, intercalate)
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

indent :: Int -> String -> String
indent n s = intercalate "\n" $ map (spaces ++) $ lines s
  where
    spaces = replicate n ' '

cartesian :: [[a]] -> [[a]]
cartesian = foldr (\x acc -> (:) <$> x <*> acc) [[]]
