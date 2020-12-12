module Main where

import Data.List.Split (splitOn)
import Util (readLinesWith, countWhere)

type Entry = (Int, Int, Char, String)

parseEntry :: String -> Entry
parseEntry s = (low, hi, head c, pass)
  where
    [cond, pass] = splitOn ": " s
    [lim, c] = splitOn " " cond
    [low, hi] = map (read :: String -> Int) $ splitOn "-" lim

isValid :: Entry -> Bool
isValid (low, hi, c, pass) = cnt >= low && cnt <= hi
  where
    cnt = countWhere (== c) pass

main :: IO ()
main = do entries <- readLinesWith parseEntry
          print $ countWhere isValid entries 
