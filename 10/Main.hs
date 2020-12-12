module Main where

import Data.List (sort)
import Data.Map.Strict ((!))
import Util (readLines, frequencies)

countDifferences :: [Int] -> Int
countDifferences nums = (freqs ! 1) * (freqs ! 3)
  where
    out    = 3 + maximum nums
    sorted = sort (0:out:nums)
    freqs = frequencies $ zipWith (-) (tail sorted) sorted

main :: IO ()
main = do nums <- readLines :: IO [Int]
          print $ countDifferences nums
