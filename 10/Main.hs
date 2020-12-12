module Main where

import Data.List (sort)
import Util (readLines, splitBetween, splitHeadAndTail, concatHeadAndTail, countWhere)

makePermutations :: [Int] -> [[Int]]
makePermutations = foldr (\ x -> (<*>) [(x:), id]) [[]]

permuteSegment :: [Int] -> [[Int]]
permuteSegment xs
    | length xs <= 2 = [xs]
    | otherwise      = concatHeadAndTail h t <$> makePermutations m
  where
    (h, m, t) = splitHeadAndTail xs

differences :: [Int] -> [Int]
differences = zipWith (-) =<< tail

validSegment :: [Int] -> Bool
validSegment = all (<=3) . differences

splitSegments :: [Int] -> [[Int]]
splitSegments nums = splitBetween (\a b -> b - a == 3) sorted
  where
    out    = 3 + maximum nums
    sorted = sort (0:out:nums)

countValidPermutations :: [Int] -> Int
countValidPermutations nums = product $ countWhere validSegment . permuteSegment <$> splitSegments nums

main :: IO ()
main = do nums <- readLines :: IO [Int]
          print $ countValidPermutations nums
