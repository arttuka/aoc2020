module Main where

import Prelude hiding (length)
import Data.List (sort)
import Data.Vector (Vector, (!), fromList, length)
import Util

findPair' :: Vector Int -> Int -> Int -> Int -> Maybe (Int, Int)
findPair' ints target i j
    | i == j            = Nothing
    | (x + y) == target = Just (x, y)
    | (x + y) < target  = findPair' ints target (i + 1) j
    | otherwise         = findPair' ints target i (j - 1)
  where
    x = ints ! i
    y = ints ! j

findPair :: [Int] -> Int -> Maybe (Int, Int)
findPair ints target = findPair' intvec target 0 (length intvec - 1)
  where
    intvec = fromList ints

main :: IO ()
main = do nums <- Util.readLines :: IO [Int]
          let result = findPair (sort nums) 2020
          putStrLn $ case result of
            Nothing -> "No result"
            Just (x, y) -> show (x * y)
