module Main where

import Prelude hiding (length)
import Data.List (sort)
import Data.Vector (Vector, (!), fromList, length)
import Util

findPair :: Vector Int -> Int -> Int -> Int -> Maybe (Int, Int)
findPair ints target i j
    | i == j            = Nothing
    | (x + y) == target = Just (x, y)
    | (x + y) < target  = findPair ints target (i + 1) j
    | otherwise         = findPair ints target i (j - 1)
  where
    x = ints ! i
    y = ints ! j

findTriplet' :: Vector Int -> Int -> Int -> Maybe (Int, Int, Int)
findTriplet' ints target k
    | k == l - 3  = Nothing
    | otherwise   = case findPair ints (target - z) (k + 1) (l - 1) of
      Nothing -> findTriplet' ints target (k + 1)
      Just (x, y) -> Just (x, y, z)
  where
    l = length ints
    z = ints ! k

findTriplet :: [Int] -> Int -> Maybe (Int, Int, Int)
findTriplet ints target = findTriplet' (fromList (sort ints)) target 0

main :: IO ()
main = do ints <- Util.readLines :: IO [Int]
          let result = findTriplet ints 2020
          putStrLn $ case result of
            Nothing -> "No result"
            Just (x, y, z) -> show (x * y * z)
