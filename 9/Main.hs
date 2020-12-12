module Main where

import Prelude hiding (length)
import Data.List (sort)
import Data.Vector ((!), fromList, length, toList, slice)
import Util (readLines)

window = 25

findPair :: [Int] -> Int -> Bool
findPair ints target = findPair' 0 (length vec - 1)
  where
    vec = fromList $ sort ints
    findPair' :: Int -> Int -> Bool
    findPair' i j
        | i == j            = False
        | (x + y) == target = True
        | (x + y) < target  = findPair' (i + 1) j
        | otherwise         = findPair' i (j - 1)
      where
        x = vec ! i
        y = vec ! j

findInvalidNumber :: [Int] -> [Int] -> Maybe Int
findInvalidNumber _ []  = Nothing
findInvalidNumber prevNums (x:xs)
  | findPair prevNums x = findInvalidNumber (take window (x:prevNums)) xs
  | otherwise           = Just x

findSet :: Int -> [Int] -> Maybe [Int]
findSet target nums = findSet' 0 1
  where
    vec = fromList nums
    max = length vec
    findSet' :: Int -> Int -> Maybe [Int]
    findSet' i l
        | i + l == max = Nothing
        | total == target = Just $ toList subvec
        | total < target  = findSet' i (l + 1)
        | otherwise       = findSet' (i + 1) (l - 1)
      where
        subvec = slice i l vec
        total = sum subvec

main :: IO ()
main = do nums <- readLines :: IO [Int]
          let foundSet = findSet 177777905 nums
          putStrLn $ case foundSet of
            (Just nums) -> show (maximum nums + minimum nums)
            Nothing  -> "Set not found"
