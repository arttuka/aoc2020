module Main where

import Prelude hiding (length)
import Data.List (sort)
import Data.Vector ((!), fromList, length)
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

main :: IO ()
main = do nums <- readLines :: IO [Int]
          let (preamble, inputs) = splitAt window nums
              invalidNumber      = findInvalidNumber (reverse preamble) inputs
          putStrLn $ case invalidNumber of
            (Just i) -> show i
            Nothing  -> "Number not found"
