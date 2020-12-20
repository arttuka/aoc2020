module Main where

import Data.List (unfoldr)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map, (!?), empty, fromList, insert)
import Data.Maybe (maybe)
import Util (readWith)

readNumbers :: [String] -> [Int]
readNumbers = map read . splitOn "," . head

speakNumbers :: [Int] -> [(Int, Int)]
speakNumbers nums = init initialNums ++ restNums
  where
    initialNums  = zip nums [0..]
    initialState = fromList $ init initialNums
    restNums     = map fst $ iterate speakNumber (last initialNums, initialState)

speakNumber :: ((Int, Int), Map Int Int) -> ((Int, Int), Map Int Int)
speakNumber ((prev, n), spoken) = ((next, n + 1), nextSpoken)
  where next = case spoken !? prev of
          Just t  -> n - t
          Nothing -> 0
        nextSpoken = insert prev n spoken

main :: IO ()
main = do numbers <- readWith readNumbers
          let spoken = speakNumbers numbers
          print $ spoken !! 2019
