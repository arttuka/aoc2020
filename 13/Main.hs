module Main where

import Data.List (sortOn)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Util (readWith)

nextDivisible :: Int -> Int -> Int
nextDivisible n divisor = case m of
    0 -> n
    _ -> n + divisor - m
  where
    m = n `mod` divisor

findNextBus :: Int -> [Int] -> (Int, Int)
findNextBus ts ids = head $ sortOn snd possibilities
  where
    possibilities = zip ids $ map (nextDivisible ts) ids

readIds :: String -> [Int]
readIds = mapMaybe readId . splitOn ","
  where
    readId :: String -> Maybe Int
    readId s = case s of
      "x" -> Nothing
      _   -> Just (read s)

readInput :: [String] -> (Int, [Int])
readInput [a,b] = (read a, readIds b)

main :: IO ()
main = do (ts, ids) <- readWith readInput
          let (id, nextTs) = findNextBus ts ids
          print (id, nextTs)
          print $ id * (nextTs - ts)
