module Main where

import Data.IntMap.Strict (IntMap, (!), fromList, insert)
import Data.List (foldl', notElem, iterate)
import Data.List.Split (chunksOf, divvy)
import Util (readWith)

maxCup = 1000000

readCups :: [String] -> [Int]
readCups = (read <$>) . chunksOf 1 . head

getDestination :: [Int] -> Int -> Int
getDestination picked current
  | current > 0 && current `notElem` picked = current
  | otherwise = getDestination picked (if current < 1 then maxCup else current - 1)

insert' :: IntMap Int -> (Int, Int) -> IntMap Int
insert' = flip $ uncurry insert

makeMove :: (Int, Int, IntMap Int) -> (Int, Int, IntMap Int)
makeMove (i, currentCup, cups) = (i + 1, nextCup, foldl' insert' cups updates)
  where
    [_, p1, p2, p3, nextCup] = take 5 $ iterate (cups !) currentCup
    destCup                  = getDestination [p1, p2, p3] (currentCup - 1)
    destNext                 = cups ! destCup
    updates                  = [(currentCup, nextCup), (destCup, p1), (p3, destNext)]
  
getAnswer :: (Int, Int, IntMap Int) -> Int
getAnswer (_, _, cups) = product $ take 3 $ iterate (cups !) 1

main :: IO ()
main = do startingCups <- readWith readCups
          let cups    = startingCups ++ [maximum startingCups + 1 .. maxCup] ++ [head startingCups]
              cupsMap = fromList [(c1, c2) | [c1, c2] <- divvy 2 1 cups]
              moves   = iterate makeMove (0, head startingCups, cupsMap)
              result  = moves !! 10000000
          print $ getAnswer result
