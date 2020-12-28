module Main where

import Data.List (iterate, splitAt)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import Data.Set (Set, fromList, member, size)
import Util (elemIndex', insertAt, readWith)

readCups :: [String] -> [Int]
readCups = (read <$>) . chunksOf 1 . head

getDestination :: Set Int -> Set Int -> Int -> Int
getDestination allCups picked = step
  where
    maxCup = maximum allCups
    step :: Int -> Int
    step current
      | current `member` allCups && not (current `member` picked) = current
      | otherwise = step (if current <= 1 then maxCup else current - 1)

makeMove :: Int -> (Set Int -> Int -> Int) -> [Int] -> [Int]
makeMove len getDestination cups = newCups
  where
    current        = head cups
    (picked, more) = splitAt 3 $ drop 1 $ cycle cups
    otherCups      = take (len - 3) more
    destination    = getDestination (fromList picked) (current - 1)
    destIndex      = elemIndex' destination otherCups
    newCups        = insertAt (destIndex + 1) picked otherCups

getAnswer :: [Int] -> String
getAnswer cups = show =<< take (len - 1) (drop (index + 1) $ cycle cups)
  where
    len   = length cups 
    index = elemIndex' 1 cups

main :: IO ()
main = do cups <- readWith readCups
          let len     = length cups
              allCups = fromList cups
              getDest = getDestination allCups
              moves   = iterate (makeMove len getDest) cups
          putStrLn $ getAnswer $ moves !! 100
