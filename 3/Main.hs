module Main where

import Data.Vector (Vector, (!), fromList)
import Util (readLinesWith)

findRoute :: [Vector Char] -> (Int, Int) -> Int
findRoute lines (dx, dy) = findRoute' lines 0
  where
    w                         = length (head lines)
    findRoute' :: [Vector Char] -> Int -> Int
    findRoute' [] _           = 0
    findRoute' (line:lines) x = add + findRoute' (drop (dy - 1) lines) x'
      where
        add = if line ! x == '#' then 1 else 0
        x'  = (x + dx) `mod` w

slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

main :: IO ()
main = do lines <- readLinesWith fromList
          let results = map (findRoute lines) slopes
          print $ product results
