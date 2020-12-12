module Main where

import Data.Vector (Vector, (!), fromList)
import Util (readLinesWith)

findRoute :: [Vector Char] -> Int
findRoute lines = findRoute' lines 0
  where
    w                         = length (head lines)
    findRoute' :: [Vector Char] -> Int -> Int
    findRoute' [] _           = 0
    findRoute' (line:lines) x = add + findRoute' lines x'
      where
        add = if line ! x == '#' then 1 else 0
        x'  = (x + 3) `mod` w

main :: IO ()
main = do lines <- readLinesWith fromList
          print $ findRoute lines
