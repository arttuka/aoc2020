module Main where

import Data.Set (fromList, intersection, size)
import Util (readGroupsWith)

readGroup :: [String] -> Int
readGroup = size . foldl1 intersection . fmap fromList

main :: IO ()
main = do groups <- readGroupsWith readGroup
          print $ sum groups
