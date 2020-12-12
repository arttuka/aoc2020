module Main where

import Control.Monad (join)
import Data.Set (fromList, size)
import Util (readGroupsWith)

readGroup :: [String] -> Int
readGroup = size . fromList . join

main :: IO ()
main = do groups <- readGroupsWith readGroup
          print $ sum groups
