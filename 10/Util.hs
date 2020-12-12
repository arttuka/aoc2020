{-# LANGUAGE TupleSections #-}
module Util where

import Control.Monad (ap)
import Data.List.Split (splitOn, split, whenElt, keepDelimsR, dropFinalBlank)
import Data.Map.Strict (Map, fromListWith)

getLines :: IO [String]
getLines = fmap lines getContents

readWith :: ([String] -> a) -> IO a
readWith = (<$> getLines)

readLinesWith :: (String -> a) -> IO [a]
readLinesWith = readWith . fmap

readLines :: Read a => IO [a]
readLines = readLinesWith read

readGroupsWith :: ([String] -> a) -> IO [a]
readGroupsWith f = fmap f . splitOn [""] <$> getLines

frequencies :: Ord a => [a] -> Map a Int
frequencies xs = fromListWith (+) $ zip xs (repeat 1)

splitBetween :: (a -> a -> Bool) -> [a] -> [[a]]
splitBetween pred = fmap (fmap fst) . splitter . ap zip tail
  where
    splitter = (split . keepDelimsR . dropFinalBlank . whenElt) (uncurry pred)

splitHeadAndTail :: [a] -> (a, [a], a)
splitHeadAndTail (x:xs) = uncurry (x,,) $ splitTail xs
  where
    splitTail :: [a] -> ([a], a)
    splitTail [x] = ([], x)
    splitTail (x:xs) = (x:ys, y)
      where
        (ys, y) = splitTail xs 

concatHeadAndTail :: a -> a -> [a] -> [a]
concatHeadAndTail h t m = h : m ++ [t]

countWhere :: (a -> Bool) -> [a] -> Int
countWhere pred = length . filter pred
