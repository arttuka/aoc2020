module Util where

import Control.Monad ((<=<))
import Data.List.Split (splitOn)

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

mapIndexed :: (Int -> a -> b) -> [a] -> [b]
mapIndexed f = zipWith f [0..]

mapcatIndexed :: (Int -> a -> [b]) -> [a] -> [b]
mapcatIndexed f = uncurry f <=< zip [0..]

boundsRange :: (Int, Int) -> [Int]
boundsRange (lo, hi) = [(lo - 1)..(hi + 1)]

between :: Int -> Int -> Int -> Bool
between low high i = i >= low && i <= high

combinations :: [[a]] -> [[a]]
combinations = foldr ((<*>) . map (:)) [[]]

zipList3 :: [a] -> [a] -> [a] -> [[a]]
zipList3 = zipWith3 (\a b c -> [a, b, c])

applyT2 :: (a -> c, b -> d) -> (a, b) -> (c, d)
applyT2 (fa, fb) (a, b) = (fa a, fb b)
