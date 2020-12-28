module Util where

import Control.Monad (liftM2)
import Data.List.Split (splitOn)
import Data.Set (Set, elemAt, empty, intersection, member, size)

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

isSingleton :: Set a -> Bool
isSingleton = (1 ==) . size

getSingleton :: Set a -> Maybe a
getSingleton s = if isSingleton s then Just (elemAt 0 s) else Nothing

if' :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
if' pred thenF elseF x = if pred x then thenF x else elseF x
