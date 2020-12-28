{-# LANGUAGE ScopedTypeVariables #-}
module Util where

import Control.Monad (join, liftM2)
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Map.Strict (Map, fromListWith)
import qualified Data.Map.Strict as M
import Data.Vector (Vector, (!), (//), fromList, toList)

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

bin2dec :: String -> Int
bin2dec = foldl' step 0
  where
    step :: Int -> Char -> Int
    step n c = 2 * n + if c == '#' then 1 else 0

reverseBin :: Int -> Int
reverseBin = step 512
  where
    step :: Int -> Int -> Int
    step _ 0 = 0
    step i n = (if even n then 0 else i) + step (i `div` 2) (n `div` 2)

lpad :: Int -> a -> [a] -> [a]
lpad l x xs = replicate (l - length xs) x ++ xs

rpad :: Int -> a -> [a] -> [a]
rpad l x xs = xs ++ replicate (l - length xs) x

dec2bin :: Int -> String
dec2bin = lpad 10 '.' . reverse . step
  where
    step :: Int -> String
    step 0 = ""
    step n = (if even n then '.' else '#') : step (n `div` 2)

powerset :: [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) = sets ++ map (x:) sets
  where sets = powerset xs

apply :: a -> (a -> a) -> a
apply x f = f x 

x = flip id

applyTo :: a -> [a -> a] -> a
applyTo = foldl' (flip id)

allEqual :: (a -> a -> Bool) -> [a] -> Bool
allEqual _ [] = True
allEqual eq (x:xs) = all (eq x) xs

juxt :: (a -> b) -> (a -> c) -> (a -> (b, c))
juxt = liftM2 (,)

groupBy :: Ord k => (a -> k) -> (a -> v) -> [a] -> Map k [v]
groupBy kf vf = fromListWith (++) . fmap (juxt kf (return . vf))

iSqrt :: Int -> Int
iSqrt = floor . sqrt . fromIntegral

findJust :: [Maybe a] -> Maybe a
findJust []           = Nothing
findJust (Just x:_)   = Just x
findJust (Nothing:xs) = findJust xs

concatLines :: [[String]] -> [String]
concatLines = map join . transpose

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x      = map head x : transpose (map tail x)

cartesian :: [[a]] -> [[a]]
cartesian = foldr (\x acc -> (:) <$> x <*> acc) [[]]

tailInit :: [a] -> [a]
tailInit = tail . init

from2DList :: [[a]] -> Vector (Vector a)
from2DList = fromList . (fromList <$>)

to2DList :: Vector (Vector a) -> [[a]]
to2DList = (toList <$>) . toList

(!!!) :: Vector (Vector a) -> (Int, Int) -> a
(!!!) v (x, y) = (v ! y) ! x

(///) :: forall a. Vector (Vector a) -> [(Int, Int, a)] -> Vector (Vector a)
(///) v vals = v // (toUpdate <$> M.toList gVals)
  where
    gVals = groupBy toKey toSubval vals
    toKey :: (Int, Int, a) -> Int
    toKey (_, y, _) = y
    toSubval :: (Int, Int, a) -> (Int, a)
    toSubval (x, _, a) = (x, a)
    toUpdate :: (Int, [(Int, a)]) -> (Int, Vector a)
    toUpdate (y, vs) = (y, (v ! y) // vs)

addT :: (Int, Int) -> (Int, Int) -> (Int, Int)
addT (a, b) (c, d) = (a + c, b + d)
