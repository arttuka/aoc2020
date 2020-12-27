module Util where

import Control.Monad (join)
import Data.List (foldl')
import Data.List.Split (splitOn)
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

groupBy :: Ord k => (a -> k) -> [a] -> Map k [a]
groupBy f xs = fromListWith (++) [(f x, [x]) | x <- xs]

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