module Util where

import Data.List.Split (splitOn)
import Data.Tuple.Select (sel1)

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

type Euclid = (Integer, Integer, Integer)

euclid :: Integer -> Integer -> Euclid
euclid a b = last $ takeWhile ((0 /=) . sel1) results
  where
    results = (a, 1, 0) : (b, 0, 1) : zipWith step results (tail results)
    step :: Euclid -> Euclid -> Euclid
    step (r0, s0, t0) (r1, s1, t1) = (r2, s2, t2)
      where
        (q, r2) = quotRem r0 r1
        s2      = s0 - (q * s1)
        t2      = t0 - (q * t1)

mmiLcm :: Integer -> Integer -> (Integer, Integer)
mmiLcm n m = (s, (n * m) `div` r)
  where
    (r, s, _) = euclid n m
