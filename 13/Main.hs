module Main where

import Data.List (foldl1')
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Util (mmiLcm, readWith)

readLines :: String -> [(Integer, Integer)]
readLines = catMaybes . zipWith readLine [0..] . splitOn ","
  where
    readLine :: Integer -> String -> Maybe (Integer, Integer)
    readLine i s = case s of
      "x" -> Nothing
      _   -> Just (i, read s)
    
readInput :: [String] -> (Integer, [(Integer, Integer)])
readInput [a,b] = (read a, readLines b)

combineLines :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
combineLines (s, oldMod) (d, newMod) = (x * newMod - d, lcm)
  where
    (mmi, lcm) = mmiLcm newMod oldMod
    x = ((d + s) * mmi) `mod` oldMod 

main :: IO ()
main = do (ts, lines) <- readWith readInput
          print $ foldl1 combineLines lines