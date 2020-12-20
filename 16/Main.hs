module Main where

import Data.List.Split (splitOn)
import Util (anyPred, between, getLines)

readLimits :: String -> Int -> Bool
readLimits s = between lo hi 
  where
    [lo, hi] = map read $ splitOn "-" s

readValidator :: String -> Int -> Bool
readValidator s = anyPred $ map readLimits $ splitOn " or " limits
  where
    [_, limits] = splitOn ": " s

readTicket :: String -> [Int]
readTicket = map read . splitOn ","

validateTicket :: (Int -> Bool) -> [Int] -> [Int]
validateTicket validator = filter $ not . validator

main :: IO ()
main = do lines <- getLines
          let [vLines, tLines] = splitOn [""] lines
              validator        = anyPred $ map readValidator vLines
              tickets          = map readTicket tLines
              invalidValues    = tickets >>= validateTicket validator
          print $ sum invalidValues

