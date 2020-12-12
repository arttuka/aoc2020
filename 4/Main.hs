module Main where

import Data.Map.Strict (Map, fromList, member)
import Data.List.Split (splitOn)
import Util (getLines, countWhere)

keys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

readKV :: String -> (String, String)
readKV s = (k, v)
  where
    [k, v] = splitOn ":" s

readLine :: String -> [(String, String)]
readLine = map readKV . splitOn " "

readPassport :: [String] -> Map String String
readPassport lines = fromList $ readLine =<< lines

isValid :: Map String String -> Bool
isValid m = all (`member` m) keys

main :: IO ()
main = do lines <- getLines
          let passports = map readPassport $ splitOn [""] lines
          print $ countWhere isValid passports
