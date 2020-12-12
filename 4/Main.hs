module Main where

import Prelude hiding (lookup)
import Data.Map.Strict (fromList, lookup)
import Data.List.Split (splitOn)
import qualified Data.Set as Set
import Text.Read (readMaybe)
import Util (readGroupsWith, countWhere, isNumeric, isHex, between)

keys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

data Passport = Passport { byr :: Maybe Int
                         , iyr :: Maybe Int
                         , eyr :: Maybe Int
                         , hgt :: Maybe String
                         , hcl :: Maybe String
                         , ecl :: Maybe String
                         , pid :: Maybe String
                         , cid :: Maybe String
                         } deriving Show

readKV :: String -> (String, String)
readKV s = (k, v)
  where
    [k, v] = splitOn ":" s

readLine :: String -> [(String, String)]
readLine = map readKV . splitOn " "

readPassport :: [String] -> Passport
readPassport lines = Passport { byr = lookup "byr" m >>= readMaybe
                              , iyr = lookup "iyr" m >>= readMaybe
                              , eyr = lookup "eyr" m >>= readMaybe
                              , hgt = lookup "hgt" m
                              , hcl = lookup "hcl" m
                              , ecl = lookup "ecl" m
                              , pid = lookup "pid" m
                              , cid = lookup "cid" m
                              }
  where
    m = fromList $ readLine =<< lines

validate :: (a -> Bool) -> Maybe a -> Bool
validate _ Nothing = False
validate pred (Just x) = pred x

validateByr :: Int -> Bool
validateByr = between 1920 2002

validateIyr :: Int -> Bool
validateIyr = between 2010 2020

validateEyr :: Int -> Bool
validateEyr = between 2020 2030

validateHgt :: String -> Bool
validateHgt x = case n of
    Nothing -> False
    Just l  -> (suffix == "cm" && between 150 193 l) || (suffix == "in" && between 59 76 l)
  where
    (prefix, suffix) = span isNumeric x
    n                = readMaybe prefix

validateHcl :: String -> Bool
validateHcl ('#':s) = all isHex s
validateHcl _       = False

eclValues = Set.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validateEcl :: String -> Bool
validateEcl x = Set.member x eclValues

validatePid :: String -> Bool
validatePid x = (length x == 9) && all isNumeric x

isValid :: Passport -> Bool
isValid p =   validate validateByr (byr p)
           && validate validateIyr (iyr p)
           && validate validateEyr (eyr p)
           && validate validateHgt (hgt p)
           && validate validateHcl (hcl p)
           && validate validateEcl (ecl p)
           && validate validatePid (pid p)

main :: IO ()
main = do passports <- readGroupsWith readPassport
          print $ countWhere isValid passports
