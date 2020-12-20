module Main where

import Prelude hiding (null)
import Data.List (foldl1, isPrefixOf, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set, delete, elemAt, empty, fromList, insert, intersection, member, null)
import Util (anyPred, between, getLines, getSingleton, isSingleton)

type Validator = (String, Int -> Bool)

readLimits :: String -> Int -> Bool
readLimits s = between lo hi 
  where
    [lo, hi] = map read $ splitOn "-" s

readValidator :: String -> Validator
readValidator s = (name, anyPred $ map readLimits $ splitOn " or " limits)
  where
    [name, limits] = splitOn ": " s

readTicket :: String -> [Int]
readTicket = map read . splitOn ","

applyValidators :: [Validator] -> Int -> Set String
applyValidators validators n = fromList $ mapMaybe (applyValidator n) validators
  where
    applyValidator :: Int -> Validator -> Maybe String
    applyValidator n (name, pred) = if pred n then Just name else Nothing 

validateTicket :: [Validator] -> [Int] -> Maybe [Set String]
validateTicket validators ticket = if any null fields then Nothing else Just fields
  where
    fields = map (applyValidators validators) ticket

limitPossibilities :: [Set String] -> [String]
limitPossibilities = step empty
  where
    step :: Set String -> [Set String] -> [String]
    step seen fields
        | all isSingleton fields = map (elemAt 0) fields
        | otherwise              = step (insert elem seen) (map (removeElem elem) fields)
      where
        elem = nextElem seen fields
    nextElem :: Set String -> [Set String] -> String
    nextElem seen = head . filter (not . (`member` seen)) . mapMaybe getSingleton
    removeElem :: String -> Set String -> Set String
    removeElem e s
        | isSingleton s = s
        | otherwise     = delete e s
    

getFields :: [[Set String]] -> [String]
getFields = limitPossibilities . map (foldl1 intersection) . transpose

getDepartureFields :: [String] -> [Int] -> [Int]
getDepartureFields fields values = catMaybes $ zipWith f fields values
  where
    f :: String -> Int -> Maybe Int
    f field value
      | "departure" `isPrefixOf` field = Just value
      | otherwise                      = Nothing

main :: IO ()
main = do lines <- getLines
          let [vLines, tLines] = splitOn [""] lines
              validators       = map readValidator vLines
              tickets          = map readTicket tLines
              fields           = getFields $ mapMaybe (validateTicket validators) tickets
          print $ product $ getDepartureFields fields $ head tickets
