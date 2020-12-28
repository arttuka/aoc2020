{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad (join)
import Data.List (intercalate, sortOn)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Util (readLinesWith, getSingleton, if', isSingleton)

readFood :: String -> ([String], [String])
readFood s = (allergens, foods)
  where
    [sf, sa]  = splitOn " (contains " s
    allergens = splitOn ", " $ init sa
    foods     = words sf

getAllergens :: ([String], [String]) -> [(String, Set String)]
getAllergens (allergens, foods) = (, Set.fromList foods) <$> allergens

findIngredientsWithAllergens :: [(String, Set String)] -> Map String String
findIngredientsWithAllergens = step Set.empty . Map.fromListWith Set.intersection
  where
    step :: Set String -> Map String (Set String) -> Map String String
    step seen allergens
        | all isSingleton allergens = Set.elemAt 0 <$> allergens
        | otherwise                 = step (Set.insert allergen seen) (removeFood food <$> allergens)
      where
        (allergen, food) = nextAllergen seen allergens
    getSingletonAllergen :: (String, Set String) -> Maybe (String, String)
    getSingletonAllergen (allergen, foods) = (allergen,) <$> getSingleton foods
    nextAllergen :: Set String -> Map String (Set String) -> (String, String)
    nextAllergen seen = head . filter (not . (`Set.member` seen) . fst) . mapMaybe getSingletonAllergen . Map.toList
    removeFood :: String -> Set String -> Set String
    removeFood food = if' isSingleton id (Set.delete food)

unsafeList :: [(String, Set String)] -> String
unsafeList = intercalate "," . fmap snd . sortOn fst . Map.toList . findIngredientsWithAllergens

main :: IO ()
main = do foods <- readLinesWith readFood
          let allergens = foods >>= getAllergens
          putStrLn $ unsafeList allergens
