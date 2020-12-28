{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad (join)
import Data.List (isInfixOf, foldl')
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Util (readLinesWith)

readFood :: String -> ([String], [String])
readFood s = (allergens, foods)
  where
    [sf, sa]  = splitOn " (contains " s
    allergens = splitOn ", " $ init sa
    foods     = words sf

getAllergens :: ([String], [String]) -> [(String, Set String)]
getAllergens (allergens, foods) = (, Set.fromList foods) <$> allergens

checkAllergens :: [(String, Set String)] -> Map String (Set String)
checkAllergens = Map.fromListWith Set.intersection

unsafeIngredients :: [(String, Set String)] -> Set String
unsafeIngredients = Map.foldl Set.union Set.empty . Map.fromListWith Set.intersection

main :: IO ()
main = do foods <- readLinesWith readFood
          let allergens      = foods >>= getAllergens
              allIngredients = foods >>= snd
              unsafe         = unsafeIngredients allergens
              safe           = Set.difference (Set.fromList allIngredients) unsafe
          print $ length $ filter (`Set.member` safe) allIngredients
