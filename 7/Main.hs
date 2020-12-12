module Main where

import Prelude hiding (lookup)
import Control.Monad ((<=<))
import Data.Char (digitToInt)
import Data.Function.Memoize
import Data.Maybe (mapMaybe)
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Map.Strict (Map, empty, lookup, insertWith)
import Util (readWith, dropLast)

type Edge  = (String, String, Int)
type Bag   = (String, Int)
type Graph = Map String [Edge]

readBags :: [String] -> [Edge]
readBags [_, "no other bags"] = []
readBags [bag, s] = readBag <$> splitOn ", " s
  where
    readBag :: String -> Edge
    readBag (n:_:s) = (bag, dropLast (if i == 1 then 4 else 5) s, i)
      where
        i = digitToInt n

readLine :: String -> [Edge]
readLine = readBags . splitOn " bags contain " . init

readGraph :: [String] -> Graph
readGraph = foldl' insertEdge empty . (readLine =<<)
  where
    insertEdge :: Graph -> Edge -> Graph
    insertEdge m edge@(from, to, _) = insertWith (++) from [edge] $ insertWith (++) to [edge] m

dfs :: Bool -> Graph -> String -> [Bag]
dfs fwd g = memoFix step
  where
    step :: (String -> [Bag]) -> String -> [Bag]
    step stepRec k = (k, 1) : maybe [] (recurse <=< mapMaybe filterEdge) (lookup k g)
      where
        filterEdge :: Edge -> Maybe Bag
        filterEdge (from, to, i)
            | fwd && k == from   = Just (to, i)
            | not fwd && k == to = Just (from, i)
            | otherwise          = Nothing
        recurse :: Bag -> [Bag]
        recurse (k, i) = fmap (* i) <$> stepRec k

main :: IO ()
main = do graph <- readWith readGraph
          let bags = tail $ snd <$> dfs True graph "shiny gold"
          print $ sum bags
