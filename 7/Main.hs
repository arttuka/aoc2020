module Main where

import Prelude hiding (lookup)
import Control.Monad ((<=<))
import Data.Char (digitToInt)
import Data.Maybe (mapMaybe)
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Map.Strict (Map, empty, lookup, insertWith)
import qualified Data.Set as Set
import Util (getLines, dropLast)

type Edge = (String, String, Int)
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

filterEdges :: Bool -> (String, Int) -> [Edge] -> [(String, Int)]
filterEdges fwd (k, i) = mapMaybe filterEdge
  where
    filterEdge :: Edge -> Maybe (String, Int)
    filterEdge (x, y, j)
        | k == from = Just (to, i * j)
        | otherwise = Nothing
      where
        (from, to) = if fwd then (x, y) else (y, x)

dfs :: Bool -> Graph -> (String, Int) -> [(String, Int)]
dfs fwd g x@(k, _) = x : maybe [] (dfs fwd g <=< filterEdges fwd x) (lookup k g)

main :: IO ()
main = do graph <- fmap readGraph getLines
          let bags = Set.fromList $ tail $ fst <$> dfs False graph ("shiny gold", 1)
          print $ length bags
