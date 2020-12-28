{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad ((<=<))
import Data.List (foldl')
import Data.Map.Strict (Map, fromListWith)
import qualified Data.Map.Strict as Map
import Data.Set (Set, fromList, member, size, toList, union)
import qualified Data.Set as Set
import Util (addT, readLinesWith)

data Dir = E | SE | SW | W | NW | NE

readLine :: String -> [Dir]
readLine [] = []
readLine ('e':xs)     = E  : readLine xs
readLine ('s':'e':xs) = SE : readLine xs
readLine ('s':'w':xs) = SW : readLine xs
readLine ('w':xs)     = W  : readLine xs
readLine ('n':'w':xs) = NW : readLine xs
readLine ('n':'e':xs) = NE : readLine xs

dirToMove :: Dir -> (Int, Int)
dirToMove E  = (1,  0)
dirToMove SE = (0,  1)
dirToMove SW = (-1, 1)
dirToMove W  = (-1, 0)
dirToMove NW = (0, -1)
dirToMove NE = (1, -1)

allMoves = fromList $ map dirToMove [E, SE, SW, W, NW, NE]

move :: [Dir] -> (Int, Int)
move = foldl' addT (0, 0) . map dirToMove

getAdjacentTiles :: (Int, Int) -> Set (Int, Int)
getAdjacentTiles t = Set.map (addT t) allMoves

countBlackTiles :: Set (Int, Int) -> (Int, Int) -> Int
countBlackTiles tiles = size . Set.filter (`member` tiles) . getAdjacentTiles

getAllTiles :: Set (Int, Int) -> Set (Int, Int)
getAllTiles tiles = foldl' union tiles (getAdjacentTiles <$> toList tiles)

isBlack :: Set (Int, Int) -> (Int, Int) -> Bool
isBlack tiles p = (c == 2) || (p `member` tiles && c == 1)
  where
    c = countBlackTiles tiles p

flipTiles :: Set (Int, Int) -> Set (Int, Int)
flipTiles tiles = Set.filter (isBlack tiles) $ getAllTiles tiles

initTiles :: [[Dir]] -> Set (Int, Int)
initTiles moves = Map.keysSet $ Map.filter id tilemap
  where
    tilemap = fromListWith ((not .) . const) $ map ((,True) . move) moves

main :: IO ()
main = do moves <- readLinesWith readLine
          let tiles  = initTiles moves 
              states = iterate flipTiles tiles
          print $ Set.size $ states !! 100
