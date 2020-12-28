module Main where

import Data.List (foldl')
import Data.Map.Strict (Map, size)
import qualified Data.Map.Strict as Map
import Util (addT, frequencies, readLinesWith)

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

move :: [Dir] -> (Int, Int)
move = foldl' addT (0, 0) . map dirToMove

countTiles :: [[Dir]] -> Int
countTiles = size . Map.filter odd . frequencies . map move

main :: IO ()
main = do moves <- readLinesWith readLine
          print $ countTiles moves
