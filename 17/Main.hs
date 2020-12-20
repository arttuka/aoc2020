module Main where

import Data.List (foldl', intercalate, transpose)
import Data.Maybe (catMaybes)
import Data.Set (Set, fromList, member, size)
import Util (applyT2, between, boundsRange, combinations, mapIndexed, mapcatIndexed, readWith)

type Coord = [Int]
type Bound = (Int, Int)
type Bounds = [Bound]

readState :: [String] -> Set Coord
readState lines = fromList $ mapcatIndexed readLine lines
  where
    readLine :: Int -> String -> [Coord]
    readLine y = catMaybes . mapIndexed (readCube y)
    readCube :: Int -> Int -> Char -> Maybe Coord
    readCube y x '#' = Just [x, y, 0]
    readCube _ _ _   = Nothing

getBounds :: Set Coord -> Bounds
getBounds = foldl' step (repeat (0, 0))
  where
    step :: Bounds -> Coord -> Bounds
    step = zipWith moveBound
    moveBound :: Bound -> Int -> Bound
    moveBound bound v = applyT2 (min v, max v) bound

getNeighbors :: Coord -> [Coord]
getNeighbors coord = filter (/= coord) $ combinations $ map makeRange coord
  where
    makeRange :: Int -> [Int]
    makeRange n = [(n - 1)..(n + 1)]

getActiveNeighbors :: Set Coord -> Coord -> [Coord]
getActiveNeighbors state = filter (`member` state) . getNeighbors

countActiveNeighbors :: Set Coord -> Coord -> Int
countActiveNeighbors state = length . filter (`member` state) . getNeighbors

getNextState :: Set Coord -> Set Coord
getNextState state = fromList $ filter checkCoord $ combinations ranges
  where
    ranges = map boundsRange $ getBounds state
    checkCoord :: Coord -> Bool
    checkCoord coord
        | coord `member` state = between 2 3 activeNeighbors
        | otherwise            = activeNeighbors == 3
      where
        activeNeighbors = countActiveNeighbors state coord

main :: IO ()
main = do initialState <- readWith readState
          let states = iterate getNextState initialState
          print $ size $ states !! 6
