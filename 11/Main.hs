{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Prelude hiding (head, length, concat)
import Control.Monad (join)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Data.Vector (Vector, fromList, toList, head, length, generate)
import Util (readLinesWith, get, getMany, tAdd, iterateTail, consecutiveSame)

data Square = Seat Bool | Floor deriving (Eq)
instance Show Square where
  show (Seat True)  = "#"
  show (Seat False) = "L"
  show Floor        = "."

type Squares = Vector (Vector Square)
showSquares :: Squares -> String
showSquares squares = intercalate "\n" $ toList $ fmap show squares

readSquare :: Char -> Square
readSquare 'L' = Seat False
readSquare '#' = Seat True
readSquare '.' = Floor

isOccupied :: Square -> Int
isOccupied (Seat True) = 1
isOccupied _           = 0

countOccupied :: (Functor f, Foldable f) => f Square -> Int
countOccupied = sum . fmap isOccupied

getClosestSeat :: [Maybe Square] -> Maybe Int
getClosestSeat (x:xs) = x >>= \case
  (Seat b) -> Just (fromEnum b)
  _        -> getClosestSeat xs

makeSquareSequences = iterateTail . tAdd <$> [(1, 1), (1, 0), (1, -1), (0, 1), (0, -1), (-1, 1), (-1, 0), (-1, -1)]
getAdjacentSquares :: Squares -> (Int, Int) -> [[Maybe Square]]
getAdjacentSquares squares = sequenceA $ (getMany squares .) <$> makeSquareSequences

countAdjOccupied :: Squares -> (Int, Int) -> Int
countAdjOccupied squares coords = sum $ mapMaybe getClosestSeat $ getAdjacentSquares squares coords

step :: Squares -> Squares
step squares = generate (length squares) stepLine 
  where
    width = length (head squares)
    stepLine :: Int -> Vector Square
    stepLine y = generate width (stepSquare . (,y))
    stepSquare :: (Int, Int) -> Square
    stepSquare coords = case get squares coords of
      Just (Seat b) -> let occupied = countAdjOccupied squares coords
                       in case () of
                         _ | b && occupied >= 5     -> Seat False
                           | not b && occupied == 0 -> Seat True
                           | otherwise              -> Seat b
      Just s -> s

run :: Squares -> Squares
run = consecutiveSame . iterate step

main :: IO ()
main = do squares <- fromList <$> readLinesWith (fromList . fmap readSquare)
          let endSquares = run squares
          print $ countOccupied $ join endSquares
