{-# LANGUAGE TupleSections #-}
module Main where

import Prelude hiding (head, length, concat)
import Control.Monad (join)
import Data.List (intercalate)
import Data.Vector (Vector, fromList, toList, head, length, generate)
import Util (readLinesWith, get, getMany, tAdd, consecutiveSame)

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

isOccupied :: Square -> Bool
isOccupied (Seat True) = True
isOccupied _           = False

isCoordOccupied :: Squares -> (Int, Int) -> Bool
isCoordOccupied squares coords = case get squares coords of
  Just (Seat True) -> True
  _                -> False

countOccupied :: (Functor f, Foldable f) => f Square -> Int
countOccupied = sum . fmap (fromEnum . isOccupied)

adjacent :: [(Int, Int)]
adjacent = [(1, 1), (1, 0), (1, -1), (0, 1), (0, -1), (-1, 1), (-1, 0), (-1, -1)]
countAdjOccupied :: Squares -> (Int, Int) -> Int
countAdjOccupied squares = countOccupied . getMany squares . sequenceA (tAdd <$> adjacent)

step :: Squares -> Squares
step squares = generate (length squares) stepLine 
  where
    width = length (head squares)
    stepLine :: Int -> Vector Square
    stepLine y = generate width (stepSquare . (,y))
    stepSquare :: (Int, Int) -> Square
    stepSquare coords = case get squares coords of
      Just (Seat False)
        | countAdjOccupied squares coords == 0 -> Seat True
      Just (Seat True)
        | countAdjOccupied squares coords >= 4 -> Seat False
      Just s -> s

run :: Squares -> Squares
run = consecutiveSame . iterate step

main :: IO ()
main = do squares <- fromList <$> readLinesWith (fromList . fmap readSquare)
          let endSquares = run squares
          print $ countOccupied $ join endSquares
