module Main where

import Data.List (foldl')
import Util (readLinesWith, tMul, tAdd)

data Dir = DirN | DirE | DirS | DirW deriving (Enum, Eq, Show)
data Turn = TurnL | TurnR deriving (Eq, Show)
data Action = N | E | S | W | L | R | F deriving (Enum, Eq, Show)
data Cmd = Cmd Action Int deriving (Eq, Show)
type State = (Dir, (Int, Int))

readAction :: Char -> Action
readAction 'N' = N
readAction 'S' = S
readAction 'E' = E
readAction 'W' = W
readAction 'L' = L
readAction 'R' = R
readAction 'F' = F

readCmd :: String -> Cmd
readCmd (a:ns) = Cmd (readAction a) (read ns)

move :: Dir -> Int -> (Int, Int) -> (Int, Int)
move DirN = tAdd . tMul (0, -1)
move DirE = tAdd . tMul (1, 0)
move DirS = tAdd . tMul (0, 1)
move DirW = tAdd . tMul (-1, 0)

toDirection :: Action -> Dir
toDirection = toEnum . fromEnum

toTurn :: Action -> Turn
toTurn L = TurnL
toTurn R = TurnR

turn :: Dir -> Turn -> Int -> Dir
turn dir lr deg = toEnum $ (mul * step + fromEnum dir) `mod` 4 
  where
    mul = case lr of
      TurnL -> -1
      TurnR -> 1
    step = deg `div` 90

executeCmd :: State -> Cmd -> State
executeCmd (dir, coords) (Cmd F i) = (dir, move dir i coords)
executeCmd (dir, coords) (Cmd L i) = (turn dir TurnL i, coords)
executeCmd (dir, coords) (Cmd R i) = (turn dir TurnR i, coords)
executeCmd (dir, coords) (Cmd c i) = (dir, move (toDirection c) i coords)

distance :: (Int, Int) -> Int
distance (x, y) = x + y

main :: IO ()
main = do cmds <- readLinesWith readCmd
          let state    = (DirE, (0, 0))
              endState = foldl' executeCmd state cmds
          print endState
          print $ distance $ snd endState
