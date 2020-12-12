module Main where

import Data.List (foldl')
import Util (readLinesWith, tMul, tAdd, rotate)

data Dir = DirN | DirE | DirS | DirW deriving (Enum, Eq, Show)
data Turn = TurnL | TurnR deriving (Eq, Show)
data Action = N | E | S | W | L | R | F deriving (Enum, Eq, Show)
data Cmd = Cmd Action Int deriving (Eq, Show)
type Pos = (Int, Int)
type State = (Pos, Pos)

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

move :: State -> Int -> State
move (pos, wpt) n = (tAdd (tMul wpt n) pos, wpt)

moveWpt :: Dir -> Int -> Pos -> Pos
moveWpt DirN = tAdd . tMul (0, -1)
moveWpt DirE = tAdd . tMul (1, 0)
moveWpt DirS = tAdd . tMul (0, 1)
moveWpt DirW = tAdd . tMul (-1, 0)

rotateWpt :: Turn -> Int -> Pos -> Pos
rotateWpt lr deg = rotate n
  where
    n = case lr of
      TurnR -> deg `div` 90
      TurnL -> 4 - deg `div` 90

toDirection :: Action -> Dir
toDirection = toEnum . fromEnum

executeCmd :: State -> Cmd -> State
executeCmd state (Cmd F n) = move state n
executeCmd (pos, wpt) (Cmd L deg) = (pos, rotateWpt TurnL deg wpt)
executeCmd (pos, wpt) (Cmd R deg) = (pos, rotateWpt TurnR deg wpt)
executeCmd (pos, wpt) (Cmd c n) = (pos, moveWpt (toDirection c) n wpt)

distance :: (Int, Int) -> Int
distance (x, y) = abs x + abs y

main :: IO ()
main = do cmds <- readLinesWith readCmd
          let state    = ((0, 0), (10, -1))
              endState = foldl' executeCmd state cmds
          print endState
          print $ distance $ fst endState
