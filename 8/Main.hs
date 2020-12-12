module Main where

import Data.Set (Set, empty, member, insert)
import Computer (Program, State, readProgram, runProgram, pointer, accumulator)
import Util (readWith)

data Result = Loop State | Terminate State

run :: Program -> Result
run program = step empty (runProgram program)
  where
    step :: Set Int -> [State] -> Result
    step _ [s] = Terminate s
    step seen (s:ss)
      | pointer s `member` seen = Loop s
      | otherwise               = step (insert (pointer s) seen) ss

main :: IO ()
main = do program <- readWith readProgram
          let (Loop state) = run program
          print $ accumulator state
