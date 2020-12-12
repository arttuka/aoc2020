module Main where

import Data.List (find)
import Data.Set (Set, empty, member, insert)
import Computer (Program, State, readProgram, runProgram, pointer, accumulator, alterProgram)
import Util (readWith)

data Result = Loop State | Terminate State
isTerminate :: Result -> Bool
isTerminate (Terminate _) = True
isTerminate _             = False
getState :: Result -> State
getState (Terminate state) = state
getState (Loop state) = state

run :: Program -> Result
run program = step empty (runProgram program)
  where
    step :: Set Int -> [State] -> Result
    step _ [s] = Terminate s
    step seen (s:ss)
      | pointer s `member` seen = Loop s
      | otherwise               = step (insert (pointer s) seen) ss

runUntilSuccess :: [Program] -> Maybe State
runUntilSuccess = fmap getState . find isTerminate . fmap run

main :: IO ()
main = do program <- readWith readProgram
          let programs = alterProgram program
          let result = runUntilSuccess programs
          print $ case result of
            (Just state) -> show $ accumulator state
            Nothing      -> "Nothing was found"
