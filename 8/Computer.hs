module Computer where

import Data.Maybe (mapMaybe)
import Data.Vector (Vector, (!), (//), fromList)
import Data.List.Split (splitOn)

data Operation = Acc | Jmp | Nop deriving (Eq, Show)
data Instruction = Instruction Operation Int deriving (Show)
type Program = Vector Instruction
data State = State { pointer     :: Int
                   , accumulator :: Int
                   , program     :: Program
                   }
instance Show State where
  show s = "State{ pointer=" ++ show (pointer s) ++ ", accumulator=" ++ show (accumulator s) ++ " }"

readOperation :: String -> Operation
readOperation input = case input of
  "acc" -> Acc
  "jmp" -> Jmp
  "nop" -> Nop

readLine :: String -> Instruction
readLine s = Instruction (readOperation o) (read i)
  where
    [o, n] = splitOn " " s
    i      = if head n == '+' then tail n else n

readProgram :: [String] -> Program
readProgram = fromList . fmap readLine

step :: State -> State
step state@State{ pointer=pointer, program=program, accumulator=accumulator } = state { accumulator = accumulator + dAcc, pointer = pointer + dPointer }
  where
    (dAcc, dPointer) = case program ! pointer of
      (Instruction Acc n) -> (n, 1)
      (Instruction Jmp n) -> (0, n)
      (Instruction Nop _) -> (0, 1)

runProgram :: Program -> [State]
runProgram program = run State { pointer = 0, accumulator = 0, program = program }
  where
    end = length program
    run :: State -> [State]
    run state
        | pointer state == end = [state]
        | otherwise            = state : run (step state)

alterProgram :: Program -> [Program]
alterProgram program = mapMaybe alter [0..(length program - 1)]
  where
    alter :: Int -> Maybe Program
    alter i = case program ! i of
        (Instruction Jmp n) -> Just $ program // [(i, Instruction Nop n)]
        (Instruction Nop n) -> Just $ program // [(i, Instruction Jmp n)]
        _                   -> Nothing
