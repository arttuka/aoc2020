module Main where

import Prelude hiding (replicate)
import Data.Bits ((.&.), (.|.))
import Data.Int (Int64)
import Data.List (foldl', isPrefixOf)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.Vector (Vector, (//), replicate, toList)
import Util (bin2dec, dec2bin, replace, readLinesWith)

type Mask = (Int64, Int64)
data Op = Assign Int Int64 | SetMask Mask
type State = (Vector Int64, Mask)

readMask :: String -> Op
readMask s = SetMask (bin2dec (replace m 'X' '1'), bin2dec (replace m 'X' '0'))
  where
    m = drop 7 s

readAssign :: String -> Op
readAssign s = Assign (read addr) (read val)
  where
    [addr, val] = splitOn "] = " $ drop 4 s

readLine :: String -> Op
readLine s
  | "mask" `isPrefixOf` s = readMask s
  | otherwise             = readAssign s

maxAddress :: [Op] -> Int
maxAddress = maximum . mapMaybe toAddress
  where
    toAddress :: Op -> Maybe Int
    toAddress (Assign addr _) = Just addr
    toAddress _               = Nothing

applyOp :: State -> Op -> State
applyOp (mem, _) (SetMask newMask) = (mem, newMask)
applyOp (mem, mask) (Assign addr val) = (newMem, mask)
  where
    (andMask, orMask) = mask
    newMem            = mem // [(addr, (val .&. andMask) .|. orMask)]

main :: IO ()
main = do program <- readLinesWith readLine
          let initialState = (replicate (1 + maxAddress program) 0, (0, 0))
              (endMem, _)= foldl' applyOp initialState program
          print $ sum $ toList endMem
