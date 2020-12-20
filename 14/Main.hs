{-# LANGUAGE TupleSections #-}
module Main where

import Prelude hiding (replicate)
import Data.Bits ((.&.), (.|.))
import Data.List (foldl', isPrefixOf)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.Map.Strict (Map, empty, fromList, union)
import Util (bin2dec, dec2bin, replace, readLinesWith, subsets)

type Mask = (Int, Int, [Int])
data Op = Assign Int Int | SetMask Mask
type State = (Map Int Int, Mask)

readMask :: String -> Op
readMask s = SetMask (andMask m, orMask m, floating)
  where
    m = drop 7 s
    step :: Int -> Char -> Maybe Int
    step i 'X' = Just i
    step _ _   = Nothing
    floating = map sum $ subsets $ catMaybes $ zipWith step (iterate (*2) 1) (reverse m)
    andMask :: String -> Int
    andMask = bin2dec . replace 'X' '0' . replace '0' '1'
    orMask :: String -> Int
    orMask = bin2dec . replace 'X' '0'

readAssign :: String -> Op
readAssign s = Assign (read addr) (read val)
  where
    [addr, val] = splitOn "] = " $ drop 4 s

readLine :: String -> Op
readLine s
  | "mask" `isPrefixOf` s = readMask s
  | otherwise             = readAssign s

applyMask :: Mask -> Int -> [Int]
applyMask (andM, orM, fl) addr = map (+ maskedAddr) fl
  where
    maskedAddr = (addr .&. andM) .|. orM

applyOp :: State -> Op -> State
applyOp (mem, _) (SetMask newMask) = (mem, newMask)
applyOp (mem, mask) (Assign addr val) = (assigns `union` mem, mask)
  where
    assigns = fromList $ map (, val) $ applyMask mask addr

main :: IO ()
main = do program <- readLinesWith readLine
          let initialState = (empty, (0, 0, [])) :: State
              (endMem, _)= foldl' applyOp initialState program
          print $ sum endMem
