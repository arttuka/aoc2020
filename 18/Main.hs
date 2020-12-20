module Main where

import Debug.Trace (trace)

import Data.List.Split (split)
import Queue (Queue, empty, push, pushAll, toList)
import Util (isDigit, isOp, readLinesWith)

data Token = Number Int | Op Char Int (Int -> Int -> Int) | Paren
instance Show Token where
  show (Number n) = show n
  show (Op c _ _) = show c
  show Paren      = "("

type Reader = String -> (Token, String)

readNumber :: String -> (Token, String)
readNumber s = (Number (read n), rest)
  where
    (n, rest) = span isDigit s

readOp :: String -> (Token, String)
readOp (x:xs) = (Op x prec op, xs)
  where
    (prec, op) = case x of
      '+' -> (2, (+))
      '*' -> (1, (*))

getReader :: Char -> Reader
getReader c
  | isDigit c = readNumber
  | isOp c    = readOp

isOpWithPrecedence :: Token -> Token -> Bool
isOpWithPrecedence _ Paren                 = False
isOpWithPrecedence (Op _ p1 _) (Op _ p2 _) = p2 >= p1

isParen :: Token -> Bool
isParen Paren = True
isParen _     = False

readExpression :: String -> [Token]
readExpression s = toList $ step s empty []
  where
    step :: String -> Queue Token -> [Token] -> Queue Token
    step "" output [] = output
    step "" output (op:ops) = step "" (push op output) ops
    step s@(x:xs) output ops
        | x == ' '  = step xs output ops
        | x == '('  = step xs output (Paren:ops)
        | x == ')'  = let (popped, restOps) = break isParen ops
                      in step xs (pushAll popped output) (tail restOps)
        | isDigit x = let (num, rest) = readNumber s
                      in step rest (push num output) ops
        | isOp x    = let (op, rest)        = readOp s
                          (popped, restOps) = span (isOpWithPrecedence op) ops
                      in step rest (pushAll popped output) (op:restOps)

evalExpression :: [Token] -> Int
evalExpression tokens = step tokens []
  where
    step :: [Token] -> [Int] -> Int
    step [] [a] = a
    step ((Op _ _ f):restTokens) (a:b:restNums) = step restTokens (f a b : restNums)
    step ((Number n):restTokens) nums           = step restTokens (n:nums)

main :: IO ()
main = do exprs <- readLinesWith readExpression
          print $ sum $ map evalExpression exprs
