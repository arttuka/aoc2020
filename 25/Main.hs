module Main where

import Data.List (elemIndex)
import Util (readLines)

transformStep :: Int -> Int -> Int
transformStep sn v = (sn * v) `mod` 20201227

transformIterations :: Int -> [Int]
transformIterations sn = iterate (transformStep sn) 1

transform :: Int -> Int -> Int
transform = (!!) . transformIterations

main :: IO ()
main = do [cardPk, doorPk] <- readLines
          let pks       = transformIterations 7
              cardLoops = elemIndex cardPk pks
              doorLoops = elemIndex doorPk pks
              enc       = transform doorPk <$> cardLoops
          print enc
