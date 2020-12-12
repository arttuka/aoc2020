module Main where

import Data.List (sort)
import Util (readLinesWith, bin2dec, fmapt)

readChar :: Char -> Char
readChar 'F' = '0'
readChar 'B' = '1'
readChar 'L' = '0'
readChar 'R' = '1'

readLine :: String -> (Int, Int)
readLine = fmapt bin2dec . splitAt 7 . fmap readChar

toSeatId :: (Int, Int) -> Int
toSeatId (row, col) = row * 8 + col

findSeat :: [Int] -> Int
findSeat (a:b:xs)
    | b - a == 2 = a + 1
    | otherwise  = findSeat (b:xs)

main :: IO ()
main = do ids <- readLinesWith (toSeatId . readLine)
          print $ findSeat $ sort ids
