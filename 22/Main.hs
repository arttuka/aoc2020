module Main where

import Prelude hiding (null)
import Control.Monad (liftM2)
import Queue (Queue, empty, fromList, null, peek, pop, pushAll, toList)
import Util (readGroupsWith)

readDeck :: [String] -> Queue Int
readDeck (_:ss) = fromList $ map read ss

playRound :: (Queue Int, Queue Int) -> Maybe (Queue Int, Queue Int)
playRound (p1, p2) = liftM2 playRound' (peek p1) (peek p2)
  where
    playRound' :: Int -> Int -> (Queue Int, Queue Int)
    playRound' c1 c2
      | c1 > c2   = (pushAll [c1, c2] (pop p1), pop p2)
      | otherwise = (pop p1, pushAll [c2, c1] (pop p2))

calculateScore :: (Queue Int, Queue Int) -> Int
calculateScore (p1, p2) = sum $ zipWith (*) [1..] $ reverse $ toList $ if null p1 then p2 else p1 

playGame :: (Queue Int, Queue Int) -> (Queue Int, Queue Int)
playGame decks = maybe decks playGame (playRound decks)

main :: IO ()
main = do [p1, p2] <- readGroupsWith readDeck
          let decks = playGame (p1, p2)
          print (fst decks)
          print (snd decks)
          print (calculateScore decks)
