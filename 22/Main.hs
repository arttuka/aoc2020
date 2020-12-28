module Main where

import Prelude hiding (length, null, take)
import Control.Monad (liftM2)
import Data.Set (Set, insert, member)
import qualified Data.Set as Set
import Queue (Queue, empty, fromList, length, null, peek, pop, pushAll, take, toList)
import qualified Queue
import Util (readGroupsWith)

type Deck = Queue Int
type Decks = (Deck, Deck)
type SeenDecks = Set Decks

readDeck :: [String] -> Deck
readDeck (_:ss) = fromList $ map read ss

playRound :: SeenDecks -> Decks -> Maybe Decks
playRound seen (p1, p2)
    | (p1, p2) `member` seen = Nothing
    | otherwise              = liftM2 playRound' (peek p1) (peek p2)
  where
    playRound' :: Int -> Int -> Decks
    playRound' c1 c2 = if p1Wins then (pushAll [c1, c2] nextP1, nextP2) else (nextP1, pushAll [c2, c1] nextP2)
      where
        nextP1 = pop p1
        nextP2 = pop p2
        p1Wins
          | length nextP1 >= c1 && length nextP2 >= c2 = isP1Winner $ playGame (take c1 nextP1, take c2 nextP2)
          | otherwise                                  = c1 > c2

isP1Winner :: Decks -> Bool
isP1Winner = not . null . fst

calculateScore :: Decks -> Int
calculateScore (p1, p2) = sum $ zipWith (*) [1..] $ reverse $ toList $ if null p1 then p2 else p1 

playGame :: Decks -> Decks
playGame decks = step Set.empty decks
  where
    step :: SeenDecks -> Decks -> Decks
    step seen decks = maybe decks (step (insert decks seen)) (playRound seen decks)

main :: IO ()
main = do [p1, p2] <- readGroupsWith readDeck
          let decks = playGame (p1, p2)
          print (fst decks)
          print (snd decks)
          print (calculateScore decks)
