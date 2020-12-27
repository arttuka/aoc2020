module Main where

import Debug.Trace (trace)

import Data.List (find, intercalate, sortOn)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe, maybe)
import Data.Ord
import Data.Vector (Vector, (!?))
import qualified Data.Vector as V
import Util (getLines, indent)

showRule :: [Int] -> String
showRule = unwords . map show

data Rule = IProd Int [[Int]] | OProd Int [Rule] | Term Int Char
instance Show Rule where
  show (IProd id rules) = "[" ++ show id ++ ": " ++ show rules ++ "]"
  show (OProd id rules) = "[" ++ show id ++ "\n" ++ intercalate "\n" (map (indent 2 . show) rules) ++ "]"
  show (Term id c)      = "[" ++ show id ++ ": " ++ show c ++ "]"
type Rules = Map Int Rule

ruleId :: Rule -> Int
ruleId (IProd id _) = id
ruleId (OProd id _) = id
ruleId (Term id _)  = id

ruleLength :: Rule -> Int
ruleLength (Term _ _) = 1
ruleLength (OProd _ rules) = sum $ map ruleLength rules

readRule :: String -> Rule
readRule s
    | head rs == '"' = Term id (rs !! 1)
    | otherwise      = IProd id $ map readSubRule $ splitOn " | " rs
  where
    [ids, rs] = splitOn ": " s
    id        = read ids
    readSubRule :: String -> [Int]
    readSubRule = map read . splitOn " "

flattenRule :: Rule -> String
flattenRule term@(Term _ c)  = [c]
flattenRule (OProd id rules) = rules >>= flattenRule

messageMatches :: String -> Rule -> Bool
messageMatches = (. flattenRule) . (==)

parseMessage :: Rules -> String -> Maybe Rule
parseMessage rules message = find (messageMatches message) $ step 0 (rules ! 0) 
  where
    len = length message
    msg = V.fromList message
    step :: Int -> Rule -> [Rule]
    step i _ | i >= len        = []
    step i term@(Term _ c)     = [term | c == msg V.! i]
    step i (IProd id subrules) = [OProd id rs | subrule <- subrules,
                                                rs      <- processSubrule i subrule ]
    processSubrule :: Int -> [Int] -> [[Rule]]
    processSubrule _ [] = [[]]
    processSubrule j (r:rs) = [x:xs | x  <- step j (rules ! r),
                                      xs <- processSubrule (j + ruleLength x) rs ]

main :: IO ()
main = do lines <- getLines
          let [rulesLines, messages] = splitOn [""] lines
              allRules               = map readRule rulesLines
              rules                  = M.fromList [(ruleId r, r) | r <- allRules]
          print $ length $ mapMaybe (parseMessage rules) messages
