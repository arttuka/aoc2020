module Main where

import Debug.Trace (trace)

import Control.Monad (join)
import Data.List (foldl', intercalate)
import Data.Map.Strict (Map, (!), (!?), fromList)
import Data.Maybe (fromMaybe, maybe)
import Data.Set (Set, empty, insert, member, singleton)
import Util (bin2dec, concatLines, findJust, groupBy, iSqrt, rpad, powerset, readGroupsWith, reverseBin, transpose)

--           Id    Rot   FlipV FlipH   U    R    D    L     Square
type Tile = (Int, (Bool, Bool, Bool), (Int, Int, Int, Int), [String])
type BorderToTile = Map Int (Map Int [Tile])

readTile :: [String] -> Tile
readTile (s:ss) = (id, (False, False, False), (bu, br, bd, bl), ss)
  where
    id   = read $ init $ drop 5 s
    bu   = bin2dec $ head ss
    br   = bin2dec $ map last ss
    bd   = bin2dec $ last ss
    bl   = bin2dec $ map head ss

transformTile :: Tile -> [Tile]
transformTile = (tfs <*>) . pure
  where
    tfs = map (foldl' (.) id) $ powerset [rotate, flipV, flipH]

showTransform :: Tile -> String
showTransform (_, (r, fv, fh), _, _) = [if r then 'T' else 'F', if fv then 'T' else 'F', if fh then 'T' else 'F']

showTile :: Tile -> String
showTile t@(id, _, _, square) = intercalate "\n" $ (show id ++ " " ++ showTransform t) : square

showGrid :: [[Tile]] -> String
showGrid = intercalate "\n" . (concatLines . map squareWithId =<<)
  where
    squareWithId :: Tile -> [String]
    squareWithId x = map (rpad 11 ' ') $ (show (tileId x) ++ " " ++ showTransform x) : tileSquare x

borderU :: Tile -> Int
borderU (_, _, (bu, _, _, _), _) = bu
borderR :: Tile -> Int
borderR (_, _, (_, br, _, _), _) = br
borderD :: Tile -> Int
borderD (_, _, (_, _, bd, _), _) = bd
borderL :: Tile -> Int
borderL (_, _, (_, _, _, bl), _) = bl
tileId  :: Tile -> Int
tileId  (id, _, _, _) = id
tileSquare :: Tile -> [String]
tileSquare (_, _, _, square) = square 

matchesAbove :: Tile -> Tile -> Bool
matchesAbove t1 t2 = borderU t1 == borderD t2 

tileEq :: Tile -> Tile -> Bool
tileEq (_, _, b1, s1) (_, _, b2, s2) = b1 == b2 && s1 == s2

rotate :: Tile -> Tile
rotate (id, (_, flipV, flipH), (bu, br, bd, bl), squares)
  = (id, (True, flipV, flipH), (reverseBin bl, bu, reverseBin br, bd), transpose (reverse squares))

flipV :: Tile -> Tile
flipV (id, (rot, _, flipH), (bu, br, bd, bl), squares)
 = (id, (rot, True, flipH), (bd, reverseBin br, bu, reverseBin bl), reverse squares)

flipH :: Tile -> Tile
flipH (id, (rot, flipV, _), (bu, br, bd, bl), squares)
 = (id, (rot, flipV, True), (reverseBin bu, bl, reverseBin bd, br), map reverse squares)

makeBorderToTile :: [Tile] -> BorderToTile
makeBorderToTile = fromList . zip [0..] . ([groupBy] <*> [borderU, borderR, borderD, borderL] <*>) . pure

arrangeTiles :: BorderToTile -> Int -> [Tile] -> Maybe [[Tile]]
arrangeTiles b2t size tiles = findJust $ map start tiles
  where
    start :: Tile -> Maybe [[Tile]]
    start t = step (singleton (tileId t)) 1 0 [[t]]
    step :: Set Int -> Int -> Int -> [[Tile]] -> Maybe [[Tile]]
    step seen x y tiles
        | y == size = Just tiles
        | otherwise = findJust nextSteps
      where
        (currRow:otherRows) = if x == 0 then []:tiles else tiles
        matches   = if y == 0 || x == 0 then const True else matchesAbove (head otherRows !! (size - x - 1))
        nextTiles = if x == 0
          then (b2t ! 2) !? borderU (last (head otherRows)) 
          else (b2t ! 1) !? borderL (head currRow) 
        nextX     = if x == (size - 1) then 0 else x + 1
        nextY     = if x == (size - 1) then y + 1 else y
        nextSteps = [step (insert id seen) nextX nextY ((t:currRow):otherRows)
                      | t <- fromMaybe [] nextTiles,
                        let id = tileId t,
                        not (id `member` seen),
                        matches t ]

calculateAnswer :: [[Tile]] -> Int
calculateAnswer tiles = product $ map tileId [head (head tiles), last (head tiles), head (last tiles), last (last tiles)]

main :: IO ()
main = do tiles <- readGroupsWith readTile
          let size = iSqrt $ length tiles
              tfTiles = tiles >>= transformTile
              b2t     = makeBorderToTile tfTiles
              result  = arrangeTiles b2t size tfTiles
              answer  = maybe 0 calculateAnswer result
          putStrLn $ maybe "No result" showGrid result
          print answer

