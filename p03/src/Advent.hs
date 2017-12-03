module Advent where

import Control.Monad (guard)
import qualified Data.Map as Map
import Data.List (find, foldl', genericTake)
import Data.Maybe (mapMaybe)

data Direction = R | U | L | D
  deriving (Eq, Show, Ord)

spiral :: [Direction]
spiral = concatMap row [0..]
  where
    row n = R : (ups n ++ rep n L ++ rep n D ++ rep n R)
    ups n = replicate (2*n + 1) U
    rep n = replicate (2*n + 2)

move :: (Integer, Integer) -> Direction -> (Integer, Integer)
move (x, y) dir =
  case dir of
    U -> (x, y + 1)
    R -> (x + 1, y)
    D -> (x, y - 1)
    L -> (x - 1, y)

spiralCoords :: [(Integer, Integer)]
spiralCoords = scanl move (0,0) spiral

getDisplacement :: Integer -> (Integer, Integer)
getDisplacement n = foldl' move (0, 0) (genericTake (n - 1) spiral)

manhattanDist :: Integer -> Integer
manhattanDist x = abs a + abs b
  where (a, b) = getDisplacement x

spiralSums = foldl' f start (drop 1 . take 1000 $ spiralCoords)
  where
    start = Map.singleton (0, 0) 1
    f acc (x, y) =
      let v = sum . mapMaybe (\coord -> Map.lookup coord acc) $ neighbors (x, y)
      in Map.insert (x, y) v acc
    neighbors (x, y) = do
      xOffset <- [-1..1]
      yOffset <- [-1..1]
      guard (xOffset /= 0 || yOffset /= 0)
      return (x + xOffset, y + yOffset)

adjSum :: Integer -> Integer
adjSum n = maybe 0 id $ Map.lookup coord spiralSums
  where coord = getDisplacement n

part1 = manhattanDist

part2 n = maybe 0 id . find (> n) $ map adjSum [0..]

cliMain :: IO ()
cliMain = do
  print $ part2 277678
