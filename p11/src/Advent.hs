module Advent where

import Text.Parsec
import Data.List (foldl')

type Coord = (Int, Int, Int)

data Direction = N
               | NE
               | NW
               | SE
               | SW
               | S
               deriving (Eq, Show, Ord)

pDirection = choice
  [ try (string "ne") *> pure NE
  , try (string "nw") *> pure NW
  , try (string "n") *> pure N
  , try (string "se") *> pure SE
  , try (string "sw") *> pure SW
  , string "s" *> pure S
  ]

pDirections = pDirection `sepBy` char ','

move :: Coord -> Direction -> Coord
move (x, y, z) dir = case dir of
  N -> (x, y + 1, z - 1)
  NE -> (x + 1, y, z - 1)
  NW -> (x - 1, y + 1, z)
  SE -> (x + 1, y - 1, z)
  SW -> (x - 1, y, z + 1)
  S -> (x, y - 1, z + 1)

dist :: [Direction] -> Int
dist = distFromOrigin . foldl' move (0,0,0)

maxDist :: [Direction] -> Int
maxDist = maximum . map distFromOrigin . scanl move (0,0,0)

distFromOrigin :: Coord -> Int
distFromOrigin (a, b, c) = maximum [abs a, abs b, abs c]

part1 :: String -> Int
part1 = either (error . show) dist . parse pDirections ""

part2 :: String -> Int
part2 = either (error . show) maxDist . parse pDirections ""

cliMain :: IO ()
cliMain = do
  input <- readFile "../inputs/11.txt"
  print $ part2 input
