module Advent where

import Control.Monad (guard)
import Data.Monoid (First(..))
import Data.List (foldl')

parseTSVline [] = []
parseTSVline xs =
  let (x, xs') = span (/= '\t') xs
  in read x : parseTSVline (drop 1 xs')

checksum :: [Integer] -> Integer
checksum xs@(x:_) = max - min
  where
    (min, max) = foldl' extrema (x, x) xs
    extrema (lower, upper) x =
      (if lower < x then lower else x, if upper > x then upper else x)

divisible :: [Integer] -> Integer
divisible [] = error "No evenly divisible numbers found."
divisible (x:xs) = maybe (divisible xs) id . getFirst $ foldMap (First . divides x) xs
  where
    divides a b = if a < b then divides b a else do
      let (dQuot, dRem) = a `quotRem` b
      guard (dRem == 0)
      pure dQuot

part1 = sum . map (checksum . parseTSVline) . lines
part2 = sum . map (divisible . parseTSVline) . lines

cliMain :: IO ()
cliMain = do
  input <- readFile "../inputs/02-1.txt"
  print $ part2 input
