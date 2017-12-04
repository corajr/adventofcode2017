module Advent where

import Control.Arrow ((&&&))
import qualified Data.Map.Strict as Map
import Data.List (sort)

isValidPassphrase :: String -> Bool
isValidPassphrase = all (== 1) . Map.elems . Map.fromListWith (+) . map (id &&& const 1) . words

isValidPassphrase2 :: String -> Bool
isValidPassphrase2 = all (== 1) . Map.elems . Map.fromListWith (+) . map (sort &&& const 1) . words

part1 = length . filter isValidPassphrase . lines
part2 = length . filter isValidPassphrase2 . lines

cliMain :: IO ()
cliMain = do
  input <- readFile "../inputs/04-1.txt"
  print $ part2 input
