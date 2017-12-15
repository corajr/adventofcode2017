module Advent where

import Data.Bits
import Text.Printf

generator :: Int -> Int -> [Int]
generator factor = drop 1 . iterate f
  where f x = (x * factor) `mod` 2147483647

generatorA = generator 16807
generatorB = generator 48271

generatorA2 = filter (\x -> x `mod` 4 == 0) . generatorA
generatorB2 = filter (\x -> x `mod` 8 == 0) . generatorB

judge :: [(Int, Int)] -> Int
judge = sum . map match
  where
    onlyLow16 x = x .&. 0x0000ffff
    match (a, b) = if onlyLow16 a == onlyLow16 b then 1 else 0

part1 (a, b) =
  judge . take 40000000 $ zip (generatorA a) (generatorB b)

part2 (a, b) =
  judge . take 5000000 $ zip (generatorA2 a) (generatorB2 b)

cliMain :: IO ()
cliMain = do
  let input = (783, 325)
  print $ part2 input
