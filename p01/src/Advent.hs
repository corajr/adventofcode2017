module Advent where

inverseCaptcha :: String -> Integer
inverseCaptcha xs =
  sum $ zipWith f xs (drop 1 xs ++ [head xs])
    where f a b = if a == b then read [a] else 0

inverseCaptcha2 :: String -> Integer
inverseCaptcha2 xs =
  sum $ zipWith f xs (drop n xs ++ take n xs)
    where
      f a b = if a == b then read [a] else 0
      n = length xs `div` 2

part1 = sum . map inverseCaptcha . lines
part2 = sum . map inverseCaptcha2. lines

cliMain :: IO ()
cliMain = do
  input <- readFile "../inputs/01-1.txt"
  print $ part2 input
