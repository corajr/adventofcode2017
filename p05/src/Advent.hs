{-# LANGUAGE BangPatterns #-}
module Advent where

import Data.List (unfoldr)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

data State = State
  { instructions :: V.Vector Int
  , programCounter :: Int
  }

parseInstructions :: String -> V.Vector Int
parseInstructions = V.fromList . map read . lines

step :: State -> Maybe ((), State)
step (State instructions programCounter) =
  if programCounter < 0 || programCounter >= V.length instructions
  then Nothing
  else Just ((), State instructions' (programCounter + current))
  where
    current = instructions V.! programCounter
    instructions' = V.modify f instructions
    f v = MV.read v programCounter >>= \x -> MV.write v programCounter (x + 1)

step2 :: State -> Int
step2 = go 0
  where
    go !acc (State !instructions !programCounter)
      | programCounter < 0 || programCounter >= V.length instructions = acc
      | otherwise = go (acc + 1) (State instructions' (programCounter + current))
          where
            current = instructions V.! programCounter
            instructions' = V.modify f instructions
            f v = MV.read v programCounter >>= \x -> MV.write v programCounter (if x >= 3 then x - 1 else x + 1)

initialState :: V.Vector Int -> State
initialState v = State v 0

part1 = length . unfoldr step . initialState . parseInstructions
part2 = step2 . initialState . parseInstructions

cliMain :: IO ()
cliMain = do
  input <- readFile "../inputs/05.txt"
  print $ part2 input
