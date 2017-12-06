{-# LANGUAGE BangPatterns #-}
module Advent where

import Control.Monad.ST (runST)
-- import Data.STRef (newSTRef, readSTRef, modifySTRef')
import Data.List (unfoldr)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

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
step2 (State instructions pc) = runST $ do
  inst <- V.thaw instructions
  go 0 inst pc
  where
    n = V.length instructions
    go !acc !inst !programCounter
      | programCounter < 0 || programCounter >= n = pure acc
      | otherwise = do
          current <- MV.read inst programCounter
          MV.modify inst (\x -> if x >= 3 then x - 1 else x + 1) programCounter
          go (acc + 1) inst (programCounter + current)

initialState :: V.Vector Int -> State
initialState v = State v 0

part1 = length . unfoldr step . initialState . parseInstructions
part2 = step2 . initialState . parseInstructions

cliMain :: IO ()
cliMain = do
  input <- readFile "../inputs/05.txt"
  print $ part2 input
