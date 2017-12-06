module Advent where

import Control.Monad (forM_)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

type MemoryBanks = V.Vector Int

parseBanks :: String -> MemoryBanks
parseBanks = V.fromList . map read . words

step :: MemoryBanks -> MemoryBanks
step v = V.modify f v
  where
    maxI = V.maxIndex v
    maxVal = v V.! maxI
    n = V.length v
    f v = do
      MV.write v maxI 0
      forM_ [maxI + 1 .. maxI + maxVal] $ \i ->
        MV.modify v (+1) (i `mod` n)

countToLoop :: MemoryBanks -> Int
countToLoop x = go (Set.singleton x) x
  where
    go acc x =
      let x' = step x
      in if x' `Set.member` acc
         then Set.size acc
         else go (Set.insert x' acc) x'

countToLoopFromSeen :: MemoryBanks -> Int
countToLoopFromSeen x = go (Map.singleton x 0) x
  where
    go acc x =
      let x' = step x
          maybeI = Map.lookup x' acc
          n = Map.size acc
      in case maybeI of
        Just i -> n - i
        Nothing -> go (Map.insert x' n acc) x'

part1 = countToLoop . parseBanks
part2 = countToLoopFromSeen . parseBanks

cliMain :: IO ()
cliMain = do
  input <- readFile "../inputs/06.txt"
  print $ part2 input
