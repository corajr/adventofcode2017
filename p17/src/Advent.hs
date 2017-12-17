module Advent where

import Control.Monad.State.Strict
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.List (unfoldr)

type Buffer = Seq Int

data BufferState = BufferState
  { buffer :: Buffer
  , currentPosition :: Int
  , value :: Int
  } deriving (Eq, Show)

type SpinlockState = State BufferState

step :: Int -> SpinlockState ()
step stepSize = do
  BufferState buffer i value <- get
  let n = Seq.length buffer
      i' = (i + stepSize) `mod` n
      i'' = (i' + 1) `mod` (n + 1)
      buffer' = Seq.insertAt i'' value buffer
      value' = value + 1
  put $! (BufferState buffer' i'' value')

getAfter :: SpinlockState Int
getAfter = do
  BufferState buffer i _ <- get
  pure $ Seq.index buffer ((i + 1) `mod` Seq.length buffer)

get1 :: SpinlockState Int
get1 = do
  BufferState buffer _ _ <- get
  pure $ Seq.index buffer 1

getAfter0 :: SpinlockState Int
getAfter0 = do
  BufferState buffer _ _ <- get
  let i = maybe (error "0 not found") id $ Seq.elemIndexL 0 buffer
  pure $ Seq.index buffer ((i + 1) `mod` Seq.length buffer)

initialState :: BufferState
initialState = BufferState (Seq.singleton 0) 0 1

part1 :: Int -> Int
part1 stepSize =
  evalState (replicateM 2017 (step stepSize) >> getAfter) initialState

part2brute :: Int -> Int -> Int
part2brute n stepSize =
  evalState (replicateM n (step stepSize) >> get1) initialState

-- part2values = Set.toList . Set.fromList $ map (\n -> part2brute n 394) [1..3000]

insertionPositions :: Int -> [(Int, Int)]
insertionPositions stepSize = iterate f (0, 1)
  where
    f (i, n) = let i' = (i + stepSize) `mod` n
                   i'' = (i' + 1) `mod` (n + 1)
               in (i'', n + 1)

position1inserts = map ((\x -> x - 1) . snd) . filter (\(i, _) -> i == 1) . insertionPositions

part2 n = last . takeWhile (<= n) . position1inserts

cliMain :: IO ()
cliMain = do
  let input = 394
      n = 50000000
  print $ part2 n input
