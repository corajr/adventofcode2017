module Advent where

import Control.Monad.State
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

data HashState = HashState
  { elements :: V.Vector Int
  , currentPosition :: Int
  , skipSize :: Int
  }

initialState :: Int -> HashState
initialState n = HashState
  { elements = V.enumFromN 0 n
  , currentPosition = 0
  , skipSize = 0
  }

type KnotHash = State HashState

reverseFrom :: Int -> Int -> V.Vector Int -> V.Vector Int
reverseFrom current len v =
  if current + len >= V.length v
  then V.concat [postwrap, mid, prewrap]
  else V.concat [before, reversed, after]
  where
    prewrapLen = V.length v - current
    postwrapLen = len - prewrapLen
    wrapped = V.slice current prewrapLen v V.++ V.slice 0 postwrapLen v
    wrappedReversed = V.reverse wrapped
    (prewrap, postwrap) = V.splitAt prewrapLen wrappedReversed
    mid = V.slice postwrapLen (current - postwrapLen) v
    before = V.slice 0 current v
    reversed = V.reverse (V.slice current len v)
    after = V.drop (current + len) v

applyLength :: Int -> KnotHash ()
applyLength len = do
  HashState el current skip <- get
  let el' = reverseFrom current len el
      newCurrent = (current + len + skip) `mod` V.length el
  put (HashState el' newCurrent (skip + 1))

part1 :: Int -> [Int] -> Int
part1 n lengths = evalState (mapM_ applyLength lengths >> f) (initialState n)
  where f = get >>= (return . V.product . V.take 2 . elements)

cliMain :: IO ()
cliMain = do
  let input = [206,63,255,131,65,80,238,157,254,24,133,2,16,0,1,3]
  print $ part1 256 input
