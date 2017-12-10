module Advent where

import Control.Monad.State
import Data.Bits (xor)
import Data.Char (ord)
import qualified Data.Vector.Unboxed as V
import Text.Printf

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

doRound :: [Int] -> KnotHash ()
doRound = mapM_ applyLength

part1 :: Int -> [Int] -> Int
part1 n lengths = evalState (doRound lengths >> f) (initialState n)
  where f = get >>= (return . V.product . V.take 2 . elements)

part2 :: String -> String
part2 xs = denseHash
  where
    lengths = map ord xs ++ [17, 31, 73, 47, 23]
    start = initialState 256
    sparse = elements $ execState (replicateM 64 (doRound lengths)) start
    splitInto16s v
      | V.null v = []
      | otherwise = let (x, rest) = V.splitAt 16 v in x : splitInto16s rest
    groups = splitInto16s sparse
    dense = map (V.foldr1' xor) groups
    denseHash = concatMap (printf "%02x") dense

cliMain :: IO ()
cliMain = do
  let input1 = [206,63,255,131,65,80,238,157,254,24,133,2,16,0,1,3]
      input2 = "206,63,255,131,65,80,238,157,254,24,133,2,16,0,1,3"
  print $ part2 input2
