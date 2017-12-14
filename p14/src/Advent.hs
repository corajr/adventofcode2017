module Advent where

import Control.Arrow ((&&&))
import Control.Monad.State
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Word (Word8)
import Data.Bits (popCount, xor, testBit)
import Data.Char (ord)
import Data.Tuple (swap)
import Data.List (foldl', unfoldr)
import qualified Data.Vector.Unboxed as V
import Debug.Trace (traceShowId)
import GHC.Base (assert)
import Text.Printf
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.DFS (noComponents)

type Coord = (Int, Int)
type CoordAdjacency = Map.Map Coord (Set.Set Coord)

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

denseHash :: String -> [Word8]
denseHash xs = dense
  where
    lengths = map ord xs ++ [17, 31, 73, 47, 23]
    start = initialState 256
    sparse = elements $ execState (replicateM 64 (doRound lengths)) start
    splitInto16s v
      | V.null v = []
      | otherwise = let (x, rest) = V.splitAt 16 v in x : splitInto16s rest
    groups = splitInto16s sparse
    dense = map (fromIntegral . V.foldr1' xor) groups

part1 :: String -> Int
part1 xs = sum (map (sum . map popCount) hashes)
  where
    strings = map (\i -> xs ++ "-" ++ show i) [0..127]
    hashes = map denseHash strings

makeDenseHashMatrix :: String -> [[Bool]]
makeDenseHashMatrix xs = mat'
  where
    strings = map (\i -> xs ++ "-" ++ show i) [0..127]
    hashes = map denseHash strings
    toBinary x = [ testBit x i | i <- [7,6..0]]
    mat = map (concatMap toBinary) hashes
    mat' = if length mat == 128 && length (head mat) == 128 then mat else error (show mat)

makeSetOfBoolMatrix :: [[Bool]] -> Set.Set Coord
makeSetOfBoolMatrix = Set.fromList . concatMap withIndices . withIndex . map withIndex
  where
    withIndex xs = zip [0..] xs
    withIndices (i, xs) = concatMap (\(j, v) -> if v then [(i,j)] else []) xs

makeAdjacentsFromHash :: String -> CoordAdjacency
makeAdjacentsFromHash xs = Map.fromList $ map (id &&& neighbors) (Set.elems set)
  where
    set = makeSetOfBoolMatrix $ makeDenseHashMatrix xs
    neighbors (i, j) = set `Set.intersection` Set.fromList [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]

toGraph :: CoordAdjacency -> Gr Coord ()
toGraph m = mkGraph mNodes mEdges
  where
    mNodes = zip [0..] (Map.keys m)
    coordToNode = Map.fromList (map swap mNodes)
    mEdges = concatMap toEdges $ Map.toList m
    toEdges (n, xs) = let n' = f n in map (\x -> (n', f x, ())) (Set.toList xs)
    f coord = Map.findWithDefault (error $ show coord) coord coordToNode

part2 :: String -> Int
part2 = noComponents . toGraph . makeAdjacentsFromHash

cliMain :: IO ()
cliMain = do
  let input = "ljoxqyyw"
  print $ part2 input
