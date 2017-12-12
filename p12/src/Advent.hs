module Advent where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Text.Parsec
import Text.ParserCombinators.Parsec.Number
import Control.Arrow (second)
import Data.List (foldl', unfoldr)
import Debug.Trace (traceShowId)

type Graph = Map.Map Int (Set.Set Int)

newtype Program = Program { unProgram :: (Int, [Int]) }
  deriving (Eq, Show, Ord)

pProgram = do
  self <- nat
  string " <-> "
  adjacents <- nat `sepBy` string ", "
  return $ Program (self, adjacents)

pPrograms = pProgram `endBy` newline <* eof

makeGraph :: [Program] -> Graph
makeGraph = Map.fromList . map (second Set.fromList . unProgram)

dfs :: Int -> Set.Set Int -> Graph -> [Int]
dfs i seen m =
  let adj = Map.findWithDefault Set.empty i m
      seen' = Set.union seen adj
  in concatMap (\i -> i : dfs i seen' m) $ Set.filter (flip Set.notMember seen) adj

sizeOfConnectedComponent :: Graph -> Int
sizeOfConnectedComponent = Set.size . Set.fromList . dfs 0 Set.empty -- . traceShowId

countComponents :: Graph -> Int
countComponents = length . unfoldr f
  where
    f m = do
      ((k, _), _) <- Map.minViewWithKey m
      let group = dfs k Set.empty m
          m' = foldl' (\acc x -> Map.delete x acc) m group
      pure (group, m')

part1 :: String -> Int
part1 = either (error . show) (sizeOfConnectedComponent . makeGraph) . parse pPrograms ""

part2 :: String -> Int
part2 = either (error . show) (countComponents . makeGraph) . parse pPrograms ""

cliMain :: IO ()
cliMain = do
  input <- readFile "../inputs/12.txt"
  print $ part2 input
