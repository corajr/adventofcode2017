{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module Advent where

import Control.Monad.RWS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Debug.Trace (traceShowId)
import qualified Data.Vector as V

type Position = (Int, Int)

data Direction = North | East | South | West
  deriving (Eq, Show, Ord)

type Grid = Map Position Bool

data CarrierState = CarrierState
  { grid :: Grid
  , position :: Position
  , direction :: Direction
  } deriving (Eq, Show)

type CarrierRWS = RWS () (Sum Int) CarrierState

offset :: Position -> Direction -> Position
offset (a, b) dir = case dir of
  North -> (a, b - 1)
  South -> (a, b + 1)
  East -> (a + 1, b)
  West -> (a - 1, b)

turnLeft, turnRight :: Direction -> Direction
turnLeft North = West
turnLeft West = South
turnLeft South = East
turnLeft East = North

turnRight North = East
turnRight West = North
turnRight South = West
turnRight East = South

parseGrid :: String -> Grid
parseGrid = centerCoords . Map.fromList . f . zip [0..] . lines
  where
    f :: [(Int, String)] -> [(Position, Bool)]
    f = concatMap (\(i, xs) -> map (\(pos, c) -> (pos, c == '#')) $ zipWith (\j y -> ((j, i), y)) [0..] xs)

centerCoords :: Grid -> Grid
centerCoords m = Map.mapKeys f m
  where
    (maxX, maxY) = fst (Map.findMax m)
    midX = maxX `div` 2
    midY = maxY `div` 2
    f (x, y) = (x - midX, y - midY)

initialState :: Grid -> CarrierState
initialState m = CarrierState m (0, 0) North

peekAt :: Position -> CarrierRWS Bool
peekAt position = do
  CarrierState { grid } <- get
  pure $ Map.findWithDefault False position grid

getCurrent :: CarrierRWS Bool
getCurrent = do
  CarrierState { position } <- get
  peekAt position

move :: CarrierRWS ()
move = do
  CarrierState { position, direction } <- get
  modify (\s -> s { position = offset position direction })

maybeTurn :: CarrierRWS ()
maybeTurn = do
  nodeInfected <- getCurrent
  let turn = if nodeInfected then turnRight else turnLeft
  modify (\s -> s { direction = turn (direction s)})

infectOrClean :: CarrierRWS ()
infectOrClean = do
  CarrierState { position } <- get
  nodeInfected <- peekAt position
  let nowInfected = not nodeInfected
  when nowInfected $ tell (Sum 1)
  modify (\s -> s { grid = Map.insert position nowInfected $ grid s })

step :: CarrierRWS ()
step = do
  maybeTurn
  infectOrClean
  move

runCarrier :: Int -> String -> Int
runCarrier n xs = wInfections
  where
    (_, _, (Sum wInfections)) = runRWS (replicateM n step) () starting
    grid = parseGrid xs
    starting = initialState grid

part1 :: Int -> String -> Int
part1 = runCarrier

cliMain :: IO ()
cliMain = do
  input <- readFile "../inputs/22.txt"
  print $ part1 10000 input
