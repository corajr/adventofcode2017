{-# LANGUAGE NamedFieldPuns #-}
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

data Status = Clean | Weakened | Infected | Flagged
  deriving (Eq, Show, Ord)

type Grid = Map Position Status

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

turnLeft, turnRight, reverseDir :: Direction -> Direction
turnLeft North = West
turnLeft West = South
turnLeft South = East
turnLeft East = North

turnRight North = East
turnRight West = North
turnRight South = West
turnRight East = South

reverseDir North = South
reverseDir West = East
reverseDir South = North
reverseDir East = West

parseGrid :: String -> Grid
parseGrid = centerCoords . Map.fromList . f . zip [0..] . lines
  where
    f :: [(Int, String)] -> [(Position, Status)]
    f = concatMap (\(i, xs) -> map (\(pos, c) -> (pos, g c)) $ zipWith (\j y -> ((j, i), y)) [0..] xs)
    g '#' = Infected
    g '.' = Clean

centerCoords :: Grid -> Grid
centerCoords m = Map.mapKeys f m
  where
    (maxX, maxY) = fst (Map.findMax m)
    midX = maxX `div` 2
    midY = maxY `div` 2
    f (x, y) = (x - midX, y - midY)

initialState :: Grid -> CarrierState
initialState m = CarrierState m (0, 0) North

peekAt :: Position -> CarrierRWS Status
peekAt position = do
  CarrierState { grid } <- get
  pure $ Map.findWithDefault Clean position grid

getCurrent :: CarrierRWS Status
getCurrent = do
  CarrierState { position } <- get
  peekAt position

move :: CarrierRWS ()
move = do
  CarrierState { position, direction } <- get
  modify (\s -> s { position = offset position direction })

maybeTurn :: CarrierRWS ()
maybeTurn = do
  nodeStatus <- getCurrent
  let turn = case nodeStatus of
               Infected -> turnRight
               Clean -> turnLeft
  modify (\s -> s { direction = turn (direction s)})

infectOrClean :: CarrierRWS ()
infectOrClean = do
  CarrierState { position } <- get
  nodeStatus <- peekAt position
  let newStatus = case nodeStatus of
                    Clean -> Infected
                    Infected -> Clean
  when (newStatus == Infected) $ tell (Sum 1)
  modify (\s -> s { grid = Map.insert position newStatus $ grid s })

step :: CarrierRWS ()
step = do
  maybeTurn
  infectOrClean
  move

maybeTurn2 :: CarrierRWS ()
maybeTurn2 = do
  nodeStatus <- getCurrent
  let turn = case nodeStatus of
               Clean -> turnLeft
               Weakened -> id
               Infected -> turnRight
               Flagged -> reverseDir
  modify (\s -> s { direction = turn (direction s)})

changeStatus :: CarrierRWS ()
changeStatus = do
  CarrierState { position } <- get
  nodeStatus <- peekAt position
  let newStatus = case nodeStatus of
                    Clean -> Weakened
                    Weakened -> Infected
                    Infected -> Flagged
                    Flagged -> Clean
  when (newStatus == Infected) $ tell (Sum 1)
  modify (\s -> s { grid = Map.insert position newStatus $ grid s })


step2 :: CarrierRWS ()
step2 = do
  maybeTurn2
  changeStatus
  move

runCarrier :: CarrierRWS () -> Int -> String -> Int
runCarrier f n xs = wInfections
  where
    (_, _, (Sum wInfections)) = runRWS (replicateM n f) () starting
    grid = parseGrid xs
    starting = initialState grid

part1 :: Int -> String -> Int
part1 = runCarrier step

part2 :: Int -> String -> Int
part2 = runCarrier step2

cliMain :: IO ()
cliMain = do
  input <- readFile "../inputs/22.txt"
  print $ part1 10000 input
  print $ part2 10000000 input
