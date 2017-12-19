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

type Maze = Map Position Char

data PacketState = PacketState
  { position :: Position
  , direction :: Direction
  } deriving (Eq, Show)

type MazeRWS = RWS Maze (String, Sum Int) PacketState

offset :: Position -> Direction -> Position
offset (a, b) dir = case dir of
  North -> (a, b - 1)
  South -> (a, b + 1)
  East -> (a + 1, b)
  West -> (a - 1, b)

parseMaze :: String -> Maze
parseMaze = Map.fromList . f . zip [0..] . lines
  where
    f :: [(Int, String)] -> [(Position, Char)]
    f = concatMap (\(i, xs) -> filter (\(_,b) -> b /= ' ') $ zipWith (\j y -> ((j, i), y)) [0..] xs)

initialState :: Maze -> PacketState
initialState m = PacketState pos South
  where
    pos = head $ filter (\(_, y) -> y == 0) $ Map.keys m

peekAt :: Position -> MazeRWS (Maybe Char)
peekAt position = do
  maze <- ask
  pure $ Map.lookup position maze

getCurrent :: MazeRWS (Maybe Char)
getCurrent = do
  PacketState { position } <- get
  peekAt position

move :: Direction -> MazeRWS ()
move dir = do
  PacketState { position } <- get
  tell ("", Sum 1)
  put $ PacketState (offset position dir) dir

crossroads :: MazeRWS ()
crossroads = do
  PacketState { position, direction } <- get
  let nextDirs = case direction of
        South -> [East, West]
        North -> [East, West]
        East -> [North, South]
        West -> [North, South]
  possibleNextDirs <- mapM (\x -> (x,) <$> peekAt (offset position x)) nextDirs
  let (dir, _) = head $ filter (isJust . snd) $ possibleNextDirs
  move dir

navigate :: MazeRWS ()
navigate = do
  current <- getCurrent
  PacketState { direction } <- get
  case current of
    Just '|' -> move direction >> navigate
    Just '-' -> move direction >> navigate
    Just '+' -> crossroads >> navigate
    Just x -> tell ([x], Sum 0) >> move direction >> navigate
    Nothing -> pure ()

doMaze :: String -> (String, Int)
doMaze xs = (wChars, wSteps)
  where
    (_, _, (wChars, Sum wSteps)) = runRWS navigate maze starting
    maze = parseMaze xs
    starting = initialState maze

part1 :: String -> String
part1 = fst . doMaze

part2 :: String -> Int
part2 = snd . doMaze

cliMain :: IO ()
cliMain = do
  input <- readFile "../inputs/19.txt"
  print $ part2 input
