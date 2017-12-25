{-# LANGUAGE NamedFieldPuns #-}
module Advent where

import Control.Monad.RWS.Strict
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Debug.Trace (traceShowId)
import Text.Parsec
import Text.ParserCombinators.Parsec.Number

type Tape = Map Int Int

data TuringState = TuringState
  { tape :: Tape
  , position :: Int
  , machineState :: MachineStateLabel
  } deriving (Eq, Show)

type Turing = RWS Blueprint () TuringState

type MachineStateLabel = Char

data Action = Action
  { writeValue :: Int
  , moveDirection :: Int
  , nextState :: MachineStateLabel
  } deriving (Eq, Show)

data StateInstructions = StateInstructions
  { if0 :: Action
  , if1 :: Action
  } deriving (Eq, Show)

data Blueprint = Blueprint
  { startState :: MachineStateLabel
  , checksumAfter :: Int
  , allStateInstructions :: Map MachineStateLabel StateInstructions
  } deriving (Eq, Show)

pStateLabel = anyChar

pStartState = do
  string "Begin in state "
  startState <- pStateLabel
  char '.'
  pure $ startState

pChecksumAfter = do
  string "Perform a diagnostic checksum after "
  numOfSteps <- int
  string " steps."
  pure $ numOfSteps

pDirection = choice
  [ try (string "left") *> pure (-1)
  , string "right" *> pure 1
  ]

pAction = do
  spaces
  string "If the current value is "
  val <- int
  char ':'
  newline
  spaces
  string "- Write the value "
  valToWrite <- int
  char '.'
  newline
  spaces
  string "- Move one slot to the "
  direction <- pDirection
  char '.'
  newline
  spaces
  string "- Continue with state "
  nextState <- pStateLabel
  char '.'
  pure $ Action valToWrite direction nextState

pStateInstruction = do
  string "In state "
  stateLabel <- pStateLabel
  char ':'
  newline
  action0 <- pAction
  newline
  action1 <- pAction
  pure (stateLabel, StateInstructions action0 action1)

pBlueprint = do
  startState <- pStartState
  newline
  checksumAfter <- pChecksumAfter
  newline
  newline
  allStateInstructions <- pStateInstruction `endBy` (many newline)
  pure $ Blueprint startState checksumAfter (Map.fromList allStateInstructions)

initialState = TuringState Map.empty 0 'A'

currentValue :: Turing Int
currentValue = do
  TuringState { tape, position } <- get
  pure $ Map.findWithDefault 0 position tape

currentInstruction :: Turing StateInstructions
currentInstruction = do
  Blueprint {allStateInstructions} <- ask
  TuringState { machineState } <- get
  pure $ allStateInstructions Map.! machineState

doAction :: Action -> Turing ()
doAction (Action writeValue offset nextState) = do
  TuringState { tape, position } <- get
  let tape' = Map.insert position writeValue tape
      position' = position + offset
  put $! TuringState tape' position' nextState

step :: Turing ()
step = do
  Blueprint {allStateInstructions} <- ask
  val <- currentValue
  StateInstructions if0 if1 <- currentInstruction
  if val == 0 then doAction if0 else doAction if1

executeInstructions :: Turing Int
executeInstructions = do
  Blueprint { startState, checksumAfter } <- ask
  replicateM_ checksumAfter step
  TuringState { tape } <- get
  pure . length . filter (== 1) $ Map.elems tape

part1 :: String -> Int
part1 = either (error . show) (\x -> fst $ evalRWS executeInstructions x initialState) . parse pBlueprint ""

cliMain :: IO ()
cliMain = do
  input <- readFile "../inputs/25.txt"
  print $ part1 input
