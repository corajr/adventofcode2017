{-# LANGUAGE NamedFieldPuns #-}
module Advent where

import Control.Monad.RWS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import Debug.Trace (traceShowId)
import qualified Data.Vector as V
import Text.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Number (int)

type Registers = Map Char Int

data MachineState = MachineState
  { registers :: Registers
  , programCounter :: Int
  , soundsPlayed :: [Int]
  , soundsRecovered :: [Int]
  } deriving (Eq, Show)

type Program = V.Vector Instruction

type ProgramRWS = RWS Program () MachineState

type Register = Char

data Value = Reg Register
           | Value Int
           deriving (Eq, Show)

data Instruction = Snd Value
                 | Set Register Value
                 | Add Register Value
                 | Mul Register Value
                 | Mod Register Value
                 | Rcv Value
                 | Jgz Value Value
                 deriving (Eq, Show)

pRegister = anyChar
pValue = try (Value <$> int) <|> Reg <$> pRegister

pSet = Set <$> (string "set " *> pRegister) <*> (space *> pValue)
pAdd = Add <$> (string "add " *> pRegister) <*> (space *> pValue)
pMul = Mul <$> (string "mul " *> pRegister) <*> (space *> pValue)
pMod = Mod <$> (string "mod " *> pRegister) <*> (space *> pValue)
pRcv = Rcv <$> (string "rcv " *> pValue)
pSnd = Snd <$> (string "snd " *> pValue)
pJgz = Jgz <$> (string "jgz " *> pValue) <*> (space *> pValue)

pInstruction = choice $
  map try [pAdd, pRcv, pMul, pMod, pSnd, pSet] ++ [pJgz]

pProgram = V.fromList <$> pInstruction `endBy` newline

initialState :: MachineState
initialState = MachineState Map.empty 0 [] []

getInstruction :: ProgramRWS Instruction
getInstruction = do
  instructions <- ask
  MachineState { programCounter } <- get
  pure $ instructions V.! programCounter

valueOf :: Value -> ProgramRWS Int
valueOf (Value x) = pure x
valueOf (Reg r) = do
  MachineState { registers } <- get
  pure $ Map.findWithDefault 0 r registers

play :: Int -> ProgramRWS ()
play i = do
  modify (\s -> s { soundsPlayed = soundsPlayed s ++ [i] })

changeReg :: (Int -> Int) -> Char -> ProgramRWS ()
changeReg f r =
  modify (\s -> s { registers = Map.alter g r (registers s) })
  where g Nothing = Just $ f 0
        g (Just x) = Just $ f x

set :: Char -> Int -> ProgramRWS ()
set r i = changeReg (const i) r

add :: Char -> Int -> ProgramRWS ()
add r i = changeReg (+ i) r

mul :: Char -> Int -> ProgramRWS ()
mul r i = changeReg (* i) r

mod' :: Char -> Int -> ProgramRWS ()
mod' r i = changeReg (`mod` i) r

rcv :: Value -> ProgramRWS ()
rcv x = valueOf x >>= \x' ->
  if x' > 0
  then modify (\s -> s { soundsRecovered = soundsRecovered s ++ [last (soundsPlayed s)] } )
  else pure ()

jump :: Value -> Value -> ProgramRWS ()
jump x y = do
  x' <- valueOf x
  y' <- valueOf y
  if x' > 0
  then modify (\s -> s { programCounter = programCounter s + y' - 1})
  else pure ()

execute :: Instruction -> ProgramRWS ()
execute (Snd x) = valueOf x >>= play
execute (Set x y) = valueOf y >>= set x
execute (Add x y) = valueOf y >>= add x
execute (Mul x y) = valueOf y >>= mul x
execute (Mod x y) = valueOf y >>= mod' x
execute (Rcv x) = rcv x
execute (Jgz x y) = jump x y

moveToNext :: ProgramRWS ()
moveToNext = do
  instructions <- ask
  MachineState { programCounter, soundsRecovered } <- get
  if programCounter < 0 || programCounter >= (V.length instructions - 1) || not (null soundsRecovered)
  then pure ()
  else modify (\s -> s { programCounter = programCounter + 1}) >> step

step :: ProgramRWS ()
step = do
  inst <- getInstruction
  execute inst
  moveToNext

getFirstRecoveredFrequency :: ProgramRWS Int
getFirstRecoveredFrequency = do
  MachineState { soundsRecovered } <- get
  pure $ head soundsRecovered

part1 :: String -> Int
part1 = either (error . show) f . parse pProgram ""
  where f xs = g $ runRWS (step >> getFirstRecoveredFrequency) xs initialState
        g (a, s, w) = a

part2 :: String -> Int
part2 = undefined

cliMain :: IO ()
cliMain = do
  input <- readFile "../inputs/18.txt"
  print $ part1 input
