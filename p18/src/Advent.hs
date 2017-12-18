{-# LANGUAGE NamedFieldPuns #-}
module Advent where

import Data.Int (Int64)
import Control.Monad.RWS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import Debug.Trace (traceShowId)
import qualified Data.Vector as V
import Text.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Number (int)

type Registers = Map Char Int64

data MachineState = MachineState
  { registers :: Registers
  , programCounter :: Int
  , messagesSent :: [Int64]
  , messagesReceived :: [Int64]
  } deriving (Eq, Show)

type Program = V.Vector Instruction

data Condition = Running | Blocked | Done
  deriving (Eq, Show)

type ProgramRWS = RWS Program [Int64] MachineState

type Register = Char

data Value = Reg Register
           | Value Int64
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

valueOf :: Value -> ProgramRWS Int64
valueOf (Value x) = pure x
valueOf (Reg r) = do
  MachineState { registers } <- get
  pure $ Map.findWithDefault 0 r registers

send :: Int64 -> ProgramRWS ()
send i = do
  tell [i]
  modify (\s -> s { messagesSent = messagesSent s ++ [i] })

changeReg :: (Int64 -> Int64) -> Char -> ProgramRWS ()
changeReg f r =
  modify (\s -> s { registers = Map.alter g r (registers s) })
  where g Nothing = Just $ f 0
        g (Just x) = Just $ f x

set :: Char -> Int64 -> ProgramRWS ()
set r i = changeReg (const i) r

add :: Char -> Int64 -> ProgramRWS ()
add r i = changeReg (+ i) r

mul :: Char -> Int64 -> ProgramRWS ()
mul r i = changeReg (* i) r

mod' :: Char -> Int64 -> ProgramRWS ()
mod' r i = changeReg (`mod` i) r

rcv :: Value -> ProgramRWS ()
rcv x = valueOf x >>= \x' ->
  if x' > 0
  then modify (\s -> s { messagesReceived = messagesReceived s ++ [last (messagesSent s)] } )
  else pure ()

rcv2 :: Value -> ProgramRWS Condition
rcv2 (Value _) = error "invalid program: rcv with integer value"
rcv2 (Reg r) = do
  MachineState { messagesReceived } <- get
  case messagesReceived of
    [] -> pure Blocked
    (x:xs) -> do
      set r x
      modify (\s -> s { messagesReceived = xs } ) >> pure Running

jump :: Value -> Value -> ProgramRWS ()
jump x y = do
  x' <- valueOf x
  y' <- valueOf y
  if x' > 0
  then modify (\s -> s { programCounter = programCounter s + fromIntegral y' - 1})
  else pure ()

execute :: Instruction -> ProgramRWS ()
execute (Snd x) = valueOf x >>= send
execute (Set x y) = valueOf y >>= set x
execute (Add x y) = valueOf y >>= add x
execute (Mul x y) = valueOf y >>= mul x
execute (Mod x y) = valueOf y >>= mod' x
execute (Rcv x) = rcv x
execute (Jgz x y) = jump x y

moveToNext :: ProgramRWS ()
moveToNext = do
  instructions <- ask
  MachineState { programCounter, messagesReceived } <- get
  if programCounter < 0 || programCounter >= (V.length instructions - 1) || not (null messagesReceived)
  then pure ()
  else modify (\s -> s { programCounter = programCounter + 1}) >> step

step :: ProgramRWS ()
step = do
  inst <- getInstruction
  execute inst
  moveToNext

moveToNext2 :: ProgramRWS Condition
moveToNext2 = do
  instructions <- ask
  MachineState { programCounter, messagesReceived } <- get
  if programCounter < 0 || programCounter >= (V.length instructions - 1)
  then pure Done
  else modify (\s -> s { programCounter = programCounter + 1}) >> pure Running

execute2 :: Instruction -> ProgramRWS Condition
execute2 (Snd x) = valueOf x >>= send >> pure Running
execute2 (Set x y) = valueOf y >>= set x >> pure Running
execute2 (Add x y) = valueOf y >>= add x >> pure Running
execute2 (Mul x y) = valueOf y >>= mul x >> pure Running
execute2 (Mod x y) = valueOf y >>= mod' x >> pure Running
execute2 (Rcv x) = rcv2 x
execute2 (Jgz x y) = jump x y >> pure Running

step2 :: Condition -> ProgramRWS Condition
step2 Done = pure Done
step2 Blocked = pure Blocked
step2 Running = do
  inst <- getInstruction
  continue' <- execute2 inst
  case continue' of
    Running -> moveToNext2
    Blocked -> pure Blocked
    Done -> pure Done

getFirstMessage :: ProgramRWS Int64
getFirstMessage = do
  MachineState { messagesReceived } <- get
  pure $ head messagesReceived

nextState :: Condition -> [Int64] -> Condition
nextState Done _ = Done
nextState Running _ = Running
nextState Blocked xs = if not (null xs) then Running else Blocked

runBoth :: Program -> Int
runBoth program = go initialState 0 Running (initialState { registers = Map.singleton 'p' 1}) 0 Running
  where
    go p0 p0sentCount p0continue p1 p1sentCount p1continue =
      let (continue0, s0, w0) = runRWS (step2 p0continue) program p0
          (continue1, s1, w1) = runRWS (step2 p1continue) program p1
          p0' = s0 { messagesSent = [], messagesReceived = messagesReceived s0 ++ w1 }
          p1' = s1 { messagesSent = [], messagesReceived = messagesReceived s1 ++ w0 }
          continue0' = nextState continue0 (messagesReceived p0')
          continue1' = nextState continue1 (messagesReceived p1')
          done = (continue0' == Blocked && continue1' == Blocked) || (continue0' == Done && continue1' == Done)
      in if done
         then p1sentCount
         else go p0' (p0sentCount + length w0) continue0' p1' (p1sentCount + length w1) continue1'

part1 :: String -> Int
part1 = either (error . show) f . parse pProgram ""
  where f xs = g $ runRWS (step >> fromIntegral <$> getFirstMessage) xs initialState
        g (a, s, w) = a

part2 :: String -> Int
part2 = either (error . show) runBoth . parse pProgram ""

cliMain :: IO ()
cliMain = do
  input <- readFile "../inputs/18.txt"
  print $ part2 input
