{-# LANGUAGE NamedFieldPuns #-}
module Advent where

import Control.Monad.RWS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import Debug.Trace (traceShowId)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Text.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Number (int)

type Registers = Map Char Int

data MachineState = MachineState
  { registers :: Registers
  , programCounter :: Int
  } deriving (Eq, Show)

type Program = V.Vector Instruction

data Condition = Running | Blocked | Done
  deriving (Eq, Show)

type ProgramRWS = RWS Program (Sum Int) MachineState

type RWSOut a = (a, MachineState, Sum Int)

type Register = Char

data Value = Reg Register
           | Value Int
           deriving (Eq, Show)

data Instruction = Set Register Value
                 | Sub Register Value
                 | Mul Register Value
                 | Jnz Value Value
                 deriving (Eq, Show)

pRegister = anyChar
pValue = try (Value <$> int) <|> Reg <$> pRegister

pSet = Set <$> (string "set " *> pRegister) <*> (space *> pValue)
pSub = Sub <$> (string "sub " *> pRegister) <*> (space *> pValue)
pMul = Mul <$> (string "mul " *> pRegister) <*> (space *> pValue)
pJnz = Jnz <$> (string "jnz " *> pValue) <*> (space *> pValue)

pInstruction = choice $
  map try [pSet, pSub, pMul] ++ [pJnz]

pProgram = V.fromList <$> pInstruction `endBy` newline

initialState :: MachineState
initialState = MachineState Map.empty 0

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

changeReg :: (Int -> Int) -> Char -> ProgramRWS ()
changeReg f r =
  modify (\s -> s { registers = Map.alter g r (registers s) })
  where g Nothing = Just $ f 0
        g (Just x) = Just $ f x

set :: Char -> Int -> ProgramRWS ()
set r i = changeReg (const i) r

sub :: Char -> Int -> ProgramRWS ()
sub r i = changeReg (\x -> x - i) r

mul :: Char -> Int -> ProgramRWS ()
mul r i = changeReg (* i) r

jnz :: Value -> Value -> ProgramRWS ()
jnz x y = do
  x' <- valueOf x
  y' <- valueOf y
  if x' /= 0
  then modify (\s -> s { programCounter = programCounter s + fromIntegral y' - 1})
  else pure ()

execute :: Instruction -> ProgramRWS ()
execute (Set x y) = valueOf y >>= set x
execute (Sub x y) = valueOf y >>= sub x
execute (Mul x y) = tell (Sum 1) >> valueOf y >>= mul x
execute (Jnz x y) = jnz x y

moveToNext :: ProgramRWS Bool
moveToNext = do
  instructions <- ask
  MachineState { programCounter } <- get
  if programCounter < 0 || programCounter >= (V.length instructions - 1)
  then pure False
  else modify (\s -> s { programCounter = programCounter + 1}) >> pure True

step :: ProgramRWS Bool
step = do
  inst <- getInstruction
  execute inst
  moveToNext

stepAll = do
  shouldContinue <- step
  when shouldContinue $ stepAll

part1 :: String -> Int
part1 = either (error . show) f . parse pProgram ""
  where f xs = g $ runRWS stepAll xs initialState
        g (a, s, (Sum w)) = w

printRegisters :: MachineState -> IO ()
printRegisters (MachineState {registers}) = print registers

printInstruction :: Instruction -> IO ()
printInstruction = print

printAllState :: Program -> MachineState -> IO ()
printAllState program s = do
  let (a, _) = evalRWS getInstruction program s
      pc = programCounter s
  printRegisters s
  putStr (show pc ++ ": ") >> printInstruction a

debugInitialState = initialState { registers = Map.singleton 'a' 1}

debug :: String -> IO ()
debug x = go debugInitialState
  where
    program = either (error . show) id . parse pProgram "" $ x
    go s = do
      let (a, s', w) = runRWS step program s
      if a
        then printAllState program s >> putStrLn "" >> getLine >> go s'
        else pure ()

isPrime n
  | n `mod` 2 == 0 && n > 2 = False
  | otherwise = all (\i -> n `mod` i /= 0) [3, 5 .. ceiling (sqrt (fromIntegral n))]

-- Full disclosure: I did not figure this one out. :(
-- https://github.com/dp1/AoC17/blob/master/day23.5.txt
part2 =
  let b = 106500
      c = 123500
  in length $ filter (not . isPrime) [b, b + 17.. c]

cliMain :: IO ()
cliMain = do
  input <- readFile "../inputs/23.txt"
  print part2
  -- debug input
