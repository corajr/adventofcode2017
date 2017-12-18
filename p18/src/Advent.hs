module Advent where

import Control.Monad.RWS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Number (int)

type Registers = Map Char Int

data MachineState = MachineState
  { registers :: Registers
  , programCounter :: Int
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
initialState = MachineState Map.empty 0

getFirstRecoveredFrequency :: ProgramRWS Int
getFirstRecoveredFrequency = pure 0

part1 :: String -> Int
part1 = either (error . show) f . parse pProgram ""
  where f xs = g $ runRWS getFirstRecoveredFrequency xs initialState
        g (a, s, w) = a

part2 :: String -> Int
part2 = undefined

cliMain :: IO ()
cliMain = do
  input <- readFile "../inputs/18.txt"
  print $ part1 input
