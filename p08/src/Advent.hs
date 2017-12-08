module Advent where

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Control.Monad.State
import Debug.Trace (traceShowId)
import Text.Parsec (manyTill, anyChar, lookAhead, try, space, string, parse, many, newline, eof, (<|>), choice)
import Text.ParserCombinators.Parsec.Number (int)
import qualified Data.Map.Strict as Map

type Register = String

data Comparison = OP_LT
                | OP_GT
                | OP_LE
                | OP_GE
                | OP_EQ
                | OP_NE
                deriving (Eq, Show, Ord)

data Instruction = Instruction
  { register :: Register
  , incDec :: Int
  , condition :: (Register, Comparison, Int)
  } deriving (Eq, Show, Ord)

pRegister = manyTill anyChar (lookAhead . try $ space)

pCompareOp =
  choice [ try (string "!=") *> pure OP_NE
         , try (string "==") *> pure OP_EQ
         , try (string ">=") *> pure OP_GE
         , try (string ">") *> pure OP_GT
         , try (string "<=") *> pure OP_LE
         , (string "<") *> pure OP_LT
         ]

pInstruction = do
  register <- pRegister
  space
  incDec <- (try (string "inc") *> space *> int) <|> (string "dec" *> space *> (fmap negate int))
  string " if "
  regCond <- pRegister
  space
  op <- pCompareOp
  space
  val <- int
  return $ Instruction register incDec (regCond, op, val)

pInstructions = many (pInstruction <* newline) <* eof

type ProgState = State (Map.Map String Int, Int)

findMax :: ProgState Int
findMax = (maximum . Map.elems . fst) <$> get

compareOp :: Comparison -> Int -> Int -> Bool
compareOp op a b =
  case op of
    OP_EQ -> a == b
    OP_NE -> a /= b
    OP_LT -> a < b
    OP_GT -> a > b
    OP_LE -> a <= b
    OP_GE -> a >= b

step :: Instruction -> ProgState ()
step (Instruction reg incDec (cond, op, val)) = do
  (registers, _) <- get
  let currentVal = Map.findWithDefault 0 reg registers
      condVal = Map.findWithDefault 0 cond registers
  when (compareOp op condVal val) $ do
    modify (\(a, b) -> (Map.insert reg (currentVal + incDec) a, b))
    newMax <- findMax
    modify (\(a, b) -> (a, if newMax > b then newMax else b))

part1 :: String -> Int
part1 x =
  case parse pInstructions "" x of
    Right inst -> evalState (mapM_ step inst >> findMax) (Map.empty, 0)
    Left err -> error (show err)

part2 :: String -> Int
part2 x =
  case parse pInstructions "" x of
    Right inst -> snd $ execState (mapM_ step inst) (Map.empty, 0)
    Left err -> error (show err)

cliMain :: IO ()
cliMain = do
  input <- readFile "../inputs/08.txt"
  print $ part2 input
