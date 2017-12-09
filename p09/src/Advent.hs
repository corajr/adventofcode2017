module Advent where

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Control.Monad.State
import Debug.Trace (traceShowId)
import Text.Parsec (manyTill, anyChar, lookAhead, try, char, parse, many, eof, (<|>), choice, optionMaybe, sepBy)
import Text.ParserCombinators.Parsec.Number (int)
import qualified Data.Map.Strict as Map

data Stream = Garbage Int
            | Group [Stream]
            deriving (Eq, Show, Ord)

pGarbageChar = do
  cancel <- optionMaybe (char '!')
  anyChar
  case cancel of
    Just _ -> return 0
    Nothing -> return 1

pGarbage = do
  char '<'
  contents <- manyTill pGarbageChar (try (char '>'))
  return $ Garbage (sum contents)

pGroup = do
  char '{'
  contents <- pGroupContents `sepBy` (char ',')
  char '}'
  return $ Group contents

pGroupContents = choice
  [ try pGarbage
  , pGroup
  ]

scoreStream :: Stream -> Int
scoreStream = go 1
  where
    go _ (Garbage _) = 0
    go n (Group xs) =
      n + (sum $ map (go (n + 1)) xs)

scoreGarbage :: Stream -> Int
scoreGarbage (Garbage n) = n
scoreGarbage (Group xs) = sum (map scoreGarbage xs)

part1 :: String -> Int
part1 = either (error . show) scoreStream . parse (pGroup <* eof) ""

part2 :: String -> Int
part2 = either (error . show) scoreGarbage . parse (pGroup <* eof) ""

cliMain :: IO ()
cliMain = do
  input <- readFile "../inputs/09.txt"
  print $ part2 (init input)
