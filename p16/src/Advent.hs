module Advent where

import Data.Maybe (fromMaybe)
import Data.List (foldl')
import Text.Parsec
import Text.ParserCombinators.Parsec.Number
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

type DancerState = V.Vector Char

data Move = Spin Int
          | Exchange Int Int
          | Partner Char Char
          deriving (Eq, Show, Ord)

pSpin = Spin <$> (char 's' *> nat)

pExchange = Exchange <$> (char 'x' *> nat) <*> (char '/' *> nat)

pPartner = Partner <$> (char 'p' *> anyChar) <*> (char '/' *> anyChar)

pMove = choice
  [ try pSpin
  , try pExchange
  , pPartner
  ]

pMoves = pMove `sepBy` char ','

initialState :: DancerState
initialState = V.enumFromTo 'a' 'p'

executeMove :: DancerState -> Move -> DancerState
executeMove starting move = case move of
  Spin x -> let (before, after) = V.splitAt (V.length starting - x) starting
            in after V.++ before
  Exchange i j -> V.modify (\v -> MV.swap v i j) starting
  Partner a b -> fromMaybe starting $ do
    i <- V.elemIndex a starting
    j <- V.elemIndex b starting
    pure $ V.modify (\v -> MV.swap v i j) starting

part1 :: String -> String
part1 = either (error . show) (V.toList . foldl' executeMove initialState). parse (pMoves) ""

part2 :: String -> String
part2 = either (error . show) (V.toList . f). parse (pMoves) ""
  where
    f moves = foldl' executeMove initialState (take 1000000000 $ cycle moves)

cliMain :: IO ()
cliMain = do
  input <- readFile "../inputs/16.txt"
  print $ part2 input
