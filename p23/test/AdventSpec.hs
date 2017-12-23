module AdventSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Text.Parsec
import Control.Monad (forM_, replicateM)
import qualified Data.Vector as V
-- import Control.Monad.State.Strict (execState, evalState)

import Advent

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

ex f input output =
  it (show input ++ " ==> " ++ show output) $
    f input `shouldBe` output

exampleInstructions = unlines
  [ "set a 1"
  , "add a 2"
  , "mul a a"
  , "mod a 5"
  , "snd a"
  , "set a 0"
  , "rcv a"
  , "jgz a -1"
  , "set a 1"
  , "jgz a -2"
  ]

exampleInstructions2 = unlines
  [ "snd 1"
  , "snd 2"
  , "snd p"
  , "rcv a"
  , "rcv b"
  , "rcv c"
  , "rcv d"
  ]

exampleInstructionsParsed = V.fromList
  [ Set 'a' (Value 1)
  , Add 'a' (Value 2)
  , Mul 'a' (Reg 'a')
  , Mod 'a' (Value 5)
  , Snd (Reg 'a')
  , Set 'a' (Value 0)
  , Rcv (Reg 'a')
  , Jgz (Reg 'a') (Value (-1))
  , Set 'a' (Value 1)
  , Jgz (Reg 'a') (Value (-2))
  ]



spec :: Spec
spec = do
  describe "pInstruction" $ do
    let ex' = ex (parse pInstruction "")
        instWithParses = zip (lines exampleInstructions) (V.toList exampleInstructionsParsed)
    forM_ instWithParses $ \(inst, parsed) ->
      ex' inst (Right parsed)
  describe "pProgram" $ do
    it "parses a complete program" $
      parse pProgram "" exampleInstructions `shouldBe` Right exampleInstructionsParsed
  describe "part1" $ do
    let ex' = ex part1
    ex' exampleInstructions 4
  describe "part2" $ do
    let ex' = ex part2
    ex' exampleInstructions2 3
