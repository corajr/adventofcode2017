module AdventSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Text.Parsec
import Control.Monad (forM_, replicateM)
import qualified Data.Map.Strict as Map
-- import Control.Monad.State.Strict (execState, evalState)

import Advent

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

ex f input output =
  it (show input ++ " ==> " ++ show output) $
    f input `shouldBe` output

exampleAction = unlines
  [ "  If the current value is 0:"
  , "    - Write the value 1."
  , "    - Move one slot to the right."
  , "    - Continue with state B."
  ]


exampleActionParsed = Action 1 1 'B'

exampleStateInstruction = unlines
  [ "In state A:"
  , "  If the current value is 0:"
  , "    - Write the value 1."
  , "    - Move one slot to the right."
  , "    - Continue with state B."
  , "  If the current value is 1:"
  , "    - Write the value 0."
  , "    - Move one slot to the left."
  , "    - Continue with state B."
  ]

exampleStateInstructionParsed = StateInstructions
  (Action 1 1 'B')
  (Action 0 (-1) 'B')

exampleBlueprint = unlines
  [ "Begin in state A."
  , "Perform a diagnostic checksum after 6 steps."
  , ""
  , "In state A:"
  , "  If the current value is 0:"
  , "    - Write the value 1."
  , "    - Move one slot to the right."
  , "    - Continue with state B."
  , "  If the current value is 1:"
  , "    - Write the value 0."
  , "    - Move one slot to the left."
  , "    - Continue with state B."
  , ""
  , "In state B:"
  , "  If the current value is 0:"
  , "    - Write the value 1."
  , "    - Move one slot to the left."
  , "    - Continue with state A."
  , "  If the current value is 1:"
  , "    - Write the value 1."
  , "    - Move one slot to the right."
  , "    - Continue with state A."
  ]

exampleStateInstructions =
  [ ('A', StateInstructions (Action 1 1 'B') (Action 0 (-1) 'B'))
  , ('B', StateInstructions (Action 1 (-1) 'A') (Action 1 1 'A'))
  ]

exampleBlueprintParsed = Blueprint
  { startState = 'A'
  , checksumAfter = 6
  , allStateInstructions = Map.fromList exampleStateInstructions
  }

spec :: Spec
spec = do
  describe "pAction" $ do
    let ex' = ex (parse pAction "")
    ex' exampleAction (Right exampleActionParsed)
  describe "pStateInstruction" $ do
    let ex' = ex (parse pStateInstruction "")
    ex' exampleStateInstruction (Right ('A', exampleStateInstructionParsed))
  describe "pBlueprint" $ do
    let ex' = ex (parse pBlueprint "")
    ex' exampleBlueprint (Right exampleBlueprintParsed)
  describe "part1" $ do
    let ex' = ex part1
    ex' exampleBlueprint 3
